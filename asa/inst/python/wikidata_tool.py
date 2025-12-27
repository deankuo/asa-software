# wikidata_tool.py
#
# LangChain-compatible Wikidata SPARQL tool for authoritative entity enumeration.
# Provides structured queries for known entity types (senators, countries, etc.)
#
import logging
import time
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Union

import requests
from langchain_core.tools import BaseTool
from pydantic import Field

logger = logging.getLogger(__name__)

# ────────────────────────────────────────────────────────────────────────
# Configuration
# ────────────────────────────────────────────────────────────────────────
WIKIDATA_SPARQL_ENDPOINT = "https://query.wikidata.org/sparql"
DEFAULT_TIMEOUT = 60.0
DEFAULT_USER_AGENT = "ASA-Research-Agent/1.0 (https://github.com/cjerzak/asa-software)"


@dataclass
class WikidataConfig:
    """Configuration for Wikidata queries."""
    timeout: float = DEFAULT_TIMEOUT
    max_results: int = 500
    retry_count: int = 3
    retry_delay: float = 2.0


# Known entity type templates for common enumeration queries
ENTITY_TEMPLATES = {
    "us_senators": {
        "description": "Current United States Senators",
        "query": """
SELECT DISTINCT ?person ?personLabel ?state ?stateLabel ?party ?partyLabel
       ?termStart ?termEnd ?website
WHERE {
  ?person wdt:P31 wd:Q5 .  # instance of: human (filter out fictional)
  ?person wdt:P39 wd:Q4416090 .  # position held: United States senator
  ?person p:P39 ?statement .
  ?statement ps:P39 wd:Q4416090 .
  OPTIONAL { ?statement pq:P580 ?termStart . }
  OPTIONAL { ?statement pq:P582 ?termEnd . }
  OPTIONAL { ?statement pq:P768 ?state . }
  OPTIONAL { ?person wdt:P102 ?party . }
  OPTIONAL { ?person wdt:P856 ?website . }
  FILTER NOT EXISTS { ?statement pq:P582 ?end . FILTER(?end < NOW()) }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
ORDER BY ?stateLabel ?personLabel
""",
        "schema": ["name", "state", "party", "term_start", "term_end", "website", "wikidata_id"]
    },
    "us_representatives": {
        "description": "Current United States Representatives",
        "query": """
SELECT DISTINCT ?person ?personLabel ?district ?districtLabel ?state ?stateLabel ?party ?partyLabel
WHERE {
  ?person wdt:P39 wd:Q13218630 .  # position held: member of US House
  ?person p:P39 ?statement .
  ?statement ps:P39 wd:Q13218630 .
  OPTIONAL { ?statement pq:P768 ?district . }
  OPTIONAL { ?district wdt:P131 ?state . }
  OPTIONAL { ?person wdt:P102 ?party . }
  FILTER NOT EXISTS { ?statement pq:P582 ?end . FILTER(?end < NOW()) }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
ORDER BY ?stateLabel ?personLabel
""",
        "schema": ["name", "district", "state", "party", "wikidata_id"]
    },
    "countries": {
        "description": "All countries in the world",
        "query": """
SELECT DISTINCT ?country ?countryLabel ?capital ?capitalLabel ?population ?area ?iso3
WHERE {
  ?country wdt:P31 wd:Q6256 .  # instance of: country
  OPTIONAL { ?country wdt:P36 ?capital . }
  OPTIONAL { ?country wdt:P1082 ?population . }
  OPTIONAL { ?country wdt:P2046 ?area . }
  OPTIONAL { ?country wdt:P298 ?iso3 . }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
ORDER BY ?countryLabel
""",
        "schema": ["name", "capital", "population", "area", "iso3", "wikidata_id"]
    },
    "fortune500": {
        "description": "Fortune 500 companies",
        "query": """
SELECT DISTINCT ?company ?companyLabel ?ceo ?ceoLabel ?industry ?industryLabel
       ?founded ?headquarters ?headquartersLabel ?website
WHERE {
  ?company wdt:P361 wd:Q631014 .  # part of: Fortune 500
  OPTIONAL { ?company wdt:P169 ?ceo . }
  OPTIONAL { ?company wdt:P452 ?industry . }
  OPTIONAL { ?company wdt:P571 ?founded . }
  OPTIONAL { ?company wdt:P159 ?headquarters . }
  OPTIONAL { ?company wdt:P856 ?website . }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
ORDER BY ?companyLabel
""",
        "schema": ["name", "ceo", "industry", "founded", "headquarters", "website", "wikidata_id"]
    },
    "us_states": {
        "description": "US States and territories",
        "query": """
SELECT DISTINCT ?state ?stateLabel ?capital ?capitalLabel ?population ?admission ?abbreviation
WHERE {
  ?state wdt:P31 wd:Q35657 .  # instance of: US state
  OPTIONAL { ?state wdt:P36 ?capital . }
  OPTIONAL { ?state wdt:P1082 ?population . }
  OPTIONAL { ?state wdt:P571 ?admission . }
  OPTIONAL { ?state wdt:P5087 ?abbreviation . }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
ORDER BY ?stateLabel
""",
        "schema": ["name", "capital", "population", "admission_date", "abbreviation", "wikidata_id"]
    }
}


def _extract_wikidata_id(uri: str) -> str:
    """Extract Wikidata ID (Q-number) from URI."""
    if uri and "wikidata.org" in uri:
        return uri.split("/")[-1]
    return uri or ""


def _parse_sparql_results(results: Dict, entity_type: Optional[str] = None) -> List[Dict[str, Any]]:
    """Parse SPARQL JSON results into list of dictionaries."""
    parsed = []
    bindings = results.get("results", {}).get("bindings", [])

    for row in bindings:
        item = {}
        for key, value in row.items():
            val = value.get("value", "")
            # Handle Label fields specially
            if key.endswith("Label"):
                base_key = key[:-5]  # Remove "Label" suffix
                # Prefer the label over raw URI
                if base_key in item and item[base_key].startswith("http"):
                    item[base_key] = val
                elif base_key not in item:
                    item[base_key] = val
            elif key in ["person", "country", "company", "state"]:
                # Extract Wikidata ID for primary entity
                item["wikidata_id"] = _extract_wikidata_id(val)
                if f"{key}Label" not in row:
                    item["name"] = val
            else:
                # Skip raw URIs if we have a label
                if not val.startswith("http://www.wikidata.org"):
                    item[key] = val

        # Normalize common field names
        if "personLabel" in row:
            item["name"] = row["personLabel"].get("value", "")
        elif "countryLabel" in row:
            item["name"] = row["countryLabel"].get("value", "")
        elif "companyLabel" in row:
            item["name"] = row["companyLabel"].get("value", "")
        elif "stateLabel" in row and "name" not in item:
            item["name"] = row["stateLabel"].get("value", "")

        parsed.append(item)

    return parsed


def execute_sparql_query(
    query: str,
    config: Optional[WikidataConfig] = None,
    timeout: Optional[float] = None
) -> List[Dict[str, Any]]:
    """Execute a SPARQL query against Wikidata endpoint.

    Args:
        query: SPARQL query string
        config: Optional WikidataConfig for timeouts/retries
        timeout: Override timeout in seconds

    Returns:
        List of dictionaries with query results

    Raises:
        requests.RequestException: On network errors after retries
    """
    config = config or WikidataConfig()
    timeout = timeout or config.timeout

    headers = {
        "Accept": "application/sparql-results+json",
        "User-Agent": DEFAULT_USER_AGENT,
    }

    last_error = None
    for attempt in range(config.retry_count):
        try:
            logger.info(f"Executing Wikidata SPARQL query (attempt {attempt + 1}/{config.retry_count})")

            response = requests.get(
                WIKIDATA_SPARQL_ENDPOINT,
                params={"query": query, "format": "json"},
                headers=headers,
                timeout=timeout
            )
            response.raise_for_status()

            results = response.json()
            parsed = _parse_sparql_results(results)
            logger.info(f"Wikidata query returned {len(parsed)} results")
            return parsed

        except requests.exceptions.Timeout as e:
            logger.warning(f"Wikidata query timeout (attempt {attempt + 1}): {e}")
            last_error = e
            if attempt < config.retry_count - 1:
                time.sleep(config.retry_delay * (attempt + 1))

        except requests.exceptions.RequestException as e:
            logger.warning(f"Wikidata query failed (attempt {attempt + 1}): {e}")
            last_error = e
            if attempt < config.retry_count - 1:
                time.sleep(config.retry_delay * (attempt + 1))

    logger.error(f"Wikidata query failed after {config.retry_count} attempts")
    raise last_error


def get_known_entity_types() -> List[str]:
    """Return list of known entity types with pre-built queries."""
    return list(ENTITY_TEMPLATES.keys())


def get_entity_template(entity_type: str) -> Optional[Dict]:
    """Get the template for a known entity type."""
    return ENTITY_TEMPLATES.get(entity_type)


def query_known_entity(
    entity_type: str,
    config: Optional[WikidataConfig] = None
) -> List[Dict[str, Any]]:
    """Query Wikidata for a known entity type using pre-built query.

    Args:
        entity_type: One of the known entity types (us_senators, countries, etc.)
        config: Optional configuration

    Returns:
        List of entity dictionaries

    Raises:
        ValueError: If entity_type is not recognized
    """
    template = ENTITY_TEMPLATES.get(entity_type)
    if not template:
        raise ValueError(
            f"Unknown entity type: {entity_type}. "
            f"Known types: {', '.join(ENTITY_TEMPLATES.keys())}"
        )

    logger.info(f"Querying Wikidata for entity type: {entity_type}")
    return execute_sparql_query(template["query"], config)


def infer_entity_type(query: str) -> Optional[str]:
    """Attempt to infer the entity type from a natural language query.

    Args:
        query: Natural language query string

    Returns:
        Matched entity type or None if no match found
    """
    query_lower = query.lower()

    # Keywords for each entity type
    keyword_map = {
        "us_senators": ["senator", "senators", "senate", "us senate", "united states senator"],
        "us_representatives": ["representative", "representatives", "congress", "house of representatives", "congressman", "congresswoman"],
        "countries": ["countries", "country", "nations", "nation", "sovereign"],
        "fortune500": ["fortune 500", "fortune500", "largest companies", "biggest companies"],
        "us_states": ["us states", "american states", "states of america", "50 states"]
    }

    for entity_type, keywords in keyword_map.items():
        for keyword in keywords:
            if keyword in query_lower:
                logger.info(f"Inferred entity type '{entity_type}' from query: {query}")
                return entity_type

    return None


# ────────────────────────────────────────────────────────────────────────
# LangChain Tool Interface
# ────────────────────────────────────────────────────────────────────────
class WikidataSearchTool(BaseTool):
    """LangChain tool for querying Wikidata SPARQL endpoint.

    This tool provides authoritative enumeration of entities like
    US Senators, countries, Fortune 500 companies, etc.
    """

    name: str = "wikidata_search"
    description: str = """Search Wikidata for authoritative lists of entities.

    Best for enumeration queries like:
    - "all current US senators"
    - "all countries in the world"
    - "Fortune 500 companies"
    - "US states and their capitals"

    For known entity types, provides complete authoritative data with Wikidata IDs.
    For custom queries, can execute raw SPARQL.

    Input format:
    - For known types: "entity_type:us_senators" or "entity_type:countries"
    - For inference: just describe what you want, e.g., "all US senators"
    - For raw SPARQL: "sparql:SELECT ..."
    """

    config: WikidataConfig = Field(default_factory=WikidataConfig)

    def _run(self, query: str) -> str:
        """Execute Wikidata search and return formatted results."""
        try:
            # Check for explicit entity type prefix
            if query.startswith("entity_type:"):
                entity_type = query.split(":", 1)[1].strip()
                results = query_known_entity(entity_type, self.config)
                return self._format_results(results, entity_type)

            # Check for raw SPARQL prefix
            if query.startswith("sparql:"):
                sparql_query = query.split(":", 1)[1].strip()
                results = execute_sparql_query(sparql_query, self.config)
                return self._format_results(results)

            # Try to infer entity type from natural language
            entity_type = infer_entity_type(query)
            if entity_type:
                results = query_known_entity(entity_type, self.config)
                return self._format_results(results, entity_type)

            # No match - return guidance
            return (
                f"Could not match query to known entity type. "
                f"Known types: {', '.join(get_known_entity_types())}. "
                f"Use 'entity_type:TYPE' for explicit queries or try rephrasing."
            )

        except Exception as e:
            logger.error(f"Wikidata search error: {e}")
            return f"Error querying Wikidata: {str(e)}"

    def _format_results(
        self,
        results: List[Dict[str, Any]],
        entity_type: Optional[str] = None
    ) -> str:
        """Format results as human-readable string."""
        if not results:
            return "No results found."

        # Get schema if known entity type
        schema = None
        if entity_type and entity_type in ENTITY_TEMPLATES:
            schema = ENTITY_TEMPLATES[entity_type].get("schema", [])

        lines = [f"Found {len(results)} results:"]

        for i, item in enumerate(results[:50], 1):  # Limit display to first 50
            # Build display line with key fields
            parts = []
            if "name" in item:
                parts.append(item["name"])
            if "state" in item:
                parts.append(f"({item['state']})")
            if "party" in item:
                parts.append(f"[{item['party']}]")
            if "wikidata_id" in item:
                parts.append(f"[{item['wikidata_id']}]")

            lines.append(f"{i}. {' '.join(parts)}")

        if len(results) > 50:
            lines.append(f"... and {len(results) - 50} more results.")

        return "\n".join(lines)

    async def _arun(self, query: str) -> str:
        """Async version - just calls sync for now."""
        return self._run(query)


def create_wikidata_tool(config: Optional[WikidataConfig] = None) -> WikidataSearchTool:
    """Factory function to create a WikidataSearchTool instance.

    Args:
        config: Optional WikidataConfig for customization

    Returns:
        Configured WikidataSearchTool instance
    """
    return WikidataSearchTool(config=config or WikidataConfig())


# ────────────────────────────────────────────────────────────────────────
# Direct query functions for R integration
# ────────────────────────────────────────────────────────────────────────
def wikidata_query_senators() -> List[Dict[str, Any]]:
    """Query all current US Senators."""
    return query_known_entity("us_senators")


def wikidata_query_representatives() -> List[Dict[str, Any]]:
    """Query all current US Representatives."""
    return query_known_entity("us_representatives")


def wikidata_query_countries() -> List[Dict[str, Any]]:
    """Query all countries."""
    return query_known_entity("countries")


def wikidata_query_fortune500() -> List[Dict[str, Any]]:
    """Query Fortune 500 companies."""
    return query_known_entity("fortune500")


def wikidata_query_us_states() -> List[Dict[str, Any]]:
    """Query US states."""
    return query_known_entity("us_states")
