# ============================================================================
# Package Constants and Defaults for asa Package
# ============================================================================
#
# Centralizes all hardcoded values that were previously scattered across files.
# Use these constants rather than hardcoding values in function signatures.
#
# ============================================================================

# ============================================================================
# BACKEND CONFIGURATION
# ============================================================================

#' Default Backend
#' @keywords internal
ASA_DEFAULT_BACKEND <- "openai"

#' Default Model
#' @keywords internal
ASA_DEFAULT_MODEL <- "gpt-4.1-mini"

#' Default Conda Environment
#' @keywords internal
ASA_DEFAULT_CONDA_ENV <- "asa_env"

#' Default Proxy URL (Tor SOCKS5)
#' @keywords internal
ASA_DEFAULT_PROXY <- "socks5h://127.0.0.1:9050"

#' Supported Backends
#' @keywords internal
ASA_SUPPORTED_BACKENDS <- c("openai", "groq", "xai", "exo", "openrouter")

# ============================================================================
# API ENDPOINTS
# ============================================================================

#' Backend API Endpoints
#' @keywords internal
ASA_API_ENDPOINTS <- list(

  openai = "https://api.openai.com/v1",
  xai = "https://api.x.ai/v1",
  openrouter = "https://openrouter.ai/api/v1"
)

#' Environment Variables for API Keys
#' @keywords internal
ASA_API_KEY_ENV_VARS <- list(
  openai = "OPENAI_API_KEY",
  groq = "GROQ_API_KEY",
  xai = "XAI_API_KEY",
  openrouter = "OPENROUTER_API_KEY",
  exo = NULL  # Local server, no API key needed
)

# ============================================================================
# MEMORY FOLDING DEFAULTS
# ============================================================================

#' Default Memory Folding Enabled
#' @keywords internal
ASA_DEFAULT_MEMORY_FOLDING <- TRUE

#' Default Memory Threshold (messages before folding)
#' @keywords internal
ASA_DEFAULT_MEMORY_THRESHOLD <- 4L

#' Default Messages to Keep After Folding
#' @keywords internal
ASA_DEFAULT_MEMORY_KEEP_RECENT <- 2L

# ============================================================================
# RATE LIMITING AND TIMEOUTS
# ============================================================================
#' Default Rate Limit (requests per second)
#' @keywords internal
ASA_DEFAULT_RATE_LIMIT <- 0.2

#' Default Request Timeout (seconds)
#' @keywords internal
ASA_DEFAULT_TIMEOUT <- 120L

#' Default Inter-Search Delay (seconds)
#' @keywords internal
ASA_DEFAULT_INTER_SEARCH_DELAY <- 0.5

#' Default Max Retries
#' @keywords internal
ASA_DEFAULT_MAX_RETRIES <- 3L

#' Rate Limit Wait Time (seconds)
#' @keywords internal
ASA_RATE_LIMIT_WAIT <- 15L

# ============================================================================
# RECURSION LIMITS
# ============================================================================

#' Recursion Limit with Memory Folding
#' @keywords internal
ASA_RECURSION_LIMIT_FOLDING <- 100L

#' Recursion Limit without Memory Folding
#' @keywords internal
ASA_RECURSION_LIMIT_STANDARD <- 20L

# ============================================================================
# SEARCH DEFAULTS
# ============================================================================

#' Default Max Search Results
#' @keywords internal
ASA_DEFAULT_MAX_RESULTS <- 10L

#' Default Page Load Wait (seconds)
#' @keywords internal
ASA_DEFAULT_PAGE_LOAD_WAIT <- 2.0

#' Default Wikipedia Top K Results
#' @keywords internal
ASA_DEFAULT_WIKI_TOP_K <- 5L

#' Default Wikipedia Content Chars
#' @keywords internal
ASA_DEFAULT_WIKI_CHARS <- 1000L

# ============================================================================
# ENUMERATION DEFAULTS
# ============================================================================

#' Default Max Workers for Enumeration
#' @keywords internal
ASA_DEFAULT_WORKERS <- 4L

#' Default Max Rounds for Enumeration
#' @keywords internal
ASA_DEFAULT_MAX_ROUNDS <- 8L

#' Default Budget: Queries
#' @keywords internal
ASA_DEFAULT_BUDGET_QUERIES <- 50L

#' Default Budget: Tokens
#' @keywords internal
ASA_DEFAULT_BUDGET_TOKENS <- 200000L

#' Default Budget: Time (seconds)
#' @keywords internal
ASA_DEFAULT_BUDGET_TIME <- 300L

#' Default Plateau Rounds for Stopping
#' @keywords internal
ASA_DEFAULT_PLATEAU_ROUNDS <- 2L

#' Default Minimum Novelty Rate
#' @keywords internal
ASA_DEFAULT_NOVELTY_MIN <- 0.05

#' Default Novelty Window
#' @keywords internal
ASA_DEFAULT_NOVELTY_WINDOW <- 20L

# ============================================================================
# BACKEND-SPECIFIC TEMPERATURES
# ============================================================================

#' Default Temperatures by Backend
#' @keywords internal
ASA_DEFAULT_TEMPERATURES <- list(
  openai = 0.5,
  groq = 0.01,
  xai = 0.5,
  exo = 0.01,
  openrouter = 0.5
)

# ============================================================================
# OUTPUT FORMATS
# ============================================================================

#' Valid Output Formats
#' @keywords internal
ASA_OUTPUT_FORMATS <- c("text", "json", "raw")

#' Valid Temporal Time Filters
#' @keywords internal
ASA_TIME_FILTERS <- c("d", "w", "m", "y")

#' Valid Temporal Strictness Levels
#' @keywords internal
ASA_TEMPORAL_STRICTNESS <- c("best_effort", "strict")

# ============================================================================
# STATUS CODES
# ============================================================================

#' Status Code: Success
#' @keywords internal
ASA_STATUS_SUCCESS <- 200L

#' Status Code: Error
#' @keywords internal
ASA_STATUS_ERROR <- 100L

# ============================================================================
# PRINT FORMATTING
# ============================================================================

#' Print Width for Output
#' @keywords internal
ASA_PRINT_WIDTH <- 76L

#' String Truncation Length
#' @keywords internal
ASA_TRUNCATE_LENGTH <- 80L

# ============================================================================
# HELPER: Get Default with Option Override
# ============================================================================

#' Get Package Option or Default
#'
#' Returns the value of an asa package option, or the default if not set.
#' Options can be set via options(asa.option_name = value).
#'
#' @param name Option name (without "asa." prefix)
#' @param default Default value if option not set
#' @return Option value or default
#' @keywords internal
.asa_option <- function(name, default) {
  getOption(paste0("asa.", name), default = default)
}

#' Get Default Backend
#' @keywords internal
.get_default_backend <- function() {

  .asa_option("default_backend", ASA_DEFAULT_BACKEND)
}

#' Get Default Model
#' @keywords internal
.get_default_model <- function() {
  .asa_option("default_model", ASA_DEFAULT_MODEL)
}

#' Get Default Conda Environment
#' @keywords internal
.get_default_conda_env <- function() {
  .asa_option("default_conda_env", ASA_DEFAULT_CONDA_ENV)
}

#' Get Default Workers
#' @keywords internal
.get_default_workers <- function() {
  .asa_option("default_workers", ASA_DEFAULT_WORKERS)
}
