# Tests for RemainingSteps-based recursion_limit handling in LangGraph graphs.

.get_python_path_langgraph <- function() {
  python_path <- system.file("python", package = "asa")
  if (python_path == "" || !dir.exists(python_path)) {
    # Development path fallback
    python_path <- file.path(getwd(), "inst/python")
    if (!dir.exists(python_path)) {
      python_path <- file.path(dirname(getwd()), "asa/inst/python")
    }
  }
  python_path
}

.skip_if_no_python_langgraph <- function() {
  python_path <- .get_python_path_langgraph()
  if (!dir.exists(python_path)) {
    skip("Python modules not found")
  }
  if (!reticulate::py_available(initialize = FALSE)) {
    skip("Python not available")
  }
  python_path
}

.skip_if_missing_python_modules_langgraph <- function(modules) {
  if (is.null(modules) || length(modules) == 0) {
    return(invisible(TRUE))
  }

  for (module in modules) {
    ok <- tryCatch({
      reticulate::import(module, convert = FALSE)
      TRUE
    }, error = function(e) FALSE)

    if (!ok) {
      skip(paste0("Python module not available: ", module))
    }
  }

  invisible(TRUE)
}

test_that("research graph stops with stop_reason='recursion_limit' (RemainingSteps)", {
  python_path <- .skip_if_no_python_langgraph()
  .skip_if_missing_python_modules_langgraph(c(
    "langchain_core",
    "langgraph",
    "langgraph.prebuilt",
    "pydantic",
    "requests"
  ))

  research <- reticulate::import_from_path("research_graph", path = python_path)

  # Stub LLM for planner node: returns minimal valid JSON.
  reticulate::py_run_string(paste0(
    "class _StubResponse:\n",
    "    def __init__(self, content):\n",
    "        self.content = content\n",
    "        self.usage_metadata = {'total_tokens': 0}\n",
    "\n",
    "class _StubLLM:\n",
    "    def invoke(self, messages):\n",
    "        return _StubResponse('{\"entity_type\":\"test\",\"wikidata_type\":null,\"search_queries\":[]}')\n",
    "\n",
    "stub_llm = _StubLLM()\n"
  ))

  cfg <- research$ResearchConfig(
    max_rounds = as.integer(999),
    budget_queries = as.integer(9999),
    budget_tokens = as.integer(999999),
    budget_time_sec = as.integer(999999),
    plateau_rounds = as.integer(0),
    novelty_min = as.numeric(0),
    use_wikidata = FALSE,
    use_web = FALSE,
    use_wikipedia = FALSE
  )

  graph <- research$create_research_graph(
    llm = reticulate::py$stub_llm,
    tools = list(),
    config = cfg,
    checkpointer = NULL,
    wikidata_tool = NULL
  )

  initial_state <- list(
    query = "test query",
    schema = list(name = "character"),
    config = list(use_web = FALSE, use_wikidata = FALSE, use_wikipedia = FALSE),
    plan = list(),
    entity_type = "",
    wikidata_type = NULL,
    results = list(),
    new_results = list(),
    seen_hashes = list(),
    novelty_history = list(),
    round_number = 0L,
    queries_used = 0L,
    tokens_used = 0L,
    start_time = 0,
    status = "planning",
    stop_reason = NULL,
    errors = list()
  )

  final_state <- NULL
  expect_silent({
    # With a very small recursion_limit, the graph should stop gracefully using
    # RemainingSteps (instead of raising GraphRecursionError).
    final_state <- graph$invoke(
      initial_state,
      config = list(recursion_limit = as.integer(3))
    )
  })

  expect_equal(final_state$status, "complete")
  expect_equal(final_state$stop_reason, "recursion_limit")
})
