asa_test_import_langgraph_module <- function(module_name,
                                             required_files = paste0(module_name, ".py"),
                                             required_modules = ASA_TEST_LANGGRAPH_MODULES,
                                             initialize = TRUE) {
  python_path <- asa_test_skip_if_no_python(
    required_files = required_files,
    initialize = initialize
  )
  if (!is.null(required_modules)) {
    asa_test_require_langgraph_stack(required_modules)
  }
  reticulate::import_from_path(module_name, path = python_path)
}


asa_test_parse_agent_json <- function(state_or_response, backend = "gemini") {
  response_text <- if (is.character(state_or_response) && length(state_or_response) > 0) {
    as.character(state_or_response[[1]])
  } else {
    asa:::.extract_response_text(state_or_response, backend = backend)
  }

  list(
    response_text = response_text,
    parsed = jsonlite::fromJSON(response_text)
  )
}


asa_test_invoke_json_agent <- function(agent,
                                       expected_schema = NULL,
                                       prompt = "Return JSON only.",
                                       input_state = NULL,
                                       config = list(recursion_limit = as.integer(3)),
                                       expected_schema_source = NULL,
                                       backend = "gemini") {
  state <- if (is.null(input_state)) {
    list(messages = list(list(role = "user", content = prompt)))
  } else {
    input_state
  }

  if (!is.null(expected_schema)) {
    state$expected_schema <- expected_schema
  }
  if (!is.null(expected_schema_source) && is.null(state$expected_schema_source)) {
    state$expected_schema_source <- expected_schema_source
  }

  final_state <- agent$invoke(state, config = config)
  parsed_payload <- asa_test_parse_agent_json(final_state, backend = backend)

  list(
    final_state = final_state,
    response_text = parsed_payload$response_text,
    parsed = parsed_payload$parsed
  )
}
