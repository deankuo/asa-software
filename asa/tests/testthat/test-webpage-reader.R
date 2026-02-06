# test-webpage-reader.R
# Tests for the optional webpage reader tool (allow_read_webpages gate)

.extract_first_excerpt <- function(open_webpage_output) {
  if (!grepl("[1]", open_webpage_output, fixed = TRUE)) {
    return("")
  }
  excerpt <- sub("(?s).*\\[1\\]\\n", "", open_webpage_output, perl = TRUE)
  excerpt <- sub("(?s)\\n\\[2\\].*$", "", excerpt, perl = TRUE)
  excerpt
}

test_that("OpenWebpage tool is gated by allow_read_webpages", {
  python_path <- asa_test_skip_if_no_python(required_files = "webpage_tool.py")
  asa_test_skip_if_missing_python_modules(
    c("curl_cffi", "bs4", "pydantic", "langchain_core")
  )

  webpage_tool <- asa_test_import_from_path_or_skip("webpage_tool", python_path)
  cfg_prev <- webpage_tool$configure_webpage_reader()

  on.exit({
    try(webpage_tool$configure_webpage_reader(
      allow_read_webpages = cfg_prev$allow_read_webpages,
      relevance_mode = cfg_prev$relevance_mode,
      embedding_provider = cfg_prev$embedding_provider,
      embedding_model = cfg_prev$embedding_model,
      prefilter_k = cfg_prev$prefilter_k,
      use_mmr = cfg_prev$use_mmr,
      mmr_lambda = cfg_prev$mmr_lambda,
      cache_enabled = cfg_prev$cache_enabled
    ), silent = TRUE)
    try(webpage_tool$clear_webpage_reader_cache(), silent = TRUE)
  }, add = TRUE)

  tool <- webpage_tool$create_webpage_reader_tool()

  webpage_tool$configure_webpage_reader(
    allow_read_webpages = FALSE,
    relevance_mode = "lexical",
    cache_enabled = FALSE
  )
  out_disabled <- tool$`_run`(
    url = "https://connorjerzak.com/collaborators/",
    query = "Europe"
  )
  expect_match(out_disabled, "Webpage reading is disabled", fixed = TRUE)
  expect_match(out_disabled, "allow_read_webpages=FALSE", fixed = TRUE)

  webpage_tool$configure_webpage_reader(
    allow_read_webpages = TRUE,
    relevance_mode = "lexical",
    cache_enabled = FALSE
  )
  out_enabled <- tool$`_run`(
    url = "http://localhost/",
    query = "test"
  )
  expect_match(out_enabled, "Refusing to open URL", fixed = TRUE)
})

test_that(".with_webpage_reader_config toggles Python allow_read_webpages", {
  python_path <- asa_test_skip_if_no_python(required_files = "webpage_tool.py")
  asa_test_skip_if_missing_python_modules(
    c("curl_cffi", "bs4", "pydantic", "langchain_core")
  )

  conda_env <- tryCatch(asa:::.get_default_conda_env(), error = function(e) NULL)
  if (is.null(conda_env) || !is.character(conda_env) || !nzchar(conda_env)) {
    skip("Default conda env not available")
  }

  webpage_tool <- asa_test_import_from_path_or_skip("webpage_tool", python_path)
  cfg_prev <- webpage_tool$configure_webpage_reader()

  inside <- tryCatch(
    asa:::.with_webpage_reader_config(
      allow_read_webpages = TRUE,
      relevance_mode = "lexical",
      max_chars = 12345,
      max_chunks = 2,
      chunk_chars = 2000,
      conda_env = conda_env,
      fn = function() {
        cfg <- webpage_tool$configure_webpage_reader()
        list(
          allow_read_webpages = cfg$allow_read_webpages,
          max_chars = cfg$max_chars,
          max_chunks = cfg$max_chunks,
          chunk_chars = cfg$chunk_chars
        )
      }
    ),
    error = function(e) {
      skip(paste0("Webpage reader config wrapper unavailable: ", conditionMessage(e)))
    }
  )

  expect_true(isTRUE(inside$allow_read_webpages))
  expect_equal(inside$max_chars, 12345)
  expect_equal(inside$max_chunks, 2)
  expect_equal(inside$chunk_chars, 2000)

  cfg_after <- webpage_tool$configure_webpage_reader()
  expect_equal(cfg_after$allow_read_webpages, cfg_prev$allow_read_webpages)
  expect_equal(cfg_after$max_chars, cfg_prev$max_chars)
  expect_equal(cfg_after$max_chunks, cfg_prev$max_chunks)
  expect_equal(cfg_after$chunk_chars, cfg_prev$chunk_chars)
})

test_that("OpenWebpage can read collaborators page (live network)", {
  skip_on_cran()

  python_path <- asa_test_skip_if_no_python(required_files = "webpage_tool.py")
  asa_test_skip_if_missing_python_modules(
    c("curl_cffi", "bs4", "pydantic", "langchain_core")
  )

  webpage_tool <- asa_test_import_from_path_or_skip("webpage_tool", python_path)
  cfg_prev <- webpage_tool$configure_webpage_reader()

  on.exit({
    try(webpage_tool$configure_webpage_reader(
      allow_read_webpages = cfg_prev$allow_read_webpages,
      relevance_mode = cfg_prev$relevance_mode,
      embedding_provider = cfg_prev$embedding_provider,
      embedding_model = cfg_prev$embedding_model,
      prefilter_k = cfg_prev$prefilter_k,
      use_mmr = cfg_prev$use_mmr,
      mmr_lambda = cfg_prev$mmr_lambda,
      cache_enabled = cfg_prev$cache_enabled
    ), silent = TRUE)
    try(webpage_tool$clear_webpage_reader_cache(), silent = TRUE)
  }, add = TRUE)

  webpage_tool$configure_webpage_reader(
    allow_read_webpages = TRUE,
    relevance_mode = "lexical",
    chunk_chars = 2000,
    max_chunks = 2,
    cache_enabled = FALSE
  )

  tool <- webpage_tool$create_webpage_reader_tool()
  out <- tool$`_run`(
    url = "https://connorjerzak.com/collaborators/",
    query = "Europe:"
  )

  excerpt1 <- .extract_first_excerpt(out)
  expect_true(nchar(excerpt1) > 0)

  pos_europe <- regexpr("Europe:", excerpt1, fixed = TRUE)
  expect_true(pos_europe[1] > -1)

  after <- substring(excerpt1, pos_europe[1] + attr(pos_europe, "match.length"))
  lines <- trimws(unlist(strsplit(after, "\n", fixed = TRUE)))
  lines <- lines[lines != ""]
  lines <- lines[!grepl("^[\\-–—]+$", lines)]

  expect_true(length(lines) >= 1)
  expect_true(grepl("Adel Daoud", lines[1], fixed = TRUE))
})

test_that("Gemini reasons about fetched webpage content (live)", {
  skip_on_cran()
  skip_if(
    tolower(Sys.getenv("ASA_CI_SKIP_API_TESTS")) %in% c("true", "1", "yes"),
    "ASA_CI_SKIP_API_TESTS is set"
  )
  skip_if(
    !any(nzchar(Sys.getenv(c("GOOGLE_API_KEY", "GEMINI_API_KEY")))),
    "Missing GOOGLE_API_KEY or GEMINI_API_KEY"
  )

  agent <- asa::initialize_agent(
    backend = "gemini",
    model = "gemini-2.0-flash"
  )

  result <- asa::run_task(
    prompt = paste(
      "Open this webpage: https://connorjerzak.com/collaborators/",
      "and tell me the name of one collaborator listed under Europe."
    ),
    agent = agent,
    allow_read_webpages = TRUE
  )

  # If the LLM saw the webpage content, it can name a real collaborator
  expect_match(
    result$message,
    "Adel Daoud|Olga Gasparyan|Miguel Rueda",
    ignore.case = TRUE
  )
})
