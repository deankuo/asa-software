# test-selenium-tier.R
# Deterministic tests that exercise the Selenium browser tier against local HTML fixtures.

.get_python_path_selenium <- function() {
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

.skip_if_no_python_selenium <- function() {
  python_path <- .get_python_path_selenium()
  if (python_path == "" || !dir.exists(python_path)) {
    skip("Python modules not found")
  }
  conda_env <- tryCatch(asa:::.get_default_conda_env(), error = function(e) NULL)
  if (!is.null(conda_env) && is.character(conda_env) && nzchar(conda_env)) {
    try(reticulate::use_condaenv(conda_env, required = FALSE), silent = TRUE)
  }
  if (!reticulate::py_available(initialize = TRUE)) {
    skip("Python not available")
  }
  python_path
}

.skip_if_missing_python_modules_selenium <- function(modules) {
  if (is.null(modules) || length(modules) == 0) {
    return(invisible(TRUE))
  }
  for (module in modules) {
    if (!reticulate::py_module_available(module)) {
      skip(paste0("Python module not available: ", module))
    }
  }
  invisible(TRUE)
}

.import_custom_ddg_selenium <- function(python_path) {
  tryCatch(
    reticulate::import_from_path("custom_ddg_production", path = python_path),
    error = function(e) {
      skip(paste0("Failed to import custom_ddg_production: ", conditionMessage(e)))
    }
  )
}

.file_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = TRUE))
}

test_that("Selenium tier can load and parse a local result fixture", {
  skip_on_cran()
  run_selenium <- tolower(Sys.getenv("ASA_RUN_SELENIUM_TESTS")) %in% c("true", "1", "yes")
  skip_if(!run_selenium, "Set ASA_RUN_SELENIUM_TESTS=true to run Selenium tier tests")

  python_path <- .skip_if_no_python_selenium()
  .skip_if_missing_python_modules_selenium(c("selenium", "bs4"))

  ddg <- .import_custom_ddg_selenium(python_path)

  setTimeLimit(elapsed = 60, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)

  fixture <- tempfile(fileext = ".html")
  writeLines(
    c(
      "<!doctype html>",
      "<html><head><meta charset='utf-8'></head><body>",
      "<a class='result__a' href='https://example.com'>Example</a>",
      "<div class='result__snippet'>Snippet text</div>",
      "</body></html>"
    ),
    fixture
  )

  cfg <- ddg$SearchConfig(
    max_results = 3L,
    timeout = 0.5,
    max_retries = 1L,
    retry_delay = 0.0,
    backoff_multiplier = 1.0,
    captcha_backoff_base = 0.0,
    page_load_wait = 0.0,
    inter_search_delay = 0.0,
    humanize_timing = FALSE,
    jitter_factor = 0.0,
    allow_direct_fallback = FALSE
  )

  res <- ddg$`_browser_search`(
    query = "irrelevant",
    max_results = 3L,
    proxy = NULL,
    headers = NULL,
    headless = TRUE,
    bypass_proxy_for_driver = TRUE,
    timeout = 0.5,
    max_retries = 1L,
    url_override = .file_url(fixture),
    config = cfg
  )

  expect_true(is.list(res) && length(res) >= 1)
  expect_equal(res[[1]][["_tier"]], "selenium")
  expect_match(res[[1]][["body"]], "__START_OF_SOURCE", fixed = TRUE)
})

test_that("Selenium tier raises a timeout on a local empty fixture", {
  skip_on_cran()
  run_selenium <- tolower(Sys.getenv("ASA_RUN_SELENIUM_TESTS")) %in% c("true", "1", "yes")
  skip_if(!run_selenium, "Set ASA_RUN_SELENIUM_TESTS=true to run Selenium tier tests")

  python_path <- .skip_if_no_python_selenium()
  .skip_if_missing_python_modules_selenium(c("selenium", "bs4"))

  ddg <- .import_custom_ddg_selenium(python_path)

  setTimeLimit(elapsed = 60, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)

  fixture <- tempfile(fileext = ".html")
  writeLines(
    c(
      "<!doctype html>",
      "<html><head><meta charset='utf-8'></head><body>",
      "<p>No results here</p>",
      "</body></html>"
    ),
    fixture
  )

  cfg <- ddg$SearchConfig(
    max_results = 3L,
    timeout = 0.2,
    max_retries = 1L,
    retry_delay = 0.0,
    backoff_multiplier = 1.0,
    captcha_backoff_base = 0.0,
    page_load_wait = 0.0,
    inter_search_delay = 0.0,
    humanize_timing = FALSE,
    jitter_factor = 0.0,
    allow_direct_fallback = FALSE
  )

  expect_error(
    ddg$`_browser_search`(
      query = "irrelevant",
      max_results = 3L,
      proxy = NULL,
      headers = NULL,
      headless = TRUE,
      bypass_proxy_for_driver = TRUE,
      timeout = 0.2,
      max_retries = 1L,
      url_override = .file_url(fixture),
      config = cfg
    ),
    regexp = "Timeout|timeout"
  )
})
