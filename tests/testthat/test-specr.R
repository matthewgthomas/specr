context("specr")

specs <- specr::setup(data = example_data,
      x = c("x1", "x2"),
      y = c("y1"),
      model = "lm",
      controls = c("c1"))


# test 1: check that the function returns an object of class 'specr.object'
test_that("function returns an object of class 'specr.object'", {
  expect_true(class(specr(specs, workers = 1)) == "specr.object")
})

# test 2: check that the function returns an error when no data is provided
test_that("function returns an error when no data is provided", {
  expect_error(specr(as_tibble(specs), data = NULL),
               "You provided a tibble or data.frame with all the specifications. In that case, you also need to provide the data set that should be used for the analyses.")
})


# test 3: check that the function returns an error when an incorrect object is provided
test_that("function returns an error when an incorrect object is provided", {
  expect_error(specr(1))
})

# test 4: check that the function returns an object of class 'tibble' when no specr.setup is provided
test_that("function returns an object of class 'tibble' when no specr.setup is provided", {
  expect_true(inherits(specr(as_tibble(specs),
                             data = example_data,
                             workers = 1),
                       "tbl_df"))
})


# test 4
test_that("Specs and results have the same number of rows", {
  results <- specr(specs)
  expect_equal(nrow(specs), nrow(results))
  expect_equal(results$n_failed, 0)
  expect_s3_class(results$failures, "tbl_df")
  expect_equal(nrow(results$failures), 0)
})

# test 5
test_that("Results include confidence intervals", {
  results <- specr(specs)
  expect_true(any(c("conf.low", "conf.high") %in% names(results$data)))
})


capture_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}


test_that("failed specifications are logged while successful models are returned", {
  failing_specs <- specr::setup(
    data = example_data,
    x = c("x1", "missing_x"),
    y = "y1",
    model = "lm"
  )

  captured <- capture_warnings(specr(failing_specs))
  results <- captured$value

  expect_length(captured$warnings, 1)
  expect_match(captured$warnings, "1 model specification failed")
  expect_match(captured$warnings, "`failures` element")
  expect_equal(results$n_specs, 1)
  expect_equal(results$n_failed, 1)
  expect_equal(results$failures$specification, 2)
  expect_equal(results$failures$x, "missing_x")
  expect_match(as.character(results$failures$formula), "missing_x")
  expect_match(results$failures$error, "object 'missing_x' not found", fixed = TRUE)
  expect_false("model_function" %in% names(results$failures))
  expect_equal(results$data$x, "x1")
})


test_that("multiple model failures produce one aggregate warning", {
  failing_specs <- specr::setup(
    data = example_data,
    x = c("x1", "missing_x1", "missing_x2"),
    y = "y1",
    model = "lm"
  )

  captured <- capture_warnings(specr(failing_specs))

  expect_length(captured$warnings, 1)
  expect_match(captured$warnings, "2 model specifications failed")
  expect_equal(captured$value$n_failed, 2)
  expect_equal(captured$value$failures$specification, c(2, 3))
})


test_that("all failed models return an empty result and a complete failure log", {
  extract_failure <- function(x) stop("parameter extraction failed")
  failing_specs <- specr::setup(
    data = example_data,
    x = c("x1", "x2"),
    y = "y1",
    model = "lm",
    fun1 = extract_failure
  )

  captured <- capture_warnings(specr(failing_specs))
  results <- captured$value

  expect_length(captured$warnings, 1)
  expect_equal(results$n_specs, 0)
  expect_equal(results$n_failed, 2)
  expect_equal(nrow(results$data), 0)
  expect_equal(nrow(results$failures), 2)
  expect_true(all(results$failures$error == "parameter extraction failed"))
})


test_that("plain tibble results retain failures as an attribute", {
  failing_specs <- specr::setup(
    data = example_data,
    x = c("x1", "missing_x"),
    y = "y1",
    model = "lm"
  )

  captured <- capture_warnings(
    specr(as_tibble(failing_specs), data = example_data)
  )
  results <- captured$value
  failures <- attr(results, "failures")

  expect_s3_class(results, "tbl_df")
  expect_length(captured$warnings, 1)
  expect_match(captured$warnings, "attr\\(result, \"failures\"\\)")
  expect_s3_class(failures, "tbl_df")
  expect_equal(failures$specification, 2)
  expect_match(failures$error, "object 'missing_x' not found", fixed = TRUE)
})


test_that("failed specifications are logged with future_pmap", {
  skip_if_not(future::supportsMulticore())

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multicore, workers = 2)

  failing_specs <- specr::setup(
    data = example_data,
    x = c("x1", "missing_x"),
    y = "y1",
    model = "lm"
  )

  captured <- capture_warnings(specr(failing_specs))

  expect_length(captured$warnings, 1)
  expect_equal(captured$value$n_specs, 1)
  expect_equal(captured$value$n_failed, 1)
  expect_equal(captured$value$failures$specification, 2)
})


