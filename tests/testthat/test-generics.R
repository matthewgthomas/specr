context("generics")

specs <- specr::setup(data = example_data,
               x = c("x1", "x2"),
               y = "y1",
               model = "lm",
               subsets = list(group1 = unique(example_data$group1)),
               controls = c("c1", "c2"))
results <- specr(specs)

# Test 1
test_that("Function plot.specr.setup creates a ggplot", {
  p <- plot(specs)
  expect_is(p, "gg")
})


# Test 2
test_that("Function plot.specr.object creates a ggplot", {
  p <- plot(results)
  expect_is(p, "gg")
})


# Test 3
test_that("Function plot.specr.object with `type = 'curve'` creates a ggplot", {
  p <- plot(results, "curve")
  expect_is(p, "gg")
})

# Test 4
test_that("Function plot.specr.object with `type = 'choices'` creates a ggplot", {
  p <- plot(results, "choices")
  expect_is(p, "gg")
})


test_that("summary and print report failed model specifications", {
  failing_specs <- specr::setup(
    data = example_data,
    x = c("x1", "missing_x"),
    y = "y1",
    model = "lm"
  )
  failing_results <- suppressWarnings(specr(failing_specs))

  summary_output <- capture.output(suppressWarnings(summary(failing_results)))
  print_output <- capture.output(suppressWarnings(print(failing_results)))

  expect_true(any(grepl("Number of failed specifications: +1", summary_output)))
  expect_true(any(grepl("Failure details are available", summary_output, fixed = TRUE)))
  expect_true(any(grepl("Number of failed specifications: 1", print_output, fixed = TRUE)))
  expect_true(any(grepl("Failure details are available", print_output, fixed = TRUE)))
})


test_that("summary and print handle objects where all specifications failed", {
  extract_failure <- function(x) stop("parameter extraction failed")
  failing_specs <- specr::setup(
    data = example_data,
    x = c("x1", "x2"),
    y = "y1",
    model = "lm",
    fun1 = extract_failure
  )
  failing_results <- suppressWarnings(specr(failing_specs))

  summary_output <- capture.output(summary(failing_results))
  print_output <- capture.output(print(failing_results))

  expect_true(any(grepl("Number of failed specifications: +2", summary_output)))
  expect_true(any(grepl("No successful model specifications", summary_output, fixed = TRUE)))
  expect_true(any(grepl("Number of failed specifications: 2", print_output, fixed = TRUE)))
  expect_true(any(grepl("No successful model specifications", print_output, fixed = TRUE)))
})


# Test 5
test_that("Function plot.specr.object with `type = 'choices'` creates a ggplot", {
  p <- plot(results, "boxplot")
  expect_is(p, "gg")
})


# Test 4
test_that("Function plot.specr.object with `type = 'choices'` creates a ggplot", {
  p <- plot(results, "choices")
  expect_is(p, "gg")
})

