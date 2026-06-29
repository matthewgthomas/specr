#' Fit models across all specifications
#'
#' @description Runs the specification/multiverse analysis across specified models.
#'    This is the central function of the package and represent the second step
#'    in the analytic framework implemented in the package \code{specr}. It estimates
#'    and returns respective parameters and estimates of models that were specified
#'    via \code{setup()}.
#'
#' @param x A `specr.setup` object resulting from \code{setup} or a tibble that
#'    contains the relevant specifications (e.g., a tibble resulting from
#'    \code{as_tibble(setup(...))}).
#' @param data If x is not an object of "specr.setup" and simply a tibble, you
#'    need to provide the data set that should be used. Defaults to NULL as it is
#'    assumend that most users will create an object of class "specr.setup" that they'll
#'    pass to `specr()`.
#' @param ... Further arguments that can be passed to \code{future_pmap}. This only becomes
#'    important if parallelization is used. For example, if a custom model function is used
#'    this involves passing `furrr_options` passing to the argument `.options`.
#'    Set `.progress = TRUE` to print a progress bar during the fitting process.
#'    In sequential runs, this prints the number of fitted models out of the total
#'    number of specifications; in parallel runs, progress is handled by
#'    \code{future_pmap}. See details for more information on parallelization.
#'
#' @return An object of class \code{specr.object}, which includes a data frame
#'   with all successful specifications and their respective results along with
#'   other useful information about the models. The \code{failures} element is a
#'   tibble containing the original row number, specification fields, and error
#'   message for each failed specification; \code{n_failed} gives the number of
#'   failures. When a plain tibble is returned, the same failure log is available
#'   via \code{attr(result, "failures")}. Parameters are extracted via the
#'   functions passed to \code{setup}. By default these are \code{broom::tidy()}
#'   and \code{broom::glance()}. Use
#'   \code{methods(class = "specr.object")} for an overview on available methods.
#'
#' @details Empirical results are often contingent on analytical decisions that
#'    are equally defensible, often arbitrary, and motivated by different reasons.
#'    This decisions may introduce bias or at least variability. To this end,
#'    specification curve analyses  (Simonsohn et al., 2020) or multiverse
#'    analyses (Steegen et al., 2016) refer to identifying the set of
#'    theoretically justified, statistically valid (and potentially also non-redundant
#'    specifications, fitting the "multiverse" of models represented by these
#'    specifications and extract relevant parameters often to display the results
#'    graphically as a so-called specification curve. This allows readers to
#'    identify consequential specifications decisions and how they affect the results
#'    or parameter of interest.
#'
#'    \bold{Use of this function}
#'
#'    A general overview is provided in the vignettes \code{vignette("specr")}.
#'    Generally, you create relevant specification using the function \code{setup()}.
#'    You then pass the resulting object of a class \code{specr.setup} to the
#'    present function \code{specr()} to run the specification curve analysis.
#'    Further note that the resulting object of class \code{specr.object} allows
#'    to use several generic function such as \code{summary()} or \code{plot()}.
#'    Use \code{methods(class = "specr.object")} for an overview on available
#'    methods and e.g., \code{?plot.specr.object} to view the dedicated help page.
#'
#'    Errors raised while fitting a specification or extracting its results do
#'    not stop the remaining specifications from being evaluated. Failed
#'    specifications are excluded from the result data, recorded in the failure
#'    log described in the Value section, and reported with one aggregate warning.
#'
#'    \bold{Parallelization}
#'
#'    By default, the function fits models across all specifications sequentially
#'    (one after the other). If the data set is large, the models complex (e.g.,
#'    large structural equation models, negative binomial models, or Bayesian models),
#'    and the number of specifications is large, it can make sense to parallelize
#'    these operations. One simply has to load the package `furrr` (which
#'    in turn, builds on `future`) up front. Then parallelizing the fitting process
#'    works as specified in the package description of `furr`/`future` by setting a
#'    "plan" before running `specr` such as:
#'
#'    `plan(multisession, workers = 4)`
#'
#'    However, there are many more ways to specifically set up the plan, including
#'    different strategy than `multisession`. For more information, see
#'    `vignette("parallelization")` and the
#'    [reference page](https://future.futureverse.org/reference/plan.html)
#'    for `plan()`.
#'
#'
#'    \bold{Disclaimer}
#'
#'    We do see a lot of value in investigating how analytical choices
#'    affect a statistical outcome of interest. However, we strongly caution
#'    against using specr as a tool to somehow arrive at a better estimate
#'    compared to a single model. Running a specification curve analysis
#'    does not make your findings any more reliable, valid or generalizable
#'    than a single analysis. The method is meant to inform about the effects
#'    of analytical choices on results, and not a better way to estimate a
#'    correlation or effect.
#'
#'
#' @references \itemize{
#'  \item Simonsohn, U., Simmons, J.P. & Nelson, L.D. (2020). Specification curve analysis. *Nature Human Behaviour, 4*, 1208–1214. https://doi.org/10.1038/s41562-020-0912-z
#'  \item Steegen, S., Tuerlinckx, F., Gelman, A., & Vanpaemel, W. (2016). Increasing Transparency Through a Multiverse Analysis. *Perspectives on Psychological Science, 11*(5), 702-712. https://doi.org/10.1177/1745691616658637
#' }
#' @export
#'
#' @seealso [setup()] for the first step of setting up the specifications.
#' @seealso [summary.specr.object()] for how to summarize and inspect the results.
#' @seealso [plot.specr.object()] for plotting results.
#'
#' @examples
#' # Example 1 ----
#' # Setup up typical specifications
#' specs <- setup(data = example_data,
#'    y = c("y1", "y2"),
#'    x = c("x1", "x2"),
#'    model = "lm",
#'    controls = c("c1", "c2"),
#'    subsets = list(group1 = unique(example_data$group1)))
#'
#' # Run analysis (not parallelized)
#' results <- specr(specs)
#'
#' # Summary of the results
#' summary(results)
#'
#'
#' # Example 2 ----
#' # Working without S3 classes
#' specs2 <- setup(data = example_data,
#'     y = c("y1", "y2"),
#'     x = c("x1", "x2"),
#'     model = "lm",
#'     controls = "c1")
#'
#' # Working with tibbles
#' specs_tibble <- as_tibble(specs2)      # extract tibble from setup
#' results2 <- specr(specs_tibble,
#'                   data = example_data) # need to provide data!
#'
#' # Results (tibble instead of S3 class)
#' head(results2)
specr <- function(x,
                  data = NULL,
                  ...){


  # Start timing
  start <- Sys.time()

  . <- out <- term <- y <- NULL

  if(!inherits(x, c("specr.setup", "tbl_df", "tbl", "data.frame"))) {
    stop("You need to provide an object of class 'specr.setup' (or a tibble or data frame of the specification setup).\n  Use 'setup()' to create such a specification setup.")
  }

  if(isTRUE(any(c("tbl_df", "tbl", "data.frame") %in% class(x))) & is.null(data)) {
    stop("You provided a tibble or data.frame with all the specifications. In that case, you also need to provide the data set that should be used for the analyses.")
  }

  # Collect data and subsets
  if("specr.setup" %in% class(x)) {

    data <- x$data
    subsets <- x$subsets
    specs <- x$specs

  } else {

    specs <- x

  }

  dots <- list(...)
  show_progress <- isTRUE(dots$.progress)
  n_specifications <- nrow(specs)

  # Fit one specification and capture failures without stopping the remaining fits
  fit_specification <- function(...) {
    tryCatch({
      l <- list(...)

      # identify the grouping columns
      group_i <- which(sapply(l, is.factor))
      s <- rep(TRUE, nrow(data))

      # Create relevant subsets
      for (i in group_i) {
        column <- names(l)[i]
        value <- l[[i]]
        if (is.na(value)) next
        s <- s & data[[column]] == value
      }

      # Iterate across specifications
      result <- do.call(
        what = l$model_function,
        args = list(
          formula = l$formula,
          data = data[s,])
      )

      list(result = result, error = NULL)
    }, error = function(e) {
      list(result = NULL, error = conditionMessage(e))
    })
  }

  # Differentiate between 1 and >1 workers
  if(methods::is(plan(), "sequential")) {

    progress_count <- 0L
    show_sequential_progress <- show_progress && n_specifications > 0L
    write_progress <- function(count) {
      width <- 30L
      filled <- floor(width * count / n_specifications)
      bar <- paste0(strrep("=", filled), strrep(" ", width - filled))
      cat(sprintf("\rFitting models: [%s] %d/%d",
                  bar,
                  count,
                  n_specifications))
      if(count == n_specifications) cat("\n")
    }

    update_progress <- function() {
      progress_count <<- progress_count + 1L
      write_progress(progress_count)
    }

    fit_specification_sequential <- function(...) {
      result <- fit_specification(...)
      if(show_sequential_progress) update_progress()
      result
    }

    if(show_sequential_progress) write_progress(0L)

    fitted <- specs %>%
      dplyr::mutate(out = pmap(., fit_specification_sequential))

  } else {

    fitted <- specs %>%
      dplyr::mutate(out = future_pmap(., fit_specification, ...))

  }

  failed <- purrr::map_lgl(fitted$out, function(x) !is.null(x$error))

  failures <- fitted[failed, , drop = FALSE]
  failures$specification <- which(failed)
  failures <- failures %>%
    dplyr::mutate(error = purrr::map_chr(.data$out, "error")) %>%
    dplyr::select(dplyr::all_of("specification"),
                  dplyr::everything(),
                  -dplyr::any_of(c("out", "model_function")))

  res <- fitted[!failed, , drop = FALSE] %>%
    dplyr::mutate(out = purrr::map(.data$out, "result")) %>%
    tidyr::unnest(out) %>%
    dplyr::select(-dplyr::any_of("out"))

  # Select relevant term
  if("term" %in% names(res)) {
    if("op" %in% names(res)) {
      res <- res %>%
        dplyr::filter(term == paste(y, "~", x))
    } else {
      res <- res %>%
        dplyr::filter(term == x)
    }
  }

  # Compute time
  end <- Sys.time()
  time <- end-start
  time <- paste(round(as.numeric(time), 3), "sec elapsed")

  # Create S2 class
  if(class(x)[1] == "specr.setup") {

  # Create S3 class
  output <- list(data = res,
                 n_specs = nrow(res),
                 failures = failures,
                 n_failed = nrow(failures),
                 x = x$x,
                 y = x$y,
                 model = x$model,
                 controls = x$model,
                 subsets = x$subsets,
                 workers = nbrOfWorkers(),
                 time = time)

  # Set class
  class(output) <- "specr.object"

  } else {

  # Create tibble
  output <- as_tibble(res)
  attr(output, "failures") <- failures

  }

  if(nrow(failures) > 0) {
    if(inherits(output, "specr.object")) {
      access_path <- "the `failures` element of the returned `specr.object`"
    } else {
      access_path <- "`attr(result, \"failures\")` on the returned tibble"
    }

    warning(
      nrow(failures),
      " model specification", ifelse(nrow(failures) == 1, "", "s"),
      " failed. See ", access_path, " for details.",
      call. = FALSE
    )
  }

  return(output)

}
