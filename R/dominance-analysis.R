#' Dominance Analysis
#'
#' @description
#' `r lifecycle::badge('stable')`
#' Convenience function to extract outputs from yhat::dominance function
#'
#' @details
#' This function uses arguments for formula and data to calculate dominance
#' values from a linear regression. When type == "general", returns a vector
#' with general dominance values for each DV. When type == "conditional",
#' returns a matrix with dominance values with columns corresponding to
#' the DV and rows corresponding to the number of variables in configuration.
#' When type == "complete", returns complete table for dominance. See
#' \href{this vignette}{https://rdrr.io/cran/domir/f/vignettes/domir_basics.Rmd}
#' for more information on interpreting dominance analysis outputs.
#'
#' @seealso [run_dominance_boot()]
#'
#' @param formula Formula for linear regression
#' @param data Dataframe containing values to calculate dominance from
#' @param type Character. Either "general", "conditional" or "complete".
#'
#' @returns Dominance values according to type argument. See details.
#' @export
#'
#' @examples
#' run_dominance(mpg~cyl+hp, data = mtcars, type = "general")
#' run_dominance(mpg~cyl+hp, data = mtcars, type = "conditional")
#' run_dominance(mpg~cyl+hp, data = mtcars, type = "complete")

run_dominance <- function(
    formula,
    data,
    type = "general"
) {
  # get dependent variable from formula
  formula_dv <- all.vars(formula)[1]
  # get independent variables from formula
  formula_ivs <- all.vars(formula)[-1]
  # get dominance output
  dom_frame <- yhat::aps(
    data,
    formula_dv,
    formula_ivs
  ) |>
    yhat::dominance()
  # return desired outputs based on type variable
  switch(
    type,
    general = dom_frame$GD,
    conditional = dom_frame$CD,
    complete = dom_frame$DA
  )
}

#' Dominance analysis with bootstrap percentile estimates
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Calculate general or conditional dominance with bootstrap confidence
#' intervals
#'
#' @details
#' This function uses a formula, data syntax to calculate dominance
#' values along with bootstrap CIs from a linear regression.
#' When type == "general", returns a vector with general dominance values for
#' each DV. When type == "conditional", returns a matrix with dominance values
#' with columns corresponding to the DV and rows corresponding to the number of
#' variables in configuration.
#' See \href{this vignette}{https://rdrr.io/cran/domir/f/vignettes/domir_basics.Rmd}
#' for more information on interpreting dominance analysis outputs.
#'
#' @param formula Formula for linear regression
#' @param data Dataframe containing values to calculate dominance from
#' @param type Character. Either "general" or "conditional"
#' @param n_replications Number of bootstrap replications to perform
#'
#' @returns
#' @export
#'
#' @examples
#' run_dominance_boot(mpg~cyl+hp, data = mtcars, type = "general")
#' run_dominance_boot(mpg~cyl+hp, data = mtcars, type = "conditional")

run_dominance_boot <- function(
  formula,
  data,
  type = "general",
  n_replications = 100
) {
  # have to first prepare formula as text so lm() plays nicely with
  # boot.yhat when passing linear model as an argument
  linear_model <- eval(
    parse(
      text = paste0(
        "lm(formula = ",
        deparse(formula),
        ", ",
        "data = ",
        deparse(substitute(data)),
        ")"
      )
    )
  )
  # calculate regression metrics
  yhat_output <- yhat::calc.yhat(
    linear_model
    )
  # bootstrap results
  bootstrap_output <- boot::boot(
    data = data,
    statistic = yhat::boot.yhat,
    R = n_replications,
    lmOut = linear_model,
    regrout0 = yhat_output
    )
  result <- yhat::booteval.yhat(
    yhat_output,
    bty= "perc", # percentile-based confidence intervals
    bootstrap_output
  )
  # combCIpm:
  general_dominance <- result$combCIpm$GenDom
  conditional_dominance <- dplyr::select(
    result$combCIpm,
    dplyr::contains("CD:")
    )
  result <- switch(
    type,
    general = general_dominance,
    conditional = conditional_dominance
  )
  if(type == "general") {
    rhs <- labels(terms(formula))
    result <- stringr::str_replace_all(result, "\\(|\\)|,", " ") |>
      trimws() |>
      stringr::str_split(pattern = " ")
    result <- do.call(rbind, result)
    result <- data.frame(result)
    colnames(result) <- c('dominance', 'lci', 'uci')
    result$var <- rhs
  }
  return(result)
}


