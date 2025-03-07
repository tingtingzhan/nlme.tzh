
#' @title confint_
#' 
#' @param x \link[nlme]{gls} or \link[nlme]{lme} object
#' 
#' @param level \link[base]{numeric} scalar, default `.95`
#' 
#' @param ... additional parameters of function \link[nlme]{intervals}
#' 
#' @returns
#' Functions [confint_.lme] and [confint_.gls] return a \link[base]{matrix} with additional attributes 
#' `'conf.level'`.
#' 
#' @examples
#' ortho1 |> confint_.lme()
#' ortho2 |> confint_.lme()
#' ovary1 |> confint_.gls()
#' @name confint_nlme
#' @importFrom nlme intervals
#' @export
confint_.lme <- function(x, level = .95, ...) {
  intervals(object = x, level = level, which = 'fixed', ...) |> # ?nlme:::intervals.lme
    confint_.intervals.lme()
}

#' @rdname confint_nlme
#' @importFrom nlme intervals
#' @export
confint_.gls <- function(x, level = .95, ...) {
  intervals(object = x, level = level, which = 'coef', ...) |> # ?nlme:::intervals.gls
    confint_.intervals.gls()
}

#' @rdname confint_nlme
#' @export
confint_.intervals.lme <- function(x, ...) {
  ci <- x[['fixed']][, -2L, drop = FALSE] # three columns 'lower', 'est.', 'upper'
  attr(ci, which = 'conf.level') <- attr(x, which = 'level', exact = TRUE)
  return(ci)
}

#' @rdname confint_nlme
#' @export
confint_.intervals.gls <- function(x, ...) {
  ci <- x[['coef']][, -2L, drop = FALSE] # three columns 'lower', 'est.', 'upper'
  attr(ci, which = 'conf.level') <- attr(x, which = 'level', exact = TRUE)
  return(ci)
}



