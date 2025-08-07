
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
#' library(nlme)
#' lme(fixed = distance ~ Sex * I(age-11), 
#'   weights = varIdent(form = ~ 1 | Sex), data = Orthodont) |> confint_.lme()
#' @name confint_nlme
#' @keywords internal
#' @importFrom nlme intervals
#' @importFrom ecip confint_
#' @export confint_.lme
#' @export
confint_.lme <- function(x, level = .95, ...) {
  intervals(object = x, level = level, which = 'fixed', ...) |> # ?nlme:::intervals.lme
    confint_.intervals.lme()
}

#' @rdname confint_nlme
#' @importFrom nlme intervals
#' @importFrom ecip confint_
#' @export confint_.gls
#' @export
confint_.gls <- function(x, level = .95, ...) {
  intervals(object = x, level = level, which = 'coef', ...) |> # ?nlme:::intervals.gls
    confint_.intervals.gls()
}

#' @rdname confint_nlme
#' @importFrom ecip confint_
#' @method confint_ intervals.lme
#' @export confint_.intervals.lme
#' @export
confint_.intervals.lme <- function(x, ...) {
  ci <- x[['fixed']][, -2L, drop = FALSE] # three columns 'lower', 'est.', 'upper'
  attr(ci, which = 'conf.level') <- attr(x, which = 'level', exact = TRUE)
  return(ci)
}

#' @rdname confint_nlme
#' @importFrom ecip confint_
#' @method confint_ intervals.gls
#' @export confint_.intervals.gls
#' @export
confint_.intervals.gls <- function(x, ...) {
  ci <- x[['coef']][, -2L, drop = FALSE] # three columns 'lower', 'est.', 'upper'
  attr(ci, which = 'conf.level') <- attr(x, which = 'level', exact = TRUE)
  return(ci)
}



