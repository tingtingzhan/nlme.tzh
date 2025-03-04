
#' @title confint2
#' 
#' @param x ..
#' 
#' @param level ..
#' 
#' @param ... ..
#' 
#' @examples
#' ortho1 |> confint2.lme()
#' ovary1 |> confint2.gls()
#' @name confint2
#' @importFrom nlme intervals
#' @export
confint2.lme <- function(x, level = .95, ...) {
  intervals(object = x, level = level, ...) |> 
    confint2.intervals.lme()
}

#' @rdname confint2
#' @importFrom nlme intervals
#' @export
confint2.gls <- function(x, level = .95, ...) {
  intervals(object = x, level = level, ...) |> 
    confint2.intervals.gls()
}

#' @rdname confint2
#' @export
confint2.intervals.lme <- function(x, ...) {
  ci <- x[['fixed']][, -2L, drop = FALSE] # three columns 'lower', 'est.', 'upper'
  attr(ci, which = 'conf.level') <- attr(x, which = 'level', exact = TRUE)
  return(ci)
}

#' @rdname confint2
#' @export
confint2.intervals.gls <- function(x, ...) {
  ci <- x[['coef']][, -2L, drop = FALSE] # three columns 'lower', 'est.', 'upper'
  attr(ci, which = 'conf.level') <- attr(x, which = 'level', exact = TRUE)
  return(ci)
}



