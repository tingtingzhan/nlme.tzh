
#' @title Model Description
#' 
#' @param x \link[nlme]{lme} or \link[nlme]{gls} object
#' 
#' @keywords internal
#' @name desc_nlme
#' @export
desc_.lme <- function(x) {
  # from ?nlme:::print.lme
  'linear mixed effects'
}


#' @rdname desc_nlme
#' @export
desc_.gls <- function(x) {
  # from ?nlme:::print.gls
  'generalized (weighted) least squares'
}


