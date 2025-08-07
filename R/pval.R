

#' @title Get \eqn{p}-values from \link[nlme]{lme} and \link[nlme]{gls} Objects
#' 
#' @param x \link[nlme]{lme} or \link[nlme]{gls} object
#' 
#' @keywords internal
#' @name pval_nlme
#' @importFrom ecip .pval
#' @method .pval summary.lme
#' @export .pval.summary.lme
#' @export
.pval.summary.lme <- function(x) {
  # `nlme:::coef.summary.lme` not show-able!!  could only see from debug(coef)
  # `nlme:::coef.summary.gls` not show-able!!  could only see from debug(coef)
  cf <- x$tTable
  ret <- cf[, 'p-value'] 
  names(ret) <- rownames(cf)
  return(ret)
}

#' @rdname pval_nlme
#' @importFrom ecip .pval
#' @method .pval summary.gls
#' @export .pval.summary.gls
#' @export
.pval.summary.gls <- .pval.summary.lme
  