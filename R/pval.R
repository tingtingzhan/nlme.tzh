

#' @title Get \eqn{p}-values from \link[nlme]{lme} and \link[nlme]{gls} Objects
#' 
#' @param x \link[nlme]{lme} or \link[nlme]{gls} object
#' 
#' @examples
#' .pval.lme(ortho1)
#' .pval.gls(ovary1)
#' 
#' @name pval_nlme
#' @export
.pval.gls <- function(x) {
  x |>
    summary() |> # ?nlme:::summary.gls
    .pval.summary.gls()
}


#' @rdname pval_nlme
#' @export
.pval.lme <- function(x) {
  x |> 
    summary() |> # ?nlme:::summary.lme
    .pval.summary.lme()
}


#' @rdname pval_nlme
#' @export
.pval.summary.lme <- function(x) {
  # `nlme:::coef.summary.lme` not show-able!!  could only see from debug(coef)
  # `nlme:::coef.summary.gls` not show-able!!  could only see from debug(coef)
  ret <- x$tTable[, 'p-value'] 
  names(ret) <- rownames(x$tTable)
  return(ret)
}

#' @rdname pval_nlme
#' @export
.pval.summary.gls <- .pval.summary.lme
  