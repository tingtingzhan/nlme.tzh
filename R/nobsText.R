
#' @title nobsText
#' 
#' @param x \link[nlme]{lme} or \link[nlme]{gls} object
#' 
#' @name nobsText_nlme
#' @keywords internal
#' @importFrom ecip nobsText
#' @export nobsText.lme
#' @export
nobsText.lme <- function(x) {
  # ?nlme:::print.lme; 'Number of Groups'
  dd <- x$dims
  ng <- dd[['ngrps']][1L:dd$Q]
  # either dd$Q == 1L or > 1L
  sprintf(fmt = '%d records from %s', dd[['N']], paste(sprintf(fmt = '%d `%s`', ng, names(ng)), collapse = ' nested in '))
}


#' @rdname nobsText_nlme
#' @importFrom ecip nobsText
#' @export nobsText.gls
#' @export
nobsText.gls <- function(x) {
  # what happens with nested groups ????
  groups <- x[['groups']]
  corrfom <- x$call$correlation$form
  sprintf(fmt = '%d records from %d `%s`', x$dims[['N']], nlevels(groups), deparse1(corrfom[[2L]][[3L]]))
}

