

# use do.call() because model frame error?  See `diagnose(ortho3)`


#nlme_cls <- function(x) {
#  if (inherits(x, what = 'nlme')) return('nlme') 
#  if (inherits(x, what = 'lme')) return('lme') 
#  if (inherits(x, what = 'gls')) return('gls') 
#  stop('illegal x: ', sQuote(class(x)[1L]))
#}

#' @title Diagnose \link[nlme]{lme} and \link[nlme]{gls}
#' 
#' @description ..
#' 
#' @param object \link[nlme]{lme} or \link[nlme]{gls} object
#' 
#' @param ... ..
#' 
#' @examples
#' library(nlme)
#' lme(distance ~ age, data = Orthodont, keep.data = TRUE) |> 
#'  diagnose.lme(lty = 2, id = .05, adj = -.4)
#' lme(fixed = distance ~ Sex * I(age-11), 
#'  weights = varIdent(form = ~ 1 | Sex), data = Orthodont) |> diagnose.lme()
#' lme(Thickness ~ 1, data = Oxide) |> diagnose.lme()
#' # gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
#' #  data = Ovary, correlation = corAR1(form = ~ 1 | Mare)) |> diagnose.gls() # bug
#' 
#' @importFrom grDevices dev.hold dev.flush devAskNewPage
#' @importFrom graphics pairs
#' @importFrom nlme getData getGroups getGroupsFormula getResponse ranef
#' @importFrom stats qqnorm
#' @name diagnose_nlme
#' @export
diagnose.lme <- function(object, ...) {
  
  #cls <- nlme_cls(object)
  #nranef1 <- if (cls != 'gls') ncol(ranef(object, level = 1)) else 0L
  nranef1 <- if (!inherits(object, what = 'gls')) ncol(ranef(object, level = 1)) else 0L
  
  dat0 <- getData(object)
  
  oask <- devAskNewPage(TRUE)
  on.exit(devAskNewPage(oask))
  
  # Assessing Error
  
  # standardized residuals vs fitted
  dev.hold()
  print(plot(object, ...))
  dev.flush()
  
  # normality of standardized residuals
  dev.hold()
  print(qqnorm(object, abline = c(0, 1), ...))
  dev.flush()
  
  # residual by subject
  dev.hold()
  print(plot(object, form = getGroups(.) ~ resid(.), abline = 0, ...))
  dev.flush()
  
  .cl_pearson <- quote(resid(., type = 'pearson'))
  
  if (length(otr <- attr(dat0, which = 'outer', exact = TRUE)[[2L]])) {
    
    # standardized residual vs. fitted values, by outer factor
    dev.hold()
    .fom <- eval(call(name = '~', 
                      .cl_pearson, 
                      call(name = '|', quote(fitted(.)), otr)))
    plot(object, form = .fom, ...)
    dev.flush()
    
    # normality of standardized residual, by outer factor
    dev.hold()
    .fom <- eval(call(name = '~', call(name = '|', .cl_pearson, otr)))
    print(qqnorm(object, form = .fom, abline = c(0,1), ...))
    dev.flush()
    
    # Assessing random effects
    if (nranef1 > 1L) {
      dev.hold()
      .fom <- eval(call(name = '~', call(name = '|', quote(ranef(., level = 1)), otr)))
      print(pairs(object, form = .fom, ...))
      dev.flush()
    }
    
  }
  
  if (is.symbol(grp <- getGroupsFormula(object)[[2L]])) { # no nested groups
    
    # standardized residual vs. fitted values, by subject
    dev.hold()
    .fom <- eval(call(name = '~', .cl_pearson, call(name = '|', quote(fitted(.)), grp)))
    print(plot(object, form = .fom, abline = c(0, 0), ...))
    dev.flush()  
    
  }
  
  # observed vs. fitted
  dev.hold()
  print(plot(object, form = getResponse(.) ~ fitted(.), abline = c(0, 1), ...))
  dev.flush()
  
  # Assessing random effects
  if (nranef1 > 1L) {
    # marginal normality of random effects; 
    dev.hold()
    print(qqnorm(object, ~ ranef(., level = 1)))
    dev.flush()
  }
  
  return(invisible())
}


#' @rdname diagnose_nlme
#' @export
diagnose.gls <- diagnose.lme


