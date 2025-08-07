

#' @title R Markdown Lines for `lme` and `gls`
#' 
#' @param x,xnm,... ..
#' 
#' @examples
#' library(nlme)
#' m1 = lme(distance ~ age, data = Orthodont, keep.data = TRUE)
#' m2 = gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
#'   data = Ovary, correlation = corAR1(form = ~ 1 | Mare)) 
#'   
#' library(ecip)
#' list('`lme`' = m1, '`gls`' = m2) |> rmd.tzh::render_(file = 'lme_gls')
#' @keywords internal
#' @name md_nlme
#' @importFrom rmd.tzh md_
#' @importFrom ecip md_ecip
#' @export md_.lme
#' @export
md_.lme <- md_ecip

#' @rdname md_nlme
#' @importFrom rmd.tzh md_
#' @importFrom ecip md_ecip
#' @export md_.gls
#' @export
md_.gls <- md_ecip

