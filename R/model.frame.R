

#' @title \link[stats]{model.frame} of \link[nlme]{gls} and \link[nlme]{lme} Objects
#' 
#' @description ..
#' 
#' @param formula \link[nlme]{gls} or \link[nlme]{lme} object
#' 
#' @param data ..
#' 
#' @param ... additional parameters of \link[stats]{model.frame.default}
#' 
#' @note
#' S3 generic \link[stats]{model.frame} is critical for function \link[stats]{.getXlevels}.
#' 
#' Without function [model.frame.lme], 
#' S3 generic \link[stats]{model.frame}
#' dispatches to \link[stats]{model.frame.default} and 
#' get `$modelStruct` (as \link[nlme]{lme} object has no `$model` element).
#' 
#' @name model_frame_nlme
#' @importFrom nlme getData
#' @importFrom stats model.frame.default
#' @method model.frame lme
#' @export model.frame.lme
#' @export
model.frame.lme <- function(
    formula, 
    data = getData(formula), # ?nlme:::getData.lme or # ?nlme:::getData.gls
    ...
) {
  model.frame.default(
    formula = formula(formula), # ?nlme:::formula.lme or ?nlme:::formula.gls
    data = data, 
    ...
  )
}

#' @rdname model_frame_nlme
#' @method model.frame gls
#' @export model.frame.gls
#' @export
model.frame.gls <- model.frame.lme


