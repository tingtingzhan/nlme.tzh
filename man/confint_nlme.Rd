% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confint.R
\name{confint_nlme}
\alias{confint_nlme}
\alias{confint_.lme}
\alias{confint_.gls}
\alias{confint_.intervals.lme}
\alias{confint_.intervals.gls}
\title{confint_}
\usage{
confint_.lme(x, level = 0.95, ...)

confint_.gls(x, level = 0.95, ...)

confint_.intervals.lme(x, ...)

confint_.intervals.gls(x, ...)
}
\arguments{
\item{x}{\link[nlme]{gls} or \link[nlme]{lme} object}

\item{level}{\link[base]{numeric} scalar, default \code{.95}}

\item{...}{additional parameters of function \link[nlme]{intervals}}
}
\value{
Functions \link{confint_.lme} and \link{confint_.gls} return a \link[base]{matrix} with additional attributes
\code{'conf.level'}.
}
\description{
confint_
}
\examples{
library(nlme)
lme(distance ~ age, data = Orthodont, keep.data = TRUE) |> confint_.lme()
lme(fixed = distance ~ Sex * I(age-11), 
  weights = varIdent(form = ~ 1 | Sex), data = Orthodont) |> confint_.lme()
gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), 
  data = Ovary, correlation = corAR1(form = ~ 1 | Mare)) |> confint_.gls()
}
