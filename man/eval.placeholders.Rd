% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ph.r
\name{eval.placeholders}
\alias{eval.placeholders}
\title{Evaluate placeholders in compiled rmd}
\usage{
eval.placeholders(cr = NULL, envir = parent.frame(), ph = cr$ph,
  type = NULL, on.error = c("null", "error", "stop")[1],
  out.type = first.none.null(cr$out.type, "html"), ...)
}
\description{
Evaluate placeholders in compiled rmd
}

