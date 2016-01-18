#' Knits the rmd txt inside a temporary directory instead of the current wd
#'
#' Does not create /figure subfolder in current wd
knit.rmd.in.temp = function(text, envir=parent.frame(), fragment.only=TRUE, quiet=TRUE) {
  owd <- setwd(tempdir())
  on.exit(setwd(owd))

  #knitr::opts_knit$set(root.dir = owd)

  html = knitr::knit2html(text=text, quiet=TRUE,envir=envir, fragment.only=fragment.only)
  html
}
