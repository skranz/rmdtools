#' Knits the rmd txt inside a temporary directory instead of the current wd
#'
#' Does not create /figure subfolder in current wd
#' @export
knit.chunk = function(text, envir=parent.frame(), fragment.only=TRUE, quiet=TRUE, encoding = getOption("encoding"), html.table = TRUE, out.type="html", knit.dir=tempdir()) {
  restore.point("knit.chunk")

  if (is.list(envir)) {
    envir =list2env(envir)
    parent.env(envir) = globalenv()
  }
  owd <- setwd(knit.dir)
  on.exit(setwd(owd))

  #knitr::opts_knit$set(root.dir = owd)
  if (html.table) {
    old.printer =.GlobalEnv$knit_print.data.frame
    .GlobalEnv$knit_print.data.frame = table.knit_print.data.frame
  }

  md = knitr::knit(text = sep.lines(text), envir = envir, encoding = "UTF8", quiet = quiet)

  if (html.table) {
    if (!is.null(old.printer)) {
      .GlobalEnv$knit_print.data.frame = old.printer
    } else {
      suppressWarnings(rm("knit_print.data.frame",envir=.GlobalEnv))
    }
  }

  if (out.type =="md" | out.type == "rmd") return(md)

  #writeClipboard(html)
  html = md2html(text=md, fragment.only=fragment.only, use.commonmark = TRUE)
  if (out.type == "shiny") return(HTML(html))
  html
}

#' Knits the rmd txt
#'
#' @export
knit.rmd = function(text, envir=parent.frame(), fragment.only=TRUE, quiet=TRUE, encoding = getOption("encoding"), html.table = TRUE, out.type="html") {
  restore.point("knit.rmd.in.temp")

  if (is.list(envir)) {
    envir =list2env(envir)
    parent.env(envir) = globalenv()
  }

  #knitr::opts_knit$set(root.dir = owd)
  if (html.table) {
    old.printer =.GlobalEnv$knit_print.data.frame
    .GlobalEnv$knit_print.data.frame = table.knit_print.data.frame
  }

  md = knitr::knit(text = text, envir = envir, encoding = encoding,
        quiet = quiet)

  if (html.table) {
    if (!is.null(old.printer)) {
      .GlobalEnv$knit_print.data.frame = old.printer
    } else {
      suppressWarnings(rm("knit_print.data.frame",envir=.GlobalEnv))
    }
  }

  if (out.type =="md" | out.type=="rmd") return(md)

  #writeClipboard(html)
  html = md2html(text=md, fragment.only=fragment.only)
  if (out.type == "shiny") return(HTML(html))
  html
}

#' Knits the rmd txt inside a temporary directory instead of the current wd
#'
#' Does not create /figure subfolder in current wd
#' @export
knit.rmd.in.temp = function(text, envir=parent.frame(),...) {
  restore.point("knit.rmd.in.temp")
  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  knit.rmd(text, envir,...)
}



#' Render with RMarkdown::render the rmd txt inside a temporary directory instead of the current wd
#'
#' Does not create /figure subfolder in current wd
#' @export
render.rmd.in.temp = function(text, envir=parent.frame(), quiet=TRUE,...) {
  restore.point("render.rmd.in.temp")

  dir = tempdir()
  owd <- setwd(dir)
  on.exit(setwd(owd))

  #knitr::opts_knit$set(root.dir = owd)
  input.file = tempfile(fileext=".Rmd", tmpdir=dir)
  writeLines(text, input.file)

  out.file = rmarkdown::render(input=input.file,output_dir=dir,envir=envir,quiet=quiet,...)
  html = readLines(out.file)
  html
}
