#' Knits the rmd txt inside a temporary directory instead of the current wd
#'
#' Does not create /figure subfolder in current wd
#' @export
knit.chunk = function(text, envir=parent.frame(), fragment.only=TRUE, quiet=TRUE, encoding = getOption("encoding"), html.table = TRUE, out.type="html", knit.dir=tempdir(), use.commonmark = TRUE, deps.action = c("add","ignore")[1], args=NULL, code=NULL) {
  restore.point("knit.chunk")

  text = sep.lines(text)

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

  old.rmarkdown.pandoc.to = opts_knit$get("rmarkdown.pandoc.to")
  if (out.type %in% c("html","shiny"))
    opts_knit$set(rmarkdown.pandoc.to="html")


  md = knitr::knit(text = text, envir = envir, encoding = "UTF8", quiet = quiet)

  meta = knit_meta(clean=FALSE)

  opts_knit$set(rmarkdown.pandoc.to=old.rmarkdown.pandoc.to)

  if (html.table) {
    if (!is.null(old.printer)) {
      .GlobalEnv$knit_print.data.frame = old.printer
    } else {
      suppressWarnings(rm("knit_print.data.frame",envir=.GlobalEnv))
    }
  }
  if (out.type =="md" | out.type=="rmd") return(md)

  #writeClipboard(html)
  html = md2html(text=md, fragment.only=fragment.only, use.commonmark = use.commonmark)


  is.dep = unlist(lapply(meta, function(el) is(el,"html_dependency")))
  deps = meta[is.dep]


  if (out.type == "shiny") {
    ui = HTML(html)
    if (deps.action=="add") {
      deps.ui = HTML(renderDependencies(deps))
      ui = tagList(deps.ui, ui)
    } else {
      #attr(ui,"knit_deps") <- deps
    }
    ui = attachDependencies(ui, deps)

    attr(ui,"knit_meta") <- meta

    return(ui)
  } else if (out.type == "html") {
    # simply add dependencies
    # there may be a lot of redudancies in this approach
    if (deps.action=="add") {
      deps.html = renderDependencies(deps)
      html = merge.lines(c(deps.html,html))
    } else {
      #attr(html,"knitr_deps") <- deps
    }
  }
  attr(html,"knit_meta") <- meta
  html
}


#' Knits the rmd txt
#'
#' @export
knit.rmd = function(text, envir=parent.frame(), fragment.only=TRUE, quiet=TRUE, encoding = getOption("encoding"), html.table = TRUE, out.type="html", use.commonmark=FALSE) {
  restore.point("knit.rmd")

  if (is.list(envir)) {
    envir =list2env(envir)
    parent.env(envir) = globalenv()
  }

  #knitr::opts_knit$set(root.dir = owd)
  if (html.table) {
    old.printer =.GlobalEnv$knit_print.data.frame
    .GlobalEnv$knit_print.data.frame = table.knit_print.data.frame
  }

  md = knitr::knit(text = text, envir = envir, encoding = encoding, quiet = quiet)
  #knitr:::.knitEnv$meta
  #knit_meta(clean=FALSE)

  if (html.table) {
    if (!is.null(old.printer)) {
      .GlobalEnv$knit_print.data.frame = old.printer
    } else {
      suppressWarnings(rm("knit_print.data.frame",envir=.GlobalEnv))
    }
  }

  if (out.type =="md" | out.type=="rmd") return(md)

  #writeClipboard(html)
  html = md2html(text=md, fragment.only=fragment.only, use.commonmark = use.commonmark)
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
