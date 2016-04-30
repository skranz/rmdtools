#' Own markdown to html converter that interfaces commonmark
#'
#' @param text the variable containing the markdown text
#' @param fragment.only only a fragment or should html headers be added
#' @param options options to markdownToHTML. These are the default options without smartypants
#' @param smart smart punctuation (relevant for commonmark)
#' @param use.commonmark use commonmark instead of markdownToHTML (no mathjax and included images)...
#' @return The created HTML as a string
#'
#' @export

md2html = function(text,fragment.only=TRUE, options=c("use_xhtml","mathjax",if (include.images) "base64_images" else NULL,"highlight_code"), include.images=TRUE, smart = FALSE, use.commonmark=FALSE,...) {
  if (!use.commonmark) {
    html = markdownToHTML(text=text, options=options,fragment.only=fragment.only,encoding="UTF-8",...)
    restore.point("md2html.1")

    html = enc2utf8(html)
    return(html)
  }


  html = commonmark::markdown_html(text, smart=smart,...)

  restore.point("md2html")

  Encoding(html) = "UTF-8"

  #html = paste0(html, collapse="\n")
  if (!fragment.only) {
    tag = "InNNer5Qwet44935t5GfECFCOPAjnKLNWAaaer6725389"
    outer = markdownToHTML(text=tag, fragment.only=FALSE,encoding="UTF-8")
    html = gsub(tag,html,outer,fixed=TRUE)
  }
  html = enc2utf8(html)
  html
}


example.rmd.blocks.to.placeholders = function() {
  setwd("D:/libraries/rmdtools")
  txt = read.as.utf8("test.rmd")

  res = set.rmd.placeholders(txt)
  ph = res$ph
  str = res$txt
  writeClipboard(str)
}

#' View an extended rmd file
#' @export
view.html = function(file=NULL, text=if (!is.null(file)) readLines(file,warn = FALSE) else NULL, ui=NULL, browser=rstudio::viewer, dir=getwd()) {
  restore.point("view.html")

  if (is.null(ui))
    ui = htmltools::htmlTemplate(text_ = text)

  library(shinyEvents)
  old.app = getApp()
  on.exit(setApp(old.app))
  try(shiny::addResourcePath("figure",paste0(dir,"/figure")), silent=TRUE)

  app = eventsApp()
  app$ui = fluidPage(ui)
  runEventsApp(app, launch.browser=browser)


  #browseURL(paste0('file://',file))

  #browseURL(url = paste0(file))
  #rstudio::viewer(file)
}

#' A simple html page
#' @export
simple.html.page = function(head, body) {
  head = paste0(head, collapse="\n")
  body = paste0(body, collapse="\n")
  html = paste0('
<!DOCTYPE html>
<!-- template.html -->
<html>
<head>
', head,'
</head>
<body>
', body,'
</body>
</html>
')
  html
}

#' Variant of htmltools::htmlTemplate
#' @export
html.template = function (file=NULL, html=NULL, envir=parent.frame(),  document_ = "auto")
{
  if (is.null(text)) {
    html <- readChar(file, file.info(file)$size, useBytes = TRUE)
    Encoding(html) <- "UTF-8"
  }

  pieces <- strsplit(html, "{{", fixed = TRUE)[[1]]
  pieces <- strsplit(pieces, "}}", fixed = TRUE)
  if (length(pieces[[1]]) != 1) {
      stop("Mismatched {{ and }} in HTML template.")
  }
  lapply(pieces[-1], function(x) {
      if (length(x) != 2) {
          stop("Mismatched {{ and }} in HTML template.")
      }
  })
  vars <- as.list(envir)
  if ("headContent" %in% names(vars)) {
      stop("Can't use reserved argument name 'headContent'.")
  }
  vars$headContent <- function() HTML("<!-- HEAD_CONTENT -->")

  pieces[[1]] <- HTML(pieces[[1]])
  pieces[-1] <- lapply(pieces[-1], function(piece) {
      tagList(eval(parse(text = piece[1]), envir), HTML(piece[[2]]))
  })
  result <- tagList(pieces)
  if (document_ == "auto") {
      document_ = grepl("<HTML>", html, ignore.case = TRUE)
  }
  if (document_) {
      class(result) <- c("html_document", class(result))
  }
  result
}

inline.dependencies = function (deps,  mustWork = TRUE)
{
  restore.point("inline.dependencies")

  txt = sapply(deps, inline.dependency, mustWork = mustWork)
  paste0(txt, collapse="\n")

}

inline.dependency = function (dependency,  mustWork = TRUE)
{
  restore.point("inline.dependency")

  dir <- dependency$src$file
  if (is.null(dir)) {
      if (mustWork) {
          stop("Dependency ", dependency$name, " ", dependency$version,
              " is not disk-based")
      }
      else {
          return(dependency)
      }
  }
  scripts = sapply(dependency$script, function(file) {
    paste0(readLines(paste0(dir,"/",file),warn = FALSE), collapse="\n")
  })
  styles = sapply(dependency$stylesheet, function(file) {
    paste0(readLines(paste0(dir,"/",file),warn=FALSE), collapse="\n")
  })
  header = paste0(
    paste0("<script>\n", scripts,"\n</script>", collapse="\n"),
    paste0("<style>\n", styles,"\n</style>", collapse="\n")
  )
  header

}
