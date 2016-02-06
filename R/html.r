md2html = function(text,fragment.only=FALSE, smart = FALSE,...) {
  html = commonmark::markdown_html(text, smart=smart,...)
  restore.point("md2html")

  Encoding(html) = "UTF8"

  #html = paste0(html, collapse="\n")
  if (!fragment.only) {
    tag = "InNNer5Qwet44935t5GfECFCOPAjnKLNWAaaer6725389"
    outer = markdownToHTML(text=tag, fragment.only=FALSE)
    html = gsub(tag,html,outer,fixed=TRUE)
  }
  html = mark_utf8(html)
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
view.html = function(file=NULL, text=readLines(file,warn = FALSE), browser=rstudio::viewer) {
  restore.point("view.html")
  if (is.null(file)) {
    file <- tempfile(fileext = ".html")
    writeLines(text, file)
  }

  library(shinyEvents)
  app = eventsApp()
  app$ui = fluidPage(HTML(text))
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
