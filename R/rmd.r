#' View an extended rmd file
#' @export
view.rmd = function(file=NULL, text=readLines(file,warn = FALSE), params=NULL, parent.env=parent.frame(), use.blocks=FALSE, use.whiskers=TRUE, start.line=NULL, set.utf8=TRUE, knit=!chunk.like.whisker, chunk.like.whisker=FALSE) {
  restore.point("view.rmd")


  html = compile.rmd(text=text, params=params, parent.env=parent.env, use.blocks=use.blocks, use.whiskers=use.whiskers, start.line = start.line, set.utf8 = set.utf8, knit=knit, chunk.like.whisker = chunk.like.whisker, out.type="html", fragment.only = FALSE)

  out.file <- tempfile(fileext = ".html")
  writeLines(html, out.file)
  rstudio::viewer(out.file)
}


#' Main function to compile rmd to html
#' @export
compile.rmd = function(file=NULL, text=readLines(file,warn = FALSE), params=NULL, parent.env=parent.frame(), use.blocks=FALSE, use.whiskers=TRUE, start.line=NULL, set.utf8=TRUE, knit=!chunk.like.whisker, chunk.like.whisker=FALSE, out.type = "html", markdown.blocks.call.list=NULL, whiskers.call.list=NULL, fragment.only=TRUE) {

  restore.point("compile.rmd")

  if (set.utf8) {
    Encoding(text) = "UTF8"
    #text = mark_utf8(text)
  }

  # skip lines until start.tag
  offset = 0
  if (is.character(start.line)) {
    rows = which(text==start.line)
    if (length(rows)>0) {
      start.line = rows[1]
    } else {
      start.line = NULL
    }
  }

  if (!is.null(start.line)) {
    text = text[(start.line+1):length(text)]
  }

  if (use.blocks & !is.null(params)) {
    text = sep.lines(text)
    text = replace.rmd.blocks(text, params, call.list = markdown.blocks.call.list)
  }
  if (use.whiskers) {
    text = paste0(text, collapse="\n")
    text = replace.whiskers(text,params, whiskers.call.list=whiskers.call.list)
  }

  if (chunk.like.whisker) {
    text = render.chunks.like.whisker(rmd=text, params=params, env=parent.env)
    text = paste0(text, collapse="\n")
  }

  if (knit) {
    if (out.type == "md") {
      return(text)
    }

    if (!is.null(params)) {
      env = as.environment(params)
      parent.env(env)<-parent.env
    } else {
      env = parent.env
    }
    html = knit.rmd.in.temp(text=text, quiet=TRUE,envir=env, fragment.only=TRUE)
    html = gsub("&lt;!&ndash;html_preserve&ndash;&gt;","",html, fixed=TRUE)
    html = gsub("&lt;!&ndash;/html_preserve&ndash;&gt;","",html, fixed=TRUE)
  } else {
    if (out.type == "md") {
      return(text)
    }
    # Neccessary to make mathjax work
    #text =gsub("\\\\","\\\\\\\\",text, fixed=TRUE)
    html = markdownToHTML(text=text, fragment.only=fragment.only)
  }

  return(html)

}

#' Scan all used block and whisker parameters in an .rmd file
#' and create a template for a list
#'
#' @export
cat.rmd.params = function(file=NULL, text=readLines(file,warn = FALSE), use.blocks=TRUE, use.whiskers=TRUE) {

  restore.point("cat.rmd.params")

  library(codeUtils)

  # Find block params

  #str = sep.lines(str)
  start = which(str.starts.with(text,"#<"))
  end = which(str.starts.with(text,"#>"))

  if (length(start) != length(end)) {
    stop(paste0("You open ", length(start), " blocks but close ", length(end), " blocks"))
  }

  if (length(start)==0) return(text)

  blocks = match.blocks.start.end(start, end)
  start_row = start+1
  end_row = end[blocks[,2]]-1
  #cbind(start_row, end_row)
  block.code = str.right.of(text[start],"#< ")
  block.calls = lapply(block.code, function(str) {
    res = parse(text=str)[[1]]
    res
  })

  df = rmdtools:::get.whiskers.with.type(text=paste0(text,collapse="\n"))

  calls = c(block.calls, df$call)
  params = unique(unlist(lapply(calls, find.variables)))


  str = paste0("params = list(",
    paste0(params, " = 0", collapse=","),")"
  )
  writeClipboard(str)
  cat(str)
}


