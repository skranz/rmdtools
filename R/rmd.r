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
    text = replace.if.blocks(text, params, call.list = markdown.blocks.call.list)
    text = replace.note.blocks(text,params)
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



#' Removes the rmd chunks with the given chunk names from rmd code
#'
#' @param rmd the rmd code as character vector, one element per line
#' @param chunk.names the list of rmd chunks that shall be removed
#' @export
remove.rmd.chunks = function(rmd, chunk.names) {
  restore.point("remove.rmd.chunks")

  df = find.rmd.chunks(rmd)
  rem.rows = unique(unlist(lapply(intersect(df$chunk.name,chunk.names), function (chunk.name) {
    row = which(df$chunk.name==chunk.name)[1]
    df$start.row[row]:df$end.row[row]
  })))
  if (length(rem.rows)>0) {
    return(rmd[-rem.rows])
  }
  rmd
}

#' Find the start and end lines of all rmd chunks
#'
#' @param rmd the rmd code as character vector, one element per line
#' @export
find.rmd.chunks = function(rmd) {
  restore.point("find.rmd.chunks")
  chunk.start = str.starts.with(rmd,"```{r")
  chunk.end = which(str.starts.with(rmd,"```") & !chunk.start)
  chunk.start = which(chunk.start)
  chunk.end = internal.remove.verbatim.end.chunks(chunk.start,chunk.end)

  names = parse.chunk.names(rmd[chunk.start])
  data.frame(chunk.name=names,start.row=chunk.start, end.row=chunk.end)
}


internal.remove.verbatim.end.chunks = function(chunk.start, chunk.end) {
  restore.point("remove.verbatim.end.chunks")
  df = data.frame(ind =c(0, seq_along(chunk.start), seq_along(chunk.end)),
                  row=c(0, chunk.start,chunk.end),
                  type=c("f",
                         rep("s",length(chunk.start)),
                         rep("e",length(chunk.end))
                       )
                  )
  df = arrange(df,row)
  df$del =  df$type == "e" & !is.true(lag(df$type) == "s")

  keep.ind = df$ind[df$type=="e" & !df$del]
  chunk.end[keep.ind]
}

#' Take a vector of chunk header lines and returns the chunk names
#'
#' @param header the chunk headers
#' @export
parse.chunk.names = function(header) {
  restore.point("parse.chunk.names")

  str = header
  res = rep("", length(header))

  str = str.between(str,"{r","}")
  rows = has.substr(str,",")
  str[rows] = str.left.of(str[rows],",")
  rows = !has.substr(str,"=")
  str[rows] = str.trim(str[rows])
  str =gsub("'","",str,fixed=TRUE)
  str =gsub('"',"",str,fixed=TRUE)
  str
}

#' Translates a chunk header to a list of its option
#'
#' @param str the chunk header as written in the rmd file
#' @param keep.name shall the chunk name be kept?
#' @export
chunk.opt.string.to.list = function(str, keep.name=FALSE) {
  restore.point("chunk.opt.string.to.list")
  #str = "```{r 'out_chunk_2_1_b', fig.width=5, fig.height=5, eval=FALSE, echo=TRUE}"

  tokens = str.split(str,",")
  str = str.between(str,"{r","}")
  code = paste0("list(",str,")")
  li = eval(base::parse(text=code,srcfile=NULL))

  if (keep.name) return(li)
  if (length(li)==0) return(li)

  #if ("replace.sol" %in% names(li))
  #  stop("nbfbfurb")
  # remove chunk.name
  if (is.null(names(li))) {
    return(li[-1])
  } else if (nchar(names(li)[1]) == 0) {
    return(li[-1])
  }
  li
}

