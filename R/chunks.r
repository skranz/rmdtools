#' Parse a n rmd chunk and store info in a list
#' @export
make.chunk.info = function(txt, id=NULL) {
  restore.point("make.chunk.info")
  if (length(txt)==1) txt = sep.lines(txt)
  header = txt[1]
  args = parse.chunk.args(header)
  content = paste0(txt[c(-1,-length(txt))], collapse="\n")
  expr = parse(text=content)
  if (is.null(id)) {
    name = normalize.id(args$label)
    id = paste0("CHUNK_",name,"_",random.string())
  }
  list(
    id = id,
    args = args,
    content = content,
    expr = expr
  )
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
    df$start[row]:df$end[row]
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
find.rmd.chunks = function(rmd, add.code = FALSE) {
  restore.point("find.rmd.chunks")
  chunk.start = str.starts.with(rmd,"```{r")
  chunk.end = which(str.starts.with(rmd,"```") & !chunk.start)
  chunk.start = which(chunk.start)
  chunk.end = internal.remove.verbatim.end.chunks(chunk.start,chunk.end)

  names = parse.chunk.names(rmd[chunk.start])
  df = data.frame(chunk.name=names,start=chunk.start, end=chunk.end)
  if (add.code) {
    df$code = sapply(seq_len(NROW(df)), function(r) {
      paste0(rmd[(df$start[r]+1):(df$end[r]-1)], collapse="\n")
    })
  }
  df
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

#' Parse the name of a knitr chunk and its arguments
#' @export
parse.chunk.args = function(header, arg.str=NULL) {
  restore.point("parse.chunk.opt.and.name")
  if (!is.null(arg.str)) {
    if (is.na(arg.str)) return(list())
    return(knitr:::parse_params(arg.str))
  }

  str = str.right.of(header,"r ",not.found = NA)
  if (is.na(str)) return(list())
  knitr:::parse_params(str.left.of(str,"}"))
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

#' Render all knitr chunks in the same way as a whisker
#'  (taking into some chunk options, like results="asis")
#' @export
eval.chunks.in.text = function(rmd, envir=parent.frame()) {
  restore.point("render.chunks.like.whisker")

  rmd = sep.lines(rmd)

  cdf = find.rmd.chunks(rmd)
  if (NROW(cdf)==0) return(rmd)

  if (is.list(envir)) {
    envir = as.environment(envir)
    parent.env(envir) = globalenv()
  }

  res = lapply(1:NROW(cdf), function(row) {
    ch = cdf[row,,drop=FALSE]
    code = rmd[(ch$start.row+1):(ch$end.row-1)]
    options = chunk.opt.string.to.list(rmd[ch$start.row], keep.name = FALSE)
    res = eval.chunk.like.whisker(code=code, options=options, env=eenv)
    res = paste0(res, collapse="\n")
  })
  rmd[cdf$start.row] = res

  rm.lines = unlist(lapply(1:NROW(cdf), function(row) {
    ch = cdf[row,,drop=FALSE]
    setdiff(ch$start.row:ch$end.row,ch$start.row)
  }))
  if (length(rm.lines)>0) {
    rmd = rmd[-rm.lines]
  }
  rmd
}


#' Render a knitr chunk in the same way as a whisker (taking into some chunk options, like results="asis")
#' @export
eval.chunk.like.whisker = function(code, call=NULL, options=NULL, env=parent.frame()) {
  restore.point("render.chunk.like.whisker")


  if (is.null(call)) {
    call = parse(text=c("{\n",code,"\n}"))[[1]]
  }

  val = eval(call,env)
  if (identical(options$results,"asis"))
    return(paste0(val,collapse="\n"))

  do.call(whisker_print,c(list(x=val),options))
}


