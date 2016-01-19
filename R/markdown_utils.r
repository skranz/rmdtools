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

