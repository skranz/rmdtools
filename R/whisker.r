
#' cat.whisker params
#' @export
cat.whisker.params = function(file=NULL, text=readLines(file,warn = FALSE)) {
  library(codeUtils)
  text = paste0(text, collapse="\n")

  df = get.whiskers.with.type(text=text)

  block.rows = df$block.start | df$block.end
  block.params = unique(df$str[block.rows])

  rows = !block.rows
  params = unique(unlist(lapply(df$call[rows], find.variables)))

  str = paste0("params = list(",
    sc(block.params, " = TRUE", collapse=","),
    if (length(block.params)>0) "," else "",
    sc(params, " = 0", collapse=","),
    ")"
  )
  writeClipboard(str)
  cat(str)
}

examples.get.whisker.with.type = function() {
  setwd("D:/libraries/investgame/investgame")
  file="game1_result.rmd"
  text = readLines(file)
  text = paste0(text, collapse="\n")
  Encoding(text)<-"UTF-8"

  params = list(
  no_offer_1 = !TRUE,has_offered_1 = TRUE,reject_1 = !TRUE,accept_1 = TRUE,no_offer_2 = !TRUE,has_offered_2 = TRUE,accept_2_good = TRUE,accept_2_bad = TRUE,
  alpha_1 = 0,C = 0,alpha_2 = 0
)
  replace.whisker.with.blocks(text, params)
}

#' replace whiskers
#' @export
replace.whiskers <- function(str, env=parent.frame(), signif= getOption("whiskerSignifDigits")
, round=  getOption("whiskerRoundDigits"), add.params=TRUE, whiskers.call.list=NULL) {
  restore.point("replace.whiskers")

  if (add.params) {
    env$params = as.list(env)
  }
  pos = str.blocks.pos(str,"{{","}}")
  if (NROW(pos$outer)==0) return(str)
  s = substring(str, pos$inner[,1],pos$inner[,2])

  if (is.null(whiskers.call.list)) {
    vals = lapply(s, function(su) {
      res = try(eval(parse(text=su),env))
      if (is(res,"try-error")) res = "`Error`"
      res
    })
  # speed up compilation
  } else {
    calls = whiskers.call.list[s]
    vals = lapply(calls, function(call) {
      res = try(eval(call,env))
      if (is(res,"try-error")) res = "`Error`"
      res
    })
  }

  vals = lapply(vals, whisker_print, signif=signif, round=round)
  res = str.replace.at.pos(str, pos$outer, unlist(vals))
  res
}

#' Print a whisker object
#' @export
whisker_print = function(x,...) {
  UseMethod("whisker_print",x)
}

#' Need to implement different methods
#' @export
whisker_print.default = function(x,...) {
  as.character(x)
}

whisker.print.numeric = function(x,round=NULL, signif=NULL...) {
  res = x
  if (!is.null(signif)) {
    digits = max(signif,ceiling(log(res+1,10)))
    res = signif(res, digits)
  }
  if (!is.null(round)) {
    res = round(res, round)
  }
  res
}

whiskers.call.list = function(str) {
  restore.point("whiskers.call.list")
  pos = str.blocks.pos(str,"{{","}}")
  if (NROW(pos$outer)==0) return(str)
  s = substring(str, pos$inner[,1],pos$inner[,2])

  s = unique(s)
  calls = lapply(s, function(txt) {
    res = parse(text=txt)[[1]]
    res
  })
  names(calls)=s
  calls
}

replace.whisker.with.blocks = function(str, env=parent.frame()) {
  restore.point("replace.whisker.with.blocks")

  pos = str.blocks.pos(str,"{{","}}")
  if (NROW(pos$outer)==0) return(str)
  s = substring(str, pos$inner[,1],pos$inner[,2])
  first2 = substring(s,1,2)
  # whisker blocks
  blocks = first2 == "# " | first2 == "/ "

  rows = which(!blocks)
  vals = lapply(s[rows], function(su) {
    res = try(eval(parse(text=su),env))
    if (is(res,"try-error")) res = "`Error`"
    res
  })
  res = str.replace.at.pos(str, pos$outer[rows,], unlist(vals))

  # render blocks with default function
  if (sum(blocks)>0) {
    res = whisker::whisker.render(res,env)
  }

  return(res)
}

#' Render all knitr chunks in the same way as a whisker
#'  (taking into some chunk options, like results="asis")
#' @export
render.chunks.like.whisker = function(rmd, params=list(), env=parent.frame()) {
  restore.point("render.chunks.like.whisker")

  rmd = sep.lines(rmd)

  cdf = find.rmd.chunks(rmd)
  if (NROW(cdf)==0) return(rmd)

  if (!is.null(params)) {
    eenv = as.environment(params)
    parent.env(eenv)<-env
  } else {
    eenv = env
  }

  res = lapply(1:NROW(cdf), function(row) {
    ch = cdf[row,,drop=FALSE]
    code = rmd[(ch$start.row+1):(ch$end.row-1)]
    options = chunk.opt.string.to.list(rmd[ch$start.row], keep.name = FALSE)
    res = render.chunk.like.whisker(code=code, options=options, env=eenv)
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
render.chunk.like.whisker = function(code, call=NULL, options=NULL, env=parent.frame()) {
  restore.point("render.chunk.like.whisker")


  if (is.null(call)) {
    call = parse(text=c("{\n",code,"\n}"))[[1]]
  }

  val = eval(call,env)
  if (identical(options$results,"asis"))
    return(paste0(val,collapse="\n"))

  do.call(whisker_print,c(list(x=val),options))
}

get.whiskers.with.type = function(text=paste0(readLines(file,warn = FALSE), collapse="\n"), file=NULL) {
  library(stringtools)
  pos = str.blocks.pos(text,start = "{{", end="}}")

  str = str.at.pos(text,pos$inner)
  start = substring(str,1,2)
  block.start = start == "# "
  block.end = start == "/ "
  str[block.start | block.end] = substring(str[block.start | block.end],3)
  calls = lapply(str, function(str) {
    try(parse(text=str)[[1]])
  })
  classes = lapply(calls, class)

  rows = ! classes %in% c("name","character","numeric")
  classes[rows] = "call"

  df = data_frame(start=pos$outer[,1], right=pos$outer[,2], str=str,call=calls, class=classes, block.start=block.start, block.end=block.end)
  df
}


#' Like paste0 but returns an empty vector if some string is empty
sc = function(..., sep="", collapse=NULL) {
  str = list(...)
  restore.point("str.combine")
  len = sapply(str,length)
  if (any(len==0))
    return(vector("character",0))
  paste0(...,sep=sep,collapse=collapse)
}
