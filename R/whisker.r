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

select.markdown.blocks = function(txt, env=parent.frame(), call.list=NULL) {
  restore.point("select.markdown.blocks")

  #str = sep.lines(str)
  start = which(str.starts.with(txt,"#<"))
  end = which(str.starts.with(txt,"#>"))

  if (length(start) != length(end)) {
    stop(paste0("You open ", length(start), " blocks but close ", length(end), " blocks"))
  }

  if (length(start)==0) return(txt)

  blocks = match.blocks.start.end(start, end)
  start_row = start+1
  end_row = end[blocks[,2]]-1
  #cbind(start_row, end_row)
  str_calls = str.right.of(txt[start],"#< ")

  if (is.null(call.list)) {
    calls = lapply(str_calls, function(str) parse(text=str))
  } else {
    calls = call.list[str_calls]
  }
  add = sapply(calls, function(call) isTRUE(try(eval(call,envir=env))))

  #add = sapply(calls, function(call) isTRUE(eval(call,envir=env)))


  del.rows = unique(unlist(lapply(which(!add),function(ind){
    start_row[ind]:end_row[ind]
  })))
  del.rows = unique(c(start,end,del.rows))

  if (length(del.rows)>0) {
    return(txt[-del.rows])
  } else {
    return(txt)
  }
}

markdown.blocks.call.list = function(txt) {
  restore.point("markdown.blocks.call.list")

  if (length(txt)==1) txt = sep.lines(txt)
  start = which(str.starts.with(txt,"#<"))
  end = which(str.starts.with(txt,"#>"))

  if (length(start) != length(end)) {
    stop(paste0("You open ", length(start), " blocks but close ", length(end), " blocks"))
  }

  if (length(start)==0) return(txt)

  blocks = match.blocks.start.end(start, end)
  start_row = start+1
  end_row = end[blocks[,2]]-1
  #cbind(start_row, end_row)
  str_calls = str.right.of(txt[start],"#< ")
  str_calls = unique(str_calls)
  call.list = lapply(str_calls, function(str) parse(text=str)[[1]])
  names(call.list) = str_calls
  call.list

}

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
      if (is.numeric(res) & !is.null(signif)) {
        digits = max(signif,ceiling(log(res+1,10)))
        res = signif(res, digits)
      }
      if (is.numeric(res) & !is.null(round)) {
        res = round(res, round)
      }
      res
    })
  # speed up compilation
  } else {
    calls = whiskers.call.list[s]
    vals = lapply(calls, function(call) {
      res = try(eval(call,env))
      if (is(res,"try-error")) res = "`Error`"
      if (is.numeric(res) & !is.null(signif)) {
        digits = max(signif,ceiling(log(res+1,10)))
        res = signif(res, digits)
      }
      if (is.numeric(res) & !is.null(round)) {
        res = round(res, round)
      }
      res
    })
  }
  res = str.replace.at.pos(str, pos$outer, unlist(vals))
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

match.blocks.start.end = function(start, end) {
  restore.point("match.blocks")

  end_pos = start_stack = rep(NA, length(start))
  start_stack_ind = 1
  start.i = 1
  end.i= 1
  start_stack[1] = 1
  start = c(start, Inf)
  while (TRUE) {
    top_ind = start_stack[start_stack_ind]

    # Add next start.i to start stack
    start.i = start.i+1

    # Try to clear start_stack
    while (end[end.i]<start[start.i]) {

      end_pos[top_ind] = end.i
      if (start[top_ind]>end[end.i]) {
        stop(paste0("A block closes in position (line) ", end[end.i], " but there is no open block."))
      }
      start_stack_ind = start_stack_ind-1
      end.i = end.i+1

      #cat("\ndel start_stack: ", paste0(start_stack[1:start_stack_ind],
      #"(",start[start_stack[1:start_stack_ind]],")"))

      if (start_stack_ind == 0) break

      top_ind = start_stack[start_stack_ind]
    }

    if (start.i >= length(start)) break

    start_stack_ind = start_stack_ind+1
    start_stack[start_stack_ind] = start.i
    #cat("\nadd start_stack: ", paste0(start_stack[1:start_stack_ind],
    #  "(",start[start_stack[1:start_stack_ind]],")"))

  }
  cbind(start_ind=seq_along(start[-length(start)]), end_ind=end_pos)
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
