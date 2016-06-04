#' Parse a whisker and create meta info
#' @export
make.whisker.info = function(txt, add.variables=FALSE) {
  restore.point("make.whisker.info")
  if (length(txt)==1) txt = sep.lines(txt)
  expr = parse(text=txt)

  if (add.variables) {
    list(
      expr = expr,
      vars = find.variables(expr)
    )
  } else {
    list(
      expr = expr
    )
  }

}



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

  replace.whiskers("{{a}} and {{b}}", list(a=5), eval=FALSE)
}

#' replace whiskers using a list of values, with several options
#' @export
replace.whiskers = function(str, values=parent.frame(), eval=TRUE, signif.digits=NULL, vector.return.first = TRUE, pos=NULL, error.val = "`Error`", empty.val=NULL, use.whisker.render = FALSE, whisker.start = "{{", whisker.end = "}}") {
  restore.point("replace.whiskers")
  if (is.null(str)) return(str)

  if (use.whisker.render) {
    if (!is.null(signif.digits)) {
      for (i in seq_along(valus)) {
        if (is.numeric(values[[i]]))
          values[[i]] = signif.or.round(values[[i]],signif.digits)
      }
    }
    whisker.render(txt,args)
  }

  if (is.null(pos))
    pos = str.blocks.pos(str,whisker.start,whisker.end)
  if (NROW(pos$outer)==0) return(str)

  s = substring(str, pos$inner[,1],pos$inner[,2])
  if (eval) {
    vals = lapply(s, function(su) {
      if (!is.null(su)) {
        res = try(eval(parse(text=su),values))
        if (is(res,"try-error")) res = error.val
      } else {
        res = error.val
      }
      if (vector.return.first) {
        return(unlist(res)[[1]])
      } else {
        paste0(as.character(res), collapse="\n")
      }
    })
  } else {
    values = as.list(values)

    unknown = setdiff(s, names(values))
    if (is.null(empty.val)) {
      values[unknown] = as.list(paste0("{{",unknown,"}}"))
    } else {
      values[unknown] = empty.val
    }
    vals = sapply(values[s], function(val) {
      if (vector.return.first) {
        return(unlist(val)[[1]])
      } else {
        paste0(as.character(unlist(val)), collapse="\n")
      }
    })
  }
  if (!is.null(signif.digits)) {
    for (i in seq_along(vals)) {
      if (is.numeric(vals[[i]]))
        vals[[i]] = signif.or.round(vals[[i]],signif.digits)
    }
  }

  res = str.replace.at.pos(str, pos$outer, unlist(vals))
  res
}

#'
#' @export
signif.or.round = function(val, digits=3) {
  if (any(val>10^digits))
    return(round(val))
  return(signif(val,digits))
}

#' evaluate whiskers and replace them in the text
#' @export
eval.whiskers.in.text <- function(str, envir=parent.frame(), signif= getOption("whiskerSignifDigits")
, round=  getOption("whiskerRoundDigits"), add.params=TRUE, whiskers.call.list=NULL) {
  restore.point("eval.whiskers.in.text")

  if (add.params) {
    envir$params = as.list(envir)
  }
  pos = str.blocks.pos(str,"{{","}}")
  if (NROW(pos$outer)==0) return(str)
  s = substring(str, pos$inner[,1],pos$inner[,2])

  if (is.null(whiskers.call.list)) {
    vals = lapply(s, function(su) {
      res = try(eval(parse(text=su),envir))
      if (is(res,"try-error")) res = "`Error`"
      res
    })
  # speed up compilation
  } else {
    calls = whiskers.call.list[s]
    vals = lapply(calls, function(call) {
      res = try(eval(call,envir))
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

replace.whisker.with.blocks = function(str, envir=parent.frame()) {
  restore.point("replace.whisker.with.blocks")

  pos = str.blocks.pos(str,"{{","}}")
  if (NROW(pos$outer)==0) return(str)
  s = substring(str, pos$inner[,1],pos$inner[,2])
  first2 = substring(s,1,2)
  # whisker blocks
  blocks = first2 == "# " | first2 == "/ "

  rows = which(!blocks)
  vals = lapply(s[rows], function(su) {
    res = try(eval(parse(text=su),envir))
    if (is(res,"try-error")) res = "`Error`"
    res
  })
  res = str.replace.at.pos(str, pos$outer[rows,], unlist(vals))

  # render blocks with default function
  if (sum(blocks)>0) {
    res = whisker::whisker.render(res,envir)
  }

  return(res)
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

