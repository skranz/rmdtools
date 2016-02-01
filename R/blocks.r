examples.find.rmd.blocks = function() {
  setwd("D:/libraries/SeminarMatching/semedit_app/yaml")
  file="studseminfo_de.rmd"
  txt = readLines(file)
  df = find.rmd.blocks(txt)
  txt = replace.rmd.blocks(txt = txt,types="note")
  writeClipboard(txt)

  #txt = replace.if.blocks(txt=txt)
}

get.block.types.df = function(types) {
  specs = lapply(types, get.block.spec)
  df = data_frame()
}

get.block.spec = function(type) {
  do.call(paste0("get.",type,".block.spec"),list())
}


#' Find all rmd blocks that start with a line `#< ...` and end with a line `#>`
#' @param txt the rmd code, separated into lines
#' @return A data.frame with the columns start, end, type, arg.str or NULL if no block was found
#' @export
find.rmd.blocks = function(txt) {
  restore.point("find.rmd.blocks")

  start = which(str.starts.with(txt,"#<"))
  end = which(str.starts.with(txt,"#>"))

  if (length(start) != length(end)) {
    stop(paste0("You open ", length(start), " blocks but close ", length(end), " blocks"))
  }

  if (length(start)==0) return(NULL)

  blocks = match.blocks.start.end(start, end)
  start_row = start
  end_row = end[blocks[,2]]
  #cbind(start_row, end_row)
  str = str.trim(str.right.of(txt[start],"#< "))

  type = str.trim(str.left.of(str," "))
  arg.str = str.right.of(str," ")

  data.frame(start=start_row, end=end_row, type=type, arg.str=arg.str,stringsAsFactors = FALSE)
}

#' compile all given types of rmd blocks
#' @export
eval.rmd.blocks.in.text = function(txt, to="html", envir=parent.frame(), call.list=NULL, block.df = NULL, only.types=NULL, ignore.types="if", use.del.rows.na=FALSE, replace.funs=NULL) {
  restore.point("eval.rmd.blocks.in.text")

  if (is.null(block.df)) {
    block.df = find.rmd.blocks(txt)
  }
  if (is.null(block.df)) return(txt)
  if (!is.null(only.types))
    block.df = block.df[block.df$type %in% only.types,]
  if (!is.null(ignore.types))
    block.df = block.df[!block.df$type %in% ignore.types,]

  if (NROW(block.df)==0) return(txt)


  for (type in types) {
    fun = replace.funs[[type]]
    if (is.null(fun)) {
      fun = eval(parse(text=paste0("eval.",type,".block")))
    }
    txt = fun(txt=txt, envir=envir, call.list=call.list, block.df=block.df, del.rows.na=use.del.rows.na)
    if (!use.del.rows.na)
      block.df = find.rmd.blocks(txt)
  }
  if (use.del.rows.na)
    txt = txt[!is.na(txt)]
  txt
}


get.blocks.txt = function(txt, block.df, inner=FALSE) {
  if (NROW(block.df)==0) return(character(0))

  sapply(1:NROW(block.df), function(row) {
    paste0(txt[(block.df$start[row]+inner):(block.df$end[row]-inner)], collapse="\n")
  })

}


replace.blocks.txt = function(txt, block.txt, block.df, del.rows.na=FALSE,...) {
  restore.point("replace.block.txt")

  if (NROW(block.df)==0) return(txt)

  block.rows = unlist(lapply(1:NROW(block.df), function(row) {
    block.df$start[row]:block.df$end[row]
  }))
  txt[block.df$start] = block.txt
  del.rows = setdiff(block.rows, block.df$start)
  if (length(del.rows)>0) {
    if (del.rows.na) {
      txt[del.rows] = NA_character_
    } else {
      txt = txt[-del.rows]
    }
  }
  txt

}

#' extract #< if blocks from a rmd txt
#' @export
replace.if.blocks = function(txt, envir=parent.frame(), call.list=NULL, block.df=NULL, warn.if.na=TRUE, del.rows.na=FALSE, if.df=NULL) {
  restore.point("replace.if.blocks")

  if (!is.null(if.df)) {
    return(replace.if.blocks.from.if.df(txt=txt, envir=envir, warn.if.na=warn.if.na,del.rows.na=del.rows.na, if.df=if.df))
  }

  if (is.null(block.df)) {
    block.df = find.rmd.blocks(txt)
  }
  if (is.null(block.df)) return(txt)

  block.df = block.df[block.df$type=="if",]
  if (NROW(block.df)==0) return(txt)

  str_calls = block.df$arg.str
  if (is.null(call.list)) {
    calls = lapply(str_calls, function(str) parse(text=str))
  } else {
    calls = call.list[str_calls]
  }

  force(envir)
  add = rep(FALSE, length(calls))
  for (i in seq_along(calls)) {
    res = try(eval(calls[[i]],envir),silent = TRUE)
    if ( warn.if.na & !is.logical(res))
      warning("Could not evaluate condition ", str_calls[[i]], " to TRUE or FALSE.")
    add[i] = isTRUE(res)
  }


  del.rows = unique(unlist(lapply(which(!add),function(ind){
    block.df$start[ind]:block.df$end[ind]
  })))
  del.rows = unique(c(del.rows, block.df$start, block.df$end))

  if (length(del.rows)>0) {
    if (del.rows.na) {
      txt[del.rows] = NA_character_
    } else {
      txt = txt[-del.rows]
    }
  }
  return(txt)
}

replace.if.blocks.from.if.df = function(txt, envir=parent.frame(), warn.if.na=TRUE, del.rows.na=FALSE, if.df=NULL,...) {
  restore.point("replace.if.blocks.from.if.df")

  if (NROW(if.df)==0) return(txt)

  if (NROW(txt)==1) txt = sep.lines(txt)

  start = match(if.df$head,txt)
  end = match(if.df$foot, txt)
  rows = which(!is.na(start))
  if (NROW(rows)==0) return(txt)

  add = sapply(rows, function(row) {
    restore.point("replace.if.blocks.add")
    call = if.df$info[[row]]$cond.call
    res = (try(eval(call,envir=envir)))
    if ( warn.if.na & !is.logical(res))
      warning("Could not evaluate condition ", if.df$info[[row]]$cond.str, " to TRUE or FALSE.")
    isTRUE(res)
  })

  del.rows = unique(unlist(lapply(which(!add),function(ind){
    start[rows[ind]]:end[rows[ind]]
  })))
  del.rows = unique(c(del.rows, start[rows], end[rows]))

  if (length(del.rows)>0) {
    if (del.rows.na) {
      txt[del.rows] = NA_character_
    } else {
      txt = txt[-del.rows]
    }
  }
  return(txt)
}


match.blocks.start.end = function(start, end) {
  restore.point("match.blocks.start.end")

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

#' Parse the name of an rmd block
#' @export
parse.block.args = function(header) {
  restore.point("parse.block.args")

  str = header
  tokens = str.split(str,",")
  str = str.trim(str.right.of(str,"#< "))
  type = str.left.of(str," ")
  str = str.right.of(str," ")
  code = paste0("list(",str,")")
  li = eval(base::parse(text=code,srcfile=NULL))

  if (length(li)==0) return(list(name=NULL))
  if (is.null(names(li))) {
    return(list(type=type,name=li[[1]]))
  } else if (nchar(names(li)[1]) == 0) {
    return(c(list(type=type,name=li[[1]]),li[-1]))
  }
  li
}


make.block.info = function(txt, type=NULL, arg.str) {
  restore.point("make.block.info")

  if (is.null(type)) {
    txt = sep.lines(txt)
    header = txt[1]
    type = str.trim(str.between(txt,"#< "," "))
  }

  fun = eval(parse(text=paste0("make.",type,".block.info")))
  fun(txt=txt, type=type, arg.str=arg.str)
}

#' Adapt header and footer of if blocks for output format
#' and parse already the if condition for faster runtime evaluation
#'
#' @export
adapt.hf.blocks = function(txt, block.df=NULL, out.type="html",only.types=c("if","note"),...) {
  restore.point("adapt.if.blocks")

  if (is.null(block.df)) {
    block.df = find.rmd.blocks(txt)
  }
  if (!is.null(only.types)) {
    block.df = block.df[block.df$type %in% only.types,,drop=FALSE]
  }
  if (NROW(block.df)==0) {
    return(list(txt=txt, if.df=NULL))
  }

  id = paste0(block.df$type,"_",random.string(n=NROW(block.df)))

  head = paste0("<!-- _START_",id," ", block.df$arg.str, " -->")
  foot = paste0("<!-- _END_",id, " -->")

  hf = data_frame(
    id = id,
    type = block.df$type,
    head = head,
    foot = foot,
    value = vector("list", NROW(block.df)),
    #value.class = rep("", NROW(block.df)),
    info = lapply(1:NROW(block.df), function(row) {
      make.block.info(txt = txt[block.df$start[row]:block.df$end[row]], arg.str = block.df$arg.str[row], type=block.df$type[row])
    })
  )
  names(hf$value) = hf$id
  txt[block.df$start] = head
  txt[block.df$end] = foot

  list(txt=txt, hf=hf)
}


eval.hf = function(txt, hf, envir = parent.frame(), dir=getwd(),out.type="html",cr=NULL, ...) {
  restore.point("eval.hf")

  fun = eval(parse(text=paste0("eval.", hf$type,".block")))
  res = fun(txt=txt,envir=envir,out.type=out.type,chunk=chunk, info=hf$info[[1]], cr=cr, has.header=FALSE)

  if (is(res,"try-error")) {
     res = paste0("`Error when evaluating ", ph$type, " ", ph$txt, ":\n\n", as.character(res),"`")
     attr(res,"value.class") = "error"
  }
  res
}


#' Adapt header and footer of if blocks for output format
#' and parse already the if condition for faster runtime evaluation
#'
#' @export
adapt.if.blocks = function(txt, block.df=NULL,out.type="html",only.types="if",...) {
  restore.point("adapt.if.blocks")
  adapt.hf.blocks(txt=txt, block.df=block.df, out.type=out.type, only.types=only.types)
}

make.if.block.info = function(txt, arg.str,...) {
  cond.call = parse(text=arg.str)[[1]]
  list(
    cond.str = arg.str,
    cond.call = cond.call
  )
}
