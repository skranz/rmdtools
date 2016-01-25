examples.find.rmd.blocks = function() {
  setwd("D:/libraries/SeminarMatching/semedit_app/yaml")
  file="studseminfo_de.rmd"
  txt = readLines(file)
  df = find.rmd.blocks(txt)
  txt = replace.rmd.blocks(txt = txt,types="note")
  writeClipboard(txt)

  #txt = replace.if.blocks(txt=txt)
}

#' Find all rmd blocks that start with a line `#< ...` and end with a line `#>`
#' @param txt the rmd code, separated into lines
#' @returns A data.frame with the columns start, end, type, arg.str or NULL if no block was found
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

#' replace all given types of rmd blocks
#' @export
replace.rmd.blocks = function(txt, env=parent.frame(), call.list=NULL, block.df = NULL, types=c("note","if"), use.del.rows.na=FALSE, replace.funs=NULL) {
  restore.point("replace.rmd.blocks")

  if (is.null(block.df)) {
    block.df = find.rmd.blocks(txt)
  }
  if (is.null(block.df)) return(txt)

  for (type in types) {
    fun = replace.funs[[type]]
    if (is.null(fun)) {
      fun = eval(parse(text=paste0("replace.",type,".blocks")))
    }
    txt = fun(txt=txt, env=env, call.list=call.list, block.df=block.df, del.rows.na=use.del.rows.na)
    if (!use.del.rows.na)
      block.df = find.rmd.blocks(txt)
  }
  if (use.del.rows.na)
    txt = txt[!is.na(txt)]
  txt
}


#' extract #< if blocks from a rmd txt
#' @export
replace.note.blocks = function(txt, env=parent.frame(), call.list=NULL, block.df=NULL, del.rows.na = FALSE) {
  restore.point("replace.note.blocks")

  if (is.null(block.df)) {
    block.df = find.rmd.blocks(txt)
  }
  if (is.null(block.df)) return(txt)

  block.df = block.df[block.df$type=="note",]
  if (NROW(block.df)==0) return(txt)

  content = sapply(1:NROW(block.df), function(row) {
    paste0(txt[(block.df$start+1):(block.df$end-1)], collapse="\n")
  })
  title = block.df$arg.str
  id.int = sample.int(.Machine$integer.max,NROW(block.df),replace = FALSE)
  id = paste0("collapse_",id.int)

  #shinyBS::bsCollapse(id ="hi", shinyBS::bsCollapsePanel(titel="Title",shiny::HTML("my_content")))

  html = paste0('
<div class="panel-group sbs-panel-group" data-sbs-multi="FALSE" id="',id,'" role="tablist">
<div class="panel panel-default" value="content">
<div class="panel-heading" role="tab" id="heading_cpanel',id.int,'">
<h4 class="panel-title">
<a data-toggle="collapse" href="#cpanel',id.int,'" data-parent="#',id,'">
', content,'
</a>
</h4>
</div>
<div id="cpanel',id.int,'" class="panel-collapse collapse" role="tabpanel">
<div class="panel-body">',title,'</div>
</div>
</div>
</div>
')


  txt = replace.block.txt(txt, html, block.df, del.rows.na)
  txt
}


replace.block.txt = function(txt, block.txt, block.df, del.rows.na=FALSE,...) {
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
replace.if.blocks = function(txt, env=parent.frame(), call.list=NULL, block.df=NULL, warn.if.na=TRUE, del.rows.na=FALSE) {
  restore.point("replace.if.blocks")

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

  add = sapply(seq_along(calls), function(i) {
    restore.point("replace.if.blocks.add")

    call = calls[[i]]
    res = (try(eval(call,envir=env)))
    if ( warn.if.na & !is.logical(res))
      warning("Could not evaluate condition ", str_calls[[i]], " to TRUE or FALSE.")
    isTRUE(res)
  })

  #add = sapply(calls, function(call) isTRUE(eval(call,envir=env)))


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
