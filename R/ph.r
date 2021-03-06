
#' Reverses whisker placeholders by their original whiskers
#'
#' May be useful if blocks evaluate whiskers themselves or
#' after rmd to html transformation
#' @export
reverse.whisker.placeholders = function(txt,ph=cr$ph,cr=NULL)  {
	ph = ph[ph$type == "whisker",]
  # replace ph with original text
	repl = as.list(paste0("{{",ph$txt,"}}"))
	names(repl) = ph$id
	txt = replace.whiskers(txt, repl,eval = FALSE)
  txt

}



#' Evaluate placeholders in compiled rmd
#' @export
eval.placeholders = function(cr=NULL,envir = parent.frame(),ph=cr$ph, type=NULL, on.error=c("null","error","stop")[1], out.type= first.none.null(cr$out.type,"html"),...) {
  restore.point("eval.placeholders")

  if (is.null(cr[["ph"]])) return(cr)
  if (!is.null(type)) {
    rows = which(cr$ph$type==type)
  } else {
    rows = seq_len(NROW(cr$ph))
  }
  cr$ph$value[rows] = lapply(rows, function(row) {
    eval.placeholder(ph=cr$ph[row,], envir=envir, on.error=on.error, out.type=out.type,cr=cr,...)
  })
  cr

}

#' Make a info for a placeholder object
#' @export
make.placeholder.info = function(txt, type, form) {
  if (form=="block") return(make.block.info(txt,type=type))
  if (form=="chunk") return(make.chunk.info(txt))
  if (form=="whisker") return(make.whisker.info(txt))
  stop(paste0("unknown placeholder form: ", form))
}

#' Evaluate a placeholder and return its value
#' @export
eval.placeholder = function(ph, envir = parent.frame(), chunks="knit", dir=getwd(),out.type="html",cr=NULL, on.error=c("null","error","stop")[1], use.commonmark=TRUE, ...) {
  restore.point("eval.placeholder")

  # chunks with output shiny or eval will always be evaluated
  if (ph$type == "chunk" & chunks=="knit" & !isTRUE(ph$info[[1]]$args$results %in% c("shiny","eval"))) {
    res = try(knit.chunk(ph$txt,envir=envir, knit.dir=dir,out.type=out.type, use.commonmark=use.commonmark))
  } else if (ph$form == "block") {
    fun = eval(parse(text=paste0("eval.", ph$type,".block")))
    res = fun(txt=ph$txt,envir=envir,out.type=out.type, info=ph$info[[1]], cr=cr)
  } else {
    res = try(eval(ph$info[[1]]$expr, envir), silent=TRUE)
  }

  if (is(res,"try-error")) {
    if (on.error == "error") {
      value = paste0("`Error when evaluating ", ph$type, " ", ph$txt, ":\n\n", as.character(res),"`")
      attr(value,"value.class") = "error"
    } else if (on.error == "stop") {
      stop(as.character(res))
    } else {
      value = NULL
    }
  } else {
    value = res
    #attr(value,"value.class") = class(value)[1]
  }
  value
}


find.placeholders = function(txt) {
  txt = paste0(txt, collapse="\n")
  str.find(txt,pattern = "\\{\\{.*\\}\\}",fixed = FALSE, matches=TRUE)
}

set.rmd.placeholders = function(txt,whisker.prefix="{{", whisker.postfix="}}", chunks=TRUE, blocks=TRUE, whiskers=TRUE, ignore.block.types="if", add.info=TRUE) {
  restore.point("set.rmd.placeholders")

  df.li = vector("list",3)

  if (whiskers) {
    res = rmd.whiskers.to.placeholders(txt, whisker.prefix=whisker.prefix, whisker.postfix=whisker.postfix, add.info=add.info)
    txt = res$txt
    df.li[[1]] = res$ph
  }

  if (chunks) {
    res = rmd.chunks.to.placeholders(txt, whisker.prefix=whisker.prefix, whisker.postfix=whisker.postfix, add.info=add.info)
    txt = res$txt
    df.li[[2]] = res$ph
  }

  if (blocks) {
    res = rmd.blocks.to.placeholders(txt, whisker.prefix=whisker.prefix, whisker.postfix=whisker.postfix,ignore.block.types=ignore.block.types, add.info=add.info)
    txt = res$txt
    df.li[[3]] = res$ph
  }

  ph = rbind(df.li[[1]], df.li[[2]], df.li[[3]])
  return(list(txt=txt, ph=ph))

}

#' Replace chunks with placeholderss of the form {{id}}
#' @export
rmd.chunks.to.placeholders = function(txt,whisker.prefix="{{", whisker.postfix="}}", del.rows.na=FALSE, add.info=TRUE, id=NULL) {
  restore.point("rmd.chunks.to.placeholders")

  df = find.rmd.chunks(txt)

  if (NROW(df)==0) {
    return(list(txt=txt, ph=NULL))
  }
  if (is.null(id)) {
    name = normalize.id(df$name)
    id = paste0("CHUNK_",name,"_",random.string(n=NROW(df),nchar=12))
  }

  chunk.txt = sapply(1:NROW(df), function(row) {
    paste0(txt[df$start[row]:df$end[row]], collapse="\n")
  })

  ph = fast_df(
    id = id,
    type = "chunk",
    form = "chunk",
    txt = chunk.txt,
    info = lapply(1:NROW(df), function(row) {
      make.chunk.info(txt = chunk.txt[row],id=id[row])
    }),
    #value.class = "",
    value = vector("list",NROW(df))
  )
  names(ph$value) = ph$id

  txt = replace.blocks.txt(txt, paste0(whisker.prefix,id, whisker.postfix), df, del.rows.na=del.rows.na)

  list(txt=txt, ph=ph)

}





#' Replace whiskers with placeholders of the form {{id}}
#' @export
rmd.whiskers.to.placeholders = function(txt, whisker.prefix="{{", whisker.postfix="}}", add.info=TRUE) {
  restore.point("rmd.whiskers.to.placeholders")

  multi.line = length(txt)>1

  if (multi.line) txt = merge.lines(txt)

  pos = str.blocks.pos(txt,"{{","}}")
  if (NROW(pos$outer)==0) {
    txt = sep.lines(txt)
    return(list(txt = txt, ph=NULL))
  }
  s = substring(txt, pos$inner[,1],pos$inner[,2])
  id = paste0("whisker_", seq_along(s),"_",random.string(length(s),nchar=12))
  info = lapply(s, make.whisker.info)
  ph = fast_df(
    id = id,
    type = "whisker",
    form = "whisker",
    txt = s,
    info = info,
    #value.class = "",
    value = vector("list",NROW(id))
  )
  names(ph$value) = ph$id

  txt = str.replace.at.pos(txt, pos$outer, paste0(whisker.prefix,id,whisker.postfix))

  txt = sep.lines(txt)
  list(txt=txt, ph=ph)

}


#' Replace blocks with placeholderss of the form {{id}}
#'
#' TODO: Need to deal with nested blocks: replace from inner to outer
#'
#' @export
rmd.blocks.to.placeholders = function(txt, block.df=NULL, whisker.prefix="{{", whisker.postfix="}}", del.rows.na = FALSE,ignore.types=NULL,only.types=NULL, add.info=TRUE,...) {
  restore.point("rmd.blocks.to.placeholders")

  if (is.null(block.df)) {
    block.df = find.rmd.blocks(txt)
  }
  if (!is.null(ignore.types)) {
    block.df = block.df[! block.df$type %in% ignore.types,,drop=FALSE]
  }
  if (!is.null(only.types)) {
    block.df = block.df[block.df$type %in% only.types,,drop=FALSE]
  }

  if (NROW(block.df)==0) {
    return(list(txt=txt, ph=NULL))
  }
  id = paste0("block_", block.df$type,"_",block.df$start,"_",random.string(NROW(block.df)))
  ph = fast_df(
    id = id,
    type = block.df$type,
    form = "block",
    txt = get.blocks.txt(txt, block.df, inner=FALSE),
    #value.class = "",
    value = vector("list",NROW(block.df))
  )
  ph$info = lapply(1:NROW(ph), function(row) {
    make.block.info(txt = ph$txt[row],type=ph$type[row])
  })

  names(ph$value) = ph$id

  txt = replace.blocks.txt(txt, paste0(whisker.prefix,id, whisker.postfix), block.df, del.rows.na=del.rows.na)

  list(txt=txt, ph=ph)
}

