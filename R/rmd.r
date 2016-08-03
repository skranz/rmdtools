
examples.compile.rmd = function() {
  setwd("D:/libraries/rmdtools/test")
  #view.rmd("test.Rmd", envir=list(x=10))

  options(warn=2)
  cr = compile.rmd(file="test.Rmd", out.type = "html")
  cr = eval.placeholders(cr, envir = list(x=0, today=Sys.Date()))
  ph = cr$ph

  html = render.compiled.rmd(cr, envir = list(x=0))
  ui = render.compiled.rmd(cr, envir = list(x=0))
  view.html(ui = ui)
  view.html(text=html)

  view.rmd(ui=rmd)

  txt
  ui = render.compiled.rmd(cr,envir = list(x=15), out.type = "shiny")
}


#' View an extended rmd file
#' @export
view.rmd = function(file=NULL, text=readLines(file,warn = FALSE), envir=list(), start.line="<!-- START -->", end.line = "<!-- END -->", set.utf8=TRUE, knit=!chunks.like.whisker, chunks.like.whisker=FALSE, out.type = "shiny", launch.browser=rstudio::viewer, use.commonmark=FALSE) {
  restore.point("view.rmd")

  cr = compile.rmd(file=file, text=text, envir=envir, out.type = out.type, start.line=start.line, end.line=end.line, chunks="ph", fragment.only=TRUE, use.commonmark=use.commonmark)

  ph = cr$ph
  if (out.type == "shiny") {
    library(shinyEvents)

    if (isTRUE(getApp()$view.rmd.app)) {
      app = getApp();
      app$view.rmd.app<-FALSE
      msg = paste0("Nested call of view.rmd. Make sure you have correctly entered the start line:\n\n", start.line,"\n\nafter your initial chunk.")
      warning(msg)
      return(msg)
    }

    app = eventsApp()
    app$view.rmd.app = TRUE
    on.exit(try({app = getApp(); app$view.rmd.app<-FALSE}))
    if (chunks.like.whisker) {
      chunks = "eval"
    } else {
      chunks = "knit"
    }
    ui = render.compiled.rmd(cr,envir=envir, out.type = "shiny", chunks=chunks)
    app$ui = fluidPage(
      ui
    )
    app$view.rmd.app=FALSE

    runEventsApp(app,launch.browser = launch.browser)
  } else {
    html = render.compiled.rmd(cr,envir=envir,fragment.only=FALSE, chunks=chunks)
    out.file <- tempfile(fileext = ".html")
    writeLines(html, out.file)
    rstudio::viewer(out.file)
  }
  return(invisible())
}

#' Main function to compile rmd to html
#' @export
compile.rmd = function(file=NULL, text=readLines(file,warn = FALSE), envir=list(), if.blocks = c("ph","render", "ignore")[1],  blocks=c("ph","render","ignore")[1], whiskers=c("ph","render","render.as.text", "ignore")[1], chunks=c("ph","knit", "render","ignore")[1], start.line="<!-- START -->",end.line = "<!-- END -->", set.utf8=TRUE, out.type = "html", fragment.only=FALSE, whiskers.call.list=NULL, blocks.call.list=NULL, add.info=TRUE, use.commonmark=FALSE) {

  restore.point("compile.rmd")

  if (set.utf8) {
    Encoding(text) = "UTF8"
    text = mark_utf8(text)
  }

  text = rmd.between.start.end.lines(text,start.line=start.line, end.line=end.line)

  if.df = hf = ph = NULL
  ph.li = vector("list",3)

  # Start with if.block eval, since we may skip
  # several other replacements

  if (if.blocks == "render") {
    text = replace.if.blocks(text,envir = envir,call.list = blocks.call.list,warn.if.na = TRUE)
  } else if (if.blocks == "ph") {
    res = adapt.if.blocks(text, envir=envir)
    if.df = res$hf
    text = res$txt
  }

  # Now proceed with whiskers, since they are not nested
  if (whiskers == "render.as.text") {
    text = paste0(text, collapse="\n")
    text = eval.whiskers.in.text(text,envir=envir, whiskers.call.list=whiskers.call.list)
  } else if (whiskers != "ignore") {
    res = rmd.whiskers.to.placeholders(text, add.info=add.info)
    text = res$txt
    ph.li[[1]] = res$ph
  }

  # Now proceed with chunks
  if (chunks == "render" | chunks == "ph") {
    res = rmd.chunks.to.placeholders(text)
    text = res$txt
    ph.li[[2]] = res$ph
  }

  block.df = find.rmd.blocks(text)
  types = unique(block.df$type)

  block.specs = lapply(types, get.block.spec)
  names(block.specs) = types
  block.libs = unique(unlist(lapply(block.specs, function(spec) spec$libs)))

  # Now proceed with blocks
  if (blocks != "ignore") {
    hf.types = types[sapply(block.specs, function(spec) spec$is.hf)]
    ph.types = setdiff(types, hf.types)

    res = adapt.hf.blocks(text, block.df=block.df, only.types=hf.types)
    hf = res$hf
    text = res$txt

    res = rmd.blocks.to.placeholders(text, add.info=add.info)
    text = res$txt
    ph.li[[3]] = res$ph
  }
  # Directly render blocks
  ph = do.call(rbind, ph.li)


  # Transform remaining text to out.type
  if (chunks == "knit") {
    text = knit.rmd.in.temp(text=text, quiet=TRUE,envir=envir, fragment.only=fragment.only, out.type=out.type,use.commonmark=use.commonmark)

    if (out.type == "html" | out.type == "shiny") {
      text = gsub("&lt;!&ndash;html_preserve&ndash;&gt;","",text, fixed=TRUE)
      text = gsub("&lt;!&ndash;/html_preserve&ndash;&gt;","",text, fixed=TRUE)
    }

  } else if (out.type=="html" | out.type == "shiny") {
    text = md2html(text, fragment.only=fragment.only,use.commonmark=use.commonmark)
  }

  body.start = NA
  if ( (out.type == "html" | out.type == "shiny") & !fragment.only) {
    text = merge.lines(text)
    ltext = tolower(text)
    head.start = str.locate.first(ltext,"<head>",fixed = TRUE)
    head.end = str.locate.first(ltext,"</head>",fixed = TRUE)
    head = substring(text, head.start[,2]+1, head.end[,1]-1)

    body.start = str.locate.first(ltext,"<body>",fixed = TRUE)
    body.end = str.locate.first(ltext,"</body>",fixed = TRUE)
    body = substring(text, body.start[,2]+1, body.end[,1]-1)
  } else {
    body = text
    head = NULL
  }


  cr = nlist(
    head = head,
    body = body,
    out.type,
    if.df,
    ph,
    hf,
    block.specs,
    block.libs,
    block.types = types
  )
  return(cr)
}

#' Extract rmd txt between start.line and end.line tag
#' @export
rmd.between.start.end.lines = function(txt,start.line="<!-- START -->", end.line = "<!-- END -->", return.start.end = FALSE) {
  restore.point("rmd.between.start.end.lines")

    # skip lines until start.tag

  if (is.character(start.line)) {
    rows = which(str.trim(txt)==start.line)
    if (length(rows)>0) {
      start.line = rows[1]+1
    } else {
      start.line = NULL
    }
  }
  if (is.character(end.line)) {
    rows = which(str.trim(txt)==end.line)
    if (length(rows)>0) {
      end.line = rows[1]-1
    } else {
      end.line = length(txt)
    }
  }

  if (!is.null(start.line) | !is.null(end.line)) {
    if (is.null(start.line)) start.line = 1
    if (is.null(end.line)) end.line = length(txt)
    txt = txt[start.line:end.line]
  }
  if (return.start.end) {
    return(list(txt = txt,start=start.line, end=end.line))
  } else {
    return(txt)
  }
}

# correct bugs in markdown conversion that destroys HTML comments
correct.hf.html = function(txt,hf=NULL, if.df=NULL) {
  restore.point("correct.hf.html")

  txt = merge.lines(txt)
  str = c(hf$head,hf$foot, if.df$head, if.df$foot)
  if (length(str)==0) return(txt)
  wrong1 = gsub("<!--","<!&ndash;",str,fixed=TRUE)
  wrong2 = gsub("-->","&ndash;>",str,fixed=TRUE)
  wrong3 = gsub("-->","&ndash;>",str,fixed=TRUE)

  txt = str.replace(txt,c(wrong1,wrong2,wrong3),rep(str, times=3), fixed=TRUE)

  txt
}



#' Render a compiled rmd
#' @export
render.compiled.rmd = function(cr=NULL,txt = cr$body,envir=parent.frame(), fragment.only = FALSE, chunks=c("knit","eval")[1], out.type=if (is.null(cr$out.type)) "html" else cr$out.type, use.print="none", overwrite.values = FALSE, on.error="error") {
  restore.point("render.compiled.rmd")

  # First replace if df
  txt = replace.if.blocks(txt,envir = envir,if.df = cr$if.df)

  txt = merge.lines(txt)

  # Let us search for all placeholders
  phs = cr$ph$id
  ph.loc = str.locate.all(txt, pattern=phs,fixed = TRUE)

  if (length(ph.loc)>0) {
    has.ph = sapply(ph.loc, function(loc) NROW(loc)>0)

    # Need to think of how to deal with value.class
    #no.val = cr$ph$value.class == "" | cr$ph$value.class == "error"
    if (overwrite.values) {
      no.val = rep(TRUE, length(phs))
    } else {
      no.val = sapply(cr$ph$value, is.null,USE.NAMES = FALSE)
    }
    comp.val = (has.ph & no.val)
    ind = 1
    new.values = lapply(which(comp.val), function(ind) {
      restore.point("inner.ph")
      val = eval.placeholder(cr$ph[ind,],envir=envir, chunks=chunks, out.type=cr$out.type, cr=cr, on.error=on.error)
      render.value(val, out.type=out.type)
    })
    cr$ph$value[comp.val] = new.values
    #cr$ph$value.class[comp.val] = sapply(new.values, function(val) attr(val, "value.class"))
  }


  # Let us search for all hf
  head.loc = str.locate.first(txt,cr$hf$head, fixed = TRUE)
  foot.loc = str.locate.first(txt,cr$hf$foot, fixed = TRUE)

  comp.val = !is.na(head.loc[,1])
  new.values = lapply(which(comp.val), function(ind) {
    restore.point("ufz8be7f47fb3w6xq")
    val = eval.hf(txt = substring(txt, head.loc[ind,2]+1,foot.loc[ind,1]-1 ), cr$hf[ind,],envir=envir, out.type=out.type, cr=cr)
    render.value(val, out.type=out.type)
  })
  cr$hf$value[comp.val] = new.values

  # replace hf with whiskers
  start = head.loc[comp.val,1]
  end = foot.loc[comp.val,2]


  if (out.type == "shiny") {
    # transform to a shiny tag.list
    # replace hf by whisker with id
    if (length(start)>0) {
      txt = str.replace.at.pos(txt,pos = cbind(start,end),new = paste0("{{",cr$hf$id[comp.val],"}}"))

    }
    vals = c(cr$ph$value,cr$hf$value)
    li = whiskered.html.to.list(txt, vals)
    return(tagList(li))
  } else {
    # transform to text
    if (length(start)>0) {
      txt = str.replace.at.pos(txt,pos = cbind(start,end),new = cr$hf$value[comp.val])
    }
    if (length(which(has.ph))>0) {
      txt = replace.whiskers(txt,values=cr$ph$value[has.ph], use.print=use.print)
    }
  }

  txt
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
    str = str.right.of(str," ")
    res = try(parse(text=str)[[1]])
    if (is(res,"try-error")) return(NULL)
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

render.value = function(val, out.type="html",...) {
  restore.point("render.value")

  if (out.type == "shiny") {
    if (is.list(val) | is(val,"shiny.stag")) {
      return(val)
    }
    if (is.character(val)) return(HTML(val))
    if (is.data.frame(val)) {
      return(table.knit_print.data.frame(val,...))
    }
    trash = capture.output(res <- knit_print(val))
    return(res)
  }

  if (is(val,"shiny.tag.list")) {
    children = lapply(val, render.value,out.type=out.type,...)
    val = paste0(children, collapse="\n")
  }

  if (is.character(val)) {
    return(val)
  }
  if (is.data.frame(val)) {
    return(table.knit_print.data.frame(val,...))
  }
  trash = capture.output(res <- knit_print(val))
  res
}

whiskered.html.to.list = function(txt, values, transform.txt.fun=HTML, use.print="none") {
  restore.point("whiskered.txt.to.list")
  whiskered.txt.to.list(txt,values,transform.txt.fun, use.print=use.print)
}


whiskered.txt.to.list = function(txt, values, transform.txt.fun=NULL, use.print="none") {
  restore.point("whiskered.txt.to.list")

  if (length(values)==0) {
    if (!is.null(transform.txt.fun))
      return(list(transform.txt.fun(txt)))
    list(txt)
  }

  pieces <- strsplit(txt, "{{", fixed = TRUE)[[1]]
  pieces <- strsplit(pieces, "}}", fixed = TRUE)
  if (length(pieces[[1]]) > 1) {
      stop("Mismatched {{ and }} in whiskered txt.")
  }
  lapply(pieces[-1], function(x) {
      if (length(x) != 2) {
          stop("Mismatched {{ and }} in whiskered txt.")
      }
  })

  if (use.print!="none") {
    print.fun = eval(parse(text(paste0(use.print,"_",print))))
    if (!is.null(transform.txt.fun)) {
      outer.transform.txt.fun = transform.txt.fun
      transform.txt.fun = function(txt) {
        outer.transform.txt.fun(print.fun(txt))
      }
    } else {
      transform.txt.fun = print.fun
    }
  }

  if (!is.null(transform.txt.fun))
    pieces[[1]] <- transform.txt.fun(pieces[[1]])

  pieces[-1] <- lapply(pieces[-1], function(piece) {
    restore.point("inner7z43r4hr")

    str = piece[[2]]
    if (!is.null(transform.txt.fun))
      str = transform.txt.fun(str)

    if (is.null(values[[piece[1]]])) {
      warning(paste0("No value for whisker ", piece[1]))
      #stop()
    }

    list(values[[piece[1]]],str)
  })
  pieces
}
