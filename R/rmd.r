

#' View an extended rmd file
#' @export
view.rmd = function(file=NULL, text=readLines(file,warn = FALSE), params=NULL, parent.env=parent.frame(), use.blocks=FALSE, use.whiskers=TRUE, start.line=NULL, set.utf8=TRUE, knit=!chunk.like.whisker, chunk.like.whisker=FALSE) {
  restore.point("view.rmd")


  html = compile.rmd(text=text, params=params, parent.env=parent.env, use.blocks=use.blocks, use.whiskers=use.whiskers, start.line = start.line, set.utf8 = set.utf8, knit=knit, chunk.like.whisker = chunk.like.whisker, out.type="html", fragment.only = FALSE)

  out.file <- tempfile(fileext = ".html")
  writeLines(html, out.file)
  rstudio::viewer(out.file)
}

examples.compile.rmd = function() {
  setwd("D:/libraries/rmdtools/test")
  cr = compile.rmd(file="test.Rmd")

  txt = render.compiled.rmd(cr, params = list(x=10))
}

#' Main function to compile rmd to html
#' @export
compile.rmd = function(file=NULL, text=readLines(file,warn = FALSE), params=NULL, parent.env=parent.frame(), if.blocks = c("ph","render", "ignore")[1],  blocks=c("ph","render","ignore")[1], whiskers=c("ph","render","render.as.text", "ignore")[1], chunks=c("ph","knit", "render","ignore")[1], start.line="<!-- START -->", set.utf8=TRUE, out.type = "html", fragment.only=FALSE, whiskers.call.list=NULL, blocks.call.list=NULL, add.info=TRUE) {

  restore.point("compile.rmd")

  if (set.utf8) {
    Encoding(text) = "UTF8"
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

  if.df = hf = ph = NULL
  ph.li = vector("list",3)

  # Start with if.block eval, since we may skip
  # several other replacements
  if (if.blocks == "render") {
    text = replace.if.blocks(text,env = params,call.list = blocks.call.list,warn.if.na = TRUE)
  } else if (if.blocks == "ph") {
    res = adapt.if.blocks(text, env=params)
    if.df = res$hf
    text = res$txt
  }

  # Now proceed with whiskers, since they are not nested
  if (whiskers == "render.as.text") {
    text = paste0(text, collapse="\n")
    text = eval.whiskers.in.text(text,params, whiskers.call.list=whiskers.call.list)
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
    if (!is.null(params)) {
      env = as.environment(params)
      parent.env(env)<-parent.env
    } else {
      env = parent.env
    }
    text = knit.rmd.in.temp(text=text, quiet=TRUE,envir=env, fragment.only=TRUE, out.type=out.type)

    if (out.type == "html") {
      text = gsub("&lt;!&ndash;html_preserve&ndash;&gt;","",html, fixed=TRUE)
      text = gsub("&lt;!&ndash;/html_preserve&ndash;&gt;","",html, fixed=TRUE)
    }

  } else if (out.type=="html") {
    text = markdownToHTML(text=text, fragment.only=fragment.only)
  }

  body.start = NA
  if (out.type == "html" & !fragment.only) {
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



render.compiled.rmd = function(cr,txt = cr$body,params,parent.env=global.env(), fragment.only = FALSE, chunks=c("knit","eval")[1], out.type="html") {
  restore.point("render.compiled.rmd")

  # First replace if df
  txt = replace.if.blocks(txt,env = params,if.df = cr$if.df)

  txt = merge.lines(txt)

  # Let us search for all placeholders
  phs = cr$ph$id
  ph.loc = str.locate.all(txt, pattern=phs,fixed = TRUE)
  has.ph = sapply(ph.loc, function(loc) NROW(loc)>0)
  no.val = cr$ph$value.class == "" | cr$ph$value.class == "error"

  env = as.environment(params)
  if (!is.null(parent.env))
    parent.env(env) = parent.env

  comp.val = (has.ph & no.val)
  new.values = lapply(which(comp.val), function(ind) {
    eval.placeholder(cr$ph[ind,],env=env, chunks=chunks, out.type=cr$out.type)
  })
  cr$ph$value[comp.val] = new.values
  cr$ph$value.class[comp.val] = sapply(new.values, function(val) attr(val, "value.class"))


  # Let us search for all hf
  head.loc = str.locate.first(txt,cr$hf$head, fixed = TRUE)
  food.loc = str.locate.first(txt,cr$hf$foot, fixed = TRUE)


  ph = cr$ph


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


