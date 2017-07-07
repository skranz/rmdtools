#' Knits the rmd txt inside a temporary directory instead of the current wd
#'
#' Does not create /figure subfolder in current wd
#' @export
knit.chunk = function(text, envir=parent.frame(), fragment.only=TRUE, quiet=TRUE, encoding = getOption("encoding"), html.table = TRUE, out.type="html", knit.dir=getwd(), use.commonmark = TRUE, deps.action = c("add","ignore")[1], args=NULL, eval_mode=c("knit","sculpt","eval","html")[1], show_code=c("no","note","open_note", "note_after","open_note_after", "before","after")[1], code.highlight=use.commonmark, ...) {
  restore.point("knit.chunk")

  text = sep.lines(text)

  if (is.list(envir)) {
    envir =list2env(envir)
    parent.env(envir) = globalenv()
  }
  owd <- setwd(knit.dir)
  on.exit(setwd(owd))

  #knitr::opts_knit$set(root.dir = owd)
  if (html.table) {
    old.printer =.GlobalEnv$knit_print.data.frame
    .GlobalEnv$knit_print.data.frame = table.knit_print.data.frame
  }

  old.rmarkdown.pandoc.to = opts_knit$get("rmarkdown.pandoc.to")
  if (out.type %in% c("html","shiny"))
    opts_knit$set(rmarkdown.pandoc.to="html")

  if (is.null(args)) args = rmdtools::parse.chunk.args(text[1])

  # also use default options
  #chunk_opts[names(args)] = args
  #args = chunk_opts

  chunk_opts = knitr::opts_chunk$get()
  eval_mode = first.non.null(args$eval_mode,chunk_opts$eval_mode, eval_mode,"knit")
  show_code = first.non.null(args$show_code,chunk_opts$show_code, show_code,"no")
  centered = isTRUE(first.non.null(args$centered,chunk_opts$centered))


  code = org.code =  text[-unique(c(1, length(text)))]

  if (eval_mode == "sculpt") {
    # only show last line of code
    restore.point("sculpt.chunk")
    code = paste0('{
      ..TEmP..FiLe <- tempfile()
      sink(..TEmP..FiLe)
      on.exit(sink())
      on.exit(file.remove(..TEmP..FiLe), add = TRUE)
    ', paste0(org.code,collapse="\n"),'
    }'
    )
    args$message = args$warning = args$echo = FALSE
    header = args.to.chunk.header(args)
    text = c(header, sep.lines(code),"```")
  }
  if (eval_mode == "html" | eval_mode == "eval") {
    if (length(code)>0) {
      html = eval(parse(text = code))
    } else {
      html = ""
    }
    meta = list()
    if (out.type =="md" | out.type=="rmd") return(html)

  } else {

    md = knitr::knit(text = text, envir = envir, encoding = "UTF8", quiet = quiet)

    meta = unique(knit_meta(clean=TRUE))

    opts_knit$set(rmarkdown.pandoc.to=old.rmarkdown.pandoc.to)

    if (html.table) {
      if (!is.null(old.printer)) {
        .GlobalEnv$knit_print.data.frame = old.printer
      } else {
        suppressWarnings(rm("knit_print.data.frame",envir=.GlobalEnv))
      }
    }
    if (out.type =="md" | out.type=="rmd") return(md)

    #writeClipboard(html)
    if (eval_mode != "sculpt") {
      html = md2html(text=md, fragment.only=fragment.only, use.commonmark = use.commonmark)
    } else {
      html = md
    }

  }



  # automatic code highlighting
  if (code.highlight) {
    html = paste0(html,'\n<script class="remove_offline">
$("pre code.r, pre code.language-r").each(function(i, e) {hljs.highlightBlock(e)});
</script>')
  }

  is.dep = unlist(lapply(meta, function(el) is(el,"html_dependency")))
  deps = meta[is.dep]
  is.head = unlist(lapply(meta, function(el) is(el,"shiny_head")))


  if (out.type == "shiny") {
    ui = HTML(html)
    if (deps.action=="add") {
      deps.ui = HTML(renderDependencies(deps))
      ui = tagList(deps.ui,meta[is.head], ui)
    } else {
      #attr(ui,"knit_deps") <- deps
    }
    ui = attachDependencies(ui, deps)

    if (centered)
      ui = tags$div(style="display: flex;
  justify-content: center;", ui)

    if (show_code != "no")
      ui = add.code.ui(ui, code=org.code, show_code=show_code)



    attr(ui,"knit_meta") <- meta

    return(ui)
  } else if (out.type == "html") {
    # simply add dependencies
    # there may be a lot of redudancies in this approach
    if (deps.action=="add") {
      deps.html = renderDependencies(deps)
      head.html = unlist(meta[is.head])
      html = merge.lines(c(deps.html,head.html,html))
    } else {
      #attr(html,"knitr_deps") <- deps
    }
  }
  attr(html,"knit_meta") <- meta
  html
}

#' Add code button with div to an ui
#' @export
add.code.ui = function(ui = NULL, code, show_code, code.highlight=TRUE)  {
  restore.point("add.code.ui")
  id = paste0("codeBtn", random.string())
  code.ui = NULL
  empty.lines = which(cumsum(nchar(code))==0)
  if (length(empty.lines)>0) {
    code = code[-(1:max(empty.lines))]
  }
  code.html = paste0('<pre><code class="r">',paste0(code, collapse="\n"),"\n</code></pre>")
  if (code.highlight) {
    code.html = paste0(code.html,
'\n<script class="remove_offline">
$("pre code.r, pre code.language-r").each(function(i, e) {hljs.highlightBlock(e)});
</script>'
    )
  }


  if (show_code %in% c("note","open_note","note_after","open_note_after")) {
    code.ui = hideShowButton(id,"Code",content=HTML(code.html), show=str.starts.with(show_code,"open_"))
    #code.ui = shinyEventsUI::slimCollapsePanel(title="code", HTML(code.html))
  } else if (show_code %in% c("before","after")) {
    code.ui = HTML(code.html)
  }

  if (str.ends.with(show_code,"_after")) {
    ui = tagList(ui,code.ui)
  } else if (!is.null(code.ui)) {
    ui = tagList(code.ui,ui)
  }
  ui

}


#' Knits the rmd txt
#'
#' @export
knit.rmd = function(text, envir=parent.frame(), fragment.only=TRUE, quiet=TRUE, encoding = getOption("encoding"), html.table = TRUE, out.type="html", use.commonmark=FALSE) {
  restore.point("knit.rmd")

  if (is.list(envir)) {
    envir =list2env(envir)
    parent.env(envir) = globalenv()
  }

  #knitr::opts_knit$set(root.dir = owd)
  if (html.table) {
    old.printer =.GlobalEnv$knit_print.data.frame
    .GlobalEnv$knit_print.data.frame = table.knit_print.data.frame
  }

  md = knitr::knit(text = text, envir = envir, encoding = encoding, quiet = quiet)
  #knitr:::.knitEnv$meta
  #knit_meta(clean=FALSE)

  if (html.table) {
    if (!is.null(old.printer)) {
      .GlobalEnv$knit_print.data.frame = old.printer
    } else {
      suppressWarnings(rm("knit_print.data.frame",envir=.GlobalEnv))
    }
  }

  if (out.type =="md" | out.type=="rmd") return(md)

  #writeClipboard(html)
  html = md2html(text=md, fragment.only=fragment.only, use.commonmark = use.commonmark)
  if (out.type == "shiny") return(HTML(html))
  html
}

#' Knits the rmd txt inside a temporary directory instead of the current wd
#'
#' Does not create /figure subfolder in current wd
#' @export
knit.rmd.in.temp = function(text, envir=parent.frame(),...) {
  restore.point("knit.rmd.in.temp")
  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  knit.rmd(text, envir,...)
}



#' Render with RMarkdown::render the rmd txt inside a temporary directory instead of the current wd
#'
#' Does not create /figure subfolder in current wd
#' @export
render.rmd.in.temp = function(text, envir=parent.frame(), quiet=TRUE,...) {
  restore.point("render.rmd.in.temp")

  dir = tempdir()
  owd <- setwd(dir)
  on.exit(setwd(owd))

  #knitr::opts_knit$set(root.dir = owd)
  input.file = tempfile(fileext=".Rmd", tmpdir=dir)
  writeLines(text, input.file)

  out.file = rmarkdown::render(input=input.file,output_dir=dir,envir=envir,quiet=quiet,...)
  html = readLines(out.file)
  html
}

#' A button that toogles whether content in a div
#' is displayed or not
#' @export
hideShowButton = function(id, label, content=NULL,div.id=NULL,shown=FALSE, ...) {
  restore.point("hideShowButton")

  btn = HTML(paste0('<button id="',id,'" style="" type="button" class="btn btn-default btn-xs no-shiny">',label,'</button>'))

  if (is.null(div.id))
    div.id = paste0(id,"-content-div")

  if (!is.null(content)) {
    div = tags$div(id = div.id,style= ifelse(shown,"display: block", "display: none"), content)
  } else {
    div = NULL
  }
  js = paste0(
'// unbind previous events
$(document).off("click","#',id,'");
$(document).on("click","#',id,'", function(event) {
  $("#',div.id,'").toggle("fast");
  event.stopPropagation();
  });
')
  tagList(
    btn,
    div,
    tags$script(HTML(js))
   )
}
