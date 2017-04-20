example.compile.note.blocks.to.html = function() {
  library(htmltools)
  txt =
'#< note "Example"
    Hi I am a note
#>'
  txt = sep.lines(txt)
  tag = compile.note.block.to.tag(txt)
  html = compile.note.block.to.html(txt)

  res = renderTags(tag)
  print(browsable(HTML(res$html)))
  dep = note.block.dependencies()

  head = renderDependencies(dep)
  page = simple.html.page(head=inline.head, body=html)
  view.html(text=page)
}


get.note.block.spec = function() {
  list(
    is.hf = TRUE,
    libs = "shinyBS",
    deps = list(htmlDependency("shinyBS", version=" ",  src=c(href = "sbs"), meta = NULL, script = "shinyBS.js",stylesheet = "shinyBS.css"))
  )
}

eval.note.block = function(txt,envir=parent.frame(), out.type=first.none.null(cr$out.type,"html"),cr=NULL,info=NULL, render.txt = !is.null(cr), has.header=TRUE, ...) {
  restore.point("eval.note.block")
  if (is.null(info)) {
    info = make.note.block.info(txt)
  }
  txt = sep.lines(txt)
  if (has.header) {
    txt = txt[-c(1, length(txt))]
  }

  #render.txt = FALSE
  if (render.txt) {
    content = render.compiled.rmd(cr=cr,txt=txt, envir=envir, out.type=out.type)
    if (out.type == "html" | is.character(content)) content = HTML(content)
  } else {
    content = HTML(paste0(txt, collapse="\n"))
  }

  ui=shinyBS::bsCollapse(id =info$id, shinyBS::bsCollapsePanel(title=info$title,content))
  ui

}

compile.note.block.to.tag = function(txt, envir=parent.frame(), info=NULL) {
  restore.point("compile.note.block.to.html")
  if (is.null(info)) {
    info = make.note.block.info(txt)
  }
  ui=shinyBS::bsCollapse(id =info$id, shinyBS::bsCollapsePanel(title=info$title,shiny::HTML(info$html)))
  ui
}

compile.note.block.to.html = function(txt, envir=parent.frame(), info=NULL) {
  restore.point("compile.note.block.to.html")
  as.character(compile.note.block.to.tag(txt, envir, info))
}

make.note.block.info = function(txt,id=NULL,...) {
  restore.point("make.note.block.info")

  if (length(txt)==1) txt = sep.lines(txt)
  header = txt[1]
  title = remove.quotes(str.right.of(header,"note "))

  if (is.null(id)) {
    id = paste0("NOTE_",random.string())
  }

  rmd = paste0(txt[c(-1,-length(txt))], collapse="\n")
  html = markdown::markdownToHTML(text=rmd,fragment.only=TRUE)
  list(
    id = id,
    title=title,
    rmd = rmd,
    html = html
  )
}



