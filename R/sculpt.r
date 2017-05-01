# Evaluate some code in a way that yields an
# output for shiny apps or pure html
#
# If the evaluated output is
# text -> interpret it as HTML
# data.frame -> show data.frame html
# ggplot or plot is generated -> show plot or img tag
# htmlwidget -> a tagList (or pure HTML if html output)
# tagList or shiny.tag -> return as.is
#

sculpt.eval = function(code, expr=NULL, envir=parent.frame(), args=list(), out.type=c("keep", "shiny", "html")[1] , figure.dir=tempdir(), eval.fun = eval,...) {
  if (is.null(expr))
    expr = parse(text=code)

  # dealing with plots not yet implemented
  obj = eval.fun(call, envir)

  if (is(out,"htmlwidget")) {
    if (!is.null(args[["width"]]))
      obj$width = args$width
    if (!is.null(args[["height"]]))
      obj$height = args$height

    obj = htmlwidgets:::toHTML(obj, knitrOptions=args)
  }

  sculpt.object(obj, out.type=out.type)
}

sculpt.output = function(obj, out.type="shiny") {
  if (out.type=="shiny") {
    if (is.character(obj)) obj = HTML(obj)
  } else if (out.type=="html") {
    obj = as.character(obj)
  }
  obj
}
