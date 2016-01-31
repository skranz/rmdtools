#' knitr data.frame printer as nice HTML table with several options
#'
#' @export
table.knit_print.data.frame = function(x, table.max.rows=100, round.digits=8, signif.digits=8, html.data.frame=TRUE, show.col.tooltips=TRUE, col.tooltips=NULL, output="html", options=NULL, ...) {
  restore.point("rtutor.knit_print.data.frame")

  # chunk options have precedent over passed arguments
  copy.non.null.fields(dest=environment(), source=options, fields=c("table.max.rows","round.digits","signif.digits","html.data.frame","show.col.tooltips"))

  MAX.ROW = table.max.rows
  if (NROW(x)>MAX.ROW) {
    rows = 1:MAX.ROW

    if (html.data.frame) {
      missing.txt = paste0("... only ", MAX.ROW ," of ", NROW(x), " rows  shown  ...")
      if (output=="html" | output=="shiny") {
        h1 = html.table(x[rows,],round.digits=round.digits, signif.digits=signif.digits, col.tooltips=col.tooltips,...)
        html = c(h1, as.character(p(missing.txt)))
      } else {
        dat = format.data.frame(x[rows,],signif.digits = signif.digits, round.digits = round.digits)
        html = paste0(c(kable(dat),missing.txt),collapse="\n")
      }
    } else {
      dat = format.data.frame(x[rows,],signif.digits = signif.digits, round.digits = round.digits)
      txt = capture.output(print(dat))
      txt = c(paste0(txt,collapse="\n"),paste0("... only ", MAX.ROW ," of ", NROW(x), " rows shown ..."))

      return(txt)
    }

  } else {
    if (html.data.frame) {
      html = RTutor:::html.table(x,round.digits=round.digits, signif.digits=signif.digits, col.tooltips=col.tooltips, ...)
    } else {
      restore.point("ndjhdbfdub")

      dat = format.data.frame(x,signif.digits = signif.digits, round.digits = round.digits)
      txt = paste0(capture.output(print(dat)), collapse="\n")
      return(txt)
    }
  }
  knitr::asis_output(html)
}

format.vals = function(vals, signif.digits=NULL, round.digits=NULL) {
  if (is.numeric(vals)) {
    if (is.null(signif.digits) & is.null(round.digits)) {
      return(vals)
    } else if (!is.null(signif.digits) & is.null(round.digits)) {
      return(signif(vals, signif.digits))
    } else if (is.null(signif.digits) & !is.null(round.digits)) {
      return(round(vals, signif.digits))
    } else {
      return(signif(round(vals, round.digits), signif.digits))
    }
  }
  vals
}

format.data.frame = function(x, signif.digits=NULL, round.digits=NULL) {
  as.data.frame(lapply(x, format.vals, signif.digits=signif.digits, round.digits=round.digits))
}
