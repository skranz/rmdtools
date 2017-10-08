mathjaxHeader = function(local.mathjax = !isTRUE(getOption("use.local.mathjax")==FALSE)) {
  if (local.mathjax) {
    if (suppressWarnings(require(MathjaxLocal, quietly=TRUE))) {
      # Header of mathjaxLocal
      return(mathjaxLocalHeader())
    }
  }

  # Header from rstudio
  path <- "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  tags$head(singleton(tags$script(src = path, type = "text/javascript")))
}

withMathJaxNoHeader = function(...) {
  tagList(..., tags$script(HTML("if (window.MathJax) MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
}

# Don't add headers here
with.mathjax = function (..., config=c("TeX-AMS_HTML", "TeX-AMS-MML_SVG")[2], typeset.mathjax = !TRUE, local.mathjax = !isTRUE(getOption("use.local.mathjax")==FALSE))
{
  return(withMathJaxNoHeader(...))
}

mathjax.dollars.to.brackets = function(txt) {
  restore.point("mathjax.dollars.to.brackets")
  library(stringtools)
  txt = merge.lines(txt)

  # replace $$ by \[ and \]
  pos = try(str.blocks.pos(txt,start = "$$", end="$$"),silent = TRUE)
  if (!is(pos,"try-error")) {
    txt = str.replace.at.pos(txt,cbind(pos$outer[,1],pos$outer[,1]+1),new = "\\[")
    txt = str.replace.at.pos(txt,cbind(pos$outer[,2]-1,pos$outer[,2]),new = "\\]")
  }

  txt = sep.lines(txt)
  txt = gsub('\\$(.+?)\\$','\\\\(\\1\\\\)',txt)
  txt = merge.lines(txt)
  #cat(txt)
  txt
}



examples.replace.mathjax.dollars = function() {


txt = "$5$ 5$
 3$ $a-4$"

}

replace.mathjax.dollars = function(txt) {
  # not yet implemented
  txt = sep.lines(txt)
  start.dollar = str.locate.all(txt,"(^| )\\$",fixed = FALSE)
  end.dollar = str.locate.all(txt,"[^ ]\\$",fixed = FALSE)
}
