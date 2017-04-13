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
