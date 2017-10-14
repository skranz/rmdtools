# vectorized functions to eval.placeholder and render compiled rmd

example.rmdvector = function() {
  txt = "
a={{a}} (Show b only if a=TRUE)
#< if a
b = {{b}}
#>
Show c only if b==2
#< if b==2
c={{c}}
#>

"
  cr = compile.rmd(text = txt,if.blocks = "ph",out.type = "md")
  df = data_frame(a=c(TRUE,FALSE),b = 1:2, c=c(10,20))
  render.vectorized.compiled.rmd(cr,df)

}

render.vectorized.compiled.rmd = function(cr,df=list(),enclos=parent.frame()) {
  restore.point("render.vectorized.compiled.rmd")


  # evaluate all placeholders
  ph.li = lapply(cr$ph$info, function(info) {
    if (!is.null(info$expr))
      eval(info$expr,df,enclos = enclos)
  })
  names(ph.li) = cr$ph$id

  ph.df = as_data_frame(ph.li)
  txt = merge.lines(cr$body)

  # no if blocks
  if (NROW(cr$if.df)==0) {
    str = paste.whiskers(txt, ph.df)
    return(str)
  }

  # we have if blocks
  # solve separately for each combination of if blocks
  str = rep("", NROW(df))


  # if we have if blocks, we must evaluate them manually for each
  # element of df
  if.li = lapply(cr$if.df$info, function(info) {
    call = info$cond.call
    is.true(eval(call,df,enclos = enclos))
  })
  names(if.li) = cr$if.df$id
  if.val.df = as_data_frame(if.li)

  if.group = do.call(paste0, if.li)

  if.groups = unique(if.group)

  ig = if.groups[1]
  for (ig in if.groups) {
    rows = which(ig == if.group)

    # text with specific if blocks used / removed
    ig.txt = merge.lines(replace.if.blocks.from.if.df(txt,if.df=cr$if.df, add=if.val.df[rows[1],]))
    str[rows] = paste.whiskers(ig.txt, ph.df[rows,])
  }

  str
}

eval.vectorized.if.df = function(if.df, df, enclos) {


}
