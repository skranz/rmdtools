examples.nested = function() {
  setwd("D:/libraries/rmdtools")

  dot.levels = c(
    exercise = 0,
    frame = 1,
    column = 2,
    success = 3,
    when = 3
  )

  txt = readLines("test.rmd")
  df = find.rmd.nested(txt, dot.levels)

  parent.types = c("frame","column","chunk")
  pt = get.levels.parents.by.types(df$level, df$type, parent.types)

  d = cbind(df,pt)
}

mutate_.NULL = select_.NULL = arrange_.NULL = filter_.NULL = function(...) NULL

#' find blocks, chunks and dot blocks and add nesting info
#' @param txt the rmd source as character vector, each line one element
#' @param dot.levels a list that describes the level of dot block types
#' @return a data.frame
#' @export
find.rmd.nested = function(txt, dot.levels = NULL) {
  restore.point("find.rmd.nested")

  bdf = find.rmd.blocks(txt) %>% mutate(form="block")
  cdf = find.rmd.chunks(txt) %>% select(start,end) %>% mutate(type="chunk", arg.str = NA_character_) %>% mutate(form="chunk")
  ddf = find.dot.blocks(txt, dot.levels) %>% mutate(form="dotblock")

  df = rbind(ddf,bdf,cdf) %>% arrange(start,-end)
  df$level = get.start.end.levels(df$start, df$end)
  df$parent = get.levels.parents(df$level)
  df
}

#' Find blocks that have only a starting line of the form
#'
#' #. type arguments
#'
#' @export
find.dot.blocks = function(txt,dot.levels = NULL, dot.start = "#. ") {
  restore.point("find.dot.blocks")

  rows = str.starts.with(txt, dot.start)
  str = txt[rows]
  n = length(str)
  if (n == 0) return(NULL)

  type = str.right.of(str,dot.start) %>% str.trim %>% str.left.of(" ")
  arg.str = str.right.of(str,dot.start) %>% str.right.of(type) %>% str.trim
  start = which(rows)

  level = rep(Inf,n)
  ml = type %in% names(dot.levels)

  if (sum(ml)>0) {
    level[ml] = dot.levels[type[ml]]
    end =
    elevel = c(level, -Inf)
    end.ind = 2:(n+1)
    while(TRUE) {
      add.rows = which(elevel[end.ind] > level)
      if (length(add.rows)==0) break
      end.ind[add.rows] = end.ind[add.rows]+1
    }
    end = c(start-1,NROW(txt))[end.ind]
  } else {
    end = c(start[-1]-1,NROW(txt))
  }

  fast_df(start=start, end=end, type=type, arg.str = arg.str)

}
