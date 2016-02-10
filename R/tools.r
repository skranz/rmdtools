#' Find the levels given ordered start and end positions of possible nested blocks
#'
#' @param start vector of start positions of the blocks
#' @param end vector of end positions of the blocks
#' @param start.level the initial level
#' @return a vector of levels of each blocks, moore deeply nested blocks have higher levels
get.start.end.levels = function(start, end, start.level = 1L) {
  n = length(start)
  if (n==0) return(NULL)
  levels = rep(start.level,n)
  end.stack = rep(NA_integer_,n)
  end.stack.ind = 0

  level = start.level
  for (i in 1:n) {
    if (end.stack.ind >0) {
      while (start[i]>end.stack[end.stack.ind]) {
        level = level-1L
        end.stack.ind = end.stack.ind -1L
        if (end.stack.ind == 0) break
      }
    }
    levels[i] = level
    level = level+1L
    end.stack.ind = end.stack.ind+1L
    end.stack[end.stack.ind] = end[i]
  }
  levels
}

#' Find the parent index of each row given a vector levels that describes the nestedness
#'
#' @param levels integer vector of levels must start with lowest level and increase by 1 or decrease to an integer number
#' @return a vector of parent indices or 0 for most outer levels
get.levels.parents = function(levels) {
  n = length(levels)
  if (n==0) return(NULL)
  parents = rep(0,n)
  min.level = levels[1]
  if (min.level != 1)
    levels = levels-min.level+1

  level.parent = rep(NA_integer_,max(levels))

  for (i in 1:n) {
    if (levels[i]>1) {
      parents[i] = level.parent[levels[i]-1]
    }
    level.parent[levels[i]] = i
  }
  parents
}

#' Find the parent index of each row given a vector levels that describes the nestedness
#'
#' @param levels integer vector of levels must start with lowest level and increase by 1 or
#'        decrease to an integer number
#' @param types character vector of types
#' @param parent.types the parent.types that shall be characterized
#' @return a matrix of parent type indices with length(levels) rows and length(parent.types) columns. If there is no parent type, we enter a 0.
get.levels.parents.by.types = function(levels, types, parent.types = setdiff(unique(types),c(NA))) {
  restore.point("get.levels.parents.by.types")

  n = length(levels)
  m = length(parent.types)
  if (n==0) return(NULL)
  min.level = levels[1]
  if (min.level != 1)
    levels = levels-min.level+1
  tp = matrix(0,n,m)
  max.level = max(levels)
  ltp = matrix(0,max.level+1,m)
  if (is.character(types)) {
    colnames(ltp) = parent.types
  }

  i = 0
  i = i+1
  for (i in 1:n) {
    tp[i,] = ltp[levels[i],]

    # reset earlier levels
    if (i>1) {
      if (levels[i]<=levels[i-1]) {
        ltp[(levels[i]:levels[i-1])+1,] = 0
      }
    }
    if (types[i] %in% parent.types) {
      ltp[(levels[i]:max.level)+1,types[i]] = i
    }
  }
  colnames(tp) = paste0("parent_",parent.types)
  tp
}


nlist = function (...)
{
    li = list(...)
    li.names = names(li)
    names = unlist(as.list(match.call())[-1])
    if (!is.null(li.names)) {
        no.names = li.names == ""
        names(li)[no.names] = names[no.names]
    }
    else {
        names(li) = names
    }
    li
}

#' Remove quotes from strings
#' @export
remove.quotes = function(str, quotes=c("'",'"')) {
  has.quotes = substring(str,1,1) %in% quotes
  str[has.quotes] = substring(str[has.quotes],2, nchar(str[has.quotes])-1)
  str
}

#' Normalize an id to letters that are allowed
#' @export
normalize.id = function(str, allowed = c(letters,LETTERS,0:9,"_"), subst="_") {
  if (is.null(str)) return("")
  li.vec = strsplit(str,"")
  res = sapply(li.vec, function(vec) {
    vec[! vec %in% allowed] = subst
    paste0(vec, collapse="")
  })

  res
}

example.random.string.collusion.prob = function() {
  random.string.collusion.prob(1e9,14)
}

random.string.collusion.prob = function(n=1000,nchar=14) {
  num.comb = (26+26+10)
  prob = (1 - exp(-n^2/(2*num.comb^nchar)))
  prob
}

#' Create n random strings of length nchar each
#' @export
random.string = function(n=1,nchar=14) {
  chars = sample(c(letters,LETTERS,0:9),nchar*n, replace = TRUE)
  if (n == 1) return(paste0(chars, collapse=""))
  mat = as.data.frame(matrix(chars, n, nchar))
  do.call(paste0,mat)
}

is.true = function(val) {
  if (length(val)==0)
    return(FALSE)
  val[is.na(val)] = FALSE
  return(val)
}

read.as.utf8 = function(file, sep.lines=TRUE) {
  text <- readLines(file)
  Encoding(text) <- "UTF-8"
  text
}

mark_utf8 = function(x) {
  if (is.character(x)) {
    Encoding(x) <- 'UTF-8'
    return(x)
  }
  if (!is.list(x)) return(x)
  attrs <- attributes(x)
  res <- lapply(x, mark_utf8)
  attributes(res) <- attrs
  res
}


#' Like paste0 but returns an empty vector if some string is empty
sc = function(..., sep="", collapse=NULL) {
  str = list(...)
  restore.point("str.combine")
  len = sapply(str,length)
  if (any(len==0))
    return(vector("character",0))
  paste0(...,sep=sep,collapse=collapse)
}

