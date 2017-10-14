
examples.fast_df = function() {

library(microbenchmark)
library(tibble)
# simple vector and list
x = 1:100
y = as.list(1:100)
microbenchmark(
  #df1 = data.frame(x=x,y=y), # does not work correctly
  df2 = data_frame(x=x,y=y),
  df3 = tibble(x=x,y=y),
  df4 = as_tibble(list(x=x,y=y)),
  df5 = fast_df(x=x,y=y)
)

# two vectors
x = 1:100
y = 1:100
microbenchmark(
  df1 = data.frame(x=x,y=y),
  df2 = data_frame(x=x,y=y),
  df3 = tibble(x=x,y=y),
  df4 = as_tibble(list(x=x,y=y)),
  df5 = fast_df(x=x,y=y)
)

}

#' Create a data_frame (tibble) considerably faster
#'
#' columns must all be named and cannot be computed from
#' earlier columns
fast_df = function(...) {
  as_tibble(list(...),validate = FALSE)
}


copy.non.null.fields = function(dest, source, fields=names(source)) {
  restore.point("copy.into.empty.fields")
  copy.fields = fields[!sapply(source[fields], is.null)]

  if (is.environment(dest)) {
    for (field in copy.fields) dest[[field]] = source[[field]]
  } else {
    dest[copy.fields] = source[copy.fields]
  }

  invisible(dest)
}



first.none.null = first.non.null = function(...) {
  args = list(...)
  for (val in args) {
    if (!is.null(val)) return(val)
  }
  return(NULL)

}

#' Gets for each element the index of its type
#' @param types a character vector of types
#' @export
get.types.inds = function(types) {
  utypes = uniqe(type)
  nu = length(utypes)
  if (nu==0) return(NULL)

  inds = rep(0, length(types))
  count = integer(nu)
  names(count) = utypes

  for (i in 1:length(types)) {
    count[types[i]] = count[types[i]]+1
    inds[i] = count[types[i]]
  }
  inds

}

#' Find the levels given ordered start and end positions of possible nested blocks
#'
#' @param start vector of start positions of the blocks
#' @param end vector of end positions of the blocks
#' @param start.level the initial level
#' @return a vector of levels of each blocks, moore deeply nested blocks have higher levels
#' @export
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
#' @param is.parent.type NULL or a optional boolean vector of those rows
#'        that are of a type for which children shall be found.
#' @return a vector of parent indices or 0 for most outer levels
#' @export
get.levels.parents = function(levels, is.parent.type=NULL) {
  n = length(levels)
  if (n==0) return(NULL)
  parents = rep(0,n)
  min.level = levels[1]
  if (min.level != 1)
    levels = levels-min.level+1

  num.levels = max(levels)

  if (is.null(is.parent.type)) {
    level.parent = rep(NA_integer_,num.levels)
    for (i in 1:n) {
      if (levels[i]>1) {
        parents[i] = level.parent[levels[i]-1]
      }
      level.parent[levels[i]] = i
    }
  } else {
    level.parent = rep(0,num.levels)
    # using is.parent.type
    parent.level = 0
    for (i in 1:n) {
      if (levels[i]-1 < parent.level) {
        parent.level= levels[i]-1
        level.parent[(parent.level+1):num.levels] = 0
      }
      if (parent.level>0) {
        parents[i] = level.parent[parent.level]
      }
      if (is.parent.type[i]) {
        parent.level = levels[i]
        level.parent[parent.level] = i
      }
    }
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
#' @export
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
  #rownames(tp) = paste0(1:n,"_",types,"_",levels)
  colnames(tp) = paste0("parent_",parent.types)

  i = 0
  i = i+1
  for (i in 1:n) {
    tp[i,] = ltp[levels[i],]

    # reset parent levels for types of a higher or equally
    # high level
    if (i>1) {
      if (levels[i]<=levels[i-1]) {
        # the values to which we want to clear
        # these are the type parent indexes of
        # lower levels
        clear.values = ltp[levels[i]+1-1,]
        # transform to matrix by row
        clear.mat = matrix(clear.values,nrow=(levels[i-1]-levels[i]+1),ncol=length(clear.values),byrow=TRUE)
        # clear ltp
        ltp[(levels[i]:levels[i-1])+1,] = clear.mat
      }
    }
    if (types[i] %in% parent.types) {
      ltp[(levels[i]:max.level)+1,types[i]] = i
    }
  }
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
  chars = sample(c(letters,LETTERS),nchar*n, replace = TRUE)
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


#' Like paste0 but returns an empty vector if some string is empty
sc = function(..., sep="", collapse=NULL) {
  str = list(...)
  restore.point("str.combine")
  len = sapply(str,length)
  if (any(len==0))
    return(vector("character",0))
  paste0(...,sep=sep,collapse=collapse)
}

