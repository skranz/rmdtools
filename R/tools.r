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

