
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



colored.html = function(txt, color="blue") {
  if (is.null(color)) return(txt)
  paste0("<font color='",color,"'>",txt,"</font>")
}

# mark the encoding of character vectors as UTF-8
mark_utf8 <- function(x) {
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

