#' Write text file in UTF-8 format
#' @export
writeUtf8 <- function(x, file, bom=F) {
  con <- file(file, "wb")
  if(bom) writeBin(BOM, con, endian="little")
  writeBin(charToRaw(x), con, endian="little")
  close(con)
}

#' Read a text file that was saved in UTF-8 format
#' @export
readUtf8 <- function(file) {
  text <- readLines(file,encoding = "UTF-8")
  text
}

#' Read a text file and convert to UTF-8
#' @export
read.as.utf8 = function(file, sep.lines=TRUE) {
  text <- readLines(file)
  Encoding(text) <- "UTF-8"
  text
}

#' Recursively encode strings in list as UTF-8
#' @export
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
