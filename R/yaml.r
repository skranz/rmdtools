#' A more convenient yaml importer
#' @export
import.yaml = function(file=NULL,text=NULL, verbose=FALSE, keep.quotes=FALSE, quote.char = "__QUOTE__",  catch.error = TRUE, check.by.row=FALSE, utf8=TRUE) {
  restore.point("read.yaml")
  if (!is.null(file)) {
    file.str = paste0(" in ", file)
  } else {
    file.str = ""
  }
  if (is.null(text)) {
    str = suppressWarnings(paste(readLines(file), collapse = "\n"))
  } else {
    str = text
  }

  if (utf8) {
   # str = enc2utf8(str)
    Encoding(str) <- "UTF-8"
    str = mark_utf8(str)
  }

  handlers=list("bool#yes"=yaml.bool.handler.yes,"bool#no"=yaml.bool.handler.no)
  if (keep.quotes) {
    str = gsub('"',quote.char,str,fixed=TRUE)
    yaml.string.handler = function(val) {
      gsub(quote.char,'"',val,fixed=TRUE)
    }
    handlers[["str"]]=yaml.string.handler
  }
  if (verbose)
    cat(str)



  if (check.by.row) {
    sep.str = strsplit(str,"\n", fixed=TRUE)[[1]]
    for (row in 1:length(sep.str)) {
      cat("\n try to read rows 1:",row,"\n")
      txt = paste0(sep.str[1:row],collapse="\n")
      tryCatch(
        yaml.load(txt, handlers=handlers),
        error = function(e) {
          str = paste0(as.character(e),file.str, " row ",row,"\n")
          rows = max(row-2,1):min(row+1,length(sep.str))
          str = paste0(str,paste0(rows,": ",sep.str[rows],collapse="\n"))
          stop(str, call.=FALSE)
        }
      )
    }
  }

  tryCatch(
    li <- yaml.load(str, handlers=handlers),
    error = function(e) {
      str = paste0(as.character(e),file.str)
      stop(str, call.=FALSE)
    }
  )

  if (utf8) {
    li = mark_utf8(li)
  }
  li
  #suppressWarnings(yaml.load(str, handlers=list("bool#yes"=yaml.bool.handler.yes,"bool#no"=yaml.bool.handler.no)))

}


# custom handler for booleans: only convert "TRUE" and "FALSE" to boolean
yaml.bool.handler.yes <- function(val) {
  #message(paste("bool: ", val))
  if (val=="TRUE" | val=="true")
    return(TRUE)
  return(val)
}

# custom handler for booleans: only convert "TRUE" and "FALSE" to boolean
yaml.bool.handler.no <- function(val) {
  #message(paste("bool: ", val))
  if (val=="FALSE" | val=="false")
    return(FALSE)
  return(val)
}

#' Reads a yaml file and returns as a list
#' @export
read.yaml = function(file=NULL, verbose=FALSE, keep.quotes=TRUE, quote.char = "__QUOTE__", text=NULL, catch.error = TRUE, check.by.row=FALSE, space.after.colon=FALSE, utf8=TRUE) {
  restore.point("read.yaml")
  if (!is.null(file)) {
    file.str = paste0(" in ", file)
  } else {
    file.str = ""
  }
  if (is.null(text)) {
    str = suppressWarnings(paste(readLines(file), collapse = "\n"))
  } else {
    str = text
  }

  if (utf8) {
   # str = enc2utf8(str)
    Encoding(str) <- "UTF-8"
  }


  #message(paste("read.yam:", file))
  # Convert tabs to spaces
  str = gsub("\t","   ",str)
  # Convert ":text" into ": text"
  if (space.after.colon) {
    str = gsub(":",": ",str)
    str = gsub(":  ",": ",str)
  }

  handlers=list("bool#yes"=yaml.bool.handler.yes,"bool#no"=yaml.bool.handler.no)
  if (keep.quotes) {
    str = gsub('"',quote.char,str,fixed=TRUE)
    yaml.string.handler = function(val) {
      gsub(quote.char,'"',val,fixed=TRUE)
    }
    handlers[["str"]]=yaml.string.handler
  }
  if (verbose)
    cat(str)



  if (check.by.row) {
    sep.str = strsplit(str,"\n", fixed=TRUE)[[1]]
    for (row in 1:length(sep.str)) {
      cat("\n try to read rows 1:",row,"\n")
      txt = paste0(sep.str[1:row],collapse="\n")
      tryCatch(
        yaml.load(txt, handlers=handlers),
        error = function(e) {
          str = paste0(as.character(e),file.str, " row ",row,"\n")
          rows = max(row-2,1):min(row+1,length(sep.str))
          str = paste0(str,paste0(rows,": ",sep.str[rows],collapse="\n"))
          stop(str, call.=FALSE)
        }
      )
    }
  }

  tryCatch(
    li <- yaml.load(str, handlers=handlers),
    error = function(e) {
      str = paste0(as.character(e),file.str)
      stop(str, call.=FALSE)
    }
  )

  if (utf8) {
    li = mark_utf8(li)
  }
  li
  #suppressWarnings(yaml.load(str, handlers=list("bool#yes"=yaml.bool.handler.yes,"bool#no"=yaml.bool.handler.no)))

}

examples.read.yaml = function() {

  fn = paste0("D:/libraries/XEconDB/Structures/Games/LureOfAuthorityAlternative.yaml")
  obj = read.yaml(fn)
  obj$variants
}


#' Prints list read from a yaml file
#' @export
print.yaml = function(obj) {
  if (class(obj)=="character") {
    cat(obj)
  } else {
    cat(as.yaml(obj))
  }
}


