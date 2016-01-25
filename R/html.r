read.as.utf8 = function(file, sep.lines=TRUE) {
  text <- readChar(file, file.info(file)$size, useBytes = TRUE)
  Encoding(text) <- "UTF-8"
  if (sep.lines) text = strsplit(text,"\n")[[1]]
  text
}

example.rmd.blocks.to.placeholder = function() {
  setwd("D:/libraries/rmdtools")
  txt = read.as.utf8("test.rmd")

  res = rmd.blocks.to.placeholder(txt)
  res = rmd.whiskers.to.placeholder(txt)
  res = rmd.chunks.to.placeholder(txt)

  ph = res$ph
  str = res$txt
  writeClipboard(str)
}

#' Replace chunks with placeholders of the form {{id}}
#' @export
rmd.chunks.to.placeholder = function(txt,whisker.prefix="{{", whisker.postfix="}}", del.rows.na=FALSE) {
  restore.point("rmd.chunks.to.placeholder")

  df = find.rmd.chunks(txt)

  if (NROW(df)==0) {
    return(list(txt=txt, ph=NULL))
  }
  rand.postfix = sample.int(.Machine$integer.max, NROW(df))
  id = paste0("chunk_", df$name,"_",df$start.row,"_",rand.postfix)
  ph = data.frame(
    id = id,
    type = "chunk",
    form = "chunk",
    txt = sapply(1:NROW(df), function(row) {
      paste0(txt[df$start[row]:df$end[row]], collapse="\n")
    }),
    stringsAsFactors = FALSE
  )

  txt = replace.blocks.txt(txt, paste0(whisker.prefix,id, whisker.postfix), df, del.rows.na=del.rows.na)

  list(txt=txt, ph=ph)

}


#' Replace whiskers with placeholders of the form {{id}}
#' @export
rmd.whiskers.to.placeholder = function(txt) {
  multi.line = length(txt)>1

  if (multi.line) txt = merge.lines(txt)

  pos = str.blocks.pos(txt,"{{","}}")
  if (NROW(pos$outer)==0) {
    txt = sep.lines(txt)
    return(txt = txt, ph=NULL)
  }
  s = substring(txt, pos$inner[,1],pos$inner[,2])
  rand.postfix = sample.int(.Machine$integer.max, length(s))
  id = paste0("whisker_", seq_along(s),"_",rand.postfix)
  ph = data.frame(
    id = id,
    type = "whisker",
    form = "whisker",
    txt = s,
    stringsAsFactors = FALSE
  )


  txt = str.replace.at.pos(txt, pos$outer, paste0("{{",id,"}}"))

  txt = sep.lines(txt)
  list(txt=txt, ph=ph)

}

#' Replace blocks with placeholders of the form {{id}}
#' @export
rmd.blocks.to.placeholder = function(txt, block.df=NULL, whisker.prefix="{{", whisker.postfix="}}", del.rows.na = FALSE) {
  restore.point("rmd.blocks.to.placeholder")

  if (is.null(block.df)) {
    block.df = find.rmd.blocks(txt)
  }
  rand.postfix = sample.int(.Machine$integer.max, NROW(block.df))
  id = paste0("block_", block.df$type,"_",block.df$start,"_",rand.postfix)
  ph = data.frame(
    id = id,
    type = block.df$type,
    form = "block",
    txt = get.blocks.txt(txt, block.df, inner=FALSE),
    stringsAsFactors = FALSE
  )

  txt = replace.blocks.txt(txt, paste0(whisker.prefix,id, whisker.postfix), block.df, del.rows.na=del.rows.na)

  list(txt=txt, ph=ph)
}


#' Variant of htmltools::htmlTemplate
#' @export
html.template = function (file=NULL, html=NULL, params=list(), parent.env=globalenv(), document_ = "auto")
{
  if (is.null(text)) {
    html <- readChar(file, file.info(file)$size, useBytes = TRUE)
    Encoding(html) <- "UTF-8"
  }

  pieces <- strsplit(html, "{{", fixed = TRUE)[[1]]
  pieces <- strsplit(pieces, "}}", fixed = TRUE)
  if (length(pieces[[1]]) != 1) {
      stop("Mismatched {{ and }} in HTML template.")
  }
  lapply(pieces[-1], function(x) {
      if (length(x) != 2) {
          stop("Mismatched {{ and }} in HTML template.")
      }
  })
  vars <- params
  if ("headContent" %in% names(vars)) {
      stop("Can't use reserved argument name 'headContent'.")
  }
  vars$headContent <- function() HTML("<!-- HEAD_CONTENT -->")
  env <- list2env(vars, parent = parent.env())
  pieces[[1]] <- HTML(pieces[[1]])
  pieces[-1] <- lapply(pieces[-1], function(piece) {
      tagList(eval(parse(text = piece[1]), env), HTML(piece[[2]]))
  })
  result <- tagList(pieces)
  if (document_ == "auto") {
      document_ = grepl("<HTML>", html, ignore.case = TRUE)
  }
  if (document_) {
      class(result) <- c("html_document", class(result))
  }
  result
}
