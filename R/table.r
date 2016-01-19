#' own function to print a compact html table from a data frame
#' with option to select row
#'
#' @export
html.table = function(df, sel.row=NULL, col.names=TRUE, row.names=FALSE, border=TRUE, bg.color =c("#dddddd","#ffffff"), font.size="80%", round.digits=8, signif.digits=8,col.tooltips=NULL, ...) {
  restore.point("html.table")
  n = NROW(df)

  #bg.color =c("#ededfe","#fcfcff")
  #F7F7F7
  row.bgcolor = rep(bg.color,length=n)

  if (!is.null(sel.row)) {
    row.bgcolor[sel.row]='#ffdc98'
    row.bgcolor[sel.row]='#00ff00'
  }

  if (col.names) {
    colnames = colnames(df)


    if (is.null(col.tooltips)) {
      inner = colnames
    } else {
      inner = paste0('<span title="', col.tooltips,'">', colnames, '<span>')
      #inner[nchar(col.tooltips)==0] = colnames
    }

    head = paste0('<th class="data-frame-th">',inner,'</th>', collapse="")
    head = paste0('<tr>', head, '</tr>')
  } else {
    head = ""
  }

  my.format.vals = function(vals) {
    format.vals(vals, signif.digits=signif.digits, round.digits=round.digits)
  }


  td.class = rep("data-frame-td", NROW(df))
  if (length(td.class)>0) {
    td.class[length(td.class)]="data-frame-td-bottom"
  }

  cols = 1:NCOL(df)
  code = paste0('"<td class=\\"",td.class,"\\" nowrap bgcolor=\\"",row.bgcolor,"\\">", my.format.vals(df[[',cols,']]),"</td>"', collapse=",")
  code = paste0('paste0("<tr>",',code,',"</tr>", collapse="\\n")')
  call = parse(text=code)
  main = eval(parse(text=code))

  tab = paste0('<table class="data-frame-table">\n', head, main, "\n</table>")

  #th.style='font-weight: bold; margin: 3px; padding: 3px; border: solid 1px black; text-align: center;'
  #td.style='font-weight: normal; margin: 3px; padding: 3px; border: solid 1px black; font-family: monospace ; text-align: left;'

  th.style='font-weight: bold; margin: 3px; padding: 3px; border: solid 1px black; text-align: center;'
  td.style='font-family: Verdana,Geneva,sans-serif; margin: 0px 3px 1px 3px; padding: 1px 3px 1px 3px; border-left: solid 1px black; border-right: solid 1px black; text-align: left;'

  if (!is.null(font.size)) {
    th.style = paste0(th.style, "font-size: ", font.size,";")
    td.style = paste0(td.style, "font-size: ", font.size,";")

  }

  tab = paste0("<style>",
    " table.data-frame-table {	border-collapse: collapse;  display: block; overflow-x: auto;}\n",
    " td.data-frame-td {", td.style,"}\n",
    " td.data-frame-td-bottom {", td.style," border-bottom: solid 1px black;}\n",
    " th.data-frame-th {", th.style,"}\n",
    " tbody>tr:last-child>td {
      border-bottom: solid 1px black;
    }\n",
    "</style>",tab
  )

  #writeLines(tab, "test.html")
  tab

  return(tab)
}

