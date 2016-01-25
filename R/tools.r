is.true = function(val) {
  if (length(val)==0)
    return(FALSE)
  val[is.na(val)] = FALSE
  return(val)
}
