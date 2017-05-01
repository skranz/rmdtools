#' Render a list of dependencies as a list of singleton head tags
#' TO DO: Some dependencies may
#' use local file names...
#' Those dependencies must be fully inlined...
#' @export
render.deps.as.singletons.tags =
function(deps, inline.local.files=TRUE) {
  restore.point("render.deps.as.singletons.tags")
  if (!is.list(deps)) return(NULL)
  is.dep = unlist(lapply(deps, function(el) is(el,"html_dependency")))
  deps = deps[is.dep]
  deps = resolveDependencies(deps)
  li = lapply(deps, function(dep) {
    if (inline.local.files & !is.null(dep$src$file) & is.null(dep$src$href)) {
      html = inline.dependency(dep)
    } else {
      html = renderDependencies(list(dep))
    }
    singleton(tags$head(HTML(html)))
  })
  li
}


#' Inline dependencies with local file references
#' @export
inline.dependencies = function (deps,  mustWork = TRUE)
{
  restore.point("inline.dependencies")

  txt = sapply(deps, inline.dependency, mustWork = mustWork)
  paste0(txt, collapse="\n")

}

#' Inline a dependency with local file references
#' @export
inline.dependency = function (dependency,  mustWork = TRUE)
{
  restore.point("inline.dependency")

  dir <- dependency$src$file
  if (is.null(dir)) {
      if (mustWork) {
          stop("Dependency ", dependency$name, " ", dependency$version,
              " is not disk-based")
      }
      else {
          return(dependency)
      }
  }
  scripts = sapply(dependency$script, function(file) {
    paste0(readLines(paste0(dir,"/",file),warn = FALSE), collapse="\n")
  })
  styles = sapply(dependency$stylesheet, function(file) {
    paste0(readLines(paste0(dir,"/",file),warn=FALSE), collapse="\n")
  })
  header = paste0(
    paste0("<script>\n", scripts,"\n</script>", collapse="\n"),
    paste0("<style>\n", styles,"\n</style>", collapse="\n")
  )
  header

}
