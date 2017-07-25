examples.transformTagDependecies = function() {
  # A knitr chunk containing an htmlwidget with severl dependencies
  rmd = '
```{r}
library(ggplot2); library(plotly)
ggplotly(
  ggplot(data=data_frame(x=1:10,y=1:10),aes(x=x,y=y)) + geom_line()
)
```
'
  # knit the chunk and return a shiny ui object
  tag = rmdtools::knit.chunk(text = rmd,out.type = "shiny",deps.action = "add")
  # the dependencies have an absolute path
  findDependencies(tag)[[1]]

  # transfrom all dependencies to have a path relative to a package
  # if the package can be found
  rtag = transformTagDependencies(tag, makeDependenciesRelativeToPackage)
  findDependencies(rtag)[[1]]
  # Making the dependencies relative should allow to save
  # rtag as an binary R file and
  # load and show it in a shiny app on another computer

  # We can get the result more conviently in knit.chunk
  # by using the argument deps.action="relative"
  tag = rmdtools::knit.chunk(text = rmd,out.type = "shiny",deps.action = "relative")
  findDependencies(tag)[[1]]

}

#' Tag a shiny tag or a list of tags, and transform all dependencies with the
#' function transform.fun which takes a list of dependecies as argument

transformTagDependencies = function (tags, transform.fun=identity)
{
  deps <- htmlDependencies(tags)
  if (!is.null(deps) && inherits(deps, "html_dependency"))
    deps <- list(deps)
  if (is.list(tags)) {
    classes = class(tags)
    tags = lapply(tags, transformDependecies, transform.fun = transform.fun)
    class(tags) = classes
  }
  deps = transform.fun(deps)
  htmlDependencies(tags) = deps
  tags
}


#' Change in a lst of dependencies absolute paths in scr$file to
#' a relative path based on a package
makeDependenciesRelativeToPackage = function(deps) {
  deps = lapply(deps, makeDependencyRelativeToPackage)
  deps
}

#' Change in an dependency an absolute path in scr$file to
#' a relative path based on a package
makeDependencyRelativeToPackage = function(dep) {
  if (!is.null(dep$package)) return(dep)
  libPaths = .libPaths()
  restore.point("makeDependencyRelativeToPackage")

  src = dep$src[["file"]]

  src = gsub("\\\\","/",src,fixed=TRUE)

  matched.paths =  which(substring(src, 1, nchar(libPaths)) == libPaths)
  # no dependency in libPath
  if (length(matched.paths)==0) return(dep)

  libPath = libPaths[matched.paths[1]]

  package = str.right.of(src,paste0(libPath,"/"))
  package = str.left.of(package,"/")

  dep$package = package
  rel.source = str.right.of(src,paste0(libPath,"/",package,"/"))
  dep$src[["file"]] = rel.source

  dep

}




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
