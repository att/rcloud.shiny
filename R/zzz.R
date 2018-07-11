rcloud.shiny.caps <- NULL

.onLoad <- function(libname, pkgname)
{
  f <- function(module.name, module.path) {
    path <- system.file("javascript", module.path, package=module.name)
    caps <- rcloud.install.js.module(module.name,
                                     paste(readLines(path), collapse='\n'))
    caps
  }
  rcloud.shiny.caps <<- f("rcloud.shiny", "rcloud.shiny.js")
  if(!is.null(rcloud.shiny.caps)) {
    rcloud.shiny.caps$init(list());
  }
  
  options(shiny.uiHttpHandler = rcloud.shiny.uiHttpHandler)
  options(shiny.htmlDependencies = rcloud.shiny.htmlDependencies)
  options(shiny.launch.browser = FALSE)
  options(shiny.serviceAppRunner = function() {})
  options(shiny.showcase.navTabsHelper = .navTabsHelperFromNotebookCells)
  options(shiny.showcase.tabContentHelper = .tabContentHelperFromNotebookCells)
  options(shiny.showcase.listNotebookRFiles = .listNotebookRFiles)
  options(shiny.showcase.getShinyAppSrcFiles = .getShinyAppSrcFiles)
}

rcloud.shiny.debugMsg <- function(content) rcloud.shiny.caps$debugMsg(content)

`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
}