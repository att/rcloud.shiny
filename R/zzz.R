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
}
