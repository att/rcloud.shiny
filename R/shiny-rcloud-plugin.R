#
# RCloud functions that are plugged in into refined shiny API
#
# These should (mostly) remain unchanged if PR to shiny project is accepted
#
# If PR is merged replacing shiny.uiHttpHandler (and renderPage) won't be necessary as only modification is the htmlDependencies, which will also be possible.
# option to replace shiny.uiHttpHandler will be exposed in shiny to allow for additional customization if needed without having to change shiny package. 


rcloud.shiny.uiHttpHandler <- function(ui, uiPattern = "^/$") {
  
  force(ui)
  
  function(req) {
    if (!identical(req$REQUEST_METHOD, 'GET'))
      return(NULL)
    
    if (!isTRUE(grepl(uiPattern, req$PATH_INFO)))
      return(NULL)
    
    textConn <- file(open = "w+")
    on.exit(close(textConn))
    
    showcaseMode <- shiny:::.globals$showcaseDefault
    if (shiny:::.globals$showcaseOverride) {
      mode <- shiny:::showcaseModeOfReq(req)
      if (!is.null(mode))
        showcaseMode <- mode
    }
    
    testMode <- shiny:::.globals$testMode %OR% FALSE
    
    # Create a restore context using query string
    bookmarkStore <- getShinyOption("bookmarkStore", default = "disable")
    if (bookmarkStore == "disable") {
      # If bookmarking is disabled, use empty context
      restoreContext <- shiny:::RestoreContext$new()
    } else {
      restoreContext <- shiny:::RestoreContext$new(req$QUERY_STRING)
    }
    
    shiny:::withRestoreContext(restoreContext, {
      uiValue <- NULL
      
      if (is.function(ui)) {
        if (length(formals(ui)) > 0) {
          # No corresponding ..stacktraceoff.., this is pure user code
          uiValue <- shiny:::..stacktraceon..(ui(req))
        } else {
          # No corresponding ..stacktraceoff.., this is pure user code
          uiValue <- shiny:::..stacktraceon..(ui())
        }
      } else {
        if (shiny:::getCurrentRestoreContext()$active) {
          warning("Trying to restore saved app state, but UI code must be a function for this to work! See ?enableBookmarking")
        }
        uiValue <- ui
      }
    })
    if (is.null(uiValue))
      return(NULL)
    
    rcloud.shiny.renderPage(uiValue, textConn, showcaseMode, testMode)
    html <- paste(readLines(textConn, encoding = 'UTF-8'), collapse='\n')
    return(shiny:::httpResponse(200, content=enc2utf8(html)))
  }
}

rcloud.shiny.htmlDependencies <- function(showcase = 0, testMode = FALSE) {
  shiny_deps <- list(
    htmlDependency("json2", "2014.02.04", c(href="shared"), script = "json2-min.js"),
    htmlDependency("jquery", "1.12.4", c(href="shared"), script = "jquery.min.js")
  )
  
  shiny_deps <- c(shiny_deps, list(
    htmlDependency("rcloud.shiny", utils::packageVersion("rcloud.shiny"), c(href="/shared.R/rcloud.shiny/"), script = "rcloud.shiny.child.js"),
    htmlDependency("shiny", utils::packageVersion("shiny"), c(href="shared"),
                   script = if (getOption("shiny.minified", TRUE)) "shiny.min.js" else "shiny.js",
                   stylesheet = "shiny.css")))
  
  if (testMode) {
    # Add code injection listener if in test mode
    shiny_deps[[length(shiny_deps) + 1]] <-
      htmlDependency("shiny-testmode", utils::packageVersion("shiny"),
                     c(href="shared"), script = "shiny-testmode.js")
  }
  invisible(shiny_deps)
}

rcloud.shiny.renderPage <- function(ui, connection, showcase=0, testMode=FALSE) {
  # If the ui is a NOT complete document (created by htmlTemplate()), then do some
  # preprocessing and make sure it's a complete document.
  if (!inherits(ui, "html_document")) {
    if (showcase > 0)
      ui <- showcaseUI(ui)
    
    # Wrap ui in body tag if it doesn't already have a single top-level body tag.
    if (!(inherits(ui, "shiny.tag") && ui$name == "body"))
      ui <- tags$body(ui)
    
    # Put the body into the default template
    ui <- htmlTemplate(
      system.file("template", "default.html", package = "shiny"),
      body = ui
    )
  }
  
  
  shiny_deps <- getOption('shiny.htmlDependencies')(showcase=showcase, testMode=testMode)
  
  html <- renderDocument(ui, shiny_deps, processDep = createWebDependency)
  shiny:::writeUTF8(html, con = connection)
}

#
# Showcase mode
#

.rcloud.shiny.tabContentHelper <- function(files, path, language, srcs = c("app.r", "server.r")) {
  lapply(files, function(file) {
    with(tags,
         div(class=paste("tab-pane",
                         if (tolower(file$filename) %in% srcs) " active"
                         else "",
                         sep=""),
             id=paste(gsub(".", "_", file$filename, fixed=TRUE),
                      "_code", sep=""),
             pre(class="shiny-code",
                 # we need to prevent the indentation of <code> ... </code>
                 HTML(format(tags$code(
                   class=paste0("language-", language),
                   paste(file$content, collapse = "\n")
                 ), indent = FALSE))))
    )
  })
}

.rcloud.shiny.navTabsHelper <- function(files, prefix = "", srcs = c("app.r", "server.r")) {
  lapply(files, function(file) {
    with(tags,
         li(class=if (tolower(file$filename) %in% srcs) "active" else "",
            a(href=paste("#", gsub(".", "_", file$filename, fixed=TRUE), "_code", sep=""),
              "data-toggle"="tab", paste0(prefix, file$filename)))
    )
  })
}

.rcloud.shiny.navTabsDropdown <- function(files, srcs = c("app.r", "server.r")) {
  if (length(files) > 0) {
    with(tags,
         li(role="presentation", class="dropdown",
            a(class="dropdown-toggle", `data-toggle`="dropdown", href="#",
              role="button", `aria-haspopup`="true", `aria-expanded`="false",
              "Assets", span(class="caret")
            ),
            ul(class="dropdown-menu", .rcloud.shiny.navTabsHelper(files, srcs = srcs))
         )
    )
  }
}

.listNotebookRFiles <- function() {
  list.notebook.files(pattern = "^part.*\\.[rR]$")
}

.staticNotebookResources <- function(path, pattern) {
  wwwFiles <- list()
  if (isTRUE(shiny:::.globals$IncludeWWW)) {
    wwwFiles$jsFiles <- list.notebook.files(pattern = "\\.js$")
    wwwFiles$cssFiles <- list.notebook.files(pattern = "\\.css$")
    wwwFiles$htmlFiles <- list.notebook.files(pattern = "\\.html$")
  }
  invisible(wwwFiles)
}

.rcloud.shiny.codeTabs <- function(codeLicense) {
  rFiles <- .listNotebookRFiles()
  srcRfiles <- .getShinyAppSrcFiles(rFiles)
  
  wwwFiles <- .staticNotebookResources()
  
  with(tags, div(id="showcase-code-tabs",
                 a(id="showcase-code-position-toggle",
                   class="btn btn-default btn-sm",
                   onclick="toggleCodePosition()",
                   icon("level-up"),
                   "show with app"),
                 ul(class="nav nav-tabs",
                    .rcloud.shiny.navTabsHelper(rFiles, srcs = srcRfiles),
                    .rcloud.shiny.navTabsDropdown(unlist(wwwFiles, recursive = FALSE), srcs = srcRfiles)
                 ),
                 div(class="tab-content", id="showcase-code-content",
                     .rcloud.shiny.tabContentHelper(rFiles, language = "r", srcs = srcRfiles),
                     .rcloud.shiny.tabContentHelper(wwwFiles$jsFiles,
                                                    language = "javascript", srcs = srcRfiles),
                     .rcloud.shiny.tabContentHelper(wwwFiles$cssFiles,
                                                    language = "css", srcs = srcRfiles),
                     .rcloud.shiny.tabContentHelper(wwwFiles$htmlFiles,
                                                    language = "xml", srcs = srcRfiles)
                 ),
                 codeLicense))
}


.getShinyAppSrcFiles <- function(rFiles) {
  if (length(rFiles) > 0) {
    tolower(names(rFiles)[1])
  } else {
    c("app.r", "server.r")
  }
}

# Get list of notebook cells
list.notebook.files <- function(pattern) {
  notebook_in_mini <- rcloud.support:::rcloud.session.notebook()
  n <- notebook_in_mini$content
  nn <- n$files
  nn <- nn[grep(pattern, names(nn))]
  invisible(nn)
}