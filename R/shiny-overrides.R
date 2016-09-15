# we override a few functions in shiny by copying, renaming, and slighly modifying them this is just
# to add another page-dependency, which needs to come before some of the built-in ones.

# the source in this file is licensed under GPL-3, copyright RStudio and contributors; see
# https://github.com/rstudio/shiny/blob/master/LICENSE and
# https://github.com/rstudio/shiny/blob/master/DESCRIPTION for details.
override.shinyApp <- function(ui=NULL, server=NULL, onStart=NULL, options=list(),
                     uiPattern="/", enableBookmarking = NULL) {
  if (is.null(server)) {
    stop("`server` missing from shinyApp")
  }

  # Ensure that the entire path is a match
  uiPattern <- sprintf("^%s$", uiPattern)

  httpHandler <- override.uiHttpHandler(ui, uiPattern)

  serverFuncSource <- function() {
    server
  }

  if (!is.null(enableBookmarking)) {
    bookmarkStore <- match.arg(enableBookmarking, c("url", "server", "disable"))
    enableBookmarking(bookmarkStore)
  }

  # Store the appDir and bookmarking-related options, so that we can read them
  # from within the app.
  shiny:::shinyOptions(appDir = getwd())
  appOptions <- shiny:::consumeAppOptions()

  structure(
    list(
      httpHandler = httpHandler,
      serverFuncSource = serverFuncSource,
      onStart = onStart,
      options = options,
      appOptions = appOptions
    ),
    class = "shiny.appobj"
  )
}

override.uiHttpHandler <- function(ui, uiPattern = "^/$") {

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
      mode <- showcaseModeOfReq(req)
      if (!is.null(mode))
        showcaseMode <- mode
    }

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

    override.renderPage(uiValue, textConn, showcaseMode)
    html <- paste(readLines(textConn, encoding = 'UTF-8'), collapse='\n')
    return(shiny:::httpResponse(200, content=enc2utf8(html)))
  }
}

override.renderPage <- function(ui, connection, showcase=0) {
  library(htmltools)
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

  shiny_deps <- list(
    htmlDependency("json2", "2014.02.04", c(href="shared"), script = "json2-min.js"),
    htmlDependency("jquery", "1.12.4", c(href="shared"), script = "jquery.min.js"),
    htmlDependency("babel-polyfill", "6.7.2", c(href="shared"), script = "babel-polyfill.min.js"),
    htmlDependency("rcloud.shiny", "0.4", c(href="/shared.R/rcloud.shiny/"), script = "rcloud.shiny.child.js"),
    htmlDependency("shiny", utils::packageVersion("shiny"), c(href="shared"),
      script = if (getOption("shiny.minified", TRUE)) "shiny.min.js" else "shiny.js",
      stylesheet = "shiny.css")
  )
  html <- renderDocument(ui, shiny_deps, processDep = createWebDependency)
  shiny:::writeUTF8(html, con = connection)
}
