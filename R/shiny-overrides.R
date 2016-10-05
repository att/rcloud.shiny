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

# copying whole function just to skip the blocking loop at the end
override.runApp <- function(appDir=getwd(),
                   port=getOption('shiny.port'),
                   launch.browser=getOption('shiny.launch.browser',
                                            interactive()),
                   host=getOption('shiny.host', '127.0.0.1'),
                   workerId="", quiet=FALSE,
                   display.mode=c("auto", "normal", "showcase")) {
  # RCloud mod: the rcloud.shiny version of the function doesn't block, so disable
  # all cleanup - obviously we'll need to fix this later
  ## on.exit({
  ##   handlerManager$clear()
  ## }, add = TRUE)

  # Enable per-app Shiny options
  oldOptionSet <- shiny:::.globals$options
  # RCloud mod
  ## on.exit({
  ##   .globals$options <- oldOptionSet
  ## },add = TRUE)

  if (is.null(host) || is.na(host))
    host <- '0.0.0.0'

  # Make warnings print immediately
  # Set pool.scheduler to support pool package
  ops <- options(warn = 1, pool.scheduler = shiny:::scheduleTask)
  # RCloud mod
  ## on.exit(options(ops), add = TRUE)

  shiny:::workerId(workerId)

  if (shiny:::inShinyServer()) {
    # If SHINY_PORT is set, we're running under Shiny Server. Check the version
    # to make sure it is compatible. Older versions of Shiny Server don't set
    # SHINY_SERVER_VERSION, those will return "" which is considered less than
    # any valid version.
    ver <- Sys.getenv('SHINY_SERVER_VERSION')
    if (utils::compareVersion(ver, .shinyServerMinVersion) < 0) {
      warning('Shiny Server v', .shinyServerMinVersion,
              ' or later is required; please upgrade!')
    }
  }

  # Showcase mode is disabled by default; it must be explicitly enabled in
  # either the DESCRIPTION file for directory-based apps, or via
  # the display.mode parameter. The latter takes precedence.
  shiny:::setShowcaseDefault(0)

  # If appDir specifies a path, and display mode is specified in the
  # DESCRIPTION file at that path, apply it here.
  if (is.character(appDir)) {
    # if appDir specifies a .R file (single-file Shiny app), look for the
    # DESCRIPTION in the parent directory
    desc <- shiny:::file.path.ci(
      if (tolower(tools::file_ext(appDir)) == "r")
        dirname(appDir)
      else
        appDir, "DESCRIPTION")
    if (file.exists(desc)) {
      con <- file(desc, encoding = checkEncoding(desc))
      # RCloud mod
      ## on.exit(close(con), add = TRUE)
      settings <- read.dcf(con)
      if ("DisplayMode" %in% colnames(settings)) {
        mode <- settings[1, "DisplayMode"]
        if (mode == "Showcase") {
          shiny:::setShowcaseDefault(1)
          # RCloud mod: not sure what this does, but it reports "object 'shiny' not found"
          # not sure if that's because we can't modify shiny package from here or if it's
          # just coz i still don't get R
          ## if ("IncludeWWW" %in% colnames(settings)) {
          ##   shiny:::.globals$IncludeWWW <- as.logical(settings[1, "IncludeWWW"])
          ##   if (is.na(shiny:::.globals$IncludeWWW)) {
          ##     stop("In your Description file, `IncludeWWW` ",
          ##          "must be set to `True` (default) or `False`")
          ##   }
          ## } else {
          ##   shiny:::.globals$IncludeWWW <- TRUE
          ## }
        }
      }
    }
  }

  ## default is to show the .js, .css and .html files in the www directory
  ## (if not in showcase mode, this variable will simply be ignored)
  # RCloud mod: same as above
  ## if (is.null(shiny:::.globals$IncludeWWW) || is.na(shiny:::.globals$IncludeWWW)) {
  ##   shiny:::.globals$IncludeWWW <- TRUE
  ## }

  # If display mode is specified as an argument, apply it (overriding the
  # value specified in DESCRIPTION, if any).
  display.mode <- match.arg(display.mode)
  if (display.mode == "normal") {
    shiny:::setShowcaseDefault(0)
  }
  else if (display.mode == "showcase") {
    shiny:::setShowcaseDefault(1)
  }

  require(shiny)

  # determine port if we need to
  if (is.null(port)) {

    # Try up to 20 random ports. If we don't succeed just plow ahead
    # with the final value we tried, and let the "real" startServer
    # somewhere down the line fail and throw the error to the user.
    #
    # If we (think we) succeed, save the value as .globals$lastPort,
    # and try that first next time the user wants a random port.

    for (i in 1:20) {
      if (!is.null(shiny:::.globals$lastPort)) {
        port <- shiny:::.globals$lastPort
        shiny:::.globals$lastPort <- NULL
      }
      else {
        # Try up to 20 random ports
        while (TRUE) {
          port <- shiny:::p_randomInt(3000, 8000)
          # Reject ports in this range that are considered unsafe by Chrome
          # http://superuser.com/questions/188058/which-ports-are-considered-unsafe-on-chrome
          if (!port %in% c(3659, 4045, 6000, 6665:6669)) {
            break
          }
        }
      }

      # Test port to see if we can use it
      tmp <- try(shiny:::startServer(host, port, list()), silent=TRUE)
      if (!inherits(tmp, 'try-error')) {
        stopServer(tmp)
        shiny:::.globals$lastPort <- port
        break
      }
    }
  }

  appParts <- as.shiny.appobj(appDir)

  # Extract appOptions (which is a list) and store them as shinyOptions, for
  # this app. (This is the only place we have to store settings that are
  # accessible both the UI and server portion of the app.)
  shiny:::unconsumeAppOptions(appParts$appOptions)

  # Set up the onEnd before we call onStart, so that it gets called even if an
  # error happens in onStart.
  # RCloud mod
  ## if (!is.null(appParts$onEnd))
  ##   on.exit(appParts$onEnd(), add = TRUE)
  if (!is.null(appParts$onStart))
    appParts$onStart()

  server <- shiny:::startApp(appParts, port, host, quiet)

  # RCloud mod: rcloud.shiny will need to do this somewhere else
  ## on.exit({
  ##   stopServer(server)
  ## }, add = TRUE)

  # RCloud mod
  ## if (!is.character(port)) {
  ##   # http://0.0.0.0/ doesn't work on QtWebKit (i.e. RStudio viewer)
  ##   browseHost <- if (identical(host, "0.0.0.0")) "127.0.0.1" else host

  ##   appUrl <- paste("http://", browseHost, ":", port, sep="")
  ##   if (is.function(launch.browser))
  ##     launch.browser(appUrl)
  ##   else if (launch.browser)
  ##     utils::browseURL(appUrl)
  ## } else {
  ##   appUrl <- NULL
  ## }

  # call application hooks
  shiny:::callAppHook("onAppStart", appUrl)
  # RCloud mod
  ## on.exit({
  ##   callAppHook("onAppStop", appUrl)
  ## }, add = TRUE)

  # RCloud mod: can't modify shiny? hope we don't need to
  ## shiny:::.globals$reterror <- NULL
  ## shiny:::.globals$retval <- NULL
  ## shiny:::.globals$stopped <- FALSE

  # RCloud mod: rcloud.shiny will poll serviceApp from RServe instead
  ## # Top-level ..stacktraceoff..; matches with ..stacktraceon in observe(),
  ## # reactive(), Callbacks$invoke(), and others
  ## ..stacktraceoff..(
  ##   captureStackTraces(
  ##     while (!.globals$stopped) {
  ##       serviceApp()
  ##       Sys.sleep(0.001)
  ##     }
  ##   )
  ## )

  ## if (isTRUE(.globals$reterror)) {
  ##   stop(.globals$retval)
  ## }
  ## else if (.globals$retval$visible)
  ##   .globals$retval$value
  ## else
  ##   invisible(.globals$retval$value)
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
