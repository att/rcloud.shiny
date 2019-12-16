# we override a few functions in shiny by copying, renaming, and slighly modifying them this is just
# to add another page-dependency, which needs to come before some of the built-in ones.

# the source in this file is licensed under GPL-3, copyright RStudio and contributors; see
# https://github.com/rstudio/shiny/blob/master/LICENSE and
# https://github.com/rstudio/shiny/blob/master/DESCRIPTION for details.
override.shinyApp <- function(ui=NULL, server=NULL, onStart=NULL, options=list(),
                              uiPattern="/", enableBookmarking=NULL) {
  if (is.null(server)) {
    stop("`server` missing from shinyApp")
  }
  
  # Ensure that the entire path is a match
  uiPattern <- sprintf("^%s$", uiPattern)
  
  httpHandler <- getOption("shiny.uiHttpHandler")(ui, uiPattern)
  
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

serviceAppRunner <- function() {
  localGlobals <- shiny:::.globals
  localGlobals$reterror <- NULL
  localGlobals$retval <- NULL
  localGlobals$stopped <- FALSE
  # Top-level ..stacktraceoff..; matches with ..stacktraceon in observe(),
  # reactive(), Callbacks$invoke(), and others
  ..stacktraceoff..(
    captureStackTraces({
      while (!localGlobals$stopped) {
        ..stacktracefloor..(shiny:::serviceApp())
        Sys.sleep(0.001)
      }
    })
  )
  
  if (isTRUE(localGlobals$reterror)) {
    stop(localGlobals$retval)
  }
  else if (localGlobals$retval$visible)
    localGlobals$retval$value
  else
    invisible(localGlobals$retval$value)
}

# copying whole function just to skip the blocking loop at the end
override.runApp <- function(appDir=getwd(),
                            port=getOption('shiny.port'),
                            launch.browser=getOption('shiny.launch.browser',
                                                     interactive()),
                            host=getOption('shiny.host', '127.0.0.1'),
                            workerId="", quiet=FALSE,
                            display.mode=c("auto", "normal", "showcase"),
                            test.mode=getOption('shiny.testmode', FALSE),
                            exit.handler=getOption('shiny.on.exit', on.exit),
                            serviceApp.runner=getOption('shiny.serviceAppRunner', serviceAppRunner)) {
  exit.handler({
    shiny:::handlerManager$clear()
  }, add = TRUE)
  
  localGlobals <- shiny:::.globals
  
  if (localGlobals$running) {
    stop("Can't call `runApp()` from within `runApp()`. If your ",
         "application code contains `runApp()`, please remove it.")
  }
  
  
  localGlobals$running <- TRUE
  
  exit.handler({
    localGlobals$running <- FALSE
  }, add = TRUE)
  
  # Enable per-app Shiny options
  oldOptionSet <- shiny:::.globals$options
  exit.handler({
    localGlobals$options <- oldOptionSet
  },add = TRUE)
  
  # Make warnings print immediately
  # Set pool.scheduler to support pool package
  ops <- options(
    # Raise warn level to 1, but don't lower it
    warn = max(1, getOption("warn", default = 1)),
    pool.scheduler = shiny:::scheduleTask
  )
  exit.handler(options(ops), add = TRUE)
  
  appParts <- as.shiny.appobj(appDir)
  
  # The lines below set some of the app's running options, which
  # can be:
  #   - left unspeficied (in which case the arguments' default
  #     values from `runApp` kick in);
  #   - passed through `shinyApp`
  #   - passed through `runApp` (this function)
  #   - passed through both `shinyApp` and `runApp` (the latter
  #     takes precedence)
  #
  # Matrix of possibilities:
  # | IN shinyApp | IN runApp | result       | check                                                                                                                                  |
  # |-------------|-----------|--------------|----------------------------------------------------------------------------------------------------------------------------------------|
  # | no          | no        | use defaults | exhaust all possibilities: if it's missing (runApp does not specify); THEN if it's not in shinyApp appParts$options; THEN use defaults |
  # | yes         | no        | use shinyApp | if it's missing (runApp does not specify); THEN if it's in shinyApp appParts$options; THEN use shinyApp                                |
  # | no          | yes       | use runApp   | if it's not missing (runApp specifies), use those                                                                                      |
  # | yes         | yes       | use runApp   | if it's not missing (runApp specifies), use those                                                                                      |
  #
  # I tried to make this as compact and intuitive as possible,
  # given that there are four distinct possibilities to check
  appOps <- appParts$options
  findVal <- function(arg, default) {
    if (arg %in% names(appOps)) appOps[[arg]] else default
  }
  
  if (missing(port))
    port <- findVal("port", port)
  if (missing(launch.browser))
    launch.browser <- findVal("launch.browser", launch.browser)
  if (missing(host))
    host <- findVal("host", host)
  if (missing(quiet))
    quiet <- findVal("quiet", quiet)
  if (missing(display.mode))
    display.mode <- findVal("display.mode", display.mode)
  if (missing(test.mode))
    test.mode <- findVal("test.mode", test.mode)
  
  if (is.null(host) || is.na(host)) host <- '0.0.0.0'
  
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
  
  localGlobals$testMode <- test.mode
  if (test.mode) {
    message("Running application in test mode.")
  }
  
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
      exit.handler(close(con), add = TRUE)
      settings <- read.dcf(con)
      if ("DisplayMode" %in% colnames(settings)) {
        mode <- settings[1, "DisplayMode"]
        if (mode == "Showcase") {
          shiny:::setShowcaseDefault(1)
          if ("IncludeWWW" %in% colnames(settings)) {
            localGlobals$IncludeWWW <- as.logical(settings[1, "IncludeWWW"])
            if (is.na(localGlobals$IncludeWWW)) {
              stop("In your Description file, `IncludeWWW` ",
                   "must be set to `True` (default) or `False`")
            }
          } else {
            localGlobals$IncludeWWW <- TRUE
          }
        }
      }
    }
  }
  
  ## default is to show the .js, .css and .html files in the www directory
  ## (if not in showcase mode, this variable will simply be ignored)
  if (is.null(localGlobals$IncludeWWW) || is.na(localGlobals$IncludeWWW)) {
    localGlobals$IncludeWWW <- TRUE
  }
  
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
      if (!is.null(localGlobals$lastPort)) {
        port <- localGlobals$lastPort
        localGlobals$lastPort <- NULL
      }
      else {
        # Try up to 20 random ports
        while (TRUE) {
          port <- shiny:::p_randomInt(3000, 8000)
          # Reject ports in this range that are considered unsafe by Chrome
          # http://superuser.com/questions/188058/which-ports-are-considered-unsafe-on-chrome
          # https://github.com/rstudio/shiny/issues/1784
          if (!port %in% c(3659, 4045, 6000, 6665:6669, 6697)) {
            break
          }
        }
      }
      
      # Test port to see if we can use it
      tmp <- try(httpuv:::startServer(host, port, list()), silent=TRUE)
      if (!inherits(tmp, 'try-error')) {
        httpuv:::stopServer(tmp)
        localGlobals$lastPort <- port
        break
      }
    }
  }
  
  # Invoke user-defined onStop callbacks, before the application's internal
  # onStop callbacks.
  exit.handler({
    localGlobals$onStopCallbacks$invoke()
    localGlobals$onStopCallbacks <- Callbacks$new()
  }, add = TRUE)
  
  # Extract appOptions (which is a list) and store them as shinyOptions, for
  # this app. (This is the only place we have to store settings that are
  # accessible both the UI and server portion of the app.)
  shiny:::unconsumeAppOptions(appParts$appOptions)
  
  # Set up the onStop before we call onStart, so that it gets called even if an
  # error happens in onStart.
  if (!is.null(appParts$onStop))
    exit.handler(appParts$onStop(), add = TRUE)
  if (!is.null(appParts$onStart))
    appParts$onStart()
  
  server <- shiny:::startApp(appParts, port, host, quiet)
  
  exit.handler({
    httpuv:::stopServer(server)
  }, add = TRUE)
  
  if (!is.character(port)) {
    # http://0.0.0.0/ doesn't work on QtWebKit (i.e. RStudio viewer)
    browseHost <- if (identical(host, "0.0.0.0")) "127.0.0.1" else host
    
    appUrl <- paste("http://", browseHost, ":", port, sep="")
    if (is.function(launch.browser))
      shiny:::launch.browser(appUrl)
    else if (launch.browser)
      utils::browseURL(appUrl)
  } else {
    appUrl <- NULL
  }
  
  # call application hooks
  shiny:::callAppHook("onAppStart", appUrl)
  exit.handler({
    shiny:::callAppHook("onAppStop", appUrl)
  }, add = TRUE)
  
  serviceApp.runner()
}
