rcloud.proxy.url <- function(port, search, hash) {
  info <- rcloud.session.info()
  paste0('/proxy.R/', info$user, '/', info$id, ':', port, '/', search, hash)
}

rcloud.shinyApp <- function(ui, server, onStart = NULL, options = list(), uiPattern = "/") {
  rcloud.shinyAppInternal(ui, server, onStart = onStart, options = options, uiPattern = uiPattern)
}

.debug.msg <- function(msg = "", debug.enabled = .isDebugEnabled(), debug.file = .getLogFile()) {
  if(debug.enabled && !is.null(debug.file)) {
    cat(msg, file = debug.file, sep = "\n", append = TRUE)
  }
}

.isDebugEnabled <- function() {
  OPT <- "rcloud.shiny.debug.enabled"
  if(rcloud.support:::nzConf(OPT)) {
    return(as.logical(rcloud.support:::getConf(OPT)))
  }
  return(FALSE)
}

.getLogFile <- function() {
  file.path(rcloud.support:::pathConf("tmp.dir"), "rcloud.shiny.log")
}

.socket <- new.env() 

.shinySocket <- function() {
  .socket <- rcloud.shiny:::.socket
  .socket$appHandlers <- NULL
  .socket$onMessageHandler <- NULL
  .socket$onCloseHandler <- NULL
  .socket$fws <- NULL
  
  fakeWebSocket <- function(id) {
    list(
      send = function(msg) {
        rcloud.shiny.caps$on_message(id, msg);
      },
      onMessage = function(h) {
        .socket$onMessageHandler <- h
      },
      onClose = function(h) {
        .socket$onCloseHandler <- h
      }, 
      close = function() {
        parentEnv <- parent.frame(2)
        errorVar <- "e"
        errorMsg <- NULL
        # Retrieve error message if socket is closed as a result of unhandled error
        if(exists(errorVar, envir =  parentEnv)) {
          error <- get("e", envir=parentEnv)
          errorMsg <- jsonlite::toJSON(list(type="rcloud-shiny-error", msg=as.character(error)), auto_unbox=TRUE)
        }
        tryCatch(rcloud.shiny.caps$on_close(id, errorMsg), error = function(e) {
          warning(paste("Failed to notify frontent about closed socket: " , e, "\n"))
        })
        
        tryCatch(.socket$onCloseHandler(), error = function(e) {
          warning(paste("Failed to execute socket onCloseHandler: " , e, "\n"))
        })
      });
  }
  
  .socket$connect <- function(id) {
    rcloud.shiny.debugMsg("Shiny connected")
    .socket$fws <- fakeWebSocket(id)
    .socket$appHandlers$ws(.socket$fws)
  }
  
  .socket$receive <- function(id, msg) {
    rcloud.shiny.debugMsg(paste("Shiny message", msg))
    .socket$onMessageHandler(FALSE, msg)
  }
  .socket
}

.executeExitCallbacks <- function(exit.env) {
  
  .debug.msg("Running shiny shutdown callbacks")
  
  if(!is.null(.socket$fws)) {
    tryCatch(.socket$fws$close(), error = function(e) {
      errMsg <- paste("Failed to close shiny socket:", as.character(e))
      warning(errMsg)
      if (exit.env$debug.enabled) {
        .debug.msg(errMsg)
        .debug.msg(traceback())
      }
    })
  }
  
  if( exit.env$exit.callbacks.executed ) {
    warning("Shiny app exit callbacks already executed, skipping")
    return(exit.env$exit.callbacks.executed)
  }
  
  .debug.msg(paste0("Number of callback functions: ", length(exit.env$exit.callbacks)))
  
  lapply(exit.env$exit.callbacks, function(callback) {
    tryCatch(callback(), error = function(e) {
      errMsg <- paste("Failed to execute shiny exit callback: " , as.character(e), "\n")
      warning(errMsg)
      .debug.msg(errMsg)
    })
  })
  
  .debug.msg("Processing shiny shutdown callbacks completed.")
  
  exit.env$exit.callbacks.executed <- TRUE
}

.registerRCloudShinyAppRunner <- function(exit.env) {
  .GlobalEnv$.ocap.idle <- function() {
    shiny:::serviceApp()
    if (isTRUE(shiny:::.globals$stopped)) {
      tryCatch(.executeExitCallbacks(exit.env), error=function(e) {
        .debug.msg(paste("Error when shutting down shiny:", as.character(e)))
      })
    }
  }
}

rcloud.shinyAppInternal <- function(ui, server, onStart = NULL, options = list(), uiPattern = "/", renderer = function(url) { rcloud.web::rcw.result(body = paste0('<iframe src="', url, '" class="rcloud-shiny" frameBorder="0" style="position: absolute; left: 0px; top: 0px; width: 100%; height: 100%;"></iframe>'))}) {
  library(rcloud.web)
  library(shiny)
  library(htmltools)
  
  .socket <- .shinySocket()

  ocaps <- list(
    connect = rcloud.support:::make.oc(.socket$connect),
    send = rcloud.support:::make.oc(.socket$receive)
  )

  exit.env <- new.env()
  exit.env$exit.callbacks <- c()
  exit.env$exit.callbacks.executed <- FALSE
  
  .registerRCloudShinyAppRunner(exit.env)
  

  app <- override.shinyApp(ui = ui, server = server, onStart = onStart, uiPattern = uiPattern)
  
  .socket$appHandlers <- shiny:::createAppHandlers(NULL, app$serverFuncSource)
  
  extraArgNames <- names(as.list(args(override.runApp)))
  extraArgNames <- extraArgNames[which(nchar(extraArgNames) > 0)]
  
  extraArgs <- options
  extraArgs <- extraArgs[which(names(extraArgs) %in% extraArgNames)]
  
  extraArgs$exit.handler <- function(expr, add = FALSE, env = parent.frame()) {
    if(add) {
      exit.env$exit.callbacks <- c(exit.env$exit.callbacks, function() { eval(deparse(expr), envir = env) })
    } else {
      exit.env$exit.callbacks <- c(function() {eval(deparse(expr), envir = env)})
    }
  }
  
  host <- rcloud.get.conf.value('host')
  appInfo <- tryCatch( {
    do.call(override.runApp, c(list(app, host=nsl(host)), extraArgs))
  }, 
  error = function(o) structure(list(error=o$message), class="shiny-startup-error"))
  
  
  if (inherits(appInfo, "shiny-startup-error")) {
    .executeExitCallbacks(exit.env)
    stop(paste0("Failed to startup shiny application. ", appInfo$error))
  } else {
    f <- .GlobalEnv$.Rserve.done
    session <- rcloud.support:::.session
    .GlobalEnv$.Rserve.done <- function(...) {
      .executeExitCallbacks(exit.env)
      if (is.function(f)) {
        f(...)
      }
    }
  }
  
  loc <- rcloud.shiny.caps$init(ocaps);
  
  renderer(rcloud.proxy.url(shiny:::.globals$lastPort, loc$search, loc$hash))
}


# based on sourceUTF from shiny
sourceNotebookCell <- function(cell, envir = globalenv()) {
  enc <- if (any(Encoding(cell$content) == 'UTF-8')) 'UTF-8' else 'unknown'
  src <- srcfilecopy(cell$filename, cell$content, isFile = FALSE)  # source reference info
  exprs <- try(parse(text = cell$content, keep.source = FALSE, srcfile = src, encoding = enc))
  # Wrap the exprs in first `{`, then ..stacktraceon..(). It's only really the
  # ..stacktraceon..() that we care about, but the `{` is needed to make that
  # possible.
  exprs <- shiny:::makeCall(`{`, exprs)
  # Need to wrap exprs in a list because we want it treated as a single argument
  exprs <- shiny:::makeCall(shiny:::..stacktraceon.., list(exprs))
  
  eval(exprs, envir)
}

# Reads in RCloud notebook assets and spawns shiny app (if the assets contain server.R and ui.R)
rcloud.runApp <- function(onStart = NULL, ...) {
  ui.src <- list.notebook.files("ui.R")
  ui <- if (length(ui.src) == 0) {
    function(req) NULL
  } else {
    localGlobals <- shiny:::.globals
    localGlobals$ui <- NULL
    result <- sourceNotebookCell(ui.src[[1]], envir = new.env(parent = globalenv()))
    if (!is.null(localGlobals$ui)) {
      result <- localGlobals$ui[[1]]
    }
    result
  }
  server.src <- list.notebook.files("server.R")
  server <- if (length(server.src) == 0) {
    stop("server.R was not found among Notebook's assets.")
  } else {
    localGlobals <- shiny:::.globals
    localGlobals$server <- NULL
    result <- sourceNotebookCell(server.src[[1]], envir = new.env(parent = globalenv()))
    if (!is.null(localGlobals$server)) {
      result <- localGlobals$server[[1]]
    }
    result
  }
  
  global.src <- list.notebook.files("global.R")
  
    # uiHandler <- function(req) {
    #   uiHandlerSource()(req)
    # }
    # 
    # wwwDir <- file.path.ci(appDir, "www")
    # fallbackWWWDir <- system.file("www-dir", package = "shiny")
    
#    shinyOptions(appDir = appDir)
    
    localOnStart <- function() {
      if (length(global.src) > 0)
        sourceNotebookCell(global.src[[1]])
      
      if(!is.null(onStart)) {
        onStart()
      }
    }
    rcloud.shinyApp(ui, server, onStart = localOnStart, ...)
  
}
