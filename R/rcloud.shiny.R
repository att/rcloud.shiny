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

rcloud.shinyAppInternal <- function(ui, server, onStart = NULL, options = list(), uiPattern = "/", renderer = function(url) { rcloud.web::rcw.result(body = paste0('<iframe src="', url, '" class="rcloud-shiny" frameBorder="0" style="position: absolute; left: 0px; top: 0px; width: 100%; height: 100%;"></iframe>'))}) {
  library(rcloud.web)
  library(shiny)
  library(htmltools)
  
  appHandlers <- NULL
  onMessageHandler <- NULL
  onCloseHandler <- NULL
  
  fakeWebSocket <- function(id) {
    list(
      send = function(msg) {
        rcloud.shiny.caps$on_message(id, msg);
      },
      onMessage = function(h) {
        onMessageHandler <<- h
      },
      onClose = function(h) {
        onCloseHandler <<- h
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
        
        tryCatch(onCloseHandler(), error = function(e) {
          warning(paste("Failed to execute socket onCloseHandler: " , e, "\n"))
        })
      });
  }
  
  fws <- NULL

  connect <- function(id) {
    rcloud.shiny.debugMsg("Shiny connected")
    fws <<- fakeWebSocket(id)
    appHandlers$ws(fws)
  }

  receive <- function(id, msg) {
    rcloud.shiny.debugMsg(paste("Shiny message", msg))
    onMessageHandler(FALSE, msg)
  }

  ocaps <- list(
    connect = rcloud.support:::make.oc(connect),
    send = rcloud.support:::make.oc(receive)
  )


  executeExitCallbacks <- function(exit.env) {
    
    .debug.msg("Running shiny shutdown callbacks")
    
    if(!is.null(fws)) {
      tryCatch(fws$close(), error = function(e) {
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
  
  exit.env <- new.env()
  exit.env$exit.callbacks <- c()
  exit.env$exit.callbacks.executed <- FALSE
  
  .GlobalEnv$.ocap.idle <- function() {
    shiny:::serviceApp()
    if (isTRUE(shiny:::.globals$stopped)) {
      tryCatch(executeExitCallbacks(exit.env), error=function(e) {
        .debug.msg(paste("Error when shutting down shiny:", as.character(e)))
      })
    }
  }
  
  appHandlers <- shiny:::createAppHandlers(NULL, serverFuncSource)
  app <- override.shinyApp(ui = ui, server = server, onStart = onStart, uiPattern = uiPattern)
  
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
    executeExitCallbacks(exit.env)
    stop(paste0("Failed to startup shiny application. ", appInfo$error))
  } else {
    f <- .GlobalEnv$.Rserve.done
    session <- rcloud.support:::.session
    .GlobalEnv$.Rserve.done <- function(...) {
      executeExitCallbacks(exit.env)
      if (is.function(f)) {
        f(...)
      }
    }
  }
  
  loc <- rcloud.shiny.caps$init(ocaps);
  serverFuncSource <- function() {
    server
  }
  renderer(rcloud.proxy.url(shiny:::.globals$lastPort, loc$search, loc$hash))
}
