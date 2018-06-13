rcloud.proxy.url <- function(port, search, hash) {
  info <- rcloud.session.info()
  paste0('/proxy.R/', info$user, '/', info$id, ':', port, '/', search, hash)
}

rcloud.shinyApp <- function(ui, server, onStart = NULL, options = list(), uiPattern = "/") {
  rcloud.shinyAppInternal(ui, server, onStart = onStart, options = options, uiPattern = uiPattern)
}

rcloud.shinyAppInternal <- function(ui, server, onStart = NULL, options = list(), uiPattern = "/", renderer = function(url) { rcloud.web::rcw.result(body = paste0('<iframe src="', url, '" class="rcloud-shiny" frameBorder="0" style="position: absolute; left: 0px; top: 0px; width: 100%; height: 100%;"></iframe>'))}) {
  library(rcloud.web)
  library(shiny)
  
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
        rcloud.shiny.caps$on_close(id, errorMsg)
        onCloseHandler()
      });
  }
  
  connect <- function(id) {
    rcloud.shiny.debugMsg("Shiny connected")
    fws <- fakeWebSocket(id)
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
  
  .GlobalEnv$.ocap.idle <- function() {
    shiny:::serviceApp()
  }
  
  appHandlers <- shiny:::createAppHandlers(NULL, serverFuncSource)
  app <- override.shinyApp(ui = ui, server = server, onStart = onStart, uiPattern = uiPattern)
  
  extraArgNames <- names(as.list(args(override.runApp)))
  extraArgNames <- extraArgNames[which(nchar(extraArgNames) > 0)]
  
  extraArgs <- options
  extraArgs <- extraArgs[which(names(extraArgs) %in% extraArgNames)]
  
  host <- rcloud.get.conf.value('host')
  appInfo <- do.call(override.runApp, c(list(app, host=nsl(host)), extraArgs))
  
  
  loc <- rcloud.shiny.caps$init(ocaps);
  serverFuncSource <- function() {
    server
  }
  renderer(rcloud.proxy.url(appInfo$port, loc$search, loc$hash))
}


