rcloud.proxy.url <- function(port) {
  info <- rcloud.session.info()
  paste0('proxy.R/', info$user, '/', info$id, ':', port, '/')
}

rcloud.shinyApp <- function(ui, server, options) {
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
      })
  }

  connect <- function(id) {
    #rcloud.print("shiny connected")
    fws <- fakeWebSocket(id)
    appHandlers$ws(fws)
  }

  receive <- function(id, msg) {
    #rcloud.print(paste("shiny message ", msg))
    onMessageHandler(FALSE, msg)
  }

  ocaps <- list(
    connect = rcloud.support:::make.oc(connect),
    send = rcloud.support:::make.oc(receive)
  );

  .GlobalEnv$.ocap.idle <- function() {
    shiny:::serviceApp()
  }

  appHandlers <- shiny:::createAppHandlers(NULL, serverFuncSource)
  app <- override.shinyApp(ui = ui, server = server)

  host <- Sys.info()['nodename']
  appInfo <- override.runApp(app, host=nsl(host))

  rcloud.shiny.caps$init(ocaps);
  serverFuncSource <- function() {
    server
  }
  rcw.result(body = paste0('<iframe src="', rcloud.proxy.url(appInfo$port), '" frameBorder="0" style="position: absolute; left: 0px; top: 0px; width: 100%; height: 100%;"></iframe>'))
}

