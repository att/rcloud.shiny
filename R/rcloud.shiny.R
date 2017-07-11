rcloud.proxy.url <- function(port, search, hash) {
  info <- rcloud.session.info()
  paste0('/proxy.R/', info$user, '/', info$id, ':', port, '/', search, hash)
}

spawnApp <- function(app) {
  
  handlers <- list()
  
  fakeWebSocket <- function(id) {
    list(
      send = function(msg) {
        rcloud.shiny.caps$on_message(id, msg);
      },
      onMessage = function(h) {
        handlers$onMessageHandler <<- h
      },
      onClose = function(h) {
        handlers$onCloseHandler <<- h
      })
  }
  
  handlers$connect <- function(id) {
    fws <- fakeWebSocket(id)
    handlers$appHandlers$ws(fws)
  }
  
  handlers$receive <- function(id, msg) {
    handlers$onMessageHandler(FALSE, msg)
  }
  
  .GlobalEnv$.ocap.idle <- function() {
    shiny:::serviceApp()
  }
  
  serverFuncSource <- function() {
    app$serverFuncSource()
  }
  
  handlers$appHandlers <- shiny:::createAppHandlers(NULL, serverFuncSource)
  
  host <- rcloud.get.conf.value('host')
  appInfo <- override.runApp(app, host=nsl(host))
  
  structure(list("appInfo" = appInfo, "handlers" = handlers), 
            class = "rcloud.shinyapp.instance")
}

shinyApp <- function(ui, server, onStart=NULL, options=list(),
                     uiPattern="/", enableBookmarking = NULL) {
  if(!requireNamespace("rcloud.web", quietly = TRUE)) {
    stop("'rcloud.web' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  override.shinyApp(ui = ui, server = server, onStart = onStart, options = options, 
                    uiPattern = uiPattern, enableBookmarking = enableBookmarking)
}

print.shiny.appobj <- function(x, ..., view = interactive()) {
  rcloudShinyAppInstance <- spawnApp(x)
  if(!rcloud.shiny.caps$is_mini()) {
    where <- paste0("rc_shinyapp_", as.integer(runif(1)*1e6))
    rcloud.html.out(paste0(
      "<div class=\"rcloud-shinyapp\">",
      "<div id=\"", where, "\"></div>",
      "</div>"))
    where <- paste0("#", where)
    
    ocaps <- registerOcaps(rcloudShinyAppInstance)
    rcloud.shiny.caps$create(where, contentDiv(buildHtml(rcloudShinyAppInstance, ocaps, style = "width: 100%; height: 400px;")))
    invisible(x)
  } else {
    ocaps <- registerOcaps(rcloudShinyAppInstance)
    invisible(rcloud.web::rcw.result(body = buildHtml(rcloudShinyAppInstance, ocaps)))
  }
}

contentDiv <- function(what) {
  where <- paste0("rc_shinyapp_content_", as.integer(runif(1)*1e6))
  paste(
    sep = "",
    "<div class=\"rcloud-shinyapp-content\" id=\"",
    where,
    "\">",
    what,
    "</div>"
  )
}

registerOcaps <- function(rcloudShinyAppInstance) {
  ocaps <- list(
    connect = rcloud.support:::make.oc(rcloudShinyAppInstance$handlers$connect),
    send = rcloud.support:::make.oc(rcloudShinyAppInstance$handlers$receive)
  );
  rcloud.shiny.caps$init(ocaps);
}

buildHtml <- function(rcloudShinyAppInstance, ocaps, style = "position: absolute; left: 0px; top: 0px; width: 100%; height: 100%;") {
  appInfo <- rcloudShinyAppInstance$appInfo
  paste0('<iframe src="', rcloud.proxy.url(appInfo$port, ocaps$search, ocaps$hash), paste0('" class="rcloud-shiny" frameBorder="0" style="', style, '"></iframe>'))
}

rcloud.shinyApp <- function(ui, server, options = list()) {
  app <- shinyApp(ui, server, options = options)
  print(app)
}

