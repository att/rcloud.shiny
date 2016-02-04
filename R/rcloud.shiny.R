braap.createWebDependency <- function(prefix)
  function(dependency) {
    dep <- shiny:::createWebDependency(dependency)
    dep$src$href <- paste0(prefix, dep$src$href);
    dep
  }

# minor changes from shiny:::renderPage
braap.renderPage <- function(prefix, ui, connection, showcase=0) {
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
    htmlDependency("shiny", utils::packageVersion("shiny"), c(href="shared"),
      script = if (getOption("shiny.minified", TRUE)) "shiny.min.js" else "shiny.js",
      stylesheet = "shiny.css")
  )
  html <- renderDocument(ui, shiny_deps, processDep = braap.createWebDependency(prefix))
  shiny:::writeUTF8(html, con = connection)
}

.renderPage <- function(prefix, ui) {
  textConn <- textConnection(NULL, "w")
  on.exit(close(textConn))
  braap.renderPage(prefix, ui, textConn, FALSE)
  paste(textConnectionValue(textConn), collapse="\n")
}

rcloud.proxy.url <- function(port) {
  info <- rcloud.session.info()
  paste0('proxy.R/', info$user, '/', info$id, ':', port, '/')
}

rcloud.shinyApp <- function(ui, server, options) {
  require(rcloud.web)
  require(shiny)

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
    send = rcloud.support:::make.oc(receive),
    service_app = rcloud.support:::make.oc(shiny:::serviceApp)
  );

  app <- shiny:::shinyApp(ui = ui, server = server)
  host <- '0.0.0.0'
  port <- 8887
  server <- shiny:::startApp(shiny:::as.shiny.appobj(app), port, host, FALSE)
  ## on.exit({
  ##   stopServer(server)
  ## }, add = TRUE)

  rcloud.shiny.caps$init(ocaps);
  serverFuncSource <- function() {
    server
  }
  appHandlers <- shiny:::createAppHandlers(NULL, serverFuncSource)
  rcw.result(body = .renderPage(rcloud.proxy.url(8887), ui))
}

