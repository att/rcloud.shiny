rcloud.shinyApp <- function(ui, server, options) {
  require(rcloud.web)
  require(shiny)

  blah.txt <- '/tmp/blah.txt'
  blahg <- function(x, append = TRUE) {
    cat(x, file=blah.txt, append = append, sep='\n')
  }

  blahg('shinyApp called', FALSE)

  info <- rcloud.session.info()
  port <- 8887

  url = paste0('proxy.R/', info$user, '/', info$id, ':', port, '/')
  blahg(paste0('here is the url: ', url))

  rcw.result(
    run = function(...) {
      blahg('start running')
      app <- shinyApp(ui = ui, server = server)
      res <- try(runApp(app, port = port, launch.browser = FALSE))
      if (inherits(res, 'try-error')) {
        blahg(paste0('failure: ', res))
      } else {
        blahg('success?  you tell me')
      }
    },
    body = paste0('<meta http-equiv="refresh" content="2; url=', url, '" />')
  )
}

