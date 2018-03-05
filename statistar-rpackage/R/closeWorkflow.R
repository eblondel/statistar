#' @name closeWorkflow
#' @aliases closeWorkflow
#' @title closeWorkflow
#' @description \code{closeWorkflow} allows to close a workflow
#'
#' @usage closeWorkflow(config)
#'                 
#' @param config a configuration object as read by \code{closeWorkflow}
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
closeWorkflow <- function(config){
  #close DB
  config$logger.info("Closing database connection")
  dbDisconnect(config$db$con)
  #Geoserver API manager
  config$logger.info("Reset Geoserver API manager")
  config$sdi$geoserver$api <- NULL
  #Geonetwork API manager
  config$logger.info("Reset Geonetwork API manager")
  config$sdi$geonetwork$api <- NULL
  setwd(config$wd)
}
