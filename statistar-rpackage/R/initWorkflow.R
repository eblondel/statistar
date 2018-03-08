#' @name initWorkflow
#' @aliases initWorkflow
#' @title initWorkflow
#' @description \code{initWorkflow} allows to init a workflow
#'
#' @usage initWorkflow(file)
#'                 
#' @param file a JSON configuration file
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
initWorkflow <- function(file){
  
  config <- jsonlite::read_json(file)
  config$src <- file
  
  #worfklow config$loggers
  config$logger <- function(type, text){cat(sprintf("[%s][%s] %s \n", config$id, type, text))}
  config$logger.info <- function(text){config$logger("INFO", text)}
  config$logger.warn <- function(text){config$logger("WARNING", text)}
  config$logger.error <- function(text){config$logger("ERROR", text)}
  
  config$logger.info("Init Workflow configuration")
  config$logger.info("========================================================================")
  
  #working dir
  config$wd <- getwd()
  
  #load packages
  #-------------
  #from CRAN
  config$logger.info("Loading R CRAN packages...")
  cran_pkgs = c("devtools", config$dependencies$packages$cran)
  invisible(sapply(cran_pkgs, function(pkg){
    if(config$dependencies$packages$cran_force_install){
      config$logger.info(sprintf("Reinstalling R CRAN package '%s'", pkg))
      eval(parse(text = sprintf("try(detach(\"package:%s\", unload=TRUE, force = TRUE))", pkg)))
      install.packages(pkg)
    }
    config$logger.info(sprintf("Loading R CRAN package '%s'", pkg))
    eval(parse(text = sprintf("require(%s)", pkg)))
  }))
  #from Github
  config$logger.info("Loading R GitHub packages...")
  github_pkgs = config$dependencies$packages$github
  invisible(sapply(github_pkgs, function(pkg){
    pkgname <- unlist(strsplit(pkg, "/"))[2]
    if(config$dependencies$packages$github_force_install){
      config$logger.info(sprintf("Reinstalling R GitHub package '%s'", pkgname))
      eval(parse(text = sprintf("try(detach(\"package:%s\", unload=TRUE, force = TRUE))", pkgname)))
      devtools::install_github(pkg, force = TRUE)
    }
    config$logger.info(sprintf("Loading R GitHub package '%s'", pkgname))
    eval(parse(text = sprintf("require(%s)", pkgname)))
  }))
  #load source scripts
  #--------------------
  config$logger.info("Loading R scripts...")
  source_scripts <- config$dependencies$scripts
  invisible(sapply(source_scripts,function(script){
    config$logger.info(sprintf("Loading R script '%s'...", script))
    source(script)
  }))
  
  #load Scripts (if config$gcube$scripts_download = TRUE, download them before)
  #-------------------------------
  config$logger.info("Loading R scripts...")
  mainDir <- config$wd
  subDir <- "scripts"
  if (!file.exists(subDir)){
    dir.create(file.path(mainDir, subDir))
  }
  setwd(file.path(mainDir, subDir))
  for(script in config$gcube$scripts){
    if(config$gcube$scripts_download){
      config$logger.info(sprintf("Downloading script '%s' from workspace", script))
      downloadFileWS(paste(config$gcube$repositories$scripts, script, sep = "/"))
    }
    
    config$logger.info(sprintf("Loading script '%s' in R", script))
    source(script)
  }
  setwd(config$wd)
  
  #load google sheets from Urls
  #-------------------------------
  if(length(config$gsheetUrls)>0){
	  config$logger.info("Loading Google sheets...")
	  config$gsheets = lapply(names(config$gsheetUrls), function(gsheetName){
		gsheetUrl <- config$gsheetUrls[[gsheetName]]
		config$logger.info(sprintf("Loading Google sheet '%s' (%s)...", gsheetUrl, gsheetName))
		out <- as.data.frame(gsheet::gsheet2tbl(gsheetUrl))
		return(out)
	  })
	  names(config$gsheets) <- names(config$gsheetUrls)
  }
  
  #connect to database
  #--------------------
  db <- config$db
  if(!is.null(db)){
	config$logger.info(sprintf("Connect to database '%s'...", db$name))
	config$db[["con"]] <- dbConnect(db$drv, dbname=db$name, user=db$user, password=db$pwd, host=db$host)
	  
	#specific to PG for now
	config$db[["dbpath"]] <- paste("PG:", paste(lapply(names(db), function(x){return(paste(x,db[[x]],sep="="))}), collapse=" "), sep="")
  }
  
  #SDI
  if(!is.null(config$sdi)){
	  #Geoserver API manager
	  #--------------------
	  gs <- config$sdi$geoserver 
	  if(!is.null(gs)){
		config$logger.info("Connect to GeoServer API...")
		config$sdi$geoserver[["api"]] <- geosapi::GSManager$new(url = gs$url, user = gs$user, pwd = gs$pwd, config$sdi$loggerLevel)
	  }
	  #Geonetwork API manager
	  #--------------------
	  gn <- config$sdi$geonetwork
	  if(!is.null(gn)){
		config$logger.info("Connect to GeoNetwork API...")
		config$sdi$geonetwork[["api"]] <- geonapi::GNManager$new(url = gn$url, user = gn$user, pwd = gn$pwd, version = gn$version,config$sdi$loggerLevel)
	  }
  }
  return(config)
}
