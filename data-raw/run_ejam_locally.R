
## load EJAM package + data
library(EJAM)

## load some other packages
library(shiny)
library(data.table)
library(tidyverse)
library(leaflet)
library(leaflet.extras2)
library(sf)

## load all EJAM functions, even if not-exported
devtools::load_all()

# quaddata and localtree seem to sometimes not get set after install 
# which causes the app to crash upon running the analysis
if(!exists("quaddata")) {
  EJAM:::dataload_from_local(varnames = "quaddata")
}
localtree <- SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")

## if you make any changes to a function, can re-run it with source
#source('R/example_function_that_I_changed.R')


## source app-related scripts
source('R/app_config.R')
source('R/app_ui.R')
source('R/app_server.R')

assign("golem_opts", list(isPublic = TRUE), envir = globalenv())
source('inst/manage-public-private.R')
source('inst/global.R')

## launch local version of Shiny app
shinyApp(app_ui, app_server)
