library(shiny)
library(bs4Dash)
library(DT)
library(tidyverse)
library(scales)
library(broom)
library(fontawesome)
library(performance)
library(see)
library(ggstatsplot)
library(billboarder)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(highcharter)
library(sf)
library(viridisLite)
library(waiter)
library(PMCMRplus)


shinyOptions(cache = cachem::cache_disk("./app_cache/cache/"))

files <- list.files("./Data", pattern = "\\.rds$")
object_names <- tools::file_path_sans_ext(basename( files))

setwd("./Data")
lapply(files, 
       readRDS) %>%
  setNames(object_names) %>%
  list2env(envir=globalenv())
setwd("..")



