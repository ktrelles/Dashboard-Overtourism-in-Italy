cat("=== Caricamento global.R ===\n")

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(sf)
library(readxl)
library(broom)
library(stargazer)
library(RColorBrewer)
library(spdep)
library(spatialreg)

# ------------------------------
# CARICA DATI
# ------------------------------
panel <- read_excel("final dataset.xlsx", sheet = "panel_regioni")
panel$anno <- as.integer(panel$anno)

panel$ID_provincia <- as.character(panel$ID_provincia)
panel$nome_provincia <- as.character(panel$nome_provincia)

var_cols <- setdiff(names(panel), c("ID_provincia", "nome_provincia", "anno"))

for (col in var_cols) {
  panel[[col]] <- suppressWarnings(as.numeric(as.character(panel[[col]])))
}

province_con_datos <- panel %>%
  distinct(ID_provincia, nome_provincia)


# ------------------------------
# SHAPEFILE
# ------------------------------
map_italia <- st_read("ProvCM01012026_g_WGS84.shp", quiet = TRUE)
map_italia <- map_italia %>%
  rename(ID_provincia = ID,
         nome_provincia = NAME)

map_italia$ID_provincia <- as.character(map_italia$ID_provincia)
map_italia$nome_provincia <- as.character(map_italia$nome_provincia)

map_data <- map_italia %>%
  inner_join(province_con_datos, by = c("ID_provincia", "nome_provincia"))

if (nrow(map_data) == 0) stop("Nessuna corrispondenza tra shapefile e dataset")

# ------------------------------
# MATRICI W
# ------------------------------
coords <- st_coordinates(st_centroid(st_geometry(map_data)))
nb_queen <- poly2nb(map_data, queen = TRUE)
W_queen  <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)
nb_knn   <- knn2nb(knearneigh(coords, k = 4))
W_knn    <- nb2listw(nb_knn, style = "W", zero.policy = TRUE)

anni_disponibili <- sort(unique(panel$anno))

cat("=== global.R caricato correttamente ===\n")