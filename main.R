
#                              ASF - SCRIPT DE LA PLANCHE SUR L'AGE DES MENAGES
#
#                                                                antoine beroud
#                                                                  jean riviere

library(sf)
library(asf)
library(mapsf)

library(readxl)

###############################################################################
########################################################## FONDS D'ALIETTE ROUX

# Lecture du fichier
irisr_e <- st_read("input/IRISrE/AR04b_sf_IRISrE.shp")
irisr_e <- st_transform(irisr_e, crs = "EPSG:2154")

# Repositionnement des DROM
irisr_e <- asf_drom(f = irisr_e, id = "IRISE_C")

mf_map(irisr_e)

# Creation de zooms
z <- asf_zoom(f = irisr_e, 
              places = c("5", "4"), 
              r = 10000)

zoom <- z$zooms
label <- z$labels
point <- z$point

# simplification des geometries du fond principal
irisr_e_simply <- asf_simplify(f = irisr_e, keep = 0.1)

# Lecture des donnees
data <- read_xlsx("input/DataDemo.xlsx")
data <- data[, c(1, 20:22)]

# Ajout des zeros manquants dans les identifiants
data$IRISrE_CODE <- ifelse(nchar(data$IRISrE_CODE) < 9,
                           paste0(strrep("0", 9 - nchar(data$IRISrE_CODE)), data$IRISrE_CODE),
                           data$IRISrE_CODE)

# Jointure entre le fond, les zooms et les donnees
fondata <- asf_fondata(f = irisr_e_simply, 
                       z = zoom, 
                       d = data, 
                       by.x = "IRISE_C", 
                       by.y = "IRISrE_CODE")

# Creation des limites departementales
dep <- fondata
dep$DEP_CODE <- substr(dep$IRISE_C, 1, 2)
dep <- asf_borders(f = dep, by = "DEP_CODE")

# Choix d'une palette
pal <- asf_palette("qua", nb = 12)

mf_map(fondata, 
       var = "Clust12", 
       type = "typo", 
       pal = pal,
       border = NA)

mf_map(dep, 
       col = "white", 
       lwd = 1, 
       add = TRUE)

mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)


