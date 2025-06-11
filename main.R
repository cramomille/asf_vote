
#                                        ASF - SCRIPT DE LA PLANCHE SUR LE VOTE
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

# Creation de zooms
z <- asf_zoom(f = irisr_e, 
              places = c("5", "4"), 
              r = 10000)

zoom <- z$zooms
label <- z$labels
point <- z$point

# Simplification des geometries du fond principal
irisr_e_simply <- asf_simplify(f = irisr_e, keep = 0.1)

# Lecture des donnees
