
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
data <- read_xlsx("input/data2022.xlsx")

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

###############################################################################
################################################################## CARTES EXPLO

# Creation des limites departementales
dep <- fondata
dep$DEP_CODE <- substr(dep$IRISE_C, 1, 2)
dep <- asf_borders(f = dep, by = "DEP_CODE")



# Boucle pour realiser toutes les cartes et les exporter en PDF
for (i in 7:21) {
  
  # Nom de la variable
  varname <- names(fondata)[i]
  
  # Ouvrir un fichier PDF
  pdf(file = paste0("output/carte_", varname, ".pdf"), width = 8, height = 8)
  
  # Palette
  pal <- asf_palette(pal = "posidonie", nb = 6)
  
  # Seuils
  q6 <- quantile(fondata[[i]], probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)
  
  # Carte choroplethe
  mf_map(fondata,
         var = varname,
         type = "choro",
         breaks = q6,
         pal = pal,
         border = NA)
  
  # Contours departements
  mf_map(dep,
         col = "white",
         lwd = 1,
         add = TRUE)
  
  # Labels
  mf_label(label,
           var = "label",
           col = "#000000",
           font = 1)
  
  # Fermer le PDF
  dev.off()
}
