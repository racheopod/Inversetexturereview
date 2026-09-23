############################
# Purpose: Create a figure showing relative flows of water balance between coarse
#           and fine textured soils at dry and wet sites
#
# Rachel R. Renne
# August 5, 2025
# Updated: November 17, 2025
###################################

# Set up folder
figdir <- "03_Figures"

# Set up dataframe of wet and dry proportions for each water balance flow
wb <- data.frame(variable = c("Interception","Runoff","Infiltration",
                              "Soil Evaporation","Transpiration","Storage","Drainage"),
                 dry_coarse = c(50,0,50,25,25,0,0),
                 dry_fine = c(45,5,45,31.5,9,4.5,0),
                 wet_coarse = c(2.5,5,92.5,27,43.5,1,21),
                 wet_fine = c(5,10,85,27,53.5,4.5,0))

################################################################################
# Get results from an ITE simulation to help make figures

# Load relevant libraries
library("rSFSW2")
library("rSOILWAT2")
library(RSQLite)
sqlite <- dbDriver("SQLite")

# Set up project directory
dirprj <- "C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/04_Simulation_Experiment/DryGB2-006_ITE_higherveg"

# Database directory
dir_dat <- file.path(dirprj, "4_Simulation")

# Database name
outDB_fname <- file.path(dir_dat, "dbOutput.sqlite3")

# Connect to output database
outDB_con <- RSQLite::dbConnect(RSQLite::SQLite(), outDB_fname)
dbListTables(outDB_con)

# Get data for entire tables (daily aggregations)

# Average daily transpiration
trans <- get.Table_Scenario(outDB_fname,
                            responseName = "aggregation_doy_Transpiration", MeanOrSD = "Mean",
                            scenario = "Current", header = TRUE)

# Average daily soil & surface evaporation
evapsoil <- get.Table_Scenario(outDB_fname,
                               responseName = "aggregation_doy_EvaporationSoil", MeanOrSD = "Mean",
                               scenario = "Current", header = TRUE)

evapsurface <- get.Table_Scenario(outDB_fname,
                                  responseName = "aggregation_doy_EvaporationSurface", MeanOrSD = "Mean",
                                  scenario = "Current", header = TRUE)

# Get deep drainage
deepdrain <- get.Table_Scenario(outDB_fname,
                                responseName = "aggregation_doy_DeepDrainage", MeanOrSD = "Mean",
                                scenario = "Current", header = TRUE)

# Get overall means
means <- get.Table_Scenario(outDB_fname,
                            responseName = "aggregation_overall", MeanOrSD = "Mean",
                            scenario = "Current", header = TRUE)


# Clean up
RSQLite::dbDisconnect(outDB_con)

# Set up results from "means" df
results <- data.frame(Label = means$Labels, mat = NA, map = NA, 
                      map_sim = apply(means[,535:546],1,sum),
                      mat_sim = apply(means[,523:534],1,mean),
                      sand = NA, clay = NA, silt = NA,
                      grass = means$PotentialNaturalVegetation_CompositionTotalGrasses_Fraction, 
                      shrub = means$PotentialNaturalVegetation_CompositionShrubs_Fraction, 
                      transpiration = means$Transpiration_Total_mm_mean,
                      evaporationtotal = means$Evaporation_Total_mm_mean,
                      evaporationsoil = means$Evaporation_Soil_Total_mm_mean,
                      evaporationinterception = means$Evaporation_InterceptedByLitter_mm_mean+means$Evaporation_InterceptedByVegetation_mm_mean,
                      evaporationsurface = means$Evaporation_SurfaceWater_mm_mean,
                      deepdrainage = means$DeepDrainage_mm_mean)

# Keep only relevant rows
results1 <- results[c(416,409,2096,2089),]

# Get MAT, MAP, Sand, Clay
results1$map <- c(150,150,650,650)
results1$mat <- 5
results1$sand <- c(80,10,80,10)
results1$clay <- 10
results1$silt <- 100 - results1$sand - results1$clay


# Set up dataframe of wet and dry proportions for each water balance flow
wb1 <- data.frame(variable = c("Interception","Runoff","Infiltration",
                              "Soil Evaporation","Transpiration","Storage","Drainage"),
                 dry_coarse = t(means[grepl("mat5_map150_mix_DryGB2-006_sand80",means$Labels),
                                    c("Interception_Total_mm_mean","Evaporation_SurfaceWater_mm_mean",
                                      "Infiltration_mm_mean",
                                      "Evaporation_Soil_Total_mm_mean","Transpiration_Total_mm_mean",
                                      "SWC_StorageChange_mm_mean","DeepDrainage_mm_mean")]),
                 dry_fine = t(means[grepl("mat5_map150_mix_DryGB2-006_sand10_clay10",means$Labels),
                                  c("Interception_Total_mm_mean","Evaporation_SurfaceWater_mm_mean",
                                    "Infiltration_mm_mean",
                                    "Evaporation_Soil_Total_mm_mean","Transpiration_Total_mm_mean",
                                    "SWC_StorageChange_mm_mean","DeepDrainage_mm_mean")]),
                 wet_coarse = t(means[grepl("mat5_map650_mix_DryGB2-006_sand80",means$Labels),
                                    c("Interception_Total_mm_mean","Evaporation_SurfaceWater_mm_mean",
                                      "Infiltration_mm_mean",
                                      "Evaporation_Soil_Total_mm_mean","Transpiration_Total_mm_mean",
                                      "SWC_StorageChange_mm_mean","DeepDrainage_mm_mean")]),
                 wet_fine = t(means[grepl("mat5_map650_mix_DryGB2-006_sand10_clay10",means$Labels),
                                  c("Interception_Total_mm_mean","Evaporation_SurfaceWater_mm_mean",
                                    "Infiltration_mm_mean",
                                    "Evaporation_Soil_Total_mm_mean","Transpiration_Total_mm_mean",
                                    "SWC_StorageChange_mm_mean","DeepDrainage_mm_mean")]))
names(wb1) <- names(wb)
rownames(wb1) <- rownames(wb)

# Get surface evap
esurf <- apply(evapsurface[evapsurface$Labels %in% means[c(416,409,2096,2089),]$Labels, 50:415],
               1,sum)[c(2,1,4,3)]

# Get soil evap
esoil <- apply(evapsoil[evapsoil$Labels %in% means[c(416,409,2096,2089),]$Labels, 50:415],
               1,sum)[c(2,1,4,3)]

# Let's consider surface evap to be runoff
wb1[2,2:5] <- esurf

# Let's calculate interception by adding runoff and infiltration
wb1[1,2:5] <- c(150,150,650,650) - (wb1[2,2:5] + wb1[3,2:5])

# Convert wb1 to relative
wb1_rel <- wb1
for (i in 1:7){
  wb1_rel[i,2:5] <- round(wb1[i,2:5]/c(150,150,650,650)*100)
}

# Make new wb df based on the above and scratch
wb2 <- wb
wb2$dry_coarse <- c(25,2,73,30,41,0,2)
wb2$dry_fine <- c(30,6,64,37,25,2,0)
wb2$wet_coarse <- c(38,3,59,4,36,1.5,17.5)
wb2$wet_fine <- c(40,7,53,6,52,3,2)

# Set wb2 to wb
wb <- wb2

################################################################################

# Create titles vector
titles <- c(expression("Interception (E"[i]*")"),
            "Runoff (R)","Infiltration (I)",
            expression("Soil Evaporation (E"[S]*")"),
            "Transpiration (T)","Storage (M)","Drainage (D)")

# Create individual plots of each flow
par(mfrow=c(3,3))
for (i in 1:7){
#png(file.path(figdir,paste0("Fig2_0",i,"_",wb[i,1],".png")), width = 5, height = 4.5, units = "in", res = 300)
par(mar = c(2,2.2,2,1), mgp = c(1,0.2,0), tcl = 0, las = 1)
barplot(t(wb[i,2:5]), beside = TRUE,
        main = titles[i], ylim = c(0,110),
        yaxt ="n", space = c(0,0,0.1,0),
        names.arg = c("coarse","fine","coarse","fine"),
        col = c("#b35806","#b35806","#01665e","#01665e"),
        density = c(50,1000,50,1000), cex.names = 1.75,
        cex.main = 2.5, lwd = 2)
axis(side = 2, at = c(0,100), cex.axis = 1.5, labels = TRUE)
abline(v=2.05,lwd = 1.5)
text(x = c(1,3),y = c(100,100), labels = c("Dry","Wet"), cex = 2.5)
box(lwd = 1.5)
#dev.off()
}

