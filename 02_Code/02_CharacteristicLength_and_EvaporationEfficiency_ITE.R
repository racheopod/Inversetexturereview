################################################################################
# Purpose:  Lehmann and Or 2024 to estimate characteristic lengths for different soils
# 
#
# Rachel R. Renne
# March 22, 2024
# Updated: August 6, 2025
################################################################################

# Set up folder
figdir <- "03_Figures"

################################################################################
# Lehmann, P., Or, D., 2024. Analytical model for bare soil evaporation dynamics 
# following wetting with concurrent internal drainage. Journal of Hydrology 631, 
# 130800.

# 3.1 Soil type affects evaporation losses with concurrent drainage

# --> Need shape parameter n of the soil water characteristics curve
#   Ranges from 1 for fine-textured clays to ~3 for sand

# These expressions come from Lehmann et al. (2020) Physical constraints for 
# improved soil hydraulic parameter estimation by pedotransfer functions. 
# Water Resources Research, 56(4), e2019WR025963.

# Parameters for equation providing constraints to make Lc more realistic:
# n_min = 1.2, a = 10.8/m, b = 2.84, c = 2.92 m/day, d = 1.49

################################################################################
#  Step 1: Use Rosetta to get shape parameters 

# https://ncss-tech.github.io/AQP/soilDB/ROSETTA-API.html
# 
# Zhang, Y., and M.G. Schaap. 2017. Weighted recalibration of the Rosetta pedotransfer 
# model with improved estimates of hydraulic parameter distributions and summary statistics 
# (Rosetta3). Journal of Hydrology 547: 39-53. doi: doi:10.1016/j.jhydrol.2017.01.004.

library(soilDB)
library(Ternary)
library(viridis)
library(soiltexture)

# Try out ROSETTA (https://github.com/usda-ars-ussl/rosetta-soil)
ROSETTA(data.frame(sand = 30,silt = 30, clay = 40), vars = c('sand','silt','clay'), v = '3')

# Create a texture dataframe
S1 = seq(0,100,by=1)
C1 = seq(0,100,by=1)
soil <- expand.grid(S1,C1)
soil$sum <- apply(soil, 1, sum)
soil <- soil[soil$sum <= 100,]
soil$clay <- 100-soil$sum
soil$sum <- apply(soil[,c(1,2,4)], 1, sum)
# Check that we get 100%
table(soil$sum)
soil <- soil[,c("Var1",'Var2',"clay")]
names(soil)[1:2] <- c("sand","silt")

# Now run this through Rosetta (v3)
r <- ROSETTA(soil, vars = c("sand","silt","clay"), v = "3")

# Now calculate characteristic length (Lc) for each of these soil types
# Transform npar and alpha from log10 transformation and calculate m
r$n <- 10^r$npar
r$m <- 1-(1/r$n)
r$alpha <- 10^r$alpha

################################################################################
# Step 2: Visualize n parameter

cols <- cut(r$n, breaks=seq(min(r$n)-0.1,max(r$n)+0.1,0.25))

par(mar = c(1,1,1,1), mfrow = c(1,1))
TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(r[,c(3,2,1)]), pch = 16, col = viridis(13)[cols],
              cex = 1, bg = viridis(13)[cols])

################################################################################
# Step 3: Calculate characteristic length (Lc)
# Note: this approach overestimates Lc for intermediate soil textures

# Look at low (3) and high (10) E0 (convert to cm/day)
E0_low <- 3/10
E0_high <- 10/10

# Calculate numerator (from Lehmann et al. 2020) for UNCONSTRAINED Lc

# Lehmann, P., Bickel, S., Wei, Z., Or, D., 2020. Physical Constraints for Improved 
# Soil Hydraulic Parameter Estimation by Pedotransfer Functions. Water Resources 
# Research 56, e2019WR025963. https://doi.org/10.1029/2019WR025963
numerator <- ((1-r$m)/r$alpha)*((1+(1/r$m))^(1+r$m))

# Calculate hc
hc <- 1/(100*r$alpha)*(1/(r$m^(1+r$m)))

# Put together two functions for estimating hydraulic conductivity
# from h (capillary pressure) and water content (theta)
# Need to transform ksat into cm/day (comes from model as log10(ksat) in cm/day)
Kunsat <- function(hc, alpha, n, m, ksat, Tau){
  Theta <- (1+(100*alpha*hc)^n)^(-m)
  kTheta <- 10*10^ksat*(Theta^Tau)*(1-(1-Theta^(1/m))^m)^2
  return(kTheta)
}

# Note we can set Tau to 0.5 as in Lehmann et al. (2020)
# Tau is tortuosity factor
khc <- Kunsat(hc, r$alpha, r$n, r$m, ksat = r$ksat, Tau = 0.5)

# Calculate denominator
denominator_low <- 1+(E0_low/(4*khc))
denominator_high <- 1+(E0_high/(4*khc))

# Finally calculate Lc (characteristic length) for low and high PET
# Transform to meters
Lc_low <- numerator/denominator_low/100
Lc_high <- numerator/denominator_high/100

# Look at range of Lc_low and Lc_high
summary(Lc_low)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.280   1.591   2.297   3.364   4.560   9.544 
summary(Lc_high)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2792  0.6757  1.1871  1.9345  2.2881  8.3440 

# Visualize Lc
cols_low <- cut(Lc_low, breaks=c(seq(0.2,1,by=0.1),2,3,4,5,6,7,8,9,10))
cols_high <- cut(Lc_high, breaks=c(seq(0.2,1,by=0.1),2,3,4,5,6,7,8,9,10))


par(mar = c(1,1,1,1))
par(mfrow = c(1,1))
TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(r[,c(3,2,1)]), pch = 16, col = rev(viridis(17))[cols_low],
              cex = 1)


TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(r[,c(3,2,1)]), pch = 16, col = rev(viridis(17))[cols_high],
              cex = 1)

####################################################################################
# Step 4: Calculate constrained Lc (we can call it Lt) from Lehmann et al., (2020)

# Calculated parameters from Rosetta3 for African desert region:
n_min = 1.196611 # Lehmann et al. recommend 1.2; Adjusted to min n-parameter to avoid NaN's
a = 10.8
b = 2.84
c = 2.92
d = 1.49

# Calculate numerator (Eq. 3 Lehmann et al. 2020)
Lt_num <- (((r$n - 1)/(2*r$n - 1))^(-2+(1/r$n)))*(1+b*(r$n - n_min))

# Calculate denominator (Eq. 3 Lehmann et al. 2020) for low and high E0
# Note: need to convert E0 from cm to m
Lt_denom_low_middle_num <- (E0_low/100)*((r$n - n_min)^(-d))*((1+(((r$n-1)/r$n)^(1-2*r$n)))^((r$n-1)/(2*r$n)))
Lt_denom_low_middle_denom <- (4*c*((1+(((r$n-1)/r$n)^(2*r$n-1)))^(-1+1/r$n)-1)^2)
Lt_denom_low <- a*r$n*(1+(Lt_denom_low_middle_num/Lt_denom_low_middle_denom))*(r$n-n_min)


Lt_denom_high_middle_num <- (E0_high/100)*((r$n - n_min)^(-d))*((1+(((r$n-1)/r$n)^(1-2*r$n)))^((r$n-1)/(2*r$n)))
Lt_denom_high_middle_denom <- (4*c*((1+(((r$n-1)/r$n)^(2*r$n-1)))^(-1+1/r$n)-1)^2)
Lt_denom_high <- a*r$n*(1+(Lt_denom_high_middle_num/Lt_denom_high_middle_denom))*(r$n-n_min)

# Calculate Lt_low and Lt_high
Lt_low <- Lt_num/Lt_denom_low
Lt_high <- Lt_num/Lt_denom_high

################################################################################
# Step 5: Visualize Lt (Constrained Lc)

# Summarize Lt
summary(Lt_low)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0001591 0.1842478 0.4880385 0.4989329 0.8174748 0.9648323 
summary(Lt_high)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 4.775e-05 5.593e-02 1.621e-01 2.036e-01 3.312e-01 6.036e-01 

# Visualize Lc
colLt_low <- cut(Lt_low, breaks=seq(0,1, by = 0.1))
colLt_high <- cut(Lt_high, breaks=seq(0,1, by = 0.1))

par(mar = c(1,1,1,1))
par(mfrow = c(1,1))
TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(r[,c(3,2,1)]), pch = 16, col = rev(viridis(10))[colLt_low],
              cex = 1)


TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(r[,c(3,2,1)]), pch = 16, col = rev(viridis(10))[colLt_high],
              cex = 1)


# Devise legend text:
leg <- levels(colLt_low)
leg1a <- gsub("\\(|\\]","",leg)
leg1a <- strsplit(leg1a,",")
# Convert from m to cm
leg1 <- vector()
for (i in 1:10){
  thispair <- as.numeric(leg1a[[i]])
  leg1[i] <- paste0(thispair[1]*100,"-",thispair[2]*100)
}


par(mfrow=c(1,1))
plot(1:7,1:7, col = "white", bty = "n", xaxt = "n", yaxt = "n")
legend("center", legend = leg1, pch = 16, bty = "n", cex = 2,
       col = rev(viridis(10)), ncol = 2)

####################################################################################
# Step 6: Create plots of Lt at low and high E0

# Fix up tri-data
td <- as.matrix(r[,c(3,2,1)])
colnames(td) <- c("CLAY","SILT","SAND")

# Set up color ramp
Lt_ramp <- colorRampPalette(rev(c("#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a",
                                  "#e31a1c","#bd0026","#800026")))

# First for low PET (E0): 3mm/day
png(file.path(figdir,"Fig4_CharacteristicLength_LowE0.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(0,0,0,0))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = rev(Lt_ramp(10))[colLt_low],
              cex = 0.75, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()


# Now for high PET (E0): 10mm/day
png(file.path(figdir,"Fig4_CharacteristicLength_HighE0.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(0,0,0,0))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = rev(Lt_ramp(10))[colLt_high],
               cex = 0.75, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()

# Make legend for Figure 4
# Set up legend polygon information
# Create axis locations for labels
axislabs <- seq(0,100,by=10)
# Set up axis label locations
axisat <- 0
for (ii in 2:(length(axislabs))){
  axisat <- c(axisat, (axisat[ii-1]+(10/length(levels(colLt_low)))))
}

# Save legend
png(file.path(figdir,"Fig4_CharacteristicLength_legend.png"), width = 6.5, height = 3, units = "in", res = 300)
par(mar = c(1,0,0,0), mgp = c(1,0.2,0))
plot(1:10,1:10, col = "white", bty = "n", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", xlim = c(0,10), ylim = c(0,10))
for (xx in 1:length(levels(colLt_high))){
  polygon(x = c(axisat[xx],axisat[xx],axisat[xx+1],axisat[xx+1]), y = c(1.73,2.73,2.73,1.73),
          border = rev(Lt_ramp(10))[xx], col = rev(Lt_ramp(10))[xx])
}
polygon(x = c(0,0,10,10), y = c(1.73,2.73,2.73,1.73), lwd = 1.5)
mtext(expression("Depth of bare soil evaporation L"[C]*" (cm)"), side = 1, line = -1, cex = 1.2)
par(tcl = -0.3)
axis(side = 1, line = -2.7, at = seq(0,10, length.out = length(axislabs)),cex.axis = 0.9,
     labels = axislabs)
dev.off()

################################################################################
# Step 7: Now look at n

# Make colors
col_n <- cut(r$n, breaks = c(seq(1,3, by = 0.2),4.5))

# Devise legend text:
lega <- levels(col_n)
leg1a <- gsub("\\(|\\]","",lega)
leg1a <- gsub(",","-", leg1a)

png(file.path(figdir,"n_parameter.png"), width = 6, height = 5, units = "in", res = 300)
layout(mat = matrix(c(1,1,1,2,
                      1,1,1,2,
                      1,1,1,2), ncol = 4, byrow = TRUE), widths = rep(1,12), heights = rep(1,12))
# layout.show(2)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = rev(viridis(11))[col_n],
               cex = 1, main = "Shape parameter n")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)

plot(1:7,1:7, col = "white", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
legend("center", legend = leg1a, bty = "n", cex = 1.5,
       fill = rev(viridis(11)), ncol = 1, border = rev(viridis(11)))
dev.off()

################################################################################
# Step 8: Create figure of water content at soil evaporation efficiency 
#           from Merlin et al. 2016 and Lehmann et al. 2018
# Note: Soil evaporation efficiency is when evap rates are 1/2 potential ET.

# This equation is from Merlin et al. (2016)
Merlin_evap0.5 <- 0.20 + 0.28*r$clay/100 - 0.16*r$sand/100

# Lehmann et al. (2018) developed a physically-based model that performs
# simliarly to the Merlin model:

# Lehmann, P., Merlin, O., Gentine, P., Or, D., 2018. Soil Texture Effects on 
# Surface Resistance to Bare-Soil Evaporation. Geophysical Research Letters 45, 
# 10,398-10,405. https://doi.org/10.1029/2018GL078803

# Use equations from data spreadsheet associated with Lehmann et al. 2018
# Again, there are different outcomes for low and high evaporative demand
# Worth trying both--start with low (3 mm/day)

# Function to calculate water content
# x here is a parameter to search along to find eratio = 0.5
# It should range from 10^-8 to 10^4.18 by 0.01
Theta <- function(theta_r, theta_s, alpha, n, m, x){
  return(theta_r + (theta_s - theta_r)*(1+(alpha*x)^n)^(-m))
}

# Function to calculate saturation proportion
Sat <- function(Theta_out, theta_s, theta_r){
  return((Theta_out - theta_r)/(theta_s - theta_r))
}

# Function to calculate hydraulic conductivity
# Ksat should be in meters
K <- function(Ksat, Sat_out, m){
  return(Ksat*sqrt(Sat_out)*(1-(1-(Sat_out^(1/m)))^m)^2)
}

# A correction to K
# Again, Ksat in m
KeffOld <- function(K_out, Ksat){
  ifelse(4*K_out>(Ksat),(Ksat),4*K_out)
}

# Function to calculate evaporation ratio
# KeffOld in mm
eratio <- function(KeffOld, E0, Khc){
  (1000*KeffOld*(1+E0/(4*Khc))/(E0+1000*KeffOld*(1+E0/(4*Khc))))
}

################################################################################
# Step 9: Verify calculations using published data from Lehmann et al. 2018

# Set up search vector
vec1 <- seq(-8,4.18,by=0.01)
svec <- 10^vec1
# Create empty results df
results <- data.frame(Theta = rep(NA,length(svec)), Sat = NA, K = NA, KeffOld = NA, eratio = NA)
# Search through svec for eratio = 0.5
for (i in 1:length(svec)){
  results$Theta[i] <- Theta(0.0576, 0.3714, 0.0291*100, 2.8453, 0.648543, svec[i])
  results$Sat[i] <- Sat(results$Theta[i], 0.3714, 0.0576)
  results$K[i] <- K(4.7483, results$Sat[i], 0.648543)
  results$KeffOld[i] <- KeffOld(results$K[i], 4.7483)
  results$eratio[i] <- eratio(results$KeffOld[i], 7.5, 13.93803)
}

# Find the two values of Theta (water content) closest to eratio = 0.5
mean(results$Theta[order(abs(0.5-results$eratio))[1:2]])

# Check additional calculations
hcrit <- 1/(100*0.0291)*(1/(0.648543^(1+0.648543)))
SatCrit <- (1+(100*0.0291*hcrit)^2.8453)^(-0.648543)
Kcrit <- 10*474.83*SatCrit^(1/2)*(1-(1-SatCrit^(1/0.648543))^0.648543)^2

################################################################################
# Step 9: Calculate theta0.5 for full soil texture triangle

# Set up search vector
vec1 <- seq(-8,4.18,by=0.01)
svec <- 10^vec1

# Create empty vector to hold theta0.5 for E0_low and E0_high
theta0.5_E0low <- vector()
theta0.5_E0high <- vector()

# Store calculated values
hcrit_vec <- NA
SatCrit_vec <- NA
Kcrit_vec <- NA

# Loop through r to get theta0.5 for each soil
for (st in 1:nrow(r)){
  
  # Add a note to keep track of this loop
  if (st %in% seq(500,5000, by = 500)){ 
    print(paste0("Now working on row ",st," of 5151 (",round(st/5151*100),"% done)"))
    }

# Create empty results df
results <- data.frame(Theta = rep(NA,length(svec)), Sat = NA, K = NA, 
                      KeffOld = NA, hcrit = NA, SatCrit = NA, Kcrit = NA,
                      eratio_e0low = NA, eratio_e0high = NA)

# Calculate some constants
hcrit <- 1/(100*r$alpha[st])*(1/(r$m[st]^(1+r$m[st])))
SatCrit <- (1+(100*r$alpha[st]*hcrit)^r$n[st])^(-r$m[st])
Kcrit <- 10*10^r$ksat[st]*SatCrit^(1/2)*(1-(1-SatCrit^(1/r$m[st]))^r$m[st])^2
hcrit_vec[st] <- hcrit
SatCrit_vec[st] <- SatCrit
Kcrit_vec[st] <- Kcrit

# Search through svec for eratio = 0.5
for (i in 1:length(svec)){
  results$Theta[i] <- Theta(r$theta_r[st], r$theta_s[st], r$alpha[st]*100, r$n[st], r$m[st], svec[i])
  results$Sat[i] <- Sat(results$Theta[i], r$theta_s[st], r$theta_r[st])
  results$K[i] <- K(10^r$ksat[st]/100, results$Sat[i], r$m[st])
  results$KeffOld[i] <- KeffOld(results$K[i],10^r$ksat[st]/100)
  results$eratio_e0low[i] <- eratio(results$KeffOld[i], E0_low*10, Kcrit)
  results$eratio_e0high[i] <- eratio(results$KeffOld[i], E0_high*10, Kcrit)
}

# Find the two values of Theta (water content) closest to eratio = 0.5
theta0.5_E0low[st] <- mean(results$Theta[order(abs(0.5-results$eratio_e0low))[1:2]])
theta0.5_E0high[st] <- mean(results$Theta[order(abs(0.5-results$eratio_e0high))[1:2]])
}

# Compare result to Merlin et al. (2016) results based on soil texture
par(mar=c(2,2,2,1), mgp = c(1,0.1,0), tcl = 0.1)
plot(Merlin_evap0.5~theta0.5_E0low, xlim = c(0,0.6), ylim = c(0,0.6),
     ylab = "Merlin et al. 2016",
     xlab = "Lehmann et al. 2018")
abline(0,1, lty = 3, col = "red", lwd = 2)

plot(Merlin_evap0.5~theta0.5_E0high, xlim = c(0,0.6), ylim = c(0,0.6),
     ylab = "Merlin et al. 2016",
     xlab = "Lehmann et al. 2018")
abline(0,1, lty = 3, col = "red", lwd = 2)

# Look at theta0.5
summary(theta0.5_E0low)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.06422 0.22332 0.26880 0.27262 0.32387 0.41061
summary(theta0.5_E0high)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.07115 0.22721 0.26963 0.27565 0.32426 0.41079 

# Make colors
wc_ramp <- colorRampPalette(rev(c("#e0ecf4","#bfd3e6","#9ebcda","#8c96c6","#8c6bb1",
                              "#88419d","#810f7c","#4d004b")))
col_theta0.5_low <- cut(theta0.5_E0low, breaks = seq(0,0.55, by = 0.05))
col_theta0.5_high <- cut(theta0.5_E0high, breaks = seq(0,0.55, by = 0.05))

# Create figures of theta0.5 (water content when eratio = 0.5) for E0_low and E0_high
png(file.path(figdir,"Fig3_WaterContent_eratio0.5_LowE0.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = rev(wc_ramp(11))[col_theta0.5_low],
               cex = 1, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()

png(file.path(figdir,"Fig3_WaterContent_eratio0.5_HighE0.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = rev(wc_ramp(11))[col_theta0.5_high],
               cex = 1, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()


# Now create a legend
legb <- levels(col_theta0.5_high)
leg1b <- gsub("\\(|\\]","",legb)
leg1b <- gsub(",","-", leg1b)

# Set up legend polygon information
# Create axis locations for labels
axislabs <- seq(0,0.55,by=0.05)
# Set up axis label locations
axisat <- 0
for (ii in 2:(length(axislabs))){
  axisat <- c(axisat, (axisat[ii-1]+(10/length(levels(col_theta0.5_high)))))
}

# Save legend
png(file.path(figdir,"Fig3_WaterContent_legend.png"), width = 6.5, height = 3, units = "in", res = 300)
par(mar = c(1,0,0,0), mgp = c(1,0.2,0))
plot(1:10,1:10, col = "white", bty = "n", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", xlim = c(0,11), ylim = c(0,11))
for (xx in 1:length(levels(col_theta0.5_high))){
  polygon(x = c(axisat[xx],axisat[xx],axisat[xx+1],axisat[xx+1]), y = c(1.73,2.73,2.73,1.73),
          border = rev(wc_ramp(11))[xx], col = rev(wc_ramp(11))[xx])
}
polygon(x = c(0,0,10,10), y = c(1.73,2.73,2.73,1.73), lwd = 1.5)
mtext(expression(paste("Water content (",plain("cm")^3,plain("cm")^-3,")")), side = 1, line = -1, cex = 1.2)
par(tcl = -0.3)
axis(side = 1, line = -2.5, at = seq(0,10, length.out = length(axislabs)),cex.axis = 0.9,
     labels = axislabs)
dev.off()


################################################################################
# Step 10: Create figure of saturated water content

summary(r$theta_s)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3608  0.3978  0.4326  0.4378  0.4689  0.5477

# Make colors (same breaks as for eratio)
col_theta_s <- cut(r$theta_s, breaks = seq(0,0.55,by=0.05))

# Create figures of theta0.5 (water content when eratio = 0.5) for E0_low and E0_high
png(file.path(figdir,"Fig3_WaterContent_Saturation.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = rev(wc_ramp(11))[col_theta_s],
               cex = 1, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()

# Make colors (same breaks as for eratio)
col_theta_r <- cut(r$theta_r, breaks = seq(0,0.55,by=0.05))

# Create figures of theta0.5 (water content when eratio = 0.5) for E0_low and E0_high
png(file.path(figdir,"Fig3_WaterContent_Residual.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = rev(wc_ramp(11))[col_theta_r],
               cex = 1, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()

################################################################################
# Step 11: Create figures of saturated and unsaturated hydraulic conductivity

# Calculate hydraulic conductivity (mm/day) for water content (Theta) 0.5, 0.25, 0.05

# # Results vectors
# k_wc0.35 <- vector()
# k_wc0.25 <- vector()
# k_wc0.15 <- vector()
#                    
# # Loop through to get K for each
# for (st in 1:nrow(r)){
#   
#   # Add a note to keep track of this loop
#   if (st %in% seq(500,5000, by = 500)){ 
#     print(paste0("Now working on row ",st," of 5151 (",round(st/5151*100),"% done)"))
#   }
#   
#   # Create empty results df
#   results <- data.frame(Theta = rep(NA,length(svec)), Sat = NA, K = NA)
# 
#   # Search through svec for eratio = 0.5
#   for (i in 1:length(svec)){
#     results$Theta[i] <- Theta(r$theta_r[st], r$theta_s[st], r$alpha[st]*100, r$n[st], r$m[st], svec[i])
#     results$Sat[i] <- Sat(results$Theta[i], r$theta_s[st], r$theta_r[st])
#     results$K[i] <- K(10^r$ksat[st]/100, results$Sat[i], r$m[st])
#   }
#   
#   # Find the two values of K closest to Theta (water content) = 0.5, 0.25, or 0.05 
#   k_wc0.35[st] <- mean(results$K[order(abs(0.35-results$Theta))[1:2]])
#   k_wc0.25[st] <- mean(results$K[order(abs(0.25-results$Theta))[1:2]])
#   k_wc0.15[st] <- mean(results$K[order(abs(0.15-results$Theta))[1:2]])
# }
# 
# 
# # Look at range of values (mm/day)
# summary(10*10^r$ksat)
# #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# #    72.44   116.50   176.46   356.37   272.55 15496.41 
# summary(k_wc0.35)
# #     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# # 7.000e-07 1.246e-04 1.571e-03 8.173e-02 1.529e-02 1.256e+01 
# summary(k_wc0.25)
# #      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# # 0.000e+00 3.000e-07 1.684e-05 1.120e-02 5.136e-04 2.836e+00 
# summary(k_wc0.15)
# #     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# # 0.000e+00 0.000e+00 1.000e-09 7.940e-04 2.054e-06 2.902e-01 
# 
# # Check range of saturated water content
# summary(r$theta_s) # Don't want to calculate Kunsat for anything above 0.36
# #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# # 0.3608  0.3978  0.4326  0.4378  0.4689  0.5477 
# 
# plot(k_wc0.35~c(10*10^r$ksat))
# points(k_wc0.25~c(10*10^r$ksat), col = "darkorange")
# points(k_wc0.15~c(10*10^r$ksat), col = "red")

# Make colors
ksat_ramp <- colorRampPalette(c("#e7e1ef","#d4b9da","#c994c7",
                                "#df65b0","#e7298a","#ce1256","#980043","#67001f"))
# Here we have ksat in cm
# Prior options for cuts (hard to choose) quantile(10^r$ksat, seq(0,1,by=0.125)))
#c(seq(0,,by=100),seq(870,15500,by=1000),1550))
col_ksat <- cut(10^r$ksat, breaks = c(0,10,15,25,50,75,100,250,500,750,1000,1250,1550))
# col_kwc0.35 <- cut(k_wc0.35, breaks = c(seq(0,1,by=0.05),seq(2,12,by=2)))
# col_kwc0.25 <- cut(k_wc0.25, breaks = c(seq(0,5,by=1),seq(10,100,by=10),seq(150,300,by=50),1550))
# col_kwc0.15 <- cut(k_wc0.15, breaks = c(seq(0,5,by=1),seq(10,100,by=10),seq(150,300,by=50),1550))

# Create figures of KSat
png(file.path(figdir,"Fig3_Ksat.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = (ksat_ramp(12))[col_ksat],
               cex = 1, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()

# Now create a legend
legc <- levels(col_ksat)
leg1c <- gsub("\\(|\\]","",legc)
leg1c <- strsplit(leg1c,",")
leg2c <- vector()
for (i in 1:length(leg1c)){
  thispair <- round(as.numeric(leg1c[[i]]),1) # Convert to cm/day
  leg2c[i] <- paste0(thispair[1],"-",thispair[2])
}

# Set up legend polygon information
# Create axis locations for labels
axislabs <- c(0,10,15,25,50,75,100,250,500,750,1000,1250,1550)
# Set up axis label locations
axisat <- 0
for (ii in 2:(length(axislabs))){
  axisat <- c(axisat, (axisat[ii-1]+(10/length(levels(col_ksat)))))
}

# Save legend
png(file.path(figdir,"Fig3_Ksat_Legend.png"), width = 6.5, height = 3, units = "in", res = 300)
par(mar = c(1,0,0,0), mgp = c(1,0.2,0))
plot(1:10,1:10, col = "white", bty = "n", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", xlim = c(0,10), ylim = c(0,10))
for (xx in 1:length(unique(col_ksat))){
  polygon(x = c(axisat[xx],axisat[xx],axisat[xx+1],axisat[xx+1]), y = c(1.73,2.73,2.73,1.73),
          border = ksat_ramp(12)[xx], col = ksat_ramp(12)[xx])
}
polygon(x = c(0,0,10,10), y = c(1.73,2.73,2.73,1.73), lwd = 1.5)
mtext(expression("K"[Sat]*" (cm/day)"), side = 1, line = -0.5, cex = 2)
par(tcl = -0.3)
axis(side = 1, line = -2.7, at = seq(0,10, length.out = length(axislabs)),cex.axis = 0.9,
     labels = axislabs)
dev.off()

# 
# # Kunsat at wc 0.35
# #png(file.path(figdir,"Fig3_Kunsat_wc0.35.png"), width = 6, height = 6, units = "in", res = 300)
# par(mar = c(1,1,1,1))
# geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
#                pch = 16, col = rev(viridis(26))[col_kwc0.35],
#                cex = 1, main = "")
# TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
#            class.line.col = "black", cex.lab = 1)
# #dev.off()
# 
# # Now create a legend
# legd <- levels(col_kwc0.35)
# leg1d <- gsub("\\(|\\]","",legd)
# leg1d <- gsub(",","-",leg1d)
# 
# #png(file.path(figdir,"Fig3_K_wc0.35_Legend.png"), width = 6, height = 6, units = "in", res = 300)
# plot(1:7,1:7, col = "white", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
# legend("center", legend = leg1d, bty = "n", cex = 0.88,
#        fill = rev(viridis(26)), ncol = 3, border = rev(viridis(26)))
# #dev.off()
