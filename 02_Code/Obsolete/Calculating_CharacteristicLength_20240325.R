############################
# Purpose:  Lehmann and Or 2024 to estimate characteristic lengths for different soils (!?)
#
# Rachel R. Renne
# March 22, 2024
###################################

# 3.1 Soil type affects evaporatio losses with concurrent drainage

# --> Need shape parameter n of the soil water characteristics curve
#   Ranges from 1 for fine-textured clays to ~3 for sand

# These expressions come from Lehmann et al. (2020) Physical constraints for 
# improved soil hydraulic parameter estimation by pedotransfer functions. 
# Water Resources Research, 56(4), e2019WR025963.

# alpha(n) = a [(n-n_min)/(1+b(n-n_min))]
# Ksat(n) = c(n-n_min)^d

# Parameters for equation providing constraints to make Lc more realistic:
# n_min = 1.2, a = 10.8/m, b = 2.84, c = 2.92 m/day, d = 1.49

################################################################################

# https://ncss-tech.github.io/AQP/soilDB/ROSETTA-API.html
# 
# Zhang, Y., and M.G. Schaap. 2017. Weighted recalibration of the Rosetta pedotransfer 
# model with improved estimates of hydraulic parameter distributions and summary statistics 
# (Rosetta3). Journal of Hydrology 547: 39-53. doi: doi:10.1016/j.jhydrol.2017.01.004.

library(soilDB)
library(Ternary)
library(viridis)

# Try out ROSETTA (https://github.com/usda-ars-ussl/rosetta-soil)
ROSETTA(data.frame(sand = 30,silt = 30, clay = 40), vars = c('sand','silt','clay'), v = '3')

# Create a texture datafame
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

# Visualize npar
cols <- cut(r$npar, breaks=seq(min(r$npar)-0.001,max(r$npar)+0.1,0.025))

par(mar = c(1,1,1,1), mfrow = c(1,1))
TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(r[,c(3,2,1)]), pch = 16, col = viridis(25)[cols],
              cex = 1, bg = viridis(25)[cols])

# Now calculate characteristic length (Lc) for each of these soil types
# Transform npar and alpha from log10 transformation and calculate m
r$n <- 10^r$npar
r$m <- 1-(1/r$n)
r$alpha <- 10^r$alpha

# Look at low (3) and high (10) E0 (convert to cm/day)
E0_low <- 3/10
E0_high <- 10/10

# Calculate numerator (from Lehmann et al. 2020) for UNCONSTRAINED Lc
numerator <- ((1-r$m)/r$alpha)*((1+1/r$m)^(1+r$m))

# Modify hc to not be NaN using log transformation:
#hc <- (1/r$alpha)*(((1-r$n)/r$n)^((1-2*r$n)/r$n))
loghc <- log(1)-log(r$alpha)+((1-2*r$n)/r$n)*log((r$n-1)/r$n)
hc <- exp(loghc)

# Put together two functions for estimating hydraulic conductivity
# from h (capillary pressure) and water content (theta)
Kunsat <- function(hc, alpha, n, m, ksat, Tau){
  Theta <- (1+(alpha*hc)^(n))^(-1*m)
  kTheta <- 4*ksat*(Theta^Tau)*(1-(1-Theta^(1/m))^m)^2
  return(kTheta)
}

# Note we can set Tau to 0.5 as in Lehmann et al. 2020
# Need to transform ksat into m/day (comes from model as log10(ksat) in cm/day)
khc <- Kunsat(hc, r$alpha, r$n, r$m, ksat = (10^r$ksat), Tau = 0.5)


# Calculate denominator
denominator_low <- 1+(E0_low/(4*khc))
denominator_high <- 1+(E0_high/(4*khc))

# Finally calculate Lc (characteristic length) for low and high PET
# Transform to meters
Lc_low <- numerator/denominator_low/100
Lc_high <- numerator/denominator_high/100

# Visualize Lc
cols_low <- cut(Lc_low, breaks=c(seq(0.1,1, by = 0.1),2,3,4,5,6,7,8,9))
cols_high <- cut(Lc_high, breaks=c(seq(0.1,1, by = 0.1),2,3,4,5,6,7,8,9))

par(mar = c(1,1,1,1))
par(mfrow = c(1,2))
TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(r[,c(3,2,1)]), pch = 16, col = rev(viridis(17))[cols_low],
              cex = 1)
TernaryPoints(matrix(c(35,38,27,
                      9,25,66,
                      9,75,16,
                      18,52,30),nrow = 4, byrow = TRUE), pch = 16, col = "black")


TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(r[,c(3,2,1)]), pch = 16, col = rev(viridis(17))[cols_high],
              cex = 1)


# Devise legend text:
leg <- levels(cols_low)
leg1 <- gsub("\\(|\\]","",leg)
leg1 <- gsub(",","-", leg1)

par(mfrow=c(1,1))
plot(1:7,1:7, col = "white", bty = "n", xaxt = "n", yaxt = "n")
legend("center", legend = leg1, pch = 16, bty = "n", cex = 2,
       col = rev(viridis(17)), ncol = 2)

####################################################################################
# Constrained Lc (we can call it Lt) from Lehmann et al., (2020)

# Need to check and correct calculations.

# Calculated parameters from Rosetta3 for African desert region:
n_min = 1.2
a = 10.8
b = 2.84
c = 2.92
d = 1.49

# Calculate numerator (Eq. 3 Lehmann et al. 2020)
Lt_num <- (((r$n - 1)/(2*r$n -1))^(-2+(1/r$n)))*(1+b*(r$n - n_min))

# Calculate denominator (Eq. 3 Lehmann et al. 2020)
# Note: need to convert E0 from cm to m
Lt_denom_low <- a*r$n*(1+(((E0_low)*((r$n - n_min)^(-d))*((1+(((r$n-1)/r$n)^(1-2*r$n)))^((r$n-1)/(2*r$n)))))/
                         (4*c*((1+(((r$n-1)/r$n)^(2*r$n-1)))^(-1+1/r$n)-1)^2))*(r$n-n_min)

Lt_denom_high <- a*r$n*(1+(((E0_high)*((r$n - n_min)^(-d))*((1+(((r$n-1)/r$n)^(1-2*r$n)))^((r$n-1)/(2*r$n)))))/
                         (4*c*((1+(((r$n-1)/r$n)^(2*r$n-1)))^(-1+1/r$n)-1)^2))*(r$n-n_min)


# Calculate Lt_low and Lt_high
Lt_low <- Lt_num/Lt_denom_low
Lt_high <- Lt_num/Lt_denom_high

####################################################################################
alpha = a*((r$n-n_min)/(1+b*(r$n - n_min)))
Ks = c*((r$n-n_min)^d)
gn = -2.56*log(r$n-1.1966)+5 # modify from log(n-1.2) to avoid -Inf
Theta = 
K = Ks*Theta^gn


# Visualize Lc
colLt_low <- cut(Lt_low, breaks=seq(0,1, by = 0.1))
colLt_high <- cut(Lt_high, breaks=seq(0,1, by = 0.1))

par(mar = c(1,1,1,1))
par(mfrow = c(1,2))
TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(r[,c(3,2,1)]), pch = 16, col = rev(viridis(10))[colLt_low],
              cex = 1)
TernaryPoints(matrix(c(35,38,27,
                       9,25,66,
                       9,75,16,
                       18,52,30),nrow = 4, byrow = TRUE), pch = 16, col = "black")


TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(r[,c(3,2,1)]), pch = 16, col = rev(viridis(10))[colLt_high],
              cex = 1)


# Devise legend text:
leg <- levels(colLt_low)
leg1 <- gsub("\\(|\\]","",leg)
leg1 <- gsub(",","-", leg1)

par(mfrow=c(1,1))
plot(1:7,1:7, col = "white", bty = "n", xaxt = "n", yaxt = "n")
legend("center", legend = leg1, pch = 16, bty = "n", cex = 2,
       col = rev(viridis(17)), ncol = 2)

####################################################################################
library(soiltexture)
# Fix up tri-data
td <- as.matrix(r[,c(3,2,1)])
colnames(td) <- c("CLAY","SILT","SAND")

layout(mat = matrix(c(1,1,2,2,
                      1,1,2,2,
                      1,1,2,2,
                      3,3,3,3), ncol = 4, byrow = TRUE), widths = rep(1,12), heights = rep(1,12))
layout.show(3)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, pch = 16, col = rev(viridis(17))[cols_low],
              cex = 1, main = "Low evaporation (3 mm/day)")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", class.line.col = "black", cex.lab = 1)

geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, pch = 16, col = rev(viridis(17))[cols_high],
               cex = 1, main = "High evaporation (10 mm/day)")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", class.line.col = "black", cex.lab = 1)

plot(1:7,1:7, col = "white", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
legend("center", legend = leg1, bty = "n", cex = 1.5, title = "Lc (m)",
       fill = rev(viridis(17)), ncol = 6, border = rev(viridis(17)))


####################### Now look at n

# Make colors
col_n <- cut(r$n, breaks = c(seq(1,3, by = 0.2),4.5))

# Devise legend text:
lega <- levels(col_n)
leg1a <- gsub("\\(|\\]","",lega)
leg1a <- gsub(",","-", leg1a)


layout(mat = matrix(c(1,1,1,2,
                      1,1,1,2,
                      1,1,1,2), ncol = 4, byrow = TRUE), widths = rep(1,12), heights = rep(1,12))

layout.show(2)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, pch = 16, col = rev(viridis(11))[col_n],
               cex = 1.2, main = "Shape parameter n")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", class.line.col = "black", cex.lab = 1)

plot(1:7,1:7, col = "white", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
legend("center", legend = leg1a, bty = "n", cex = 1.5,
       fill = rev(viridis(11)), ncol = 1, border = rev(viridis(11)))

#################################################
# Evaporation efficiency from Merlin et al. 2016 and Lehmann et al. 2018

# Evaporation efficiency is when evap rates are 1/2 potential ET.

evap0.5 <- 0.20 + 0.28*r$clay/100 - 0.16*r$sand/100

# Make colors
col_e.5 <- cut(evap0.5, breaks = seq(0.04,0.48, by = 0.04))

# Devise legend text:
legb <- levels(col_e.5)
leg1b <- gsub("\\(|\\]","",legb)
leg1b <- gsub(",","-", leg1b)


layout(mat = matrix(c(1,1,1,2,
                      1,1,1,2,
                      1,1,1,2), ncol = 4, byrow = TRUE), widths = rep(1,12), heights = rep(1,12))

layout.show(2)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, pch = 16, col = rev(viridis(11))[col_n],
               cex = 1.2, main = "Critical water content")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", class.line.col = "black", cex.lab = 1)

plot(1:7,1:7, col = "white", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
legend("center", legend = leg1b, bty = "n", cex = 1.5,
       fill = rev(viridis(11)), ncol = 1, border = rev(viridis(11)))
