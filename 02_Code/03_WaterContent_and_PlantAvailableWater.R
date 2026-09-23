################################################################################
# Purpose:  Use van Genuchen equation to calculate water content at fc, wilting point, etc
# 
#
# Rachel R. Renne
# August 7, 2025
################################################################################

# Set up folder
figdir <- "03_Figures"

# Load relevant libraries
library(soilDB)
library(soiltexture)

################################################################################
#  Step 1: Use Rosetta to get shape parameters 

# https://ncss-tech.github.io/AQP/soilDB/ROSETTA-API.html
# 
# Zhang, Y., and M.G. Schaap. 2017. Weighted recalibration of the Rosetta pedotransfer 
# model with improved estimates of hydraulic parameter distributions and summary statistics 
# (Rosetta3). Journal of Hydrology 547: 39-53. doi: doi:10.1016/j.jhydrol.2017.01.004.

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

# Now calculate characteristic length (Lc) for each of these soil types
# Transform npar and alpha from log10 transformation and calculate m
r$n <- 10^r$npar
r$m <- 1-(1/r$n)
r$alpha <- 10^r$alpha

################################################################################
#  Step 2: Calculate field capacity, wilting points: -1.5 MPa and -3.5 Mpa, 
#           and water availability between each

# Create van Genuchen function
# Note: alpha needs to be in meters, h needs to be in kPa
vanG <- function(theta_s, theta_r, alpha, h, n, m){
  # Convert kPa to pressure head in meters h = P/(1000 kg/m3*9.81m/s2)
  h_m = (h*1000)/(1000*9.81)
  theta_r + (theta_s - theta_r)/((1 + (alpha*abs(h_m))^n)^m)
}

# Calculate water content at field capacity, -1.5Mpa, and -3.5Mpa
wcfc <- vanG(r$theta_s,r$theta_r,100*r$alpha,33,r$n,r$m)
wc1500 <- vanG(r$theta_s,r$theta_r,100*r$alpha,1500,r$n,r$m)
wc3500 <- vanG(r$theta_s,r$theta_r,100*r$alpha,3500,r$n,r$m)

# Calculate difference between fc and wilting points
wc1500diff <- wcfc - wc1500
wc3500diff <- wcfc - wc3500


################################################################################
#  Step 3: Make figures of fc and wp

# Fix up tri-data
td <- as.matrix(r[,c(3,2,1)])
colnames(td) <- c("CLAY","SILT","SAND")

# Make colors
wc_ramp <- colorRampPalette(rev(c("#e0ecf4","#bfd3e6","#9ebcda","#8c96c6","#8c6bb1",
                                  "#88419d","#810f7c","#4d004b")))
col_fc <- cut(wcfc, breaks = seq(0,0.55, by = 0.05))
col_wc1500 <- cut(wc1500, breaks = seq(0,0.55, by = 0.05))
col_wc3500 <- cut(wc3500, breaks = seq(0,0.55, by = 0.05))

# Create figures of field capacity and wilting points
png(file.path(figdir,"Fig5_WaterContent_FieldCapacity.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = rev(wc_ramp(11))[col_fc],
               cex = 1, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()

png(file.path(figdir,"Fig5_WaterContent_1500Kpa.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = rev(wc_ramp(11))[col_wc1500],
               cex = 1, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()

png(file.path(figdir,"Fig5_WaterContent_3500Kpa.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = rev(wc_ramp(11))[col_wc3500],
               cex = 1, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()

################################################################################
#  Step 4: Make figures of water content differences

summary(wc1500diff)
summary(wc3500diff)

# Make colors
wcdiff_ramp <- colorRampPalette(c("#f7fcb9","#d9f0a3","#addd8e","#78c679",
                                  "#41ab5d","#238443","#006837","#004529"))
col_diff1500 <- cut(wc1500diff, breaks = seq(0,0.3, by = 0.025))
col_diff3500 <- cut(wc3500diff, breaks = seq(0,0.3, by = 0.025))

# Now create figures of difference in water content between fc and wp
png(file.path(figdir,"Fig5_AvailableWaterContent_FC-1500Kpa.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = wcdiff_ramp(11)[col_diff1500],
               cex = 1, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()

png(file.path(figdir,"Fig5_AvailableWaterContent_FC-3500Kpa.png"), width = 6, height = 6, units = "in", res = 300)
par(mar = c(1,1,1,1))
geo <- TT.plot(class.sys = "none", cex.lab = 1.2, cex.axis = 1.2, tri.data = td, 
               pch = 16, col = wcdiff_ramp(11)[col_diff3500],
               cex = 1, main = "")
TT.classes(geo = geo, class.sys = "USDA.TT", lwd.axis = 2, class.lab.col = "black", 
           class.line.col = "black", cex.lab = 1)
dev.off()


# Now create a legend
# Set up legend polygon information
# Create axis locations for labels
axislabs <- seq(0,0.3,by=0.025)
# Set up axis label locations
axisat <- 0
for (ii in 2:(length(axislabs))){
  axisat <- c(axisat, (axisat[ii-1]+(10/length(levels(col_diff1500)))))
}

# Save legend
png(file.path(figdir,"Fig5_WaterContentdiff_Legend.png"), width = 6.5, height = 3, units = "in", res = 300)
par(mar = c(1,0,0,0), mgp = c(1,0.2,0))
plot(1:10,1:10, col = "white", bty = "n", xaxt = "n", yaxt = "n", 
     xlab = "", ylab = "", xlim = c(0,10), ylim = c(0,10))
for (xx in 1:length(levels(col_diff1500))){
  polygon(x = c(axisat[xx],axisat[xx],axisat[xx+1],axisat[xx+1]), y = c(1.73,2.73,2.73,1.73),
          border = (wcdiff_ramp(12))[xx], col = (wcdiff_ramp(12))[xx])
}
polygon(x = c(0,0,10,10), y = c(1.73,2.73,2.73,1.73), lwd = 1.5)
mtext(expression(paste("Available water (",plain("cm")^3,plain("cm")^-3,")")), side = 1, line = -1, cex = 1.2)
par(tcl = -0.3)
axis(side = 1, line = -2.7, at = seq(0,10, length.out = length(axislabs)),cex.axis = 0.9,
     labels = axislabs)
dev.off()
