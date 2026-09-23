############################
# Purpose: Use Saxton and Rawls (2006) to estimate saturated and unsaturated conductivity for any soil type
#
# Rachel R. Renne
# February 7, 2024
###################################

# Inputs: S = % sand, C = % clay (by weight)
S1 = c(.05,.10,.25,.50,.75)
C1 = c(.05,.10,.25,.50)
soil <- expand.grid(S1,C1)
soil$sum <- apply(soil, 1, sum)
soil <- soil[soil$sum < 1,]
S = soil$Var1
C = soil$Var2
OM = rep(0.005,length(S))

# Test against paper
S = 0.63
C = 0.10
OM = 0.015

# To plot Ks
S1 = seq(0.01,1,by=0.01)
C1 = seq(0.02,1,by=0.01)
soil <- expand.grid(S1,C1)
soil$sum <- apply(soil, 1, sum)
soil <- soil[soil$sum < 1,]
S = soil$Var1
C = soil$Var2
OM = rep(0.005,length(S))

# Calculate variables
theta_1500t = -0.024*S + 0.487*C + 0.006*OM +
              0.005*(S*OM) - 0.013*(C*OM) +
              0.068*(S*C) + 0.031

theta_1500 = theta_1500t + (0.14*theta_1500t - 0.02)

# Option to convert negatives to 0
theta_1500[theta_1500 < 0] <- 0

theta_33t = -0.251*S + 0.195*C + 0.011*OM +
            0.006*(S*OM) - 0.027*(C*OM) +
            0.452*(S*C) + 0.299 
  
theta_33 = theta_33t + (1.283*(theta_33t^2) - 0.374*(theta_33t) - 0.015)

theta_S_33t = 0.278*S + 0.034*C + 0.022*OM -
              0.018*(S*OM) - 0.027*(C*OM) -
              0.584*(S*C) + 0.078

theta_S_33 = theta_S_33t + (0.636*theta_S_33t - 0.107)

theta_S = theta_33 + theta_S_33 - 0.097*S + 0.043

B = (log(1500)-log(33))/(log(theta_33)-log(theta_1500))

lambda = 1/B


# Saturated conductivity (need to do in steps to deal with R NaN quirk)
a = (theta_S - theta_33)
b = 3 - lambda
Ks = 1930*(a^b)

# Unsaturated conductivity function
K_theta <- function(Ks, theta, theta_S, lambda){
  return(Ks*((theta/theta_S)^(3 + (2/lambda))))
}

log10(K_theta(Ks, theta = 45, theta_S, lambda))


cols = c(1,2,3,4,5,1,2,3,4,5,1,2,3,4,1,2,3)
ltys = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4)

plot(log10(K_theta(Ks[1], theta=seq(0.05,0.5,by=0.05), theta_S[1], lambda[1]))~seq(0.05,0.5,by=0.05),
     xlab = "Water content (%)", ylab = "Conductivity", type = "l", col = 1, ylim = c(-30,10),
     lty = 1, lwd = 2)
for (i in 2:nrow(soil)){
  lines(log10(K_theta(Ks[i], theta=seq(0.05,0.5,by=0.05), theta_S[i], lambda[i]))~seq(0.05,0.5,by=0.05),
        col = cols[i], lty = ltys[i], lwd = 2)
}
legend("bottom", legend = c("0.05 S","0.10 S","0.25 S","0.50 S","0.75 S"),
       col = c(1,2,3,4,5), lwd = 2, bty = "n", y.intersp = 0.7, cex = 0.7)
legend("topleft", legend = c("0.05 C","0.10 C","0.25 C","0.50 C"), lwd = 2,
       lty = c(1,2,3,4), bty = "n", y.intersp = 0.7, cex = 0.7)


######################### Compare saturated and unsaturated conductivity
library(Ternary)
library(viridis)
soil1 <- soil[,1:2]
soil1$silt <- 1-apply(soil1, 1, sum)
names(soil1)[1:2] <- c("sand","clay")

cols <- cut(Ks, breaks=c(0,0.003,1,2.5,5,10,25,50,75,100,150,200,253))

par(mar = c(1,1,1,1))
TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(soil1[,c(2,3,1)]), pch = 23, col = viridis(12)[cols],
              cex = 0.4, bg = viridis(12)[cols])
legend('topleft', legend = c("<0.003","0.003-1","1-2.5","2.5-5",
                             "5-10","10-25","25-50","50-75","75-100",
                             "100-150","150-200","200-253"), y.intersp = 0.7,
       col = viridis(12), pt.bg = viridis(12), cex = 0.7, pch = 23, bty = "n")

# Unsaturated conductivity at 20% moisture
K20 <- K_theta(Ks, theta = 0.2, theta_S, lambda)

col20 <- cut(K20, breaks = c(0,0.003,1,2.5,5,10,25,50,75,100,150,200,253))

TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(soil1[,c(2,3,1)]), pch = 23, col = viridis(12)[col20],
              cex = 0.4, bg = viridis(12)[col20])

# Unsaturated conductivity at 30% moisture
K30 <- K_theta(Ks, theta = 0.3, theta_S, lambda)

col30 <- cut(K30, breaks = c(0,0.003,1,2.5,5,10,25,50,75,100,150,200,253))

TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(soil1[,c(2,3,1)]), pch = 23, col = viridis(12)[col30],
              cex = 0.4, bg = viridis(12)[col30])

# Unsaturated conductivity at 40% moisture
K40 <- K_theta(Ks, theta = 0.4, theta_S, lambda)

col40 <- cut(K40, breaks = c(0,0.003,1,2.5,5,10,25,50,75,100,150,200,253))

TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(soil1[,c(2,3,1)]), pch = 23, col = viridis(12)[col40],
              cex = 0.4, bg = viridis(12)[col40])

# Unsaturated conductivity at 50% moisture
K50 <- K_theta(Ks, theta = 0.5, theta_S, lambda)

col50 <- cut(K50, breaks = c(0,0.003,1,2.5,5,10,25,50,75,100,150,200,10000))

TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(soil1[,c(2,3,1)]), pch = 23, col = viridis(12)[col50],
              cex = 0.4, bg = viridis(12)[col50])

# Unsaturated conductivity at wilting point
Kwilt <- K_theta(Ks, theta = theta_1500, theta_S, lambda)

colwilt <- cut(Kwilt, breaks = c(0,0.003,1,2.5,5,10,25,50,75,100,150,200,253))

TernaryPlot(alab = "Clay", blab = "Silt", clab = "Sand")
TernaryPoints(as.matrix(soil1[,c(2,3,1)]), pch = 23, col = viridis(12)[colwilt],
              cex = 0.4, bg = viridis(12)[colwilt])
