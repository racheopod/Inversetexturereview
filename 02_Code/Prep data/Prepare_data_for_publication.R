################################################################################
# Purpose: Combine citation grids for final "dataset"
# 
#
# Rachel R. Renne
# August 13, 2025
# Revised: September 10, 2026
################################################################################

# Load relevant libraries
library(readxl)
library(tidyr)

# Set up folder
datdir <- "01_Data/Other"

################################################################################
# Step 1: Bring in data

# Read in Combined NoyMeir and Sala et al. citations
cits <- read.csv(file.path(datdir,"CombinedZoteroExport_Citing_NoyMeir1973_Salaetal1988_withnotes.csv"))
dim(cits)

# Count number of citations for each source
sum(cits$Noymeir)
# [1] 215
sum(cits$Salaetal)
# [1] 111
sum(cits$Noymeir == 1 & cits$Salaetal == 1)
# [1] 88
sum(cits$Noymeir == 1 & cits$Salaetal == 0)
# [1] 127
sum(cits$Noymeir == 0 & cits$Salaetal == 1)
# [1] 23

# Change binary to "Yes" and "No" in cits columns
cits[!is.na(cits$Support.) & cits$Support. == "0",]$Support. <- "No"
cits[!is.na(cits$Support.) & cits$Support. == "1",]$Support. <- "Yes"
cits[!is.na(cits$Investigation) & cits$Investigation == "0",]$Investigation <- "No"
cits[!is.na(cits$Investigation) & cits$Investigation == "1",]$Investigation <- "Yes"

# Read in Review Grid
rg <- read_xlsx(file.path(datdir,"ReviewGrid_20250721.xlsx"), sheet = "All papers")

################################################################################
# Step 2: Combine information from cits and rg

names(cits)
names(rg)

# Merge on Year, Author names
dat1 <- merge(cits[,c(2:6,11:18)], rg[,c(1,2,5:12)], by.x = c("Publication.Year","Author"),
              by.y = c("Year","Author"), all.x = TRUE)
# Fix one case where two papers have same year and author, but one isn't an
# investigation and the other is.
dat1[dat1$Publication.Year == 2010 & dat1$Author == "Kochendorfer, J. P.; Ramirez, J. A." & is.na(dat1$Support.),c(14:21)] <- NA

# Check for agreement with Support column
table(dat1$Support. == dat1$`Supports ITE`)
# FALSE  TRUE 
# 14    73 
dat1[!is.na(dat1$`Supports ITE`) & !is.na(dat1$Support.) & dat1$`Supports ITE` != dat1$Support.,c(9,18)]

table(dat1$Investigation.x == dat1$Investigation.y)
# FALSE  TRUE 
# 3    84 
dat1[!is.na(dat1$Investigation.y) & !is.na(dat1$Investigation.x) & dat1$Investigation.y != dat1$Investigation.x,c(11,10,19)]

# Replace Support and Investigation columns from cits with rg
dat1[!is.na(dat1$`Supports ITE`) & !is.na(dat1$Support.) & dat1$`Supports ITE` != dat1$Support.,]$Support. <- dat1[!is.na(dat1$`Supports ITE`) & !is.na(dat1$Support.) & dat1$`Supports ITE` != dat1$Support.,]$`Supports ITE`
dat1[!is.na(dat1$Investigation.y) & !is.na(dat1$Investigation.x) & dat1$Investigation.y != dat1$Investigation.x,]$Investigation.x <- dat1[!is.na(dat1$Investigation.y) & !is.na(dat1$Investigation.x) & dat1$Investigation.y != dat1$Investigation.x,]$Investigation.y 

# Now keep only relevant columns in dat and rearrange
dat <- dat1[,c("Publication.Year","Author","Title","Publication.Title","DOI","Noymeir","Salaetal",
               "Introduction.Review","Methods","Results","Discussion",
               "Continent","Approach","Plant types","Type","ITE Results",
               "ITE = Woody plants","Support.","Investigation.x")]
# Rename
names(dat) <- c("Year","Author","Title","Publication","DOI","Noymeir","Salaetal",
                "IntroductionOrReview","Methods","Results","Discussion",
                "Continent","Approach","Plant types","Type","ITE Results",
                "ITE = Woody plants","Supports ITE","Investigation")

# Remove theses that later got published
dat1 <- dat[!is.na(dat$IntroductionOrReview),]
dim(dat1)
# [1] 221  18

# Save to file
write.csv(dat1, file.path(datdir,"CombinedZoteroExport_Citing_NoyMeir1973_Salaetal1988_final.csv"),
          row.names = FALSE)

