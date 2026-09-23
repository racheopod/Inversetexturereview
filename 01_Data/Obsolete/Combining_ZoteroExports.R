# Read in Zotero report from all sources found on Google Scholar on 10/5/2024
# That cite Noy Meir (1973) and include "inverse-texture" OR "inverse texture"
dat <- read.csv("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/ZoteroExport_Citing NoyMeir 1973 & Inverse texture.csv")

# Par down to relevant data
datx <- dat[,c(2:6,9,16,18,19)]

# Now those that cite Sala et al. (1988) and include "inverse-texture" OR "inverse texture"
dat1 <- read.csv("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/ZoteroExport_Citing Sala et al. 1988 & Inverse texture.csv")

# Par down to relevant data
dat1x <- dat1[,c(2:6,9,16,18,19)]

# Check that names match
table(names(dat1x) == names(datx))

# Make relevant source column
dat1x$Salaetal <- 1
datx$Noymeir <- 1

# Combine:
dat2 <- merge(datx, dat1x, all = TRUE)

# Fix NAs
dat2[is.na(dat2$Salaetal),]$Salaetal <- 0
dat2[is.na(dat2$Noymeir),]$Noymeir <- 0

# Order by year and author
dat2 <- dat2[order(dat2$Publication.Year, dat2$Author),]

# Save to file
write.csv(dat2, "C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/CombinedZoteroExport_Citing_NoyMeir1973_Salaetal1988.csv",
          row.names = FALSE)
