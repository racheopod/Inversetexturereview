# Now those that cite Sala et al. (1988) and include "inverse-texture" OR "inverse texture"
sal1 <- read.csv("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/ZoteroExport_Citing Sala et al. 1988 & Inverse texture_v2.csv")
sal2 <- read.csv("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/ZoteroExport_Citing Sala et al. 1988 & Inverse texture_edited.csv")

# Par down to relevant data
sal1a <- sal1[,c(2:6,9,16,18,19)]
sal2a <- sal2[,c(2:6,9,16,18,19)]

# Check that names match
table(names(sal1a) == names(sal2a))

# Make relevant source column
sal1a$Salaetal_v1 <- 1
sal2a$Salaetal_v2 <- 1

# Combine:
sal3 <- merge(sal1a, sal2a, all = TRUE)

# Fix NAs
sal3[is.na(sal3$Salaetal_v1),]$Salaetal_v1 <- 0
sal3[is.na(sal3$Salaetal_v2),]$Salaetal_v2 <- 0

# Order by year and author
sal3 <- sal3[order(sal3$Publication.Year, sal3$Author),]

# Save to file
write.csv(sal3, "C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/Compare_Salaetal1988_Citations.csv",
          row.names = FALSE)
