# Read in Zotero report from all sources found on Google Scholar on 10/5/2024
# That cite Noy Meir (1973) and include "inverse-texture" OR "inverse texture"
dat <- read.csv("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/ZoteroExport_Citing NoyMeir 1973 & Inverse texture.csv")

# Keep only relevant names
dat1 <- dat[,c(1:6,12)]

# Go ahead and make citations by year figure
yeartable <- data.frame(number = table(dat1$Publication.Year))
names(yeartable) <- c("year","count")
yeardf <- data.frame(year = 1973:2024)
# Merge to get all years
yeartable1 <- merge(yeardf, yeartable, by = "year", all = TRUE)
yeartable1[is.na(yeartable1$count),]$count <- 0

# Make figure of citation per year for Noy Meir 
png("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/sourcesciting_noymeir1973.png",
    width = 6, height = 4, units = "in", res = 300)
par(mar = c(2,2,2,1), mgp = c(1,0.1,0), tcl = 0.1)
plot(yeartable1$count~yeartable1$year, lwd = 2,
     type = "l",
     xlab = "Year",
     ylab = "Number of citations",
     main = "Inverse texture citations (Noy Meir 1973)")
abline(h=0)
abline(v=1988, lty = 2, col = "red")
text(x = 1981.3, y = 13, "Sala et al. (1988)")
dev.off()

# Get authors
authorlist <- dat1$Author

# Go through and separate out authors then only keep last names
authors <- vector()
for (i in 1:length(authorlist)){
  # Get each row of authors and split them up
  theseauthors <- authorlist[i]
  theseauthors <- strsplit(theseauthors, "; ")[[1]]
  thislist <- strsplit(theseauthors,", ")
  # Get just last name and initials
  for (ii in 1:length(thislist)){
    thisauthor <- thislist[[ii]]
    thisauthor <- paste0(thisauthor[1], ", ",substr(thisauthor[2], 1, 1))
    authors <- append(authors, thisauthor)
  }
}

# Now tabulate authors
atable <- data.frame(author = table(authors))

# Order descending
atable <- atable[order(atable$author.Freq, decreasing = TRUE),]

# Save file
png("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/authorsciting_noymeir1973.png",
    width = 6, height = 8, units = "in", res = 300)
par(mar = c(2,6,2,1), las = 1, mgp = c(1,0.1,0), tcl = 0.1)
barplot(rev(atable$author.Freq[atable$author.Freq > 2]) , horiz = TRUE,
        names.arg = rev(atable$author.authors[atable$author.Freq > 2]),
        xlim = c(0,12.5),
        xlab = "Number of publications",
        main = "Authors of ITE papers citing Noy Meir")
box()
dev.off()
