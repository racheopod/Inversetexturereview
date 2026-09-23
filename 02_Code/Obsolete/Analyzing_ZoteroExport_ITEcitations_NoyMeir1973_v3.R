# Read in Zotero report from all sources found on Google Scholar on 10/5/2024
# That cite Noy Meir (1973) and include "inverse-texture" OR "inverse texture"
# AND/OR that cite Sala et al. (1988) and include "inverse-texture" OR "inverse texture"
dat <- read.csv("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/Compiling Inverse Texture Sources/CombinedZoteroExport_Citing_NoyMeir1973_Salaetal1988_workingversion.csv")

# Remove theses that later got published
dat1 <- dat[!is.na(dat$Introduction.Review),c(1:20)]

# Go ahead and make citations by year figure
yeartable <- data.frame(number = table(dat1$Publication.Year))
names(yeartable) <- c("year","count")
yeardf <- data.frame(year = 1973:2024)
# Merge to get all years
yeartable1 <- merge(yeardf, yeartable, by = "year", all = TRUE)
yeartable1[is.na(yeartable1$count),]$count <- 0

# Make figure of citation per year for Noy Meir 
png("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/Compiling Inverse Texture Sources/sourcesciting_noymeir1973_v3.png",
    width = 6, height = 4, units = "in", res = 300)
par(mar = c(2,2,2,1), mgp = c(1,0.1,0), tcl = 0.1)
plot(yeartable1$count~yeartable1$year, lwd = 2,
     type = "l",
     xlab = "Year",
     ylab = "Number of citations",
     main = "Inverse texture citations (Noy Meir 1973 & Sala et al. 1988)")
abline(h=0)
abline(v=1988, lty = 2, col = "red")
text(x = 1981.3, y = 13, "Sala et al. (1988)")
mtext(paste0("n = ",nrow(dat1)), side = 3, line = -1)
dev.off()

# Now compile types of citations by year

# Now only get investigations
invest <- dat1[dat1$Investigation == 1,]
dim(invest)
# 31 studies directly investigated
# Make table of investigation year
invyear <- data.frame(investigation_year = table(invest$Publication.Year))

# Make a table of those that just mention it
mentionyear <- data.frame(mention_year = table(dat1[is.na(dat1$Support.) & dat1$Investigation != 1,]$Publication.Year))

verdict <- dat1[!is.na(dat1$Support.) & dat1$Investigation != 1,]
dim(verdict)
# 95 studies made a verdict (42% of total; i.e. 58% only mentioned in passing
table(verdict$Support.)
# 21 did not support (22%)
# 67 supported (71%)
# 7 had mixed results (7%)

# verdict year
verdictyear <- data.frame(verdictyear = table(verdict$Publication.Year))

# Merge investigation with yeardf
citetype <- merge(yeardf, invyear, by.x = "year", by.y = "investigation_year.Var1", all.x = TRUE)
names(citetype)[2] <- "investigation"
citetype[is.na(citetype$investigation),]$investigation <- 0

# add in verdict papers
citetype1 <- merge(citetype, verdictyear, by.x = "year", by.y = "verdictyear.Var1", all.x = TRUE)
names(citetype1)[3] <- "verdict"
citetype1[is.na(citetype1$verdict),]$verdict <- 0

# Add in mention papers
citetype2 <- merge(citetype1, mentionyear, by.x = "year", by.y = "mention_year.Var1", all.x = TRUE)
names(citetype2)[4] <- "mention"
citetype2[is.na(citetype2$mention),]$mention <- 0

# Make figure of citation per year for Noy Meir 
png("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/Compiling Inverse Texture Sources/citationtype_noymeir1973.png",
    width = 6, height = 4, units = "in", res = 300)
par(mar = c(2,2,2,1), mgp = c(1,0.1,0), tcl = 0.1)
plot(citetype2$investigation~citetype2$year, lwd = 2,
     type = "l", ylim = c(1,15), col = "white",
     xlab = "Year",
     ylab = "Number of citations",
     main = "Inverse texture citations (Noy Meir 1973 & Sala et al. 1988)")
abline(v=1988, lty = 2, col = "red")
text(x = 1981.3, y = 13, "Sala et al. (1988)")
for (i in 1:nrow(citetype2)){
  if (citetype2$investigation[i] > 0){
    points(x = rep(citetype2$year[i],citetype2$investigation[i]), y = 1:citetype2$investigation[i], lwd = 2,
           col = "red", pch = 16)
  }
  if (citetype2$verdict[i] > 0){
    points(x = rep(citetype2$year[i],citetype2$verdict[i]), y = (citetype2$investigation[i] + 1):(citetype2$investigation[i] + citetype2$verdict[i]), lwd = 2,
           col = "black", pch = 16)
  }
  if (citetype2$mention[i] > 0){
    points(x = rep(citetype2$year[i],citetype2$mention[i]), y = (citetype2$verdict[i] + citetype2$investigation[i] + 1):(citetype2$verdict[i] + citetype2$investigation[i] + citetype2$mention[i]), lwd = 2,
           col = "grey", pch = 16)
  }
  
}
legend("top", legend = c(paste0("investigation (",sum(citetype2$investigation),")"),
                         paste0("cast vote (",sum(citetype2$verdict),")"),
                         paste0("mention only (",sum(citetype2$mention),")")),
       bty = "n", pch = 16, col = c("red","black","grey"))

dev.off()

# Get authors (from investigations only)
authorlist <- invest$Author

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
png("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/Compiling Inverse Texture Sources/authorsinvestigating_ITE.png",
    width = 6, height = 7, units = "in", res = 300)
par(mar = c(2,6,2,1), las = 1, mgp = c(1,0.1,0), tcl = 0.1)
barplot(rev(atable$author.Freq[atable$author.Freq > 1]) , horiz = TRUE,
        names.arg = rev(atable$author.authors[atable$author.Freq > 1]),
        xlim = c(0,6.5), cex.names = 0.7,
        xlab = "Number of publications",
        main = "Authors of papers investigating ITE")
box()
dev.off()


# Get authors (from verdict & investigations only)
authorlist <- dat1[!is.na(dat1$Support.),]$Author

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
png("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/Compiling Inverse Texture Sources/authors_with_substantialITEreferences.png",
    width = 6, height = 7, units = "in", res = 300)
par(mar = c(2,6,2,1), las = 1, mgp = c(1,0.1,0), tcl = 0.1)
barplot(rev(atable$author.Freq[atable$author.Freq > 1]) , horiz = TRUE,
        names.arg = rev(atable$author.authors[atable$author.Freq > 1]),
        xlim = c(0,9.5), cex.names = 0.7,
        xlab = "Number of publications",
        main = "Authors with substantial ITE reference")
box()
dev.off()


# Now make a new year table that accounts for citations of Noy Meir vs Sala et al
dat1[!is.na(dat1$Introduction.Review),]$citing <- dat1$Noymeir+dat1$Salaetal

cite_both <- data.frame(table(dat1[dat1$citing == 2, ]$Publication.Year))
names(cite_both) <- c("year","both")
noymeir_only <- data.frame(table(dat1[dat1$Noymeir == 1 & dat1$citing == 1, ]$Publication.Year))
names(noymeir_only) <- c("year","noymeir")
salaetal_only <- data.frame(table(dat1[dat1$Salaetal == 1 & dat1$citing == 1, ]$Publication.Year))
names(salaetal_only) <- c("year","salaetal")

years <- merge(cite_both, noymeir_only, all = TRUE)
years1 <- merge(years, salaetal_only, all = TRUE)
years2 <- merge(yeardf, years1, by = "year", all = TRUE)


# Make a figure showing who is cited
png("C:/Users/rache/Dropbox/Doctoral_projects/Projects/Inverse_texture/Compiling Inverse Texture Sources/sourcesciting_noymeir1973&Salaetal1988.png",
    width = 8, height = 4, units = "in", res = 300)
par(mar = c(2,2,2,1), mgp = c(1,0.1,0), tcl = 0.1)
plot(1:15~seq(1973,2024,length.out = 15),col = "white", xlim = c(1973,2024),
     ylim = c(0,15.5), xlab = "Year", ylab = "Number of publications",
     main = "Inverse texture citations")
for (i in 1:nrow(years2)){
  thisyear <- years2[i,]
  if (!is.na(thisyear$both)|!is.na(thisyear$noymeir)|!is.na(thisyear$salaetal)){
  points(x = rep(thisyear$year,sum(thisyear[,2:4], na.rm = TRUE)), y = 1:(sum(thisyear[,2:4], na.rm = TRUE)),
         pch = 16, cex = 1.7,
         col = c(rep("#de2d26",max(c(thisyear$noymeir,0), na.rm=TRUE)),
                 rep("black",max(c(thisyear$both,0), na.rm=TRUE)),
                 rep("#2b8cbe",max(c(thisyear$salaetal,0), na.rm=TRUE))))
  }
}
abline(h=0)
legend("topleft", legend = c("Noy Meir 1973","Both","Sala et al. 1988"), pt.cex = 1.7,
       pch = 16, col = c("#de2d26","black","#2b8cbe"), bty = "n")
dev.off()


par(mgp=c(0.5,0.1,0))
curve(0.2*x, from = 0, to = 1000, ylim = c(0,900), col = "blue", lwd = 2,
      xlab = "Precipitation", ylab = "Proportion of precipitation", 
      yaxt = "n",xaxt = "n") # FINE
curve((200 + -0.2*x), 0,1000, add = T, col = "red", lwd = 2) # FINE

curve((x - (200 + (0)*x)), 200,1000,add = T, lwd = 2) # FINE

curve((-5 + 0.3*x), 0,1000,add = T, lty = 2, col = "blue", lwd = 2)
curve((100-0.1*x), 0 ,1000, add = T, lty = 2, col = "red", lwd = 2)

curve((x - (-5+100 + (0.3-0.1)*x)), 120,1000, lty = 2, add = TRUE, lwd = 2)
abline(h=0)
