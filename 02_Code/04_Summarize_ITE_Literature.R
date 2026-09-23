################################################################################
# Purpose: Summarize inverse texture literature
# 
#
# Rachel R. Renne
# August 13, 2025
################################################################################

# Load relevant libraries
library(tidyr)

# Set up folder
datdir <- "01_Data"
figdir <- "03_Figures"

################################################################################
# Step 1: Bring in data

# Read in Combined NoyMeir and Sala et al. citations
dat1 <- read.csv(file.path(datdir,"CombinedZoteroExport_Citing_NoyMeir1973_Salaetal1988_final.csv"))
dim(dat1)

# Count final number of citations for each source
sum(dat1$Noymeir)
# [1] 198
sum(dat1$Salaetal)
# [1] 105
sum(dat1$Noymeir == 1 & dat1$Salaetal == 1)
# [1] 82
sum(dat1$Noymeir == 1 & dat1$Salaetal == 0)
# [1] 116
sum(dat1$Noymeir == 0 & dat1$Salaetal == 1)
# [1] 23

# Summarize number of citations citing ITE in different parts of paper
mean(dat1$IntroductionOrReview)*100
# 55%
mean(dat1$Methods)*100
# 4%
mean(dat1$Results)*100
# 5%
mean(dat1$Discussion)*100
# 66%
mean((dat1$Discussion + dat1$IntroductionOrReview > 0) & is.na(dat1$Supports.ITE))*100
# 57% 
mean(is.na(dat1$Supports.ITE))*100
# 60%
table(dat1$Supports.ITE)/sum(!is.na(dat1$Supports.ITE))*100
#    Mixed       No      Yes 
# 19.10112 21.34831 59.55056 

table(dat1$Investigation)
#  No Yes 
# 191  30
sum(!is.na(dat1$Supports.ITE))
# [1] 89

###############################################################################
# Step 2: Compile types of citations by year

# Count citations/year
yeartable <- data.frame(number = table(dat1$Year))
names(yeartable) <- c("year","count")
yeardf <- data.frame(year = 1973:2024)
# Merge to get all years
yeartable1 <- merge(yeardf, yeartable, by = "year", all = TRUE)
yeartable1[is.na(yeartable1$count),]$count <- 0

# Now only get investigations
invest <- dat1[dat1$Investigation == "Yes",]
# Make table of investigation year
invyear <- data.frame(investigation_year = table(invest$Year))
# Make table of support for ITE/year
invyearsupport <- data.frame(table(invest$Year, invest$Supports.ITE))
# Make wider
iys <- pivot_wider(invyearsupport, id_cols = Var1, names_from = Var2, 
                   values_from = Freq, values_fill = 0)
names(iys) <- c("Year","Mixed.invest","No.invest","Yes.invest")
# Merge wth invyear
invyear1 <- merge(invyear, iys, by.x = "investigation_year.Var1", by.y = "Year", all = TRUE)

# Make a table of those that just mention it
mentionyear <- data.frame(mention_year = table(dat1[is.na(dat1$Supports.ITE) & dat1$Investigation != "Yes",]$Year))

# Get just those that cast a vote
verdict <- dat1[!is.na(dat1$Supports.ITE) & dat1$Investigation != "Yes",]
# verdict year
verdictyear <- data.frame(verdictyear = table(verdict$Year))
# Make table of support for ITE/year
verdictyearsupport <- data.frame(table(verdict$Year, verdict$Supports.ITE))
# Make wider
vys <- pivot_wider(verdictyearsupport, id_cols = Var1, names_from = Var2, 
                   values_from = Freq, values_fill = 0)
names(vys) <- c("Year","Mixed.verdict","No.verdict","Yes.verdict")
# Merge wth verdictyear
verdictyear1 <- merge(verdictyear, vys, by.x = "verdictyear.Var1", by.y = "Year", all = TRUE)

# Merge investigation with yeardf
citetype <- merge(yeardf, invyear1, by.x = "year", by.y = "investigation_year.Var1", all.x = TRUE)
names(citetype)[2] <- "investigation"
citetype[is.na(citetype$investigation),2:5] <- 0

# add in verdict papers
citetype1 <- merge(citetype, verdictyear1, by.x = "year", by.y = "verdictyear.Var1", all.x = TRUE)
names(citetype1)[6] <- "verdict"
citetype1[is.na(citetype1$verdict),6:9] <- 0

# Add in mention papers
citetype2 <- merge(citetype1, mentionyear, by.x = "year", by.y = "mention_year.Var1", all.x = TRUE)
names(citetype2)[10] <- "mention"
citetype2[is.na(citetype2$mention),]$mention <- 0
# Check number of papers
sum(citetype2[,c(3:5,7:10)])
# [1] 221 Looks good!

# Make figure of citation per year for Noy Meir 
png(file.path(figdir,"Fig7_citationtype_noymeir1973&Salaetal1988.png"),
    width = 9, height = 4, units = "in", res = 300)
par(mar = c(2,2,1,1), mgp = c(1,0.1,0), tcl = 0.1)
plot(citetype2$investigation~citetype2$year, lwd = 2,
     type = "l", ylim = c(1,16), xlim = c(1970,2024), col = "white",
     xlab = "Year",
     ylab = "Number of citations",
     main = "")
points(x = 1973, y = 1, col = "#2166ac", pch = 16, cex = 1.8)
#abline(v=1988, lty = 2, col = "red")
#text(x = 1981.3, y = 13, "Sala et al. (1988)")
for (i in 1:nrow(citetype2)){
  if (citetype2$investigation[i] > 0){
      points(x = rep(citetype2$year[i],citetype2$investigation[i]), 
             y = 1:citetype2$investigation[i], 
             cex = 1.8,
             col = rgb(0.7,0.1,0.2,1), pch = 16)
  }
  if (citetype2$verdict[i] > 0){
      points(x = rep(citetype2$year[i],citetype2$verdict[i]), 
             y = (citetype2$investigation[i]+1):(citetype2$investigation[i]+citetype2$verdict[i]), 
             cex = 1.8,
             col = "black", pch = 16)
  }
  if ((citetype2$Yes.invest[i] > 0) | (citetype2$Yes.verdict[i] > 0)){
    text(x = citetype2$year[i], y = 16, labels = citetype2$Yes.invest[i]+citetype2$Yes.verdict[i])
  }
  if (citetype2$mention[i] > 0){
    points(x = rep(citetype2$year[i],citetype2$mention[i]), 
           y = (citetype2$verdict[i] + citetype2$investigation[i] + 1):(citetype2$verdict[i] + citetype2$investigation[i] + citetype2$mention[i]), 
           cex = 1.8,
           col = "grey", pch = 16)
  }
}
abline(h=15.5, lty = 3)
legend(x = 1971, y = 15, legend = c(paste0("investigation (",sum(citetype2$investigation),")"),
                         paste0("cast vote (",sum(citetype2$verdict),")"),
                         paste0("mention only (",sum(citetype2$mention),")"),
                         "Noy-Meir (1973)"),
       bty = "n", pch = 16, col = c(rgb(0.7,0.1,0.2,1),"black","grey","#2166ac"), pt.cex = 1.8)
dev.off()

###############################################################################
# Step 4: Look at citing authors

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
png(file.path(figdir,"authorsinvestigating_ITE.png"),
    width = 6, height = 7, units = "in", res = 300)
par(mar = c(2,6,2,1), las = 1, mgp = c(1,0.1,0), tcl = 0.1)
barplot(rev(atable$author.Freq[atable$author.Freq > 1]) , horiz = TRUE,
        names.arg = rev(atable$author.authors[atable$author.Freq > 1]),
        xlim = c(0,6.5), cex.names = 0.7,
        xlab = "Number of publications",
        main = "Authors investigating the ITE")
box()
dev.off()


# Get authors (from verdict & investigations only)
authorlist <- dat1[!is.na(dat1$Supports.ITE),]$Author

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
png(file.path(figdir,"authors_casting_ITE_votes.png"),
    width = 6, height = 7, units = "in", res = 300)
par(mar = c(2,6,2,1), las = 1, mgp = c(1,0.1,0), tcl = 0.1)
barplot(rev(atable$author.Freq[atable$author.Freq > 1]) , horiz = TRUE,
        names.arg = rev(atable$author.authors[atable$author.Freq > 1]),
        xlim = c(0,9.5), cex.names = 0.7,
        xlab = "Number of publications",
        main = "Authors with substantial ITE reference")
box()
dev.off()

###############################################################################
# Step 5: Summarize other metadata

# Look at this woody plant = ITE idea
table(dat1$Continent, dat1$ITE...Woody.plants)

round(table(dat1$ITE...Woody.plants)/nrow(dat1)*100)

# Look at geographic distribution of sources
table(dat1$Continent)

# Look at types of studies
table(dat1$Type)
