################################################################################
# Purpose: Create citation network
# 
#
# Rachel R. Renne
# August 14, 2025
################################################################################

library(tidyverse)
library(igraph)
library(readxl)

# Set up folder
datdir <- "01_Data"
figdir <- "03_Figures"

################################################################################
# Step 1: Bring in data

# Read in Combined NoyMeir and Sala et al. citations
rg <- read.csv(file.path(datdir,"CombinedZoteroExport_Citing_NoyMeir1973_Salaetal1988_final.csv"))
dim(rg)

# Get author lists
authorlist <- rg$Author[!is.na(rg$Supports.ITE)]

# Go through and get individual authors
authors <- vector()
authorslisted <- list()
for (i in 1:length(authorlist)){
  # Get each row of authors and split them up
  theseauthors <- authorlist[i]
  theseauthors <- strsplit(theseauthors, "; ")[[1]]
  thislist <- strsplit(theseauthors,", ")
  # Get just last name and initials
  authorlisted1 <- vector()
  for (ii in 1:length(thislist)){
    thisauthor <- thislist[[ii]]
    thisauthor <- paste0(thisauthor[1], ", ",substr(thisauthor[2], 1, 1))
    authors <- append(authors, thisauthor)
    authorlisted1 <- append(authorlisted1,thisauthor)
  }
  # Add another list item to authorslisted
  authorslisted[[i]] <- authorlisted1
}

# Collect unique authors
allAuthors <- unique(authors)
length(allAuthors)
# [1] 381, no 217?
# Sort authors alphabetically
allAuthors <- sort(allAuthors)

# Create author table
authortable <- data.frame(table(authors))

# The following code is from: "https://eiko-fried.com/create-your-collaborator-network-in-r/"

# Create list 
SymXDis <- do.call(cbind,lapply(authorslisted,function(x){1*(allAuthors %in% x)}))

# Make adjacency:
adj <- SymXDis %*% t(SymXDis)

# Labels:
labs <- gsub(",.*","",allAuthors)

# Set up size vector based on number of citations per author
authorsize <- authortable$Freq

# List of authors with > 2 citations investigating ITE
invauth <- c("Lauenroth, W", "Burke, I", "Rodriguez-Iturbe, I","Belnap, J",
             "Fernanzdez-Illescas, C", "Laio, F", "Porporato, A", "Sala, O",
             "Williams, D") 

# Set up limited adjacency matrix removing authors with < 2 citations
adjgt2 <- adj[-which(authorsize < 2),-which(authorsize < 2)]

# Make figure of citation network
png(file.path(figdir,"CitationNetwork.png"),
    width = 12, height = 12, units = "in", res = 300)
qgraph(adjgt2, labels = labs[-which(authorsize < 2)], 
       color = c("#eeeeee","#92c5de")[(authortable$authors[-which(authorsize < 2)] %in% invauth)+1],  
       vsize = 6, diag = FALSE, shape="circle",
       layout = "spring", edge.color = "#666666", repulsion = 0.90, 
       border.width=2, border.color='#444444', label.color="#555555")
dev.off()