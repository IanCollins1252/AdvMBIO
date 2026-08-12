# Name: Ian Collins
# JCUID: 14873467
# Date: 27/07/2026
# Last Modified: 27/07/2026
# Purpose: Calculate and analyze the diversity of the intertidal zones in Pioneer Bay on Orpheus Island

#Dependancies
library(tidyverse)
library(dplyr)

#Read in csv
intertidal <- read_csv("School/MBIO/5000/FinalReport/data/OIRS_Intertidal_Diversity.csv")
#view(intertidal)

#Shannon's Diversity Indicy
sdi <- function(p) {
  return(p*log(p))
}

#Sum all of the indicies
sum_sdi <- function(ps, total) {
  s <- 0
  for (p in ps) {
    s = sdi(p/total) + s
  }
  return(s)
}

#Iterate over each zone and replicate to create average and stddiv
ave_sdi <- data.frame(Zone = character(),
                      Rep = numeric(),
                      Phylum = numeric(),
                      Class = numeric(),
                      Order = numeric(),
                      Family = numeric(),
                      Geneus = numeric())
zones = c("L", "ML", "MH", "H")
reps = c(1, 2, 3, 4, 5)
for (z in zones) {
  for (r in reps) {
    rep_sdi <- c(z, r)
    sample <- intertidal %>%
      filter(Zone == z, Rep == r)
    #tables exclude NA values
    p <- stack(table(sample[,5]))
    c <- stack(table(sample[,6]))
    o <- stack(table(sample[,7]))
    f <- stack(table(sample[,8]))
    g <- stack(table(sample[,9]))
    
  }
}