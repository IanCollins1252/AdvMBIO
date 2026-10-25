# Name: Ian Collins
# JCUID: 14873467
# Date: 27/07/2026
# Last Modified: 27/07/2026
# Purpose: Calculate and analyze the diversity of the intertidal zones in Pioneer Bay on Orpheus Island

#Dependancies
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstatix)

#Read in csv
intertidal <- read_csv("~/School/MBIO/5000/FinalReport/data/OIRS_Intertidal_Diversity.csv")
#view(intertidal)

#Shannon's Diversity Indicy
sdi <- function(p) {
  return(p*log(p))
}

sdiAve <- function(taxa) {
  total <- 0
  s <- sum(taxa)
  for (t in taxa) {
    total <- total + sdi(t/s)
  }
  return(total*-1)
}

#Iterate over each zone and replicate to calc SDI
col <- c("zone", "total", "phylum", "order", "class", "family", "geneus")
class_sdi <- data.frame(matrix(nrow = 0, ncol = length(col)))
zones = c("L", "ML", "MH", "H")
reps = c(1, 2, 3, 4, 5)
t = 1
s = 0 #Sample size
#Iter Zones
for (z in zones) {
  #Iter Reps
  for (r in reps) {
    if (z == "H" & (r == 1 | r == 2)) next
    #Record
    s = intertidal$Total[t]
    t = t + s
    rep_sdi <- c(z, s)
    #Set up row and subset
    sample <- intertidal %>%
      filter(Zone == z, Rep == r)
    #tables exclude NA values
    p <- as.numeric(unname(as.array(table(sample$Phylum))))
    o <- as.numeric(unname(as.array(table(sample$Order))))
    c <- as.numeric(unname(as.array(table(sample$Class))))
    f <- as.numeric(unname(as.array(table(sample$Family))))
    if (sum(is.na(sample[, 9]) == length(sample[, 9]))) {
      #Catch empty table error
      g <- 0
    } else {
      g <- as.numeric(unname(as.array(table(sample$Geneus))))
    }
    
    #Calc SDI for taxa 
    rep_sdi <- append(rep_sdi, sdiAve(p))
    rep_sdi <- append(rep_sdi, sdiAve(o))
    rep_sdi <- append(rep_sdi, sdiAve(c))
    rep_sdi <- append(rep_sdi, sdiAve(f))
    if (length(g) > 1) {
      rep_sdi <- append(rep_sdi, sdiAve(g))
    } else {
      rep_sdi <- append(rep_sdi, g)
    }
    #TODO: ave sdi per taxa class
    #Append to table
    class_sdi <- rbind(class_sdi, rep_sdi)
  }
}
t = t -1
colnames(class_sdi) <- col
class_sdi <- class_sdi %>%
  select(zone, total, phylum, order, class, family, geneus) %>%
  mutate(across(c(total, phylum, order, class, family, geneus), as.numeric))
write_csv(class_sdi, "~/School/MBIO/5000/FinalReport/data/class_sdi.csv", append = FALSE)
ave_sdi <- class_sdi %>%
  group_by(zone) %>%
  summarise(across(c(phylum, order, class, family, geneus), 
                   list(mean = mean, sd = sd))) %>%
  mutate(phylum_se = phylum_sd / sqrt(5),
         order_se = order_sd / sqrt(5),
         class_se = class_sd / sqrt(5),
         family_se = family_sd / sqrt(5),
         geneus_se = geneus_sd / sqrt(5)) %>%
  mutate(phylum_low = phylum_mean - phylum_se,
         phylum_high = phylum_mean + phylum_se,
         order_low = order_mean - order_se,
         order_high = order_mean + order_se,
         class_low = class_mean - class_se,
         class_high = class_mean + class_se,
         family_low = family_mean - family_se,
         family_high = family_mean + family_se,
         geneus_low = geneus_mean - geneus_se,
         geneus_high = geneus_mean + geneus_se)
write_csv(ave_sdi, "~/School/MBIO/5000/FinalReport/data/ave_sdi.csv")

#ANOVA mean
oneway.test(phylum ~ factor(zone), 
            data = class_sdi,
            var.equal = TRUE)
oneway.test(order ~ factor(zone), 
            data = class_sdi,
            var.equal = TRUE)
oneway.test(class ~ factor(zone), 
            data = class_sdi,
            var.equal = TRUE)
oneway.test(family ~ factor(zone), 
            data = class_sdi,
            var.equal = TRUE)
oneway.test(geneus ~ factor(zone), 
            data = class_sdi)
#Normality
class_sdi %>% select(zone, phylum) %>%
  ggplot(aes(x = phylum)) +
  geom_histogram()
ggsave("~/School/MBIO/5000/FinalReport/image/normal_phylum.jpg", width=4, height=4, dpi=300)
class_sdi %>% select(zone, order) %>%
  ggplot(aes(x = order)) +
  geom_histogram()
ggsave("~/School/MBIO/5000/FinalReport/image/normal_order.jpg", width=4, height=4, dpi=300)
class_sdi %>% select(zone, class) %>%
  ggplot(aes(x = class)) +
  geom_histogram()
ggsave("~/School/MBIO/5000/FinalReport/image/normal_class.jpg", width=4, height=4, dpi=300)
class_sdi %>% select(zone, family) %>%
  ggplot(aes(x = family)) +
  geom_histogram()
ggsave("~/School/MBIO/5000/FinalReport/image/normal_family.jpg", width=4, height=4, dpi=300)
class_sdi %>% select(zone, geneus) %>%
  ggplot(aes(x = geneus)) +
  geom_histogram()
ggsave("~/School/MBIO/5000/FinalReport/image/normal_geneus.jpg", width=4, height=4, dpi=300)
#Variance assumption test
vmax_p = ave_sdi %>% #Found VMAX. Realize she killed her sister. Spider-Man takes a bullet for her.
  select(zone, phylum_sd) %>%
  arrange(desc(phylum_sd)) %>%
  filter(zone != zone[1] & zone != zone[4])
class_sdi %>%
  select(zone, phylum) %>%
  filter(zone != vmax_p$zone[1] & zone != vmax_p$zone[2]) %>%
  levene_test(phylum ~ factor(zone))
vmax_o = ave_sdi %>% #Found VMAX. Realize she killed her sister. Spider-Man takes a bullet for her.
  select(zone, order_sd) %>%
  arrange(desc(order_sd)) %>%
  filter(zone != zone[1] & zone != zone[4])
class_sdi %>%
  select(zone, order) %>%
  filter(zone != vmax_o$zone[1] & zone != vmax_o$zone[2]) %>%
  levene_test(order ~ factor(zone))
vmax_c = ave_sdi %>% #Found VMAX. Realize she killed her sister. Spider-Man takes a bullet for her.
  select(zone, class_sd) %>%
  arrange(desc(class_sd)) %>%
  filter(zone != zone[1] & zone != zone[4])
class_sdi %>%
  select(zone, class) %>%
  filter(zone != vmax_c$zone[1] & zone != vmax_c$zone[2]) %>%
  levene_test(class ~ factor(zone))
vmax_f = ave_sdi %>% #Found VMAX. Realize she killed her sister. Spider-Man takes a bullet for her.
  select(zone, family_sd) %>%
  arrange(desc(family_sd)) %>%
  filter(zone != zone[1] & zone != zone[4])
class_sdi %>%
  select(zone, family) %>%
  filter(zone != vmax_f$zone[1] & zone != vmax_f$zone[2]) %>%
  levene_test(family ~ factor(zone))
vmax_g = ave_sdi %>% #Found VMAX. Realize she killed her sister. Spider-Man takes a bullet for her.
  select(zone, geneus_sd) %>%
  arrange(desc(geneus_sd)) %>%
  filter(zone != zone[1] & zone != zone[4])
class_sdi %>%
  select(zone, geneus) %>%
  filter(zone != vmax_g$zone[1] & zone != vmax_g$zone[2]) %>%
  levene_test(geneus ~ factor(zone))
#
# NOTE: Only geneus has statistically significant different variance from the largest to the smallest variance
#

#bar graphs
bar_p = ggplot(aes(x = factor(zone, level = c('H', 'MH', 'ML', 'L'))), data = ave_sdi) +
  geom_col(aes(y = phylum_mean, fill = factor(zone, level = c('H', 'MH', 'ML', 'L')))) +
  geom_errorbar(aes(min = phylum_low, ymax = phylum_high), width = 0.5) +
  scale_fill_discrete("Zones", label = c('High', 'Mid High', 'Mid Low', 'Low')) +
  xlab("Intertidal Zones") +
  ylab("Mean SDI")
ggsave("~/School/MBIO/5000/FinalReport/image/meanSDIphylum.jpg", width=4, height=4, dpi=300)
bar_o = ggplot(aes(x = factor(zone, level = c('H', 'MH', 'ML', 'L'))), data = ave_sdi) +
  geom_col(aes(y = order_mean, fill = factor(zone, level = c('H', 'MH', 'ML', 'L')))) +
  geom_errorbar(aes(min = order_low, ymax = order_high), width = 0.5) +
  scale_fill_discrete("Zones", label = c('High', 'Mid High', 'Mid Low', 'Low')) +
  xlab("Intertidal Zones") +
  ylab("Mean SDI")
ggsave("~/School/MBIO/5000/FinalReport/image/meanSDIorder.jpg", width=4, height=4, dpi=300)
bar_c = ggplot(aes(x = factor(zone, level = c('H', 'MH', 'ML', 'L'))), data = ave_sdi) +
  geom_col(aes(y = class_mean, fill = factor(zone, level = c('H', 'MH', 'ML', 'L')))) +
  geom_errorbar(aes(min = class_low, ymax = class_high), width = 0.5) +
  scale_fill_discrete("Zones", label = c('High', 'Mid High', 'Mid Low', 'Low')) +
  xlab("Intertidal Zones") +
  ylab("Mean SDI")
ggsave("~/School/MBIO/5000/FinalReport/image/meanSDIclass.jpg", width=4, height=4, dpi=300)
bar_f = ggplot(aes(x = factor(zone, level = c('H', 'MH', 'ML', 'L'))), data = ave_sdi) +
  geom_col(aes(y = family_mean, fill = factor(zone, level = c('H', 'MH', 'ML', 'L')))) +
  geom_errorbar(aes(min = family_low, ymax = family_high), width = 0.5) +
  scale_fill_discrete("Zones", label = c('High', 'Mid High', 'Mid Low', 'Low')) +
  xlab("Intertidal Zones") +
  ylab("Mean SDI")
ggsave("~/School/MBIO/5000/FinalReport/image/meanSDIfamily.jpg", width=4, height=4, dpi=300)
bar_g = ggplot(aes(x = factor(zone, level = c('H', 'MH', 'ML', 'L'))), data = ave_sdi) +
  geom_col(aes(y = geneus_mean, fill = factor(zone, level = c('H', 'MH', 'ML', 'L')))) +
  geom_errorbar(aes(min = geneus_low, ymax = geneus_high), width = 0.5) +
  scale_fill_discrete("Zones", label = c('High', 'Mid High', 'Mid Low', 'Low')) +
  xlab("Intertidal Zones") +
  ylab("Mean SDI")
ggsave("~/School/MBIO/5000/FinalReport/image/meanSDIgeneus.jpg", width=4, height=4, dpi=300)
