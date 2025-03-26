#simulated data test

#load packages
library(vegan)
library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(car)
library(MASS)

diameter_core <- 8.6  #cm

#load data
inverts <- read.csv("./data_raw/BIOL404_invert_counts_real_data.csv",stringsAsFactors = TRUE)

#calculate species richness using shannon diversity index

# Select only the species columns

species_data <- dplyr::select(inverts,"Oribatids":"other_arthropods")

# Calculate Shannon Diversity Index for each row
inverts <- inverts %>% 
  mutate(density=Soil.dry.weight....2.5g.for.plastic.bag./(pi*depth.of.sample..cm.*(diameter_core/2)^2),
         shannon_index=diversity(species_data, index = "shannon"),
         species_richness=specnumber(species_data),
         species_evenness=(shannon_index/log(species_richness)),
         total_abundance=rowSums(species_data)
         )

#save as csv
write.csv(inverts, "./data_cleaned/inverts_cleaned.csv")

# ===Dummy R sript analysis

hist(inverts$species_richness)  #Roughly poisson
mean(inverts$species_richness)/var(inverts$species_richness)  #2.605351 slightly underdispersed
hist(inverts$species_evenness)  #Very left skewed
hist(inverts$total_abundance)    #roughly poisson
mean(inverts$total_abundance)/var(inverts$total_abundance)   #0.3545879, Overdispersed

richness_model <- glm.nb(species_richness ~ d_from_path_m + (1|transect), data = inverts)

residuals_richness <- residuals(richness_model)
qqnorm(residuals_richness)
qqline(residuals_richness)
hist(residuals_richness)

summary(richness_model)
anova(richness_model)
Anova(richness_model)

evenness_model <- glm(species_evenness ~ d_from_path_m + (1|transect),family = poisson, data = inverts)   #will probably use a different link function

residuals_evenness <- residuals(evenness_model)
qqnorm(residuals_evenness)
qqline(residuals_evenness)
hist(residuals_evenness)

summary(evenness_model)
anova(evenness_model)
Anova(evenness_model)

abundance_model <- glm.nb(total_abundance ~ d_from_path_m + (1|transect), data = inverts)

residuals_abundance <- residuals(abundance_model)
qqnorm(residuals_abundance)
qqline(residuals_abundance)
hist(residuals_abundance)

summary(abundance_model)
anova(abundance_model)
Anova(abundance_model)

