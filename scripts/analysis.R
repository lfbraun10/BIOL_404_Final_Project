#simulated data test

#load packages
library(vegan)
library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(car)

#load data
inverts_simulated <- read_csv("./data_raw/BIOL404 - invert counts - simulated_data.csv")

#calculate species richness using shannon diversity index

# Select only the species columns

species_data <- dplyr::select(inverts_simulated,"Mesostigmatids":"Other arachnids")

# Calculate Shannon Diversity Index for each row
inverts_simulated <- inverts_simulated %>% 
  mutate(shannon_index=diversity(species_data, index = "shannon"),
         species_richness=specnumber(species_data),
         species_evenness=(shannon_index/log(species_richness)),
         total_abundance=rowSums(species_data)
         )

#save as csv
write.csv(inverts_simulated, "./data_cleaned/inverts_simulated.csv")

# ===Dummy R sript analysis

hist(inverts_simulated$species_richness)  #These will probably follow a poisson distrubution because they are based count data
hist(inverts_simulated$species_evenness)  #Don't know what this will look like yet
hist(inverts_simulated$total_abundance)

richness_model <- glm(species_richness ~ d_from_path_m + (1|Transect),family = poisson, data = inverts_simulated)

residuals_richness <- residuals(richness_model)
qqnorm(residuals_richness)
qqline(residuals_richness)
hist(residuals_richness)

summary(richness_model)
anova(richness_model)
Anova(richness_model)

evenness_model <- glm(species_evenness ~ d_from_path_m + (1|Transect),family = poisson, data = inverts_simulated)   #will probably use a different link function

residuals_evenness <- residuals(evenness_model)
qqnorm(residuals_evenness)
qqline(residuals_evenness)
hist(residuals_evenness)

summary(evenness_model)
anova(evenness_model)
Anova(evenness_model)

abundance_model <- glm(total_abundance ~ d_from_path_m + (1|Transect),family = poisson, data = inverts_simulated)

residuals_abundance <- residuals(abundance_model)
qqnorm(residuals_abundance)
qqline(residuals_abundance)
hist(residuals_abundance)

summary(abundance_model)
anova(abundance_model)
Anova(abundance_model)

