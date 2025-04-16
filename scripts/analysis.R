#simulated data test

#load packages
library(vegan)
library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(car)
library(MASS)
# "install.packages("glmmTMB")"
library(glmmTMB)

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
         species_evenness = ifelse(species_richness == 1, 
                                    1, 
                                    shannon_index / log(species_richness)), #where species richness is = 1, make eveness also = 1
         total_abundance=rowSums(species_data)
         )


#save as csv
write.csv(inverts, "./data_cleaned/inverts_cleaned.csv")

# ===Dummy R sript analysis

inverts$species_richness <- as.numeric(inverts$species_richness)

hist(inverts$species_richness)  #Roughly poisson
mean(inverts$species_richness)/var(inverts$species_richness)  #2.605351 slightly underdispersed
hist(inverts$species_evenness)  #Very left skewed
hist(inverts$total_abundance)    #roughly poisson
mean(inverts$total_abundance)/var(inverts$total_abundance)   #0.3545879, Overdispersed

#Test distributions of distance and density

hist(inverts$d_from_path_m)
hist(inverts$density)

#Test for correlation (Pearson's product-moment correlation)

cor.test(inverts$d_from_path_m, inverts$density)

richness_model <- glmmTMB(species_richness ~ d_from_path_m + (1|transect),family=nbinom2,data = inverts)  #Must be glmm() to account for random effect

summary(richness_model)

richness.dispersion.ratio <- 58.5/16

inverts <- inverts %>% 
  mutate(corrected_species_richness=round(species_richness/richness.dispersion.ratio, digits = 2)*100)

richness_model <- glmmTMB(corrected_species_richness ~ d_from_path_m + (1|transect),family=nbinom2,data = inverts)  #Uncorrected is better

richness_model <- glmmTMB(species_richness ~ d_from_path_m + (1|transect),family=nbinom2,data = inverts)  #Must be glmm() to account for random effect

residuals_richness <- residuals(richness_model)
qqnorm(residuals_richness)
qqline(residuals_richness)
hist(residuals_richness)

summary(richness_model)
anova(richness_model)
Anova(richness_model)

evenness_model <- glmmTMB(species_evenness ~ d_from_path_m + (1|transect),family = binomial, data = inverts)   #will probably use a different link function. Beta distribution did not work, as y <= 1

residuals_evenness <- residuals(evenness_model)
qqnorm(residuals_evenness)
qqline(residuals_evenness)

hist(residuals_evenness)

summary(evenness_model)
anova(evenness_model)
Anova(evenness_model)

abundance_model <- glmmTMB(total_abundance ~ d_from_path_m + (1|transect), family=nbinom2, data = inverts)   #Negative binomial is slightly better than poisson

summary(abundance_model)

abundance.dispersion.ratio <- 99/16

inverts <- inverts %>% 
  mutate(corrected_abundance=round(total_abundance/abundance.dispersion.ratio, digits = 2)*100)

abundance_model <- glmmTMB(corrected_abundance ~ d_from_path_m + (1|transect), family=nbinom2, data = inverts)   #Slightly worse than the regular model without the transformed Y variable.

residuals_abundance <- residuals(abundance_model)
qqnorm(residuals_abundance)
qqline(residuals_abundance)
hist(residuals_abundance)

summary(abundance_model)
anova(abundance_model)
Anova(abundance_model)

abundance_model <- glmmTMB(total_abundance ~ d_from_path_m + (1|transect), family=nbinom2, data = inverts)   #Negative binomial is slightly better than poisson

residuals_abundance <- residuals(abundance_model)
qqnorm(residuals_abundance)
qqline(residuals_abundance)
hist(residuals_abundance)

summary(abundance_model)
anova(abundance_model)
Anova(abundance_model)

# ===Density

richness_model <- glmmTMB(species_richness ~ density + (1|transect),family=nbinom2,data = inverts)  #Must be glmm() to account for random effect

summary(richness_model)

richness.dispersion.ratio <- 58.5/16

inverts <- inverts %>% 
  mutate(corrected_species_richness=round(species_richness/richness.dispersion.ratio, digits = 2)*100)

richness_model <- glmmTMB(corrected_species_richness ~ density + (1|transect),family=nbinom2,data = inverts)  #Uncorrected is better

richness_model <- glmmTMB(species_richness ~ density + (1|transect),family=nbinom2,data = inverts)  #Must be glmm() to account for random effect

residuals_richness <- residuals(richness_model)
qqnorm(residuals_richness)
qqline(residuals_richness)
hist(residuals_richness)

summary(richness_model)
anova(richness_model)
Anova(richness_model)

evenness_model <- glmmTMB(species_evenness ~ density + (1|transect),family = binomial, data = inverts)   #will probably use a different link function. Beta distribution did not work, as y <= 1

residuals_evenness <- residuals(evenness_model)
qqnorm(residuals_evenness)
qqline(residuals_evenness)

hist(residuals_evenness)

summary(evenness_model)
anova(evenness_model)
Anova(evenness_model)

abundance_model <- glmmTMB(total_abundance ~ density + (1|transect), family=nbinom2, data = inverts)   #Negative binomial is slightly better than poisson

summary(abundance_model)

abundance.dispersion.ratio <- 97.7/16

inverts <- inverts %>% 
  mutate(corrected_abundance=round(total_abundance/abundance.dispersion.ratio, digits = 2)*100)

abundance_model <- glmmTMB(corrected_abundance ~ density + (1|transect), family=nbinom2, data = inverts)   #Uncorrected is better

abundance_model <- glmmTMB(total_abundance ~ density + (1|transect), family=nbinom2, data = inverts)   #Negative binomial is slightly better than poisson

residuals_abundance <- residuals(abundance_model)
qqnorm(residuals_abundance)
qqline(residuals_abundance)
hist(residuals_abundance)

summary(abundance_model)
anova(abundance_model)
Anova(abundance_model)
