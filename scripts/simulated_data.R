#simulated data test

#load packages
library(vegan)
library(tidyverse)
library(dplyr)

#load data
inverts_simulated <- read_csv("C:/Users/sophi/OneDrive/Desktop/BIOL404 - invert counts - simulated_data.csv")

#calculate species richness using shannon diversity index

# Select only the species columns
species_data <- inverts_simulated[, 3:7]

# Calculate Shannon Diversity Index for each row
inverts_simulated$shannon_index <- diversity(species_data, index = "shannon")

inverts_simulated$species_richness <- specnumber(species_data)

#calculate Pielous J for species eveness
inverts_simulated$species_evenness <- inverts_simulated$shannon_index / log(inverts_simulated$species_richness)

#calculate species abundance
inverts_simulated$total_abundance <- rowSums(species_data)

#save as csv
write.csv(inverts_simulated, "./data_cleaned/inverts_simulated.csv")
                            
                            