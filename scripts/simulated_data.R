#simulated data test

#load packages
library(vegan)
library(tidyverse)
library(dplyr)

#load data
inverts_simulated <- read_csv("./data_raw/BIOL404 - invert counts - simulated_data.csv")

#calculate species richness using shannon diversity index

# Select only the species columns

species_data <- select(inverts_simulated,"Mesostigmatids":"Other arachnids")

# Calculate Shannon Diversity Index for each row
inverts_simulated <- inverts_simulated %>% 
  mutate(shannon_index=diversity(species_data, index = "shannon"),
         species_richness=specnumber(species_data),
         species_evenness=(shannon_index/log(species_richness)),
         total_abundance <- rowSums(species_data)
         )


#save as csv
write.csv(inverts_simulated, "./data_cleaned/inverts_simulated.csv")
                            
                            