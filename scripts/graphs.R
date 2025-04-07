library(tidyverse)

View(inverts)
 

#plot for species evenness
figure_1 <- ggplot(inverts, aes(x = d_from_path_m, y = species_evenness)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), color = "blue") +
  labs(x = "Distance from Path (m)", y = "Species Evenness",
       ) +
  theme_minimal()

print(figure_1)

ggsave("./figures/evenness_distance_plot.PNG",figure_1)

#plot for species richness
figure_2 <- ggplot(inverts, aes(x = d_from_path_m, y = species_richness)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), color = "blue") +
  labs(x = "Distance from Path (m)", y = "Species Richness",
       ) +
  theme_minimal()

print(figure_2)

ggsave("./figures/richness_distance_plot.PNG",figure_2)

#plot for species abundance
figure_3 <- ggplot(inverts, aes(x = d_from_path_m, y = total_abundance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), color = "blue") +
  labs(x = "Distance from Path (m)", y = "Species Abundance",
       ) +
  theme_minimal()

print(figure_3)

ggsave("./figures/abundance_distance_plot.PNG",figure_3)

# ====Density


#plot for species evenness
figure_4 <- ggplot(inverts, aes(x = density, y = species_evenness)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), color = "blue") +
  labs(x = "Density (g/cm^3)", y = "Species Evenness",
  ) +
  theme_minimal()

print(figure_4)

ggsave("./figures/evenness_density_plot.PNG",figure_4)


#plot for species richness
figure_5 <- ggplot(inverts, aes(x = density, y = species_richness)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), color = "blue") +
  labs(x = "Density (g/cm^3)", y = "Species Richness",
  ) +
  theme_minimal()

print(figure_5)

ggsave("./figures/richness_density_plot.PNG",figure_5)

#plot for species abundance
figure_6 <- ggplot(inverts, aes(x = density, y = total_abundance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), color = "blue") +
  labs(x = "Density (g/cm^3)", y = "Species Abundance",
  ) +
  theme_minimal()

print(figure_6)

ggsave("./figures/abundance_density_plot.PNG",figure_6)

