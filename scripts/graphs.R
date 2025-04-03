View(inverts)
 

#plot for species evenness
ggplot(inverts, aes(x = d_from_path_m, y = species_evenness)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), color = "blue") +
  labs(x = "Distance from Path (m)", y = "Species Evenness",
       ) +
  theme_minimal()


#plot for species richness
ggplot(inverts, aes(x = d_from_path_m, y = species_richness)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), color = "blue") +
  labs(x = "Distance from Path (m)", y = "Species Richness",
       ) +
  theme_minimal()

#plot for species abundance
ggplot(inverts, aes(x = d_from_path_m, y = total_abundance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), color = "blue") +
  labs(x = "Distance from Path (m)", y = "Species Abundance",
       ) +
  theme_minimal()
