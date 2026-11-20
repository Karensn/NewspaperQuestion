# Making an infographics 

#Loading the packages 
library(ggplot2)
library(dplyr)
library(ggrepel) # Needed for not having the names overlap

# Prepare and clear the data
sleep_data = msleep |>
  filter(
    !is.na(bodywt), !is.na(sleep_total), !is.na(vore), bodywt > 0 ) |>  # CLean the data for NA and irrelevant values
  mutate(
    diet_group = case_when(
      vore == "carni" ~ "Carnivores", vore == "herbi" ~ "Herbivores",
      vore == "omni" ~ "Omnivores", vore == "insecti" ~ "Insectivores"  # Group them into dietary preferances
    )
  )

# Set the colors (Using HEX)
set_manual_colors = c(
  "Herbivores" = "#399B7E", "Carnivores" = "#BDBDBD",
  "Omnivores" = "#7795B6", "Insectivores" = "#D89A50")

# Identify the five biggest animals
biggest_animals = sleep_data |> 
  slice_max(bodywt, n = 5) # Select row based on highest calue in column



p = ggplot(
  sleep_data,
  aes(
    x = bodywt,
    y = sleep_total
  )
) +
  
  geom_point(aes(size = bodywt, colour = diet_group), alpha = 0.70) + # Customizing the points (dots)

  # Adding names and preventing them from overlapping 
  geom_text_repel(data = sleep_data,
                  aes(label = name, fontface = if_else(name %in% biggest_animals$name, "bold", "plain")), # Highlight the selected
    size = 2.5, colour = "grey30", box.padding = 0.4,
    max.overlaps = Inf, segment.colour = "grey70") +
  
  scale_colour_manual(values = set_manual_colors) + # Apply the manually set colours
  
  scale_size_continuous( range = c(2, 16), trans = "sqrt", guide = "none") + # Control the size of the points (generally make them bigger)
  
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                labels = c("0.01", "0.1", "1", "10", "100", "1,000", "10,000")) + # Make it a logarithmic scale on x-axis
  
  # Adding Title and texts
  labs(title = "Does diet or size affect sleep in mammals?", 
    subtitle = paste("Daily sleep duration compared to body weight and diet."),
    x = "Body Weight", y = "Total Sleep",
    caption = paste("Body weight shown on logarithmic scale.\n", 
                    "Source: ggplot2 – msleep dataset.")) +
  
  theme_minimal(base_size = 14) + # Set the theme (background white with grid)
  
  # Adjusting the look of the plot
  theme(
    # Adjustments for title and subtitle
    plot.title = element_text(size = 23, face = "bold"),
    plot.subtitle = element_text(size = 14),
    
    # Adjustment for axis title and text
    axis.title = element_text(face = "bold", colour = "grey40"), 
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
    axis.text = element_text(colour = "grey45"),
    
    panel.grid.minor = element_blank(), # Clean up the grid
    
    legend.position = "top", legend.justification = "left", legend.text = element_text(size = 11),
    
    plot.caption = element_text(hjust = 0)) # Position the caption

# Save the plot as PNG
ggsave(
  filename = "mammal_sleep.png",
  plot = p,
  width = 16,
  height = 12,
  dpi = 300,
  bg = "white"
)