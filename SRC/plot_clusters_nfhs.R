setwd("C:/Users/Sayan/Desktop/NFHS/")

library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)

# Read the NFHS geographic data from shapefile
# Replace 'nfhs_geo_data.shp' with your actual shapefile name
# Note: Make sure all associated files (.dbf, .prj, .shx) are in the same directory
nfhs_sf <- st_read("./DATA/IAGE7AFL.shp")

# Simple NFHS Cluster Map in R
# This script plots NFHS clusters on an India map using your existing sf object

# Get India map data
india_map <- ne_countries(scale = "medium", country = "India", returnclass = "sf")

total_clusters <- nrow(nfhs_sf)
print(paste("Total number of clusters:", total_clusters))


# Create a basic map with all clusters
basic_map <- ggplot() +
  # Add India base map
  geom_sf(data = india_map, fill = "lightgray", color = "darkgray") +
  # Add NFHS clusters colored by urban/rural status
  geom_sf(data = nfhs_sf, aes(color = URBAN_RURA), size = 0.8, alpha = 0.7) +
  # Set appropriate colors and legend
  scale_color_manual(values = c("R" = "forestgreen", "U" = "red"),
                     labels = c("R" = "Rural", "U" = "Urban"),
                     name = "Cluster Type") +
  # Add titles and captions
  labs(title = "NFHS Cluster Locations in India",
       subtitle = "Based on GPS coordinates (with displacement for confidentiality)",
       caption = "Source: NFHS 2020") +
  # Set theme
  theme_minimal() +
  # Format text elements
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption = element_text(hjust = 0, face = "italic"))

# Display the map
print(basic_map)

# Save the map
ggsave("./IMAGES/nfhs_cluster_map.png", plot = basic_map, width = 10, height = 12, dpi = 300)

# Create a map by state/region
state_map <- ggplot() +
  # Add India base map
  geom_sf(data = india_map, fill = "lightgray", color = "darkgray") +
  # Add NFHS clusters colored by state
  geom_sf(data = nfhs_sf, aes(color = ADM1NAME), size = 0.8, alpha = 0.7) +
  # Add titles and captions
  labs(title = "NFHS Clusters by State/UT",
       subtitle = "Color-coded by administrative region",
       caption = "Source: NFHS 2020") +
  # Set theme
  theme_minimal() +
  # Format text elements
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption = element_text(hjust = 0, face = "italic"),
        legend.position = "right",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8))

# Display the state map
print(state_map)

# Save the state map
ggsave("nfhs_cluster_by_state_map.png", plot = state_map, width = 12, height = 12, dpi = 300)

# If you want to create a more detailed map with zoomed regions, try this:
# Create a map with facets for different regions
region_map <- ggplot() +
  # Add India base map
  geom_sf(data = india_map, fill = "lightgray", color = "darkgray") +
  # Add NFHS clusters colored by urban/rural status
  geom_sf(data = nfhs_sf, aes(color = URBAN_RURA), size = 0.8, alpha = 0.7) +
  # Set appropriate colors and legend
  scale_color_manual(values = c("R" = "forestgreen", "U" = "red"),
                     labels = c("R" = "Rural", "U" = "Urban"),
                     name = "Cluster Type") +
  # Facet by DHSREGNA (DHS region)
  facet_wrap(~ DHSREGNA, scales = "free") +
  # Add titles and captions
  labs(title = "NFHS Cluster Locations by Region",
       subtitle = "Separated by DHSREGNA regions",
       caption = "Source: NFHS 2020") +
  # Set theme
  theme_minimal() +
  # Format text elements
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption = element_text(hjust = 0, face = "italic"),
        strip.text = element_text(size = 8, face = "bold"))

# Display the region map
# Note: This might create a very large plot if there are many regions
# print(region_map)

# Save the region map (larger size to accommodate facets)
# ggsave("nfhs_cluster_by_region_map.png", plot = region_map, width = 18, height = 15, dpi = 300)

# If you want an interactive map, use leaflet (uncomment to use)
# if (!require("leaflet")) install.packages("leaflet")
# 
# 
# # Create a color palette for urban/rural
 pal <- colorFactor(c("forestgreen", "red"), domain = c("R", "U"))
# 
# # Create the interactive map
 interactive_map <- leaflet(nfhs_sf) %>%
   # Add base map tiles
   addTiles() %>%
#   # Add cluster markers with popups
   addCircleMarkers(
     color = ~pal(URBAN_RURA),
     radius = 3,
     opacity = 0.7,
     popup = ~paste(
       "<strong>Cluster ID:</strong>", DHSCLUST, "<br>",
       "<strong>State:</strong>", ADM1NAME, "<br>",
       "<strong>Region:</strong>", DHSREGNA, "<br>",
       "<strong>Type:</strong>", ifelse(URBAN_RURA == "U", "Urban", "Rural"), "<br>",
       "<strong>Altitude:</strong>", ALT_DEM, "meters"
     )
   ) %>%
   # Add a legend
   addLegend(
     position = "bottomright",
     pal = pal,
     values = c("R", "U"),
     labels = c("Rural", "Urban"),
     title = "Cluster Type"
   )
# 
# # Display the interactive map
 interactive_map
# 
# # Save the interactive map as HTML
# htmlwidgets::saveWidget(interactive_map, "nfhs_interactive_map.html")


