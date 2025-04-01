setwd("C:/Users/Sayan/Desktop/NFHS/")

# Find clusters within 10 km radius of a specific point
# Load libraries
library(sf)
library(dplyr)
library(units)

# Define the reference point (the coordinates you provided)
ref_lat <- 23.65614444
ref_lon <- 86.86746944

# Create an sf point object for the reference location
ref_point <- st_point(c(ref_lon, ref_lat)) %>% # Note: order is longitude first, then latitude
  st_sfc(crs = 4326) # WGS84 coordinate system

# Calculate distances from each cluster to the reference point
# First, ensure that nfhs_sf has the same CRS as the reference point
nfhs_sf <- st_transform(nfhs_sf, 4326)

# Calculate distance to each cluster in meters
nfhs_sf$distance_m <- st_distance(nfhs_sf, ref_point) %>% 
  as.numeric()

# Convert to kilometers for easier reading
nfhs_sf$distance_km <- nfhs_sf$distance_m / 1000

# Find clusters within 10 km
clusters_within_10km <- nfhs_sf %>%
  filter(distance_km <= 10) %>%
  arrange(distance_km)

# Display the count of clusters within 10 km
print(paste("Number of clusters within 10 km:", nrow(clusters_within_10km)))

# Display the clusters within 10 km
print(clusters_within_10km)

# If you want to save the results to a CSV file
write.csv(st_drop_geometry(clusters_within_10km), 
          "./DATA/clusters_within_10km.csv", 
          row.names = FALSE)

# Create a simple plot showing the reference point and nearby clusters
if (nrow(clusters_within_10km) > 0) {
  # Install ggplot2 if needed
  if (!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)
  
  # Create a buffer around the reference point (10 km circle)
  buffer_10km <- st_buffer(ref_point, dist = 10000)
  
  # Create a simple map
  ggplot() +
    # Add the 10 km buffer circle
    geom_sf(data = buffer_10km, fill = "lightblue", alpha = 0.3) +
    # Add all clusters in the vicinity (you might want to limit this to avoid plotting too many)
    geom_sf(data = nfhs_sf %>% filter(distance_km <= 20), 
            aes(color = "All clusters"), size = 2, alpha = 0.4) +
    # Add clusters within 10 km
    geom_sf(data = clusters_within_10km, 
            aes(color = "Within 10 km"), size = 3) +
    # Add the reference point
    geom_sf(data = ref_point, aes(color = "Reference point"), size = 4) +
    # Set colors
    scale_color_manual(values = c("All clusters" = "gray", 
                                  "Within 10 km" = "blue", 
                                  "Reference point" = "red"),
                       name = "") +
    # Add a title
    labs(title = "NFHS Clusters Within 10 km Radius",
         subtitle = paste0("Reference point: ", ref_lat, ", ", ref_lon)) +
    # Adjust theme
    theme_minimal()
  
  # Save the plot
  ggsave("./IMAGES/clusters_within_10km_map.png", width = 8, height = 8, dpi = 300)
}

# If you want more detailed information, you could also:
# 1. Calculate average characteristics of clusters within 10 km
if (nrow(clusters_within_10km) > 0) {
  # For example, percent urban vs. rural
  urban_rural_summary <- table(clusters_within_10km$URBAN_RURA)
  print("Urban/Rural distribution within 10 km:")
  print(urban_rural_summary)
  print(paste("Percent Urban:", 
              round(urban_rural_summary["U"] / sum(urban_rural_summary) * 100, 1), 
              "%"))
}
