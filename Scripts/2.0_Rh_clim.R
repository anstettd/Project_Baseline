#####################################################################################################
#Black-Eyed Susan Climate Differences
#####################################################################################################

#Libraries
library(tidyverse)
library(sf)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
#library(devtools)
#devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)

#Create output folder for graphs
dir.create("Graphs/Clim_diff", recursive = TRUE, showWarnings = FALSE)

#Import data
pops <- read_csv("Data/Rh_pops.csv")
clim2013 <- read_csv("Data/Rh_clim_Year_2013Y.csv")
clim2014 <- read_csv("Data/Rh_clim_Year_2014Y.csv")
clim2015 <- read_csv("Data/Rh_clim_Year_2015Y.csv")
clim2024 <- read_csv("Data/Rh_clim_Year_2024Y.csv")
clim2025 <- read_csv("Data/Rh_clim_Year_2025Y.csv")

#Merge
clim2013 <- clim2013 %>% mutate(Year = 2013)
clim2014 <- clim2014 %>% mutate(Year = 2014)
clim2015 <- clim2015 %>% mutate(Year = 2015)
clim2024 <- clim2024 %>% mutate(Year = 2024)
clim2025 <- clim2025 %>% mutate(Year = 2025)

clim_all <- bind_rows(clim2013, clim2014, clim2015, clim2024, clim2025)

#####################################################################################################
#Generate data frame for difference in climate between sampling years and resampling
#####################################################################################################

clim_vars <- c("MAT","MWMT","MCMT","TD","MAP","MSP","AHM","SHM","DD_0","DD5","DD_18","DD18",
               "NFFD","bFFP","eFFP","FFP","PAS","EMT","EXT","rsds","Eref","CMD","RH","CMI","DD1040")

# Late average (2024, 2025) per ID1 -- same for every population
late_avg <- clim_all %>%
  filter(Year %in% c(2024, 2025)) %>%
  group_by(ID1) %>%
  summarise(across(all_of(clim_vars), \(x) mean(x, na.rm = TRUE))) %>%
  rename_with(~ paste0(.x, "_late"), all_of(clim_vars))

# Early average depends on each pop's Year (2014 -> avg(2013,2014); 2015 -> avg(2014,2015))
early_avg <- pops %>%
  select(pop, Year) %>%
  rowwise() %>%
  mutate(early_years = list(if (Year == 2015) c(2014, 2015) else c(2013, 2014))) %>%
  unnest(early_years) %>%
  rename(ID1 = pop, early_year = early_years) %>%
  left_join(clim_all, by = c("ID1", "early_year" = "Year")) %>%
  group_by(ID1) %>%
  summarise(across(all_of(clim_vars), \(x) mean(x, na.rm = TRUE))) %>%
  rename_with(~ paste0(.x, "_early"), all_of(clim_vars))

# Combine
clim_diff <- pops %>%
  rename(ID1 = pop) %>%
  left_join(early_avg, by = "ID1") %>%
  left_join(late_avg, by = "ID1")

# Add difference columns (late - early) for each climate variable
for (v in clim_vars) {
  clim_diff[[paste0(v, "_diff")]] <- clim_diff[[paste0(v, "_late")]] - clim_diff[[paste0(v, "_early")]]
}

#####################################################################################################
#Histograms of Diff
#####################################################################################################

hist_MAT <- ggplot(clim_diff, aes(x = MAT_diff)) +
  geom_histogram(bins = 20, fill = "grey30", color = "white") +
  theme_classic() +
  theme(
    axis.text = element_text(face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 20)
  ) +
  labs(x = "MAT_diff", y = "Count")
hist_MAT
ggsave("Graphs/Clim_diff/hist_MAT_diff.pdf", plot = hist_MAT, width = 8, height = 6)

hist_MAP <- ggplot(clim_diff, aes(x = MAP_diff)) +
  geom_histogram(bins = 20, fill = "grey30", color = "white") +
  theme_classic() +
  theme(
    axis.text = element_text(face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 20)
  ) +
  labs(x = "MAP_diff", y = "Count")
hist_MAP
ggsave("Graphs/Clim_diff/hist_MAP_diff.pdf", plot = hist_MAP, width = 8, height = 6)

#####################################################################################################
#Latitude
#####################################################################################################

lat_MAT <- ggplot(clim_diff, aes(x = lat, y = MAT_diff)) +
  geom_point() +
  theme_classic() +
  theme(
    axis.text = element_text(face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 20)
  ) +
  labs(x = "Latitude", y = "MAT_diff")
lat_MAT
ggsave("Graphs/Clim_diff/latitude_MAT_diff.pdf", plot = lat_MAT, width = 8, height = 6)

lat_MAP <- ggplot(clim_diff, aes(x = lat, y = MAP_diff)) +
  geom_point() +
  theme_classic() +
  theme(
    axis.text = element_text(face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 20)
  ) +
  labs(x = "Latitude", y = "MAP_diff")
lat_MAP
ggsave("Graphs/Clim_diff/latitude_MAP_diff.pdf", plot = lat_MAP, width = 8, height = 6)

#####################################################################################################
#Map the difference
#####################################################################################################

# Convert clim_diff to an sf object using lat/long
clim_sf <- st_as_sf(clim_diff, coords = c("long", "lat"), crs = 4326, remove = FALSE)

# Get US states background
us_states <- ne_states(country = "United States of America", returnclass = "sf")

# Define bounding box roughly spanning Colorado to the East Coast
bbox_coords <- st_bbox(c(xmin = -109, xmax = -66, ymin = 24, ymax = 49), crs = st_crs(4326))

# MAT_diff map (skyblue -> orange -> red -> dark red, low to high)
mat_palette <- colorRampPalette(c("skyblue", "orange", "red", "darkred"))(100)

map_MAT <- tm_shape(us_states, bbox = bbox_coords) +
  tm_borders(col = "grey50") +
  tm_shape(clim_sf) +
  tm_dots(col = "MAT_diff", palette = mat_palette, style = "cont", size = 0.3,
          title = "MAT_diff") +
  tm_layout(legend.text.size = 1, legend.title.size = 1.2, frame = FALSE)
map_MAT
tmap_save(map_MAT, "Graphs/Clim_diff/map_MAT_diff.pdf", width = 8, height = 6)

# MAP_diff map (diverging, centered at 0)
map_MAP <- tm_shape(us_states, bbox = bbox_coords) +
  tm_borders(col = "grey50") +
  tm_shape(clim_sf) +
  tm_dots(col = "MAP_diff", palette = "RdBu", style = "cont", size = 0.4,
          title = "MAP_diff", midpoint = 0) +
  tm_layout(legend.text.size = 1, legend.title.size = 1.2, frame = FALSE)
map_MAP
tmap_save(map_MAP, "Graphs/Clim_diff/map_MAP_diff.pdf", width = 8, height = 6)




