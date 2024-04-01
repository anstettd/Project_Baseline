####################################################################################################
#Project Baseline Data Org
####################################################################################################

#Libraries
library(tidyverse)
library(sf)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
#library(devtools)
#devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
library(RColorBrewer)
####################################################################################################
#Data Import
pb_all <- read_csv("Data/PB_ALL_populations.csv")
colnames(pb_all) <- c("Species","Pop","Latitude","Longitude",
                      "site_name","survey_date","maternal_number","comments")

#Print out table of number of pop per species
species_count_df <- pb_all %>%
  group_by(Species) %>%
  summarise(Number = n(), 
            Min_Lat = min(Latitude), 
            Max_Lat = max(Latitude),
            Lat_Difference = Max_Lat - Min_Lat
            )

#Export
write_csv(species_count_df, "data/species_count.csv")

#Plot lat range versus number of pop's available
plot(species_count_df$Number,species_count_df$Lat_Difference)



####################################################################################################
####################################################################################################
#Mapping setup

#Define CRS
EPSG4326<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #setup WGS 1984 CRS

#Make pop/species project baseline dataframe into sf object
pb_sf <- st_as_sf(pb_all ,coords=c("Longitude","Latitude"), crs=EPSG4326)

# USA Map Setup
usa_states <- c(
  "Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
  "Delaware", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa",
  "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
  "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
  "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
  "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
  "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
  "West Virginia", "Wisconsin", "Wyoming"
)

states <- ne_states(country = "united states of america", returnclass = "sf")

states <- states[!states$name %in% c("Alaska", "Hawaii"), ]


####Data frame object per species and put in list
# Get unique species names
unique_species <- unique(pb_all$Species)

# Create a list to store separate data frames for each species
species_data_frames <- list()

# Loop through each unique species
for(species_name in unique_species) {
  # Subset the data for the current species
  species_data <- pb_all %>%
    filter(Species == species_name)
  
  # Store the subset data frame in the list
  species_data_frames[[species_name]] <- species_data
}


####Make sf object per species and put in list

# Loop through each unique species
for(species_name in unique_species) {
  # Subset the data for the current species
  species_data <- pb_all %>%
    filter(Species == species_name)
  
  # Convert data to sf object
  species_sf <- st_as_sf(species_data, coords=c('Longitude', 'Latitude'), crs=st_crs(4326))
  
  # Store the sf object in the list
  species_sf_list[[species_name]] <- species_sf
}







####################################################################################################
#Map all sites

tmap_mode("plot")
tmap_mode("view")
map_test <-
  tm_shape(states)+
  tm_borders()+
  tm_shape(pb_sf)+
  tm_dots(size=0.02,shape=21,col="black",border.col ="black")#+
  tm_layout(legend.position = c(1.03, 0.73),legend.title.size = 0.001,frame.lwd = NA)
  map_test
#tmap_save(mim, filename = "Graphs/Maps/base_time.png",width=4, height=7)

####################################################################################################
  
#Map All specific species
  
  # Set tmap mode to "plot"
  tmap_mode("plot")
  
  # Loop through each sf object in the list
  for(species_name in names(species_sf_list)) {
    # Create tmap object for the current species
    map_test <- tm_shape(states) +
      tm_borders(col = "black", lwd = 0.5) +
      tm_shape(species_sf_list[[species_name]]) +
      tm_dots(size = 0.04, shape = 21, col = "black", border.col = "black") +
      tm_layout(title=species_name, title.position = c("right", "top"))
    
    # Save the tmap object as a PDF with a unique filename for each species
    tmap_save(map_test, filename = as.character(paste0("Maps/Single_Maps/", species_name, "_map.pdf")), width = 7, height = 4, units = "in")
  }
  
  
  
  
  
  
  

  
  ####################################################################################################
  #Rough
  

  
  
  tmap_mode("plot")
  map_test <-
    tm_shape(states)+
    tm_borders()+
    tm_shape(pb_sf)+
    tm_dots(size=0.03,shape=21,col="black",border.col ="black")#+
  tm_layout(legend.position = c(1.03, 0.73),legend.title.size = 0.001,frame.lwd = NA)
  map_test
  tmap_save(map_test, filename = "Maps/species.pdf",width=7, height=7)
  
  
  
  tmap_mode("plot")
  map_test <-
    tm_shape(states)+
    tm_borders()+
    tm_shape(pb_sf)+
    tm_dots(size=0.03,shape=21,col="black",border.col ="black")#+
  tm_layout(legend.position = c(1.03, 0.73),legend.title.size = 0.001,frame.lwd = NA)
  map_test
  tmap_save(map_test, filename = "Maps/species.pdf",width=7, height=7)
  
  

