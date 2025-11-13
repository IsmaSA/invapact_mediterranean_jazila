
# 01 Mediterranean Basin Jazila

# Code: Ismael Soto

# Packages 
pacman::p_load(sf,terra,dplyr,ggplot,rgbif, invapact, mapview)

data(invapact)
  
# Remove rows with NA in start/end sampling years
invapact <- invapact[!is.na(invapact$start_sampling_year), ]
invapact <- invapact[!is.na(invapact$end_sampling_year), ]
invapact <- invapact[!is.na(invapact$InvaPact_score), ]

str(invapact)

invapact_sf <- invapact %>%  mutate(
    sampling_longitude = as.numeric(sampling_longitude),
    sampling_latitude = as.numeric(sampling_latitude) ) %>%
    filter(!is.na(sampling_longitude), !is.na(sampling_latitude)) %>%
  filter(sampling_latitude > 25, sampling_latitude < 48) %>%      
  filter(sampling_longitude > -3, sampling_longitude < 40) %>%     
  st_as_sf(coords = c("sampling_longitude", "sampling_latitude"), crs = 4326, remove = FALSE)

mediterraneo <- st_read("./Mediterranean/Med/Med_africa.gpk.gpkg")
mediterraneo <- st_transform(mediterraneo, crs = 4326)

mediterraneo1 <- st_read("./Mediterranean/Med/Med_europa.gpk.gpkg")
mediterraneo1 <- st_transform(mediterraneo1, crs = 4326)

# Check the layers from Carlos and use whatever you think is best:
mapview(mediterraneo, col.regions = "lightgrey", alpha.regions = 0.5) +
mapview(mediterraneo1, col.regions = "lightgrey", alpha.regions = 0.5) +
  mapview(invapact_sf,
          color = "red",      
          col.regions = "red", 
          cex = 1.5,          
          alpha = 0.7, legend = FALSE)


### I want to check also the NAs of coordinates  to see if we can get something: 

invapact1 <- invapact %>%  mutate(
    sampling_longitude = as.numeric(sampling_longitude),
    sampling_latitude = as.numeric(sampling_latitude) ) %>%
    filter(is.na(sampling_longitude), is.na(sampling_latitude)) 

nrow(invapact1) #9,472 missing info


unique(invapact1$sampling_country) # 127

remove <- c("United Kingdom","United States","Australia","Belgium","USA","Puerto Rico","South Africa","New Zealand",
"Bahamas","Brazil","Russia","Japan","Poland","Lithuania","Republic of the Congo","Canada","Netherlands","Switzerland",
"Finland","Chile","Ireland","Hungary","Democratic Republic of the Congo","United States of America","Colombia","Argentina",
"United States;Canada","Turks and Caicos Islands","The Bahamas","Hungary;Austria;Czech Republic;Germany;Switzerland",
"Ireland; United Kingdom","Czech Republic","Romania","Senegal","Germany","Iceland","Slovakia","Sweden","Mexico","Indonesia",
"Chile; Argentina","England","Kenya","Togo","Saudi Arabia","Norway","China","Nepal","Thailand","Vietnam","Malaysia",
"Ecuador","Ethiopia","Hungary;Romania","India","Samoa; American Samoa","Madagascar","Mozambique","Cook Islands","Peru","Croatia",
"Zambia","USA; Mexic","Dominican Republic","Honduras","Cuba","Bolivia","Republic of Korea","Panama","Canada; United States","Taiwan",
"Trinidad and Tobago","Guatemala","Tanzania","Ghana","Costa Rica","Namibia","Belgium; Netherlands","Fiji","Uganda","Belarus",
"Pakistan","South Georgia and the South Sandwich Islands","French Southern and Antarctic Lands","Sierra Leone","Guinea",
"Norway; Sweden","Sudan","Scotland","Uruguay","Bangladesh","The Netherlands","Burkina Faso","Yugoslavia","El Salvador",
"Papua New Guinea","Iran")

invapact1 <- invapact1[!invapact1$sampling_country %in% remove, ]


unique(invapact1$sampling_city) # 11

remove <- c("Figueira da Foz","AlcalÃ¡ de Henares","Vigo","Cantanhede" ,"Marinha Grande", "Douala","Niamey","Antony; Sucy-en-Brie")

invapact1 <- invapact1[!invapact1$sampling_city %in% remove, ]
nrow(invapact1) # 1478

unique(invapact1$sampling_site) # 207
unique(invapact1$sampling_region) # 116

remove <- c("Littoral Central Portugal","Galicia; RegiÃ£o Norte","Northern Caspian; Middle Caspian; Southern Caspian",
"Middle Caspian; Southern Caspian","Middle Caspian","Alentejo","Central Europe, Belgium and the Netherlands","Western France",
"Alentejo; Ribatejo","Algarve","Brittany","South Korea","north-western Switzerland; south-western Germany",
"Southwestern South Korea","Scottish Lowlands","Northern England","Southeast England","Ireland","Wales","Midlands",
"Scottish Highlands & Islands","Southwest England","Arizona; New Mexico; Sonora","JaÃ©n","Southwest Portugal",
"Mainland Portugal","Tierra del Fuego Archipelago","Northern Andean Patagonia","Tenerife, Canary Islands","Barents Sea",
"Great Lakes basin","SE Madeira Island","SW Madeira Island","North America","Southwestern Cameroon","Tenerife","Normandy")

invapact1 <- invapact1[!invapact1$sampling_region %in% remove, ]
nrow(invapact1) # 1261

unique(invapact1$sampling_site) # 168
unique(invapact1$sampling_region) # 81
