fill.coords <- function(data, use_sampling_site = TRUE) {
  if (!require("tidygeocoder")) install.packages("tidygeocoder")
  if (!require("dplyr")) install.packages("dplyr")
  library(tidygeocoder)
  library(dplyr)       
  
  data_filled <- data
  
  if (is.numeric(data_filled$sampling_latitude)) {
    data_filled$sampling_latitude <- as.character(data_filled$sampling_latitude)
  }
  if (is.numeric(data_filled$sampling_longitude)) {
    data_filled$sampling_longitude <- as.character(data_filled$sampling_longitude)
  }
  
  missing_coords <- is.na(data_filled$sampling_latitude) | 
    is.na(data_filled$sampling_longitude) |
    data_filled$sampling_latitude == "" |
    data_filled$sampling_longitude == ""
  
  cat("GEOCODING MISSING COORDINATES\n")
  cat("Total rows:", nrow(data_filled), "\n")
  cat("Rows with missing coordinates:", sum(missing_coords), "\n\n")
  
  if (sum(missing_coords) == 0) {
    cat("✓ No missing coordinates to fill!\n")
    return(data_filled)
  }
  
  to_geocode <- data_filled[missing_coords, ]
  
  if (use_sampling_site && "sampling_site" %in% names(to_geocode)) {
    to_geocode$location_string_1 <- paste(
      ifelse(is.na(to_geocode$sampling_site) | to_geocode$sampling_site == "", 
             "", to_geocode$sampling_site),
      ifelse(is.na(to_geocode$sampling_country) | to_geocode$sampling_country == "", 
             "", to_geocode$sampling_country),
      sep = ", " )
  } else {
    to_geocode$location_string_1 <- ""
  }
  
  to_geocode$location_string_2 <- paste(
    ifelse(is.na(to_geocode$sampling_city) | to_geocode$sampling_city == "", 
           "", to_geocode$sampling_city),
    ifelse(is.na(to_geocode$sampling_region) | to_geocode$sampling_region == "", 
           "", to_geocode$sampling_region),
    ifelse(is.na(to_geocode$sampling_country) | to_geocode$sampling_country == "", 
           "", to_geocode$sampling_country),
    sep = ", "
  )
  
  to_geocode$location_string_3 <- paste(
    ifelse(is.na(to_geocode$sampling_region) | to_geocode$sampling_region == "", 
           "", to_geocode$sampling_region),
    ifelse(is.na(to_geocode$sampling_country) | to_geocode$sampling_country == "", 
           "", to_geocode$sampling_country),
    sep = ", "
  )
  
  to_geocode$location_string_4 <- ifelse(
    is.na(to_geocode$sampling_country) | to_geocode$sampling_country == "",
    "", 
    to_geocode$sampling_country)
  
  for (i in 1:4) {
    col_name <- paste0("location_string_", i)
    to_geocode[[col_name]] <- gsub("^,\\s*|,\\s*,|,\\s*$", "", to_geocode[[col_name]])
    to_geocode[[col_name]] <- gsub("\\s+", " ", to_geocode[[col_name]])
    to_geocode[[col_name]] <- trimws(to_geocode[[col_name]])
  }
  
  to_geocode$location_string <- ""
  for (i in 1:nrow(to_geocode)) {
    if (to_geocode$location_string_1[i] != "") {
      to_geocode$location_string[i] <- to_geocode$location_string_1[i]
    } else if (to_geocode$location_string_2[i] != "") {
      to_geocode$location_string[i] <- to_geocode$location_string_2[i]
    } else if (to_geocode$location_string_3[i] != "") {
      to_geocode$location_string[i] <- to_geocode$location_string_3[i]
    } else if (to_geocode$location_string_4[i] != "") {
      to_geocode$location_string[i] <- to_geocode$location_string_4[i]
    }
  }
  
  valid_locations <- to_geocode$location_string != ""
  
  cat("Valid locations to geocode:", sum(valid_locations), "\n")
  
  if (sum(valid_locations) == 0) {
    cat("⚠ No valid location strings found!\n")
    cat("This means none of the rows have sampling_site, city, region, or country data.\n")
    return(data_filled)
  }
  
  cat("(This may take a while - rate limited to respect API terms)\n\n")
  
  # Geocode in batches
  valid_data <- to_geocode[valid_locations, ]
  batch_size <- 100
  n_batches <- ceiling(nrow(valid_data) / batch_size)
  
  all_geocoded <- list()
  
  for (batch in 1:n_batches) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, nrow(valid_data))
    
    cat("Processing batch", batch, "of", n_batches, 
        "(rows", start_idx, "to", end_idx, ")...\n")
    
    batch_data <- valid_data[start_idx:end_idx, ]
    
    tryCatch({
      batch_geocoded <- batch_data %>%
        geocode(
          address = location_string,
          method = "osm",
          lat = geo_lat,
          long = geo_long,
          full_results = FALSE,
          quiet = TRUE
        )
      
      all_geocoded[[batch]] <- batch_geocoded
      
      if (batch < n_batches) {
        Sys.sleep(1)  # Rate limiting
      }
      
    }, error = function(e) {
      cat("  ✗ Error in batch", batch, ":", e$message, "\n")
      all_geocoded[[batch]] <- batch_data %>%
        mutate(geo_lat = NA, geo_long = NA)
    })
  }
  
  geocoded <- bind_rows(all_geocoded)
  
  missing_indices <- which(missing_coords)
  valid_indices <- missing_indices[valid_locations]
  
  if (!"geocoded" %in% names(data_filled)) {
    data_filled$geocoded <- FALSE
  }
  
  for (i in seq_along(valid_indices)) {
    row_idx <- valid_indices[i]
    if (!is.na(geocoded$geo_lat[i])) {
      data_filled$sampling_latitude[row_idx] <- as.character(geocoded$geo_lat[i])
      data_filled$sampling_longitude[row_idx] <- as.character(geocoded$geo_long[i])
      data_filled$geocoded[row_idx] <- TRUE
    }
  }
  
  n_filled <- sum(!is.na(geocoded$geo_lat))
  n_failed <- sum(is.na(geocoded$geo_lat))
  

  cat("✓ Successfully:", n_filled, "\n")
  cat("✗ Failed: ", n_failed, "\n")
  
  if (n_failed > 0) {
    cat("\n⚠ Failed locations (first 10):\n")
    failed <- geocoded[is.na(geocoded$geo_lat), ]
    print(head(failed %>% select(location_string), 10))
  }
  
  still_missing <- is.na(data_filled$sampling_latitude) | 
    is.na(data_filled$sampling_longitude) |
    data_filled$sampling_latitude == "" |
    data_filled$sampling_longitude == ""

  return(data_filled)
}



