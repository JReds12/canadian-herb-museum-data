1# Project: GLANSIS Canadian Herbaria Data Cleaning

# Project Description: Publicly available Canadian Herbarium GBIF datasets were 
# cleaned to fill in spatial gaps in invasive species Canadian data in the Great 
# Lakes basin. Datasets were subset to have only GLANSIS plant species 
# (nonindigenous & watchlist) and occurrences within the Great Lakes basin. 
# Entries missing coordinates, the locality description columns were used to 
# fill in approximate coordinates. Outputs include include files with invasive
# plant species records in the Great Lakes, files where GPS coordinates could
# not be determined, and files with Phragmites australis. 

# Creator: Joseph Redinger (jredinger11@gmail.com)
# Date: 24-05-2024

# Import Libraries ----
library(readxl)
library(rgbif)
library(tidyr)
library(dplyr)
library(stringr)
library(tidygeocoder)
library(sf)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(writexl)

# GBIF Initial Data Download ----
# This only needs to be run once to download GBIF datasets
# Requires GBIF account

# selected GBIF dataset keys
gbif_dataset_keys <- c("cd4cd181-ae56-4595-8c80-d4311654ad03",
                       "e8a25a42-f125-476c-8554-3ec21cd51a84",
                       "55a29b7c-9cb3-4721-b1e3-40561c955e21",
                       "faa5dc7a-3524-4a99-8a10-f259631c7827",
                       "2429287b-ef65-4cfd-afcc-11cc3ba95cca",
                       "07fd0d79-4883-435f-bba1-58fef110cd13",
                       "5678b0b3-450b-4513-82e1-2b32c3c50b54",
                       "b22b10e7-86da-4b80-8f49-f3f958a2463d",
                       "84687bdc-f762-11e1-a439-00145eb45e9a",
                       "2fd02649-fc08-4957-9ac5-2830e072c097",
                       "0348540a-e644-4496-89d3-c257da9ad776",
                       "1e61b812-b2ec-43d0-bdbb-8534a761f74c",
                       "2d18c666-ee2b-40a3-a708-280ea19c0fbc",
                       "d9522343-146c-4d6b-b312-543d4d8ca0e8")

# empty list for datasets 
df_list = list()

# empty list for download_keys
download_keys = list()

# credentials for occ_download() - enter new info for each new user
username = ''
password = ''
email = ''

# loop through dataset keys
for (i in seq_along(gbif_dataset_keys)) {
  
  key = gbif_dataset_keys[[i]]
  
  # Create a download request
  download_request <- occ_download(
    pred("datasetKey", key),
    user = user,
    pwd = pwd,
    email = email)

  # get GBIF download key
  meta = occ_dowload_meta(download_request)
  download_key <- meta$key
  download_keys[[i]] = download_key

  # Check the status of the download until it's done
  download_status <- meta$status

  while (download_status != "SUCCEEDED") {

    Sys.sleep(60)  # Wait for 60 seconds before checking the status again
    download_status <- occ_download_meta(download_key)$status
    print(paste("Current status for", download_key, ":", download_status))

    if (download_status == "FAILED") {
      print(paste("Download failed for dataset key:", key))
      break
    }
  }

  if (download_status == "SUCCEEDED") {
    print(paste("Download completed for dataset key:", key))

    # import data
    data_download <- occ_download_get(download_key)
    gbif_data <- occ_download_import(data_download)
    big_df_list[[i]] = gbif_data$data

  }
}

# Write the list to a text file - this keeps from from having to do data requests
# each time R is restarted
writeLines(download_keys, "gbif_download_keys.txt")


# Import Data - GBIF Datasets, GLANSIS Plant Names, & Great Lakes Shapefile ----

# download key list
download_keys = readLines("gbif_download_keys.txt")

# import data using GBIF download keys
df_list = list()
for (i in seq_along(download_keys)) {
  tryCatch({
    key <- download_keys[i]
    data_download <- occ_download_get(key, path = "datasets/", overwrite = T)
    gbif_data <- occ_download_import(data_download)
    df_list[[i]] = gbif_data
    cat("Processed:", key, "\n")
  }, error = function(e) {
    cat("Error processing", key, ": ", conditionMessage(e), "\n")
  })
}

#df_list[[1]]$rightsHolder = "Memorial University of Newfoundland"

# import species list
invasive_sp = read_excel("species_names_replace.xlsx", sheet = "nonindigenous")
watchlist_sp = read_excel("species_names_replace.xlsx", sheet = "watchlist")

# stack GLANSIS species lists
new_list = rbind(invasive_sp, watchlist_sp)

# clean species names for plants
new_list = new_list %>% 
  filter(Group == "Plants") %>%
  separate_rows(synonym.edit, sep = ", ") %>%
  mutate(synonym.edit = trimws(synonym.edit))

# make a list of species names
species_list = c(new_list$'Scientific Name', new_list$synonym.edit) %>%
  unique() %>%
  na.omit() %>%
  sort()

# find species names that do not follow genus species format
pattern <- "\\w+\\s+\\w+\\s+\\w+.*"
matched_indices <- grep(pattern, species_list)
matched_strings <- species_list[matched_indices]
print(matched_strings)

# import Great Lakes boundary
gl_basin = st_read("shp/great-lakes-basin.shp")
gl_basin = st_cast(gl_basin, "POLYGON")

# import Great Lakes boundary
gl_boundary = st_read("shp/great-lakes-boundary.shp")
gl_boundary = st_cast(gl_boundary, "POLYGON")


# Subset: Select Only Records With GLANSIS Plant Species ----

# create function to subset only Great Lakes invasive species
# use the str_detect() function to select for any partial matches in scientificNames columns
subset.function = function(df) {
  df <- df %>% 
    filter(str_detect(scientificName, paste (species_list, collapse = "|")))
  return(df)
}  

# subset each dataframe to only plant species of interest
for (i in seq_along(df_list)) {
  tryCatch({
    df_list[[i]] <- subset.function(df_list[[i]])
    cat("Processed dataframe", i, "\n")
  }, error = function(e) {
    cat("Error processing dataframe", i, ": ", conditionMessage(e), "\n")
  })
}

# Add Missing GPS Coordinates ----

# function to add in Accuracy and Source columns for rows with GPS coordinates
add.columns <- function(df) {
  df$Accuracy <- ifelse(df$decimalLatitude != "" & df$decimalLongitude != "", "Accurate", NA)
  df$Source <- ifelse(df$decimalLatitude != "" & df$decimalLongitude != "", "reported", NA)
  return(df)
}

# add new columns to list
for (i in seq_along(df_list)) {
  tryCatch({
    df_list[[i]] <- add.columns(df_list[[i]])
    cat("Processed dataframe", i, "\n")
  }, error = function(e) {
    cat("Error processing dataframe", i, ": ", conditionMessage(e), "\n")
  })
}

# function to add location description columns to a dataframe
add.location.description <- function(df, cols) {
  concat_strings <- function(row, cols) {
    values <- sapply(cols, function(col) if (col %in% names(df)) df[[col]][row] else NA)
    paste(na.omit(values), collapse = ", ")
  }
  
  # add loc.desc and loc.desc2 columns
  df$loc.desc <- sapply(1:nrow(df), function(i) concat_strings(i, cols))
  df$loc.desc2 <- sapply(1:nrow(df), function(i) concat_strings(i, cols[-length(cols)]))
  df$loc.desc3 <- sapply(1:nrow(df), function(i) concat_strings(i, cols[1:5]))
  df$loc.desc4 <- sapply(1:nrow(df), function(i) concat_strings(i, cols[2:5]))
  
  return(df)
}

# columns to be concatenated
columns_to_concat <- c("country", "countryCode", "stateProvince", "county", "municipality", "waterBody", "islandGroup", "island", "locality")

# add new location descriptions columns to each dataframe in the list
for (i in seq_along(df_list)) {
  tryCatch({
    df_list[[i]] <- add.location.description(df_list[[i]], columns_to_concat)
    cat("Processed dataframe", i, "\n")
  }, error = function(e) {
    cat("Error processing dataframe", i, ": ", conditionMessage(e), "\n")
  })
}

# function to geocode locality
geocode.locality <- function(location.desc) {
  tryCatch(
    expr = {
      geo <- geo(location.desc)
      return(geo)
    },
    error = function(e) {
      return(NA)
    }
  )
}

# geocode for missing GPS locations
for (index in seq_along(df_list)) {
  df <- df_list[[index]]
  
  for (i in 1:nrow(df)) {
    if (is.na(df$decimalLatitude[i]) & is.na(df$decimalLongitude[i])) {
      loc_descriptions <- c(df$loc.desc[i], df$loc.desc2[i], df$loc.desc3[i], df$loc.desc4[i])
      for (loc in loc_descriptions) {
        geo_dat <- geocode.locality(loc)
        
        if (!is.na(geo_dat$lat) & !is.na(geo_dat$long)) {
          df$decimalLatitude[i] <- geo_dat$lat
          df$decimalLongitude[i] <- geo_dat$long
          df$Accuracy[i] <- "Approximate"
          df$Source[i] <- "Mapped derived"
          break
        }
      }
    }
  }
  
  df_list[[index]] <- df
}

# drop loc.desc columns
for (i in seq_along(df_list)) {
  tryCatch({
    df_list[[i]] <- subset(df_list[[i]], select = -c(loc.desc, loc.desc2, loc.desc3, loc.desc4))
    cat("Processed dataframe", i, "\n")},
    error = function(e) {
      cat("Error processing dataframe", i, ":", conditionMessage(e), "\n")
    })
}

# Seperate records where GPS can not be found ----

data_with_gps = list()
data_without_gps = list()

for (i in seq_along(df_list)) {
  tryCatch({
    data_with_gps[[i]] <- df_list[[i]][!is.na(df_list[[i]]$Accuracy),]
    data_without_gps[[i]] <- df_list[[i]][is.na(df_list[[i]]$Accuracy),]
    cat("Processed dataframe", i, "\n")},
    error = function(e) {
      cat("Error processing dataframe", i, ":", conditionMessage(e), "\n")
    })
}


# Select Observations Within the Great Lakes Basin ----

# convert each dataframe into a spatial object
spatial_obj_list = list()
for (i in seq_along(data_with_gps)) {
  tryCatch({
    data_with_gps[[i]]$Latitude <- data_with_gps[[i]]$decimalLatitude
    data_with_gps[[i]]$Longitude <- data_with_gps[[i]]$decimalLongitude
    spatial_obj_list[[i]] <- st_as_sf(data_with_gps[[i]], 
                             coords = c("Longitude", "Latitude"), 
                             crs = st_crs(gl_basin))
    
    cat("Processed dataframe", i, "\n")
  }, error = function(e) {
    cat("Error processing dataframe", i, ":", conditionMessage(e), "\n")
  })
}

# clip to Great Lakes basin 
gl_points = list()
for (i in seq_along(spatial_obj_list)) {
  tryCatch({
    gl_points[[i]] <- st_intersection(spatial_obj_list[[i]], gl_basin)
    
    cat("Processed dataframe", i, "\n")
  }, error = function(e) {
    cat("Error processing dataframe", i, ":", conditionMessage(e), "\n")
  })
}


# Report and Plot ----
# add rightHolder to Agnes Marion Ayre Herbarium
gl_points[[9]]$datasetName = "Royal Ontario Museum Green Plant Herbarium (TRT)"

# remove empty elements from gl_points
non_empty_gl_points <- Filter(function(x) nrow(x) > 0, gl_points)


# create the plots and store them in a list
plot_list <- lapply(non_empty_gl_points, function(points) {
  ggplot() +
    geom_sf(data = gl_basin, fill = "lightgreen") +
    geom_sf(data = gl_boundary, fill = "blue", color = NA) +
    geom_sf(data = points, color = "red", shape = 19, size = 1) +
    ggtitle(str_wrap(points$datasetName[1], width = 40)) +
    theme_void()
})

# arrange plots using grid.arrange from gridExtra
grid.arrange(grobs = plot_list, ncol = 2, nrow = ceiling(length(plot_list)/2))


# report table
info_df <- data.frame(Dataset = character(), Publisher = character(), Records = integer())

for (i in seq_along(non_empty_gl_points)) {
  if (nrow(non_empty_gl_points[[i]]) > 0) {
    name <- non_empty_gl_points[[i]]$datasetName[1]
    pub <- non_empty_gl_points[[i]]$rightsHolder[1]
    rows <- nrow(non_empty_gl_points[[i]])
    info_df <- rbind(info_df, data.frame(Dataset = name, Publisher = pub, Records = rows))
  }
}

total <- sum(info_df$Records)
info_df <- rbind(info_df, data.frame(Dataset = 'Total', Publisher = "", Records = total))

info_df %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria")


# Separate Phragmites australis Occurrences ----

# function to select for any partial matches in scientificNames columns
subset.phragmite.function = function(df) {
  df <- df %>% 
    filter(str_detect(scientificName, "Phragmites australis"))
  return(df)
}  

subset.non.phragmite.function = function(df) {
  df <- df %>% 
    filter(!str_detect(scientificName, "Phragmites australis"))
  return(df)
}

# subset each dataframe to only plant species of interest
data_without_gps_phragmites = list()
for (i in seq_along(data_without_gps)) {
  tryCatch({
    data_without_gps_phragmites[[i]] <- subset.phragmite.function(data_without_gps[[i]])
    data_without_gps[[i]] <- subset.non.phragmite.function(data_without_gps[[i]])
    cat("Processed dataframe", i, "\n")
  }, error = function(e) {
    cat("Error processing dataframe", i, ": ", conditionMessage(e), "\n")
  })
}

# subset each dataframe to only plant species of interest
gl_phragmites_points = list()
for (i in seq_along(gl_points)) {
  tryCatch({
    gl_phragmites_points[[i]] <- subset_phragmite_function(gl_points[[i]])
    gl_points[[i]] <- subset_non_phragmite_function(gl_points[[i]])
    cat("Processed dataframe", i, "\n")
  }, error = function(e) {
    cat("Error processing dataframe", i, ": ", conditionMessage(e), "\n")
  })
}


# Export to Excel Files ----

# remove empty elements from gl_points
gl_points <- Filter(function(x) nrow(x) > 0, gl_points)
gl_phragmites_points <- Filter(function(x) nrow(x) > 0, gl_phragmites_points)
data_without_gps <- Filter(function(x) nrow(x) > 0, data_without_gps)
data_without_gps_phragmites <- Filter(function(x) nrow(x) > 0, data_without_gps_phragmites)

# export to Excel
for (i in seq_along(gl_points)) {
  if (nrow(gl_points[[i]]) > 0) {
    file_name <- paste("invasive-species-data/output-", gl_points[[i]]$rightsHolder[1], ".xlsx", sep = "")
    write_xlsx(gl_points[[i]], path = file_name)
  }
}

for (i in seq_along(gl_phragmites_points)) {
  if (nrow(gl_points[[i]]) > 0) {
    file_name <- paste("phragmites-data/output-", gl_phragmites_points[[i]]$rightsHolder[1], ".xlsx", sep = "")
    write_xlsx(gl_phragmites_points[[i]], path = file_name)
  }
}

for (i in seq_along(data_without_gps)) {
  if (nrow(gl_points[[i]]) > 0) {
    file_name <- paste("invasive-species-data/no-gps-output-", data_without_gps[[i]]$rightsHolder[1], ".xlsx", sep = "")
    write_xlsx(data_without_gps[[i]], path = file_name)
  }
}

for (i in seq_along(data_without_gps_phragmites)) {
  if (nrow(gl_points[[i]]) > 0) {
    file_name <- paste("phragmites-data/no-gps-output-", data_without_gps_phragmites[[i]]$rightsHolder[1], ".xlsx", sep = "")
    write_xlsx(data_without_gps_phragmites[[i]], path = file_name)
  }
}

