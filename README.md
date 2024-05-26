GLANSIS Canadian Herbaria Data Cleaning
Project Description:
This project involves cleaning publicly available Canadian Herbarium datasets obtained from GBIF (Global Biodiversity Information Facility). The aim is to fill in spatial gaps in invasive species data within the Great Lakes basin in Canada. The datasets are subset to include only GLANSIS (Great Lakes Aquatic Nonindigenous Species Information System) plant species, both nonindigenous and those on the watchlist, and occurrences within the Great Lakes basin. Entries missing GPS coordinates are supplemented using locality descriptions. Outputs include files containing records of invasive plant species in the Great Lakes, files indicating occurrences where GPS coordinates could not be determined, and files specific to Phragmites australis, a common reed species.

Creator:
Joseph Redinger (jredinger11@gmail.com)

Date:
24-05-2024

Libraries Used:
readxl
rgbif
tidyr
dplyr
stringr
tidygeocoder
sf
ggplot2
gridExtra
kableExtra
writexl
Instructions:
GBIF Initial Data Download:
Run this section once to download GBIF datasets.
Requires a GBIF account with credentials (username, password, and email).
Import Data:
Ensure the downloaded keys are saved in "gbif_download_keys.txt" file.
Import GBIF datasets, GLANSIS Plant Names, and Great Lakes Shapefile.
Subset: Select Only Records With GLANSIS Plant Species:
Subset each dataframe to only include GLANSIS plant species.
Add Missing GPS Coordinates:
Add Accuracy and Source columns for rows with GPS coordinates.
Fill in missing GPS coordinates using locality descriptions.
Separate Records Where GPS Cannot be Found:
Separate data into two lists: one with GPS coordinates and one without.
Select Observations Within the Great Lakes Basin:
Convert each dataframe into a spatial object.
Clip data points to the Great Lakes basin boundary.
Report and Plot:
Generate plots and report tables for datasets within the Great Lakes basin.
Separate Phragmites australis Occurrences:
Subset data to separate Phragmites australis occurrences from other species.
Export to Excel Files:
Export processed data to Excel files for further analysis.
Note:
Ensure necessary files (shapefiles, Excel sheets) are available in the specified directories.
Review the code comments for additional details and customization options.
