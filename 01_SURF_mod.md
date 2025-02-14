---
title: "SURF Data Prep"
author: "Erika Whitney"
date: "2/20/2021"
output:
  html_document:
    code_download: true
    keep_md: true
---




```r
library(data.table)
library(lubridate)
library(sf)
library(tidyverse)
```

# Intro

This markdown covers the process to restrict downloaded SURF data to the study region and time period of our project, and initial quality control required prior to integration with the CEDEN data.

**Output/Result:** SURF_sed.csv and SURF_water.csv -

<br>

### SURF Data {.tabset}

SURF Data was acquired at the [DPR SURF database web](https://www.cdpr.ca.gov/docs/emon/surfwtr/surfcont.htm) page as CSVs via FTP download on 2/17/2021:

* ftp://transfer.cdpr.ca.gov/pub/outgoing/EM/Surface/SURF_water.csv (309.8 MB)
* ftp://transfer.cdpr.ca.gov/pub/outgoing/EM/Surface/SURF_SED.csv (52.2 MB)

According to email correspondence with Dr. Xuyang Zhang at CDPR on 2/17/2021, latitude and longitude data are in WGS84.

<br>

#### Load Data & Temporal Query


```r
### Read SURF Tables and restrict to project time frame

SURF.Sed <- fread("Data/SURF_SED.csv") %>% 
  filter(between(Sample_date, 
        as_date("2009-10-01"),as_date("2019-09-30"))) # filter dates before transform to sf, otherwise errors arise. 

SURF.WQ <- fread("Data/SURF_water.csv") %>% 
  filter(between(Sample_date, 
        as_date("2009-10-01"),as_date("2019-09-30"))) # filter dates before transform to sf, otherwise errors arise.
```

<br>

#### Rename columns

We renamed columns with analogous data to match CEDEN column names.


```r
### SURF WATER

# Move units from embedded in Result column name to their own column
SURF.WQ$Unit <- "ppb"

# Rename columns with analogous data to match CEDEN column names.
SURF.WQ <- SURF.WQ %>% rename(Date = Sample_date,
          Analyte = Chemical_name, 
          Result = Concentration..ppb., 
          CollectionMethod = Sample_type, 
          StationCode = Site_code,
          StationName = Site_name,
          MDL = Method_detection_level..ppb.,
          LOQ = Level_of_quantification..ppb.)

### SURF SEDIMENT

# Move units from embedded in Result column name to their own column
SURF.Sed$Unit <- "ppb"

# Rename columns with analogous data to match CEDEN column names.
SURF.Sed <- SURF.Sed %>% rename(Date = Sample_date,
          Analyte = Chemical_name, 
          Result = Concentration..ppb., 
          CollectionMethod = Sample_type, 
          StationCode = Site_code,
          StationName = Site_name,
          MDL = Method_detection_level..ppb.,
          LOQ = Level_of_quantification..ppb.)
```

<br>

#### Spatial query

Retain only data within our project boundaries.


```r
### Transform SURF data to SF
SURF.Sed.sf <- st_as_sf(SURF.Sed, coords = c("Longitude", "Latitude"), remove = F, crs = "WGS84")

SURF.WQ.sf <- st_as_sf(SURF.WQ, coords = c( "Longitude", "Latitude"), remove = F, crs = "WGS84")

### Load Risk Regions from GitHub CEDEN repository (change if moved)

USFE.RiskRegions.z <- "https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/USFE_RiskRegions_9292020.zip"

unzip_shape <- function(InputShapeZip){
  dl.temp <- tempfile() # Create local temp file for zipped shapefile
  dl.temp2 <- tempfile() # Create a second local temp file to store unzipped shapefile
  download.file(InputShapeZip, dl.temp, quiet=T) # Downloads zip file from InputShape
  unzip(zip = dl.temp, exdir = dl.temp2) # Unzips zip file
  shapefile.out <-list.files(dl.temp2, pattern = ".shp$",full.names=TRUE) # stores file path of files with .shp ext in dl.temp2
  sf::st_read(shapefile.out) # Reads shapefile as sf object
}

USFE.RiskRegions <- unzip_shape(USFE.RiskRegions.z) # CRS is WGS 84
```

```
## Reading layer `RiskRegions_DWSC_Update_9292020' from data source `C:\Users\Erika\AppData\Local\Temp\Rtmp0Gj1Pv\file21e075bd7f58\RiskRegions_DWSC_Update_9292020.shp' using driver `ESRI Shapefile'
## Simple feature collection with 6 features and 6 fields
## geometry type:  POLYGON
## dimension:      XYZ
## bbox:           xmin: -122.1431 ymin: 37.62499 xmax: -121.1967 ymax: 38.58916
## z_range:        zmin: 0 zmax: 0
## geographic CRS: WGS 84
```

```r
#### Spatially query data within the project boundaries

# SURF Sed
SURF.Sed.sf <- st_join(SURF.Sed.sf, USFE.RiskRegions[1], left = T) %>%
  filter(!is.na(Subregion))

#SURF Water
SURF.WQ.sf <- st_join(SURF.WQ.sf, USFE.RiskRegions[1], left = T) %>%
  filter(!is.na(Subregion))
```

After temporal and spatial query, there are 35,346 records in SURF.sediment, and 91,021 records in SURF.water

#### Transform to NAD83

Transform the sf to NAD83, and store the new coordinates in another column. The difference between WGS83 and NAD83 does not appear to have changed any coordinates

```r
#### Transform to match CEDEN coordinate system

# Transform data to NAD83 (same coordinate system as CEDENSURF)
SURF.Sed.sf <- SURF.Sed.sf %>% 
  st_transform(., "NAD83") %>% 
  mutate(Datum = "NAD83")

SURF.WQ.sf <- SURF.WQ.sf %>% 
  st_transform(., "NAD83")%>% 
  mutate(Datum = "NAD83")
```

We thought about redefining the coordinates (latitude and longitude) so that they would reflect the transformed coordinate system. Using the code below, I found that none of the coordinates changed.


```r
st_crs(SURF.Sed.sf)

# Store coordinates for that projection in new columns
Sed.check <- SURF.Sed.sf %>% 
  dplyr::mutate(long_NAD83 = sf::st_coordinates(.)[,1],
                lat_NAD83 = sf::st_coordinates(.)[,2])

WQ.check <- SURF.WQ.sf %>% 
  dplyr::mutate(long_NAD83 = sf::st_coordinates(.)[,1],
                lat_NAD83 = sf::st_coordinates(.)[,2])

Check <- Sed.check %>% st_set_geometry(NULL) %>%
  distinct(StationName, .keep_all= TRUE) %>% 
  select(Latitude, Longitude, lat_NAD83,long_NAD83, StationName)

subset(Check, Longitude != long_NAD83) # no results where latitudes (or long) don't match

Check <- WQ.check %>% st_set_geometry(NULL) %>%
  distinct(StationName, .keep_all= TRUE) %>% 
  select(Latitude, Longitude, lat_NAD83,long_NAD83, StationName)

subset(Check, Latitude != lat_NAD83) # no results where latitudes (or long) don't match
```


#### Save modified data


```r
# write.csv is confused by the comma in "geometry" and will separate them to two columns. write_csv is faster, and can handle commas in columns correctly

# Export SURF Tables
write_csv(SURF.WQ.sf, "Data/Output/SURFMod_water.csv") # Note: coerces empty data fields to NA

write_csv(SURF.Sed.sf, "Data/Output/SURFMod_SED.csv") # Note: coerces empty data fields to NA
```

