---
title: "SURF Data Prep"
author: "Erika Whitney"
date: "6/2/2022"
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

THIS BRANCH USES DIFFERENT RISK REGIONS THAN ORIGINAL

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

SURF.Sed <- fread("Data/SURF_SED.csv") # 1970 - 2020, n = 150,516
#summary(SURF.Sed)

SURF.WQ <- fread("Data/SURF_water.csv") # 1925 - 2020, n = 829,527
#summary(SURF.WQ)

# Filter to start at 1995 (as requested to match other's data)

SURF.Sed <-SURF.Sed %>% 
  filter(between(Sample_date, 
        as_date("1995-01-01"),as_date("2019-12-31"))) # filter dates before transform to sf, otherwise errors arise. 

SURF.WQ <- SURF.WQ %>% 
  filter(between(Sample_date, 
        as_date("1995-01-01"),as_date("2019-12-31")))
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
```



```r
## Load new risk regions from shp file
USFE.RiskRegions.NAD <- st_read("Data/subregions/subregions.shp") %>% rename(Subregion = SUBREGION)
```

```
## Reading layer `subregions' from data source 
##   `C:\Users\Erika\Documents\GitHub\CEDENSURF-mod\Data\subregions\subregions.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 12 features and 17 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: -122.1485 ymin: 37.53355 xmax: -120.9861 ymax: 38.60452
## Geodetic CRS:  NAD83
```

```r
st_crs(USFE.RiskRegions.NAD)
```

```
## Coordinate Reference System:
##   User input: NAD83 
##   wkt:
## GEOGCRS["NAD83",
##     DATUM["North American Datum 1983",
##         ELLIPSOID["GRS 1980",6378137,298.257222101,
##             LENGTHUNIT["metre",1]]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433]],
##     CS[ellipsoidal,2],
##         AXIS["latitude",north,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433]],
##         AXIS["longitude",east,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433]],
##     ID["EPSG",4269]]
```

```r
  ## Convert from NAD83 to WGS
  USFE.RiskRegions <- st_transform(USFE.RiskRegions.NAD, "WGS84")
  st_crs(USFE.RiskRegions)
```

```
## Coordinate Reference System:
##   User input: WGS84 
##   wkt:
## GEOGCRS["WGS 84",
##     DATUM["World Geodetic System 1984",
##         ELLIPSOID["WGS 84",6378137,298.257223563,
##             LENGTHUNIT["metre",1]]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433]],
##     CS[ellipsoidal,2],
##         AXIS["geodetic latitude (Lat)",north,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433]],
##         AXIS["geodetic longitude (Lon)",east,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433]],
##     ID["EPSG",4326]]
```

```r
## Preview Risk Regions

ggplot() +
  geom_sf(data = USFE.RiskRegions, fill = NA) +
  ggtitle("Updated Risk Regions")
```

![](01_SURF_Mod_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
## Spatially query data within the project boundaries

  # SURF Sed
  SURF.Sed.sf <- st_join(SURF.Sed.sf, USFE.RiskRegions[8], left = T) %>%
    filter(!is.na(Subregion))
  
  #SURF Water
  SURF.WQ.sf <- st_join(SURF.WQ.sf, USFE.RiskRegions[8], left = T) %>%
    filter(!is.na(Subregion))
  
## Check output count (increased from previous risk regions)
  length(SURF.Sed.sf$Subregion)
```

```
## [1] 34893
```

```r
  length(SURF.WQ.sf$Subregion)
```

```
## [1] 159485
```


After temporal and spatial query, 
- there WERE (old risk regions): 35,005 records in SURF.sediment, and 183,497 records in SURF.water. 
- there ARE (new risk regions): 34,941 records in surf sediment, and 182,849 records in SURF.water.  

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

## Compare Old and New {.tabset}

### CEDEN

#### Load Data

```r
library(gh)
library(httr)
library(gitcreds)
gh::gh_whoami()
```

```
## {
##   "name": "Erika Whitney",
##   "login": "whitneyerika",
##   "html_url": "https://github.com/whitneyerika",
##   "scopes": "gist, repo, user, workflow",
##   "token": "ghp_...p1ar"
## }
```

```r
# Load Old CEDENMOD Data

tmp <- tempfile()

CEDENMod <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDEN-mod/main/Data/Output/CEDENMod_Toxicity.csv",
                 .token = gh_token(),
                 .destfile = tmp)
  
CEDENMod_OldTox <- read_csv(tmp) 

rm(tmp, CEDENMod) #Clean up to avoid overwrite issues

# Load Old RR

USFE.OLDRR <-  st_read("Data/USFE_RiskRegions_9292020/RiskRegions_DWSC_Update_9292020.shp")%>%
    st_transform(., "NAD83")
```

```
## Reading layer `RiskRegions_DWSC_Update_9292020' from data source 
##   `C:\Users\Erika\Documents\GitHub\CEDENSURF-mod\Data\USFE_RiskRegions_9292020\RiskRegions_DWSC_Update_9292020.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 6 features and 6 fields
## Geometry type: POLYGON
## Dimension:     XYZ
## Bounding box:  xmin: -122.1431 ymin: 37.62499 xmax: -121.1967 ymax: 38.58916
## z_range:       zmin: 0 zmax: 0
## Geodetic CRS:  WGS 84
```

```r
st_crs(USFE.OLDRR)
```

```
## Coordinate Reference System:
##   User input: NAD83 
##   wkt:
## GEOGCRS["NAD83",
##     DATUM["North American Datum 1983",
##         ELLIPSOID["GRS 1980",6378137,298.257222101,
##             LENGTHUNIT["metre",1]]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433]],
##     CS[ellipsoidal,2],
##         AXIS["geodetic latitude (Lat)",north,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433]],
##         AXIS["geodetic longitude (Lon)",east,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433]],
##     ID["EPSG",4269]]
```

```r
# Load New CEDENMOD Data

tmp <- tempfile()

CEDENMod <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDEN-mod/30YRS/Data/Output/CEDENMod_Toxicity_26JUN2022.csv",
                 .token = gh_token(),
                 .destfile = tmp)
  
CEDENMod_Tox <- read_csv(tmp) 

rm(tmp, CEDENMod) #Clean up


# Load NEW Risk Regions
USFE.regions <-  st_read("Data/Subregions/Subregions.shp") %>% 
  rename(Subregion = SUBREGION)
```

```
## Reading layer `subregions' from data source 
##   `C:\Users\Erika\Documents\GitHub\CEDENSURF-mod\Data\subregions\subregions.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 12 features and 17 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: -122.1485 ymin: 37.53355 xmax: -120.9861 ymax: 38.60452
## Geodetic CRS:  NAD83
```

```r
st_crs(USFE.regions) #NAD83
```

```
## Coordinate Reference System:
##   User input: NAD83 
##   wkt:
## GEOGCRS["NAD83",
##     DATUM["North American Datum 1983",
##         ELLIPSOID["GRS 1980",6378137,298.257222101,
##             LENGTHUNIT["metre",1]]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433]],
##     CS[ellipsoidal,2],
##         AXIS["latitude",north,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433]],
##         AXIS["longitude",east,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433]],
##     ID["EPSG",4269]]
```
#### Details


```r
min(as.Date(CEDENMod_OldTox$Date))
```

```
## [1] "2009-10-06"
```

```r
min(as.Date(CEDENMod_Tox$Date))
```

```
## [1] "1995-02-14"
```

#### Plot Data


```r
# Convert ToxOld table to sf
CedToxOld.sf <- st_as_sf(CEDENMod_OldTox,
      coords = c("Longitude", "Latitude"), 
                remove = F, # Keep coordinate columns
                crs = "NAD83") # Assumed NAD83 Datum
  
# Convert ToxNew table to sf
CedToxNew.sf <- st_as_sf(CEDENMod_Tox,
      coords = c("Longitude", "Latitude"), 
                remove = F, # Keep coordinate columns
                crs = "NAD83") # Assumed NAD83 Datum,
```


```r
# Map Check - Compare RR
ggplot() +
  geom_sf(data = USFE.regions, color = "blue", fill= NA)+
  geom_sf(data = USFE.OLDRR, fill = NA) +
  geom_sf(data = CedToxNew.sf, color = "orange") +
  geom_sf(data = CedToxOld.sf, )
```

![](01_SURF_Mod_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

### SURF

#### Load Data

```r
library(gh)
library(httr)
library(gitcreds)
gh::gh_whoami()
```

```
## {
##   "name": "Erika Whitney",
##   "login": "whitneyerika",
##   "html_url": "https://github.com/whitneyerika",
##   "scopes": "gist, repo, user, workflow",
##   "token": "ghp_...p1ar"
## }
```

```r
# Load Old CEDENMOD Data

tmp <- tempfile()

SURFMod <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDENSURF-mod/main/Data/Output/SURFMod_SED.csv",
                 .token = gh_token(),
                 .destfile = tmp)
  
SURFMod_OldSed <- read_csv(tmp) 

rm(tmp, SURFMod) #Clean up to avoid overwrite issues

# Load Old RR

USFE.OLDRR <-  st_read("Data/USFE_RiskRegions_9292020/RiskRegions_DWSC_Update_9292020.shp")%>%
    st_transform(., "NAD83")
```

```
## Reading layer `RiskRegions_DWSC_Update_9292020' from data source 
##   `C:\Users\Erika\Documents\GitHub\CEDENSURF-mod\Data\USFE_RiskRegions_9292020\RiskRegions_DWSC_Update_9292020.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 6 features and 6 fields
## Geometry type: POLYGON
## Dimension:     XYZ
## Bounding box:  xmin: -122.1431 ymin: 37.62499 xmax: -121.1967 ymax: 38.58916
## z_range:       zmin: 0 zmax: 0
## Geodetic CRS:  WGS 84
```

```r
st_crs(USFE.OLDRR)
```

```
## Coordinate Reference System:
##   User input: NAD83 
##   wkt:
## GEOGCRS["NAD83",
##     DATUM["North American Datum 1983",
##         ELLIPSOID["GRS 1980",6378137,298.257222101,
##             LENGTHUNIT["metre",1]]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433]],
##     CS[ellipsoidal,2],
##         AXIS["geodetic latitude (Lat)",north,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433]],
##         AXIS["geodetic longitude (Lon)",east,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433]],
##     ID["EPSG",4269]]
```

```r
# Load New CEDENMOD Data

tmp <- tempfile()

SURFMod <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDENSURF-mod/30YRS/Data/Output/SURFMod_SED.csv",
                 .token = gh_token(),
                 .destfile = tmp)
  
SURFMod_sed <- read_csv(tmp) 

rm(tmp, SURFMod) #Clean up


# Load NEW Risk Regions
USFE.regions <-  st_read("Data/Subregions/Subregions.shp") %>% 
  rename(Subregion = SUBREGION)
```

```
## Reading layer `subregions' from data source 
##   `C:\Users\Erika\Documents\GitHub\CEDENSURF-mod\Data\subregions\subregions.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 12 features and 17 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: -122.1485 ymin: 37.53355 xmax: -120.9861 ymax: 38.60452
## Geodetic CRS:  NAD83
```

```r
st_crs(USFE.regions) #NAD83
```

```
## Coordinate Reference System:
##   User input: NAD83 
##   wkt:
## GEOGCRS["NAD83",
##     DATUM["North American Datum 1983",
##         ELLIPSOID["GRS 1980",6378137,298.257222101,
##             LENGTHUNIT["metre",1]]],
##     PRIMEM["Greenwich",0,
##         ANGLEUNIT["degree",0.0174532925199433]],
##     CS[ellipsoidal,2],
##         AXIS["latitude",north,
##             ORDER[1],
##             ANGLEUNIT["degree",0.0174532925199433]],
##         AXIS["longitude",east,
##             ORDER[2],
##             ANGLEUNIT["degree",0.0174532925199433]],
##     ID["EPSG",4269]]
```

#### Prep Data


```r
# Convert Old table to sf
SURFSedOld.sf<- st_as_sf(SURFMod_OldSed,
      coords = c("Longitude", "Latitude"), 
                remove = F, # Keep coordinate columns
                crs = "NAD83") # Assumed NAD83 Datum

# Convert New table to sf
SURFSedNew.sf<- st_as_sf(SURFMod_sed,
      coords = c("Longitude", "Latitude"), 
                remove = F, # Keep coordinate columns
                crs = "NAD83") # Assumed NAD83 Datum
```

#### Plot Overlay

```r
# Map Check - Compare RR
ggplot() +
  geom_sf(data = USFE.regions, color = "blue", fill= NA)+
  geom_sf(data = USFE.OLDRR, fill = NA) +
  geom_sf(data = SURFSedNew.sf, color = "orange") +
  geom_sf(data = SURFSedOld.sf, )
```

![](01_SURF_Mod_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
