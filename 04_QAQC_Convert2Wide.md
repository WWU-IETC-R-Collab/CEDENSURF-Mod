---
title: "Conversions"
author: "Erika W"
date: "6/25/2021"
output:
  html_document:
    code_download: true
    keep_md: true
    toc: true
    toc_float:
      toc_collapsed: true
    toc_depth: 3
    theme: lumen
---





```r
rm(list = ls())
library(data.table)
library(lubridate)
library(sf)
library(tidyverse)
```


```r
# packages b/c accessing private repo

library(httr)
library(tidyverse)
library(gh)
library(gitcreds)
```

# Introduction

Because NETICA requires the nodes to be columns, this data needs to be transformed to wide format.

1. Start with the integrated, modified CEDEN and SURF datasets that have had the conceptual model categories appended

2. Convert units to be consistent within each analyte category (node)

3. Summarize into wide format


This markdown/ branch utilizes a wider date range than the original and different risk regions. To compare:

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
# Load Old Data

tmp <- tempfile()

Limited <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDENSURF-mod/main/Data/Output/CEDENSURF_Limited_FixedUnits.csv",
                 .token = gh_token(),
                 .destfile = tmp)
  
Limited.old <- read_csv(tmp) 

rm(tmp, Limited) #Clean up to avoid overwrite issues

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
# Load New Data

tmp <- tempfile()

Limited <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDENSURF-mod/30YRS/Data/Output/CEDENSURF_Limited_FixedUnits.csv",
                 .token = gh_token(),
                 .destfile = tmp)
  
Limited.new <- read_csv(tmp) 

rm(tmp, Limited) #Clean up


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

#### Summary

**Old Limited Dataset**
- 48559records
- Dates: 2009-10-06 to 2019-09-26

**New Limited Dataset**
- 89870records
- Dates: 1995-01-03 to 2019-12-19


#### Prep Data


```r
# Convert Old table to sf
Limited.old <- st_as_sf(Limited.old,
      coords = c("Longitude", "Latitude"), 
                remove = F, # Keep coordinate columns
                crs = "NAD83") # Assumed NAD83 Datum

# Convert New table to sf
Limited.new<- st_as_sf(Limited.new,
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
  geom_sf(data = Limited.new, color = "orange") +
  geom_sf(data = Limited.old, )
```

![](04_QAQC_Convert2Wide_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


# Unit Conversions

## 1. Load Data


```r
# Load CEDEN Data

tmp <- tempfile()

CEDENSURF <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDENSURF-mod/30YRS/Data/Output/CEDENSURF_Limited.csv",
                   .token = gh_token(), 
                   .destfile = tmp)

CEDENSURF <- read_csv(tmp)
```

```
## Rows: 94814 Columns: 34
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (23): Agency, Analyte, CollectionMethod, County, Data.source, Datum, ge...
## dbl  (10): Latitude, Longitude, LOQ, MDL, rb_number, Record_id, Result, RL, ...
## date  (1): Date
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

Preview number of subregions each analyte was measured in:

```r
# Check coverage for each analyte. 
CEDENSURF %>%
  group_by(Analyte,Matrix) %>%
  summarise(coverage = (length(unique(Subregion))),
            n = n()) 
```

```
## `summarise()` has grouped output by 'Analyte'. You can override using the `.groups` argument.
```

```
## # A tibble: 108 x 4
## # Groups:   Analyte [65]
##    Analyte            Matrix   coverage     n
##    <chr>              <chr>       <int> <int>
##  1 atrazine           sediment        5   249
##  2 atrazine           water           9  2796
##  3 atrazine degradate water           2   211
##  4 bifenthrin         sediment       10   478
##  5 bifenthrin         water           8  1799
##  6 cadmium            sediment        8   205
##  7 cadmium            water          10  1059
##  8 chlorpyrifos       sediment        5   378
##  9 chlorpyrifos       water          10  4252
## 10 clothianidin       water           8   699
## # ... with 98 more rows
```

We realized there are 213 records with negative concentrations. This could be an error of the researchers calibration equations, or otherwise, but at this point the safest assumption would be to assign those 0-results. 


```r
# How many records have <0 results?

CEDENSURF %>% filter(Result < 0) %>% nrow(.) # 213 records out of 44,000 with negative values
```

```
## [1] 976
```

```r
# Create subset of just those negative results

Negative <- CEDENSURF %>% filter(Result < 0)

# summary(Negative$Result) # preview range and average

# Set all negative results to 0

Negative$Result <- 0

# Remove negative results from original dataframe

CEDENSURF <- CEDENSURF %>% filter(!Result < 0) 

# Bind back on records with negatives converted to 0

CEDENSURF<- rbind(CEDENSURF, Negative)
```

<br/>

## 2. Convert Units & Names

Units should be consistent within each analyte-Matrix combination. Initially, they were not. I separated the dataframe into categories of the conceptual model, and worked within those WideSubsets to unify units of each analyte-matrix combination.

Because Netica cannot allow spaces, -, /, or other symbols in the column names, analyte names must also be converted to usable formats.


```r
CEDENSURF %>% group_by(Analyte, Matrix) %>%
  summarize(n_unit = n_distinct(Unit)) %>% head(.)
```

```
## `summarise()` has grouped output by 'Analyte'. You can override using the `.groups` argument.
```

```
## # A tibble: 6 x 3
## # Groups:   Analyte [4]
##   Analyte            Matrix   n_unit
##   <chr>              <chr>     <int>
## 1 atrazine           sediment      2
## 2 atrazine           water         3
## 3 atrazine degradate water         1
## 4 bifenthrin         sediment      3
## 5 bifenthrin         water         3
## 6 cadmium            sediment      2
```

### WQP  {.tabset}

Few discrepancies in units between analytes in each category:

1. Phosphorous; 33/900 records had mg/Kg dw rather than mg/L

2. Turbidity; One analyte is specifically turbidity, units are NTU. Some variables that may also indicate turbidity had different units, but are possibly not comparable.

No conversions were made for WQP Parameters, though some analytes were omitted.


```r
WQP <- CEDENSURF %>% filter(SelectList == "WQP") 
```

#### Univariate - names and units as expected

Temp - *C
pH - no units

#### Oxygen - mg/L

**water** (3504 records) mg/L
**sediment** (281 records) mg/L

89 water records were converted from %DO, assuming an average water temperature of 20C.
    *Oxygen mg/L = (% Saturation)/10.995*
    https://www.waterontheweb.org/under/waterquality/dosatcalc.html


```r
# Oxygen - 
# Water Units = 3,416 records in mg/L and 89 in %
# Sediment Unit = 281 records mg/L

# Convert % to mg/L

WQP$Result[WQP$Analyte == "oxygen" & WQP$Unit == "%"] <- WQP$Result[WQP$Analyte == "oxygen" & WQP$Unit == "%"] /10.995

# Correct units

WQP$Unit[WQP$Analyte == "oxygen"] <- "mg/L"

# Check

WQP %>% filter(Analyte == "oxygen") %>% filter(Matrix == "water") %>% distinct(Unit)

WQP %>% filter(Analyte == "oxygen") %>% filter(Matrix == "water") %>% nrow(.)
```

#### Nitrogen

*Water* - mg/L
nitrate as n (379 records)
nitrite as n (444 records)

Total N:
nitrogen (709 records)
nitrate + nitrite as n (844 records)

*Sediment* - % dw
nitrogen (33 records)


```r
# Nitrate - Unit = mg/L (nitrate as n)

WQP %>% filter(Analyte== "nitrate as n") %>% distinct(Unit)
WQP$Analyte[WQP$Analyte == "nitrate as n"] <- "nitrate" #rename to omit spaces

# Nitrite  - Unit = mg/L (nitrite as n) 

WQP %>% filter(Analyte== "nitrite as n") %>% distinct(Unit)
WQP$Analyte[WQP$Analyte == "nitrite as n"] <- "nitrite"

# Combined as Nitrogen - 748 records; mg/L and %dw

    # Nitrogen/sediment is in % dw

    WQP %>% filter(grepl("nitro", Analyte)) %>% 
            filter(Matrix == 'sediment') %>% distinct(Unit)

    # Nitrogen/water is in mg/L
    
    WQP %>% filter(grepl("nitro", Analyte)) %>% 
            filter(Matrix == 'water') %>% distinct(Unit)
    
# Combined as Nitra + Nitrite - 844 records; mg/L (all water)

    WQP %>% filter(Analyte == "nitrate + nitrite as n") %>% 
            filter(Matrix == 'water') %>% distinct(Unit)
    # rename
    WQP$Analyte[WQP$Analyte == "nitrate + nitrite as n"] <- "nitrogen" 
```

#### Phosphorous 

**water** (819 records) mg/L
**sediment** (27 records) mg/Kg dw


```r
# Phosphorous ("phosphorous as p")

# Water - 819 records (mg/L)

WQP %>% filter(grepl("phos", Analyte)) %>% 
            filter(Matrix == 'water') %>% distinct(Unit)

# Sediment - 27 records (mg/Kg dw)

WQP %>% filter(grepl("phos", Analyte)) %>% 
            filter(Matrix == 'sediment') %>% distinct(Unit)

# Correct Name

WQP$Analyte[WQP$Analyte == "phosphorus as p"] <- "phosphorus"
```

#### Turbidity - NTU

Units differ between three analytes that might refer to turbidity:
    # 1,600 turbidity (NTU), 11 settleable solids (mL/L/hr), 
    # 94 suspended sediment concentration (mg/L)
    
I chose to remove all but Turbidity from our dataset.


```r
# Turbidity

    ## Units differ between three analytes that might refer to turbidity:
    # 1,600 turbidity (NTU), 11 settleable solids (mL/L/hr), 
    # 94 suspended sediment concentration (mg/L)
    
    WQP %>% filter(grepl("turb", Analyte)) %>% distinct(Unit)
    
    ## Chose to remove the other two from our DF
    WQP<- WQP %>% filter(!grepl("settle", Analyte)) %>%
                  filter(!grepl("suspend", Analyte))
```

#### Salinity - ppt

Converted all to ppt


```r
# Salinity
  # Check units:
  # WQP %>% filter(grepl("salinity", Analyte))%>%distinct(Unit)

    # ppt and psu are equivalent measures. Convert all to ppt
    
    # Run conversion on subset of data
    Sal <- WQP %>% filter(Analyte == "salinity") %>% 
            mutate(Unit = "ppt") 
    
    # Remove that subset from Main df
    WQP<- WQP %>% filter(!Analyte == "salinity")
    
    # Bind converted back to Main df
    WQP <- rbind(WQP, Sal)
```

#### ElectricalConductivity

Units are in umhos/cm and in uS/cm

A mho per meter (mho/m) is an older unit of electrical conductivity (also known as specific conductance). The mho is the reciprocal of the ohm. Though siemens was introduced in the late 1970s, this unit can still be found in some old measurement instruments. 1 mho/m = 1 S/m.

1 mho/m = 1 S/m

Therefore umhos/cm and uS/cm are equivalent units

```r
# Correct name for all units (equivalent)
WQP$Unit[WQP$Analyte == "electricalconductivity"] <- "uS/cm"
```

#### **WQP Result**


```r
WQP %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```
## # A tibble: 20 x 5
## # Groups:   Analyte, Matrix [19]
##    Analyte                Matrix   Unit         n      mean
##    <chr>                  <chr>    <chr>    <int>     <dbl>
##  1 electricalconductivity sediment uS/cm        2  462.    
##  2 electricalconductivity water    uS/cm      559  433.    
##  3 nitrate                water    mg/L       723    0.444 
##  4 nitrite                water    mg/L      1093    0.0298
##  5 nitrogen               sediment % dw       159    0.0890
##  6 nitrogen               water    mg/L      2742    1.05  
##  7 oxygen                 sediment mg/L       360    7.46  
##  8 oxygen                 water    mg/L      6401    8.17  
##  9 ph                     sediment none       504    7.66  
## 10 ph                     water    none      6730    7.69  
## 11 phosphorus             sediment mg/Kg dw    18  378.    
## 12 phosphorus             water    mg/L      1536    0.219 
## 13 phosphorus             water    ug/L        53  510.    
## 14 salinity               sediment ppt         57   18.9   
## 15 salinity               water    ppt        612    5.25  
## 16 sodium                 sediment mg/Kg dw    18 3910.    
## 17 sodium                 water    mg/L       147  136.    
## 18 temperature            sediment Deg C      234   22.1   
## 19 temperature            water    Deg C     6443   18.7   
## 20 turbidity              water    NTU       3117   30.6
```


```r
Wide.WQP.Waterdf <- WQP %>% filter(!Matrix == "sediment") %>%
  group_by(Date, Latitude, Longitude, Analyte, Matrix) %>%
  summarize(Subregion = first(Subregion),
            Mean = mean(Result, na.rm = T)) %>%
  pivot_wider(names_from = Analyte,
              names_repair = "check_unique",
              values_from = Mean) # Values to fill columns
```

```
## `summarise()` has grouped output by 'Date', 'Latitude', 'Longitude', 'Analyte'. You can override using the `.groups` argument.
```



```r
write.csv(x = Wide.WQP.Waterdf , file = "Data/Output/WideSubsets/WQP.Wide.water.csv", 
          row.names = F)
```

<br>

### Metals  {.tabset}

Metals were measured in ug/L and mg/Kg dw. Mercury also had few measures in ng/L and ug/Kg - these values were divided by 1000 to convert to ug/L and mg/Kg. 

**water**
mercury ug/L 
selenium (208 records) mg/L

**sediment**

mercury (mg/Kg dw)
selenium (38 records) mg/Kg dw


```r
Metal <- CEDENSURF %>% filter(SelectList == "Metal")

# Cadmium
        # Convert mg to ug
          Metal$Result[
            Metal$Analyte == "cadmium" & 
              Metal$Unit == "mg/Kg dw"] <- Metal$Result[
              Metal$Analyte == "cadmium" & Metal$Unit == "mg/Kg dw"]*1000
          
        # Correct units
          Metal$Unit[Metal$Analyte == "cadmium" & 
                       Metal$Matrix == "sediment"] <- "ug/Kg dw"

# Copper
        # Convert mg to ug
          Metal$Result[
            Metal$Analyte == "copper" & 
              Metal$Unit == "mg/Kg dw"] <- Metal$Result[
              Metal$Analyte == "copper" & Metal$Unit == "mg/Kg dw"]*1000
          
        # Correct units
          Metal$Unit[Metal$Analyte == "copper" & 
                       Metal$Matrix == "sediment"] <- "ug/Kg dw"
# Selenium

    # Water - 208 records, ug/L
    Metal %>% filter(grepl("selenium", Analyte))  %>% 
              filter(Matrix == 'water') %>% distinct(Unit)

    # Sediment - 38 records, mg/Kg dw
    Metal %>% filter(grepl("selenium", Analyte))  %>% 
               filter(Matrix == 'sediment') %>% distinct(Unit)
    
     # Convert mg to ug
          Metal$Result[
            Metal$Analyte == "selenium" & 
              Metal$Unit == "mg/Kg dw"] <- Metal$Result[
              Metal$Analyte == "selenium" & Metal$Unit == "mg/Kg dw"]*1000
          
        # Correct units
          Metal$Unit[Metal$Analyte == "selenium" & 
                       Metal$Matrix == "sediment"] <- "ug/Kg dw"
          

# Mercury

    # Water - ug/L & ng/L
    
    Metal %>% filter(grepl("mercury", Analyte))%>% 
              filter(Matrix == 'water') %>% distinct(Unit)
    
        # Convert ng to ug
          Metal$Result[
            Metal$Analyte == "mercury" & 
              Metal$Unit == "ng/L"] <- Metal$Result[
              Metal$Analyte == "mercury" & Metal$Unit == "ng/L"]/1000
          
        # Correct units
          Metal$Unit[Metal$Analyte == "mercury" & 
                       Metal$Matrix == "water"] <- "ug/L"
    
          
    # Sediment - (19) ug/Kg dw	& (57) mg/Kg dw
    
    Metal %>% filter(grepl("mercury", Analyte))%>% 
              filter(Matrix == 'sediment') %>% distinct(Unit)

            # Convert mg to ug
              Metal$Result[
                Metal$Analyte == "mercury" & 
                  Metal$Unit == "mg/Kg dw"] <- Metal$Result[
                  Metal$Analyte == "mercury" & Metal$Unit == "mg/Kg dw"]*1000
              
            # Correct units
              Metal$Unit[Metal$Analyte == "mercury" &
                           Metal$Matrix == "sediment"] <- "ug/Kg dw"
```


#### **Metal Result**


```r
Metal %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = round(mean(Result),3))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```
## # A tibble: 8 x 5
## # Groups:   Analyte, Matrix [8]
##   Analyte  Matrix   Unit         n      mean
##   <chr>    <chr>    <chr>    <int>     <dbl>
## 1 cadmium  sediment ug/Kg dw   205   259.   
## 2 cadmium  water    ug/L      1059     0.02 
## 3 copper   sediment ug/Kg dw   209 38732.   
## 4 copper   water    ug/L      1470     3.78 
## 5 mercury  sediment ug/Kg dw   236   109.   
## 6 mercury  water    ug/L       996     0.003
## 7 selenium sediment ug/Kg dw   190   213.   
## 8 selenium water    ug/L      1433     0.689
```



```r
Wide.Metal.Waterdf <- Metal %>% filter(!Matrix == "sediment") %>%
  group_by(Date, Latitude, Longitude, Analyte, Matrix) %>%
  summarize(Subregion = first(Subregion),
            Mean = mean(Result, na.rm = T)) %>%
  pivot_wider(names_from = Analyte,
              names_repair = "check_unique",
              values_from = Mean) # Values to fill columns
```

```
## `summarise()` has grouped output by 'Date', 'Latitude', 'Longitude', 'Analyte'. You can override using the `.groups` argument.
```

```r
write.csv(x = Wide.Metal.Waterdf , file = "Data/Output/WideSubsets/Metal.Wide.water.csv", 
          row.names = F)
```

<br>

### Organophosphates  {.tabset}


```r
OrganoP <- CEDENSURF %>% filter(SelectList == "OrganoP")
```

#### chlorpyrifos - ppb


```r
# chlorpyrifos

  # Sediment:	ppb	331	, ng/g dw	71	(equivalent units)
    OrganoP$Unit[OrganoP$Analyte == "chlorpyrifos" &
                           OrganoP$Matrix == "sediment"] <- "ppb"

  # Water:	ng/L	220, pg/L	5, ppb	1275	
      # ppb = pg / (1000*1000)
      # ppb = ng/L / 1000
    
      # Convert ng/L to ppb
        OrganoP$Result[OrganoP$Analyte == "chlorpyrifos" &
                  OrganoP$Unit == "ng/L"] <- OrganoP$Result[
                    OrganoP$Analyte == "chlorpyrifos" &
                    OrganoP$Unit == "ng/L"] /1000
      # Convert pg/L to ppb
            OrganoP$Result[OrganoP$Analyte == "chlorpyrifos" &
                  OrganoP$Unit == "pg/L"] <- OrganoP$Result[
                    OrganoP$Analyte == "chlorpyrifos" &
                    OrganoP$Unit == "pg/L"] /(1000*1000)
      # Correct units
       OrganoP$Unit[OrganoP$Analyte == "chlorpyrifos" &
                    OrganoP$Matrix == "water"] <- "ppb"
       
OrganoP %>% filter(Analyte== "chlorpyrifos") %>% distinct(Unit)
```

#### diazinon - ppb

diazinon (2239 records) ppb

diazinon degradate (95 records) ppb

diazinon oxon (14 records) ppb

diazoxon (234 records) ppb

When diazinon enters the body, it is oxidatively decomposed to diazoxon, an organophosphate compound that is much more poisonous than diazinon; it mainly causes the inhibition of AChE.

All three analytes (diazinon oxon, diazoxon, and diazinon degradate) refer to this oxidized degradate, and were therefore renamed for consistency to "diazoxon"


```r
# diazinon

  # Sediment:	ppb	and ng/g dw	(equivalent units)
    OrganoP$Unit[OrganoP$Analyte == "diazinon" &
                           OrganoP$Matrix == "sediment"] <- "ppb"

  # Water:	pg/L, ng/L, ug/L, ppb
    
      # ppb = pg / (1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/L
    
      # Convert ng/L to ppb
        OrganoP$Result[OrganoP$Analyte == "diazinon" &
                  OrganoP$Unit == "ng/L"] <- OrganoP$Result[
                    OrganoP$Analyte == "diazinon" &
                    OrganoP$Unit == "ng/L"] /1000
      # Convert pg/L to ppb
        OrganoP$Result[OrganoP$Analyte == "diazinon" &
              OrganoP$Unit == "pg/L"] <- OrganoP$Result[
                OrganoP$Analyte == "diazinon" &
                OrganoP$Unit == "pg/L"] /(1000*1000)
      # Correct units
         OrganoP$Unit[OrganoP$Analyte == "diazinon" &
                      OrganoP$Matrix == "water"] <- "ppb"
```


```r
# Diazoxon

    # Assess coverage
    OrganoP %>% filter(Analyte == "diazoxon") %>%
      distinct(Subregion) # coverage in 3 regions
    
    # Convert ng/L to ppb
      OrganoP$Result[OrganoP$Analyte == "diazoxon" &
                OrganoP$Unit == "ng/L"] <- OrganoP$Result[
                  OrganoP$Analyte == "diazoxon" &
                  OrganoP$Unit == "ng/L"] /1000
    # Correct units
      OrganoP$Unit[OrganoP$Analyte == "diazoxon"] <- "ppb"

# Diazinon degradate
      
    # Assess coverage
    OrganoP %>% filter(Analyte == "diazinon degradate") %>%
      distinct(Subregion) # all 95 records in Sacramento river.
    # Units
    OrganoP %>% filter(Analyte == "diazinon degradate") %>%
      distinct(Unit) # all 95 records in Sacramento river.
    # Rename
    OrganoP$Analyte[OrganoP$Analyte == "diazinon degradate"] <-
      "diazoxon" 

# Diazinon oxon           
    
      # Convert ng/L to ppb 
      OrganoP$Result[OrganoP$Analyte == "diazinon oxon" &
                OrganoP$Unit == "ng/L"] <- OrganoP$Result[
                  OrganoP$Analyte == "diazinon oxon" &
                  OrganoP$Unit == "ng/L"] /1000
      
    # Correct units
      OrganoP$Unit[OrganoP$Analyte == "diazinon oxon"] <- "ppb"
      
    # Rename
      OrganoP$Analyte[OrganoP$Analyte == "diazinon oxon"] <-
        "diazoxon"
```

#### malathion - ppb


```r
# malathion

  # Sediment:	ppb	and ng/g dw	(equivalent units)
    OrganoP$Unit[OrganoP$Analyte == "malathion" &
                           OrganoP$Matrix == "sediment"] <- "ppb"

  # Water: ng/L, ug/L, ppb
    
      # ppb = ng/L / 1000
      # ppb = ug/L
    
      # Convert ng/L to ppb
        OrganoP$Result[OrganoP$Analyte == "malathion" &
                  OrganoP$Unit == "ng/L"] <- OrganoP$Result[
                    OrganoP$Analyte == "malathion" &
                    OrganoP$Unit == "ng/L"] /1000
      
      # Correct units
         OrganoP$Unit[OrganoP$Analyte == "malathion" &
                      OrganoP$Matrix == "water"] <- "ppb"

OrganoP %>% filter(Analyte== "malathion") %>% distinct(Unit)
```

```
## # A tibble: 1 x 1
##   Unit 
##   <chr>
## 1 ppb
```


```r
# dichlorvos

  # Sediment:	ng/g = ppb(equivalent units) - 24
    OrganoP$Unit[OrganoP$Analyte == "dichlorvos" &
                           OrganoP$Matrix == "sediment"] <- "ppb"

  # Water:	ug/L, ng/L	
      # ppb = pg / (1000*1000)
      # ppb = ng/L / 1000
    
      # Convert ng/L to ppb
        OrganoP$Result[OrganoP$Analyte == "dichlorvos" &
                  OrganoP$Unit == "ng/L"] <- OrganoP$Result[
                    OrganoP$Analyte == "dichlorvos" &
                    OrganoP$Unit == "ng/L"] /1000
        
      # Correct units
       OrganoP$Unit[OrganoP$Analyte == "dichlorvos" &
                    OrganoP$Matrix == "water"] <- "ppb"
       
OrganoP %>% filter(Analyte== "dichlorvos") %>% distinct(Unit)
```


```r
# phorate

  # Sediment:	ppb & ng/g dw	(equivalent units)
    OrganoP$Unit[OrganoP$Analyte == "phorate" &
                           OrganoP$Matrix == "sediment"] <- "ppb"

  # Water:	ng/L	220, pg/L	5, ppb	1275	
      # ppb = pg / (1000*1000)
      # ppb = ng/L / 1000
    
      # Convert ng/L to ppb
        OrganoP$Result[OrganoP$Analyte == "phorate" &
                  OrganoP$Unit == "ng/L"] <- OrganoP$Result[
                    OrganoP$Analyte == "phorate" &
                    OrganoP$Unit == "ng/L"] /1000
      # Convert pg/L to ppb
            OrganoP$Result[OrganoP$Analyte == "phorate" &
                  OrganoP$Unit == "pg/L"] <- OrganoP$Result[
                    OrganoP$Analyte == "phorate" &
                    OrganoP$Unit == "pg/L"] /(1000*1000)
      # Correct units
       OrganoP$Unit[OrganoP$Analyte == "phorate" &
                    OrganoP$Matrix == "water"] <- "ppb"
       
OrganoP %>% filter(Analyte== "phorate") %>% distinct(Unit)

OrganoP %>% filter(Analyte == "phorate") %>%
  group_by(Subregion, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Subregion', 'Matrix'. You can override using the `.groups` argument.
```



#### **OrganoP Result**

```r
OrganoP %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```
## # A tibble: 12 x 5
## # Groups:   Analyte, Matrix [12]
##    Analyte      Matrix   Unit      n    mean
##    <chr>        <chr>    <chr> <int>   <dbl>
##  1 chlorpyrifos sediment ppb     378 0.210  
##  2 chlorpyrifos water    ppb    4252 0.0110 
##  3 diazinon     sediment ppb     327 0      
##  4 diazinon     water    ppb    3904 0.0358 
##  5 diazoxon     sediment ppb      48 0      
##  6 diazoxon     water    ppb     448 0      
##  7 dichlorvos   sediment ppb      13 0      
##  8 dichlorvos   water    ppb     711 0.00398
##  9 malathion    sediment ppb     296 0      
## 10 malathion    water    ppb    3241 0.00411
## 11 phorate      sediment ppb      31 0      
## 12 phorate      water    ppb    2482 0.00269
```


```r
Wide.OrganoP.Waterdf <- OrganoP %>% filter(!Matrix == "sediment") %>%
  group_by(Date, Latitude, Longitude, Analyte, Matrix) %>%
  summarize(Subregion = first(Subregion),
            Mean = mean(Result, na.rm = T)) %>%
  pivot_wider(names_from = Analyte,
              names_repair = "check_unique",
              values_from = Mean) # Values to fill columns
```

```
## `summarise()` has grouped output by 'Date', 'Latitude', 'Longitude', 'Analyte'. You can override using the `.groups` argument.
```

```r
write.csv(x = Wide.OrganoP.Waterdf , file = "Data/Output/WideSubsets/OrganoP.Wide.water.csv", 
          row.names = F)
```

<br>

### Pyrethroids  {.tabset}


```r
Pyre <- CEDENSURF %>% filter(SelectList == "Pyrethroids")

Pyre %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```
## # A tibble: 28 x 5
## # Groups:   Analyte, Matrix [12]
##    Analyte    Matrix   Unit         n     mean
##    <chr>      <chr>    <chr>    <int>    <dbl>
##  1 bifenthrin sediment ng/g dw     80 7.00    
##  2 bifenthrin sediment ppb        366 2.66    
##  3 bifenthrin sediment ug/Kg dw    32 0.126   
##  4 bifenthrin water    ng/L       304 0.414   
##  5 bifenthrin water    ppb       1073 0.000698
##  6 bifenthrin water    ug/L       422 0.000766
##  7 cyfluthrin sediment ng/g dw     80 0.0148  
##  8 cyfluthrin sediment ppb        369 0.314   
##  9 cyfluthrin sediment ug/Kg dw    36 0       
## 10 cyfluthrin water    ng/L       274 0.0485  
## # ... with 18 more rows
```

#### cyfluthrin - ppb


```r
# cyfluthrin

  # Sediment:	ppb	and ng/g dw	(equivalent units)
    Pyre$Unit[Pyre$Analyte == "cyfluthrin" &
              Pyre$Matrix == "sediment"] <- "ppb"

  # Water: pg/L, ng/L, ug/L, ppb
      # ppb = pg/L /(1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/L
      
      # Convert pg/L to ppb
        Pyre$Result[Pyre$Analyte == "cyfluthrin" &
              Pyre$Unit == "pg/L"] <- Pyre$Result[
                Pyre$Analyte == "cyfluthrin" &
                Pyre$Unit == "pg/L"] /(1000*1000)
        
      # Convert ng/L to ppb
        Pyre$Result[Pyre$Analyte == "cyfluthrin" &
                  Pyre$Unit == "ng/L"] <- Pyre$Result[
                    Pyre$Analyte == "cyfluthrin" &
                    Pyre$Unit == "ng/L"] /1000
      
      # Correct units
         Pyre$Unit[Pyre$Analyte == "cyfluthrin" &
                      Pyre$Matrix == "water"] <- "ppb"

Pyre %>% filter(Analyte== "cyfluthrin") %>% distinct(Unit)
```
#### bifenthrin


```r
# bifenthrin

  # Sediment:	ug/Kg dw, ppb, ng/g dw (equivalent units)
    Pyre$Unit[Pyre$Analyte == "bifenthrin" &
              Pyre$Matrix == "sediment"] <- "ppb"

  # Water: pg/L, ng/L, ug/L, ppb
      # ppb = pg/L /(1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/L
      
      # Convert pg/L to ppb
        Pyre$Result[Pyre$Analyte == "bifenthrin" &
              Pyre$Unit == "pg/L"] <- Pyre$Result[
                Pyre$Analyte == "bifenthrin" &
                Pyre$Unit == "pg/L"] /(1000*1000)
        
      # Convert ng/L to ppb
        Pyre$Result[Pyre$Analyte == "bifenthrin" &
                  Pyre$Unit == "ng/L"] <- Pyre$Result[
                    Pyre$Analyte == "bifenthrin" &
                    Pyre$Unit == "ng/L"] /1000
      
      # Correct units
         Pyre$Unit[Pyre$Analyte == "bifenthrin" &
                      Pyre$Matrix == "water"] <- "ppb"

Pyre %>% filter(Analyte== "bifenthrin") %>% distinct(Unit)
```

#### esfenvalerate

In the analyte list, there are "esfenvalerate", "esfenvalerate/fenvalerate", "esfenvalerate/fenvalerate-1",
"esfenvalerate/fenvalerate-2"

Fenvalerate is a synthetic pyrethroid insecticide. It is a mixture of four optical isomers which have different insecticidal activities. 

Esfenvalerate, the 2-S alpha (or SS) configuration of fenvalerate, is the most insecticidally active isomer. Fenvalerate consists of about 23% of this isomer.

MM fenvalerate = MM esfenvalerate = 419.91 g/mol

For our purposes, it would be appropriate to rename all of these to esfenvalerate, since by definition the isomer always contains 23% of the fenvalerate isomer. 


```r
# esfenvalerate

  # Sediment:	ppb	and ng/g dw, and ug/kg	(equivalent units)
    Pyre$Unit[Pyre$Analyte == "esfenvalerate" &
                           Pyre$Matrix == "sediment"] <- "ppb"

  # Water: ng/L, ug/L, ppb
      # ppb = ng/L / 1000
      # ppb = ug/L
        
      # Convert ng/L to ppb
        Pyre$Result[Pyre$Analyte == "esfenvalerate" &
                  Pyre$Unit == "ng/L"] <- Pyre$Result[
                    Pyre$Analyte == "esfenvalerate" &
                    Pyre$Unit == "ng/L"] /1000
      
      # Correct units
         Pyre$Unit[Pyre$Analyte == "esfenvalerate" &
                      Pyre$Matrix == "water"] <- "ppb"

# esfenvalerate/fenvalerate

  # Sediment:	ng/g dw	and	ug/Kg dw (equivalent units)
    Pyre$Unit[Pyre$Analyte == "esfenvalerate/fenvalerate" &
                           Pyre$Matrix == "sediment"] <- "ppb"

  # Water: pg/L, ng/L, ug/L, ppb
      # ppb = pg/L /(1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/L
      
      # Convert pg/L to ppb
        Pyre$Result[Pyre$Analyte == "esfenvalerate/fenvalerate" &
              Pyre$Unit == "pg/L"] <- Pyre$Result[
                Pyre$Analyte == "esfenvalerate/fenvalerate" &
                Pyre$Unit == "pg/L"] /(1000*1000)
        
      # Convert ng/L to ppb
        Pyre$Result[Pyre$Analyte == "esfenvalerate/fenvalerate" &
                  Pyre$Unit == "ng/L"] <- Pyre$Result[
                    Pyre$Analyte == "esfenvalerate/fenvalerate" &
                    Pyre$Unit == "ng/L"] /1000
      
      # Correct units
         Pyre$Unit[Pyre$Analyte == "esfenvalerate/fenvalerate" &
                      Pyre$Matrix == "water"] <- "ppb"
         
# Correct units for others (sed samples, equivalent to ppb)
  Pyre$Unit[Pyre$Analyte == "esfenvalerate/fenvalerate-1"] <- "ppb"
  Pyre$Unit[Pyre$Analyte == "esfenvalerate/fenvalerate-2"] <- "ppb"

# Correct name
  
Pyre$Analyte[Pyre$Analyte == "esfenvalerate/fenvalerate"] <- "esfenvalerate"

Pyre$Analyte[Pyre$Analyte == "esfenvalerate/fenvalerate-1"] <- "esfenvalerate"

Pyre$Analyte[Pyre$Analyte == "esfenvalerate/fenvalerate-2"] <- "esfenvalerate"
```

#### **Pyre Result**

```r
Pyre %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```
## # A tibble: 6 x 5
## # Groups:   Analyte, Matrix [6]
##   Analyte       Matrix   Unit      n     mean
##   <chr>         <chr>    <chr> <int>    <dbl>
## 1 bifenthrin    sediment ppb     478 3.21    
## 2 bifenthrin    water    ppb    1799 0.000666
## 3 cyfluthrin    sediment ppb     485 0.241   
## 4 cyfluthrin    water    ppb    1771 0.000135
## 5 esfenvalerate sediment ppb     492 1.25    
## 6 esfenvalerate water    ppb    1719 0.00216
```



```r
Wide.Pyre.Waterdf <- Pyre %>% filter(!Matrix == "sediment") %>%
  group_by(Date, Latitude, Longitude, Analyte, Matrix) %>%
  summarize(Subregion = first(Subregion),
            Mean = mean(Result, na.rm = T)) %>%
  pivot_wider(names_from = Analyte,
              names_repair = "check_unique",
              values_from = Mean) # Values to fill columns
```

```
## `summarise()` has grouped output by 'Date', 'Latitude', 'Longitude', 'Analyte'. You can override using the `.groups` argument.
```

```r
write.csv(x = Wide.Pyre.Waterdf , file = "Data/Output/WideSubsets/Pyre.Wide.water.csv", 
          row.names = F)
```
<br>

### GABA inhibitors  {.tabset}

AKA anything fipronil. Info on Fipronil and its degradates summarized from http://npic.orst.edu/factsheets/archive/fiptech.html


```r
GABA <- CEDENSURF %>% filter(SelectList == "GABA")

GABA %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```
**Fipronil** is a broad-spectrum insecticide that belongs to the phenylpyrazole chemical family. Fipronil disrupts the insect central nervous system.

**Fipronil-sulfone** is the primary biological metabolite of fipronil, is reported to be twenty times more active at mammalian chloride channels than at insect chloride channels. 10 Fipronil-sulfone is reportedly six times more potent in blocking vertebrate GABA-gated chloride channels than fipronil, but demonstrates similar toxicity to the parent compound in mammals.

**Fipronil-desulfinyl**, the primary environmental metabolite (photoproduct) of fipronil, is 9-10 times more active at the mammalian chloride channel than the parent compound, reducing the selectivity between insects and humans when exposed to this metabolite.

**Fipronil-amide** is another degradate of Fipronil (https://pubmed.ncbi.nlm.nih.gov/32574918/)


#### Constant units, no changes needed

fipronil detrifluoromethylsulfinyl - (sediment only) ug/Kg dw

#### fipronil

fipronil - ppb (sed and water)
fipronil amide


```r
# fipronil

  # Sediment:	ppb	and ng/g dw, and ug/kg	(equivalent units)
    GABA$Unit[GABA$Analyte == "fipronil" &
                           GABA$Matrix == "sediment"] <- "ppb"

  # Water: ng/L, ug/L, ppb
      # ppb = pg/L / (1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/L
        
      # Convert pg/L to ppb
        GABA$Result[GABA$Analyte == "fipronil" &
              GABA$Unit == "pg/L"] <- GABA$Result[
                GABA$Analyte == "fipronil" &
                GABA$Unit == "pg/L"] /(1000*1000)
        
      # Convert ng/L to ppb
        GABA$Result[GABA$Analyte == "fipronil" &
                  GABA$Unit == "ng/L"] <- GABA$Result[
                    GABA$Analyte == "fipronil" &
                    GABA$Unit == "ng/L"] /1000
      
      # Correct units
         GABA$Unit[GABA$Analyte == "fipronil" &
                      GABA$Matrix == "water"] <- "ppb"
```

fipronil amide - ppb


```r
# fipronil amide
  # Assess coverage
    GABA %>% filter(Analyte == "fipronil amide") %>%
      distinct(Subregion) # coverage in 2 regions

  # Sediment:	ppb	and ng/g dw, and ug/kg	(equivalent units)
    GABA$Unit[GABA$Analyte == "fipronil amide" &
                           GABA$Matrix == "sediment"] <- "ppb"

  # Water: ug/L and ppb (equivalent units)
    GABA$Unit[GABA$Analyte == "fipronil amide" &
                GABA$Matrix == "water"] <- "ppb"
    
  # Name fix
    GABA$Analyte[GABA$Analyte == "fipronil amide"] <- "fipronil_amide"
```

fipronil desulfinyl = desulfinyl fipronil

sediment = ppb
water = ppb


```r
# fipronil desulfinyl
  
  # correct reversed name
  GABA$Analyte[GABA$Analyte == "desulfinyl fipronil"] <- "fipronil desulfinyl"
  
  # Units?
  GABA %>% filter(Analyte == "fipronil desulfinyl") %>% distinct(Unit)
  
  # Sediment: ppb, ng/g dw, and ug/kg	(equivalent units to ppb)
  GABA$Unit[GABA$Analyte == "fipronil desulfinyl" &
                           GABA$Matrix == "sediment"] <- "ppb"
  
  # Water: pg/L, ng/L, ug/L, ppb
    
      # ppb = pg/L / (1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/L
    
      # Convert pg/L to ppb
        GABA$Result[GABA$Analyte == "fipronil desulfinyl" &
              GABA$Unit == "ng/L"] <- GABA$Result[
                GABA$Analyte == "fipronil desulfinyl" &
                GABA$Unit == "ng/L"]/(1000*1000)
        
      # Convert ng/L to ppb
        GABA$Result[GABA$Analyte == "fipronil desulfinyl" &
              GABA$Unit == "ng/L"] <- GABA$Result[
                GABA$Analyte == "fipronil desulfinyl" &
                GABA$Unit == "ng/L"]/1000
        
      # Correct units
        GABA$Unit[GABA$Analyte == "fipronil desulfinyl" &
                      GABA$Matrix == "water"] <- "ppb"
```

fipronil desulfinyl amide = desulfinyl fipronil amide

sediment = ppb
water = ppb


```r
# fipronil desulfinyl amide
      # correct reversed name
      GABA$Analyte[GABA$Analyte == "desulfinyl fipronil amide"] <- 
            "fipronil desulfinyl amide"
      
      # Units?
      GABA %>% filter(Analyte == "fipronil desulfinyl amide") %>%
        distinct(Unit, Matrix)
  
  # Water: pg/L, ng/L, ug/L, ppb
      # ppb = pg/L / (1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/LConvert ng/L to ppb (ug/L)
      
      GABA$Result[GABA$Analyte == "fipronil desulfinyl amide" &
                  GABA$Unit == "ng/L"] <- GABA$Result[
                    GABA$Analyte == "fipronil desulfinyl amide" &
                    GABA$Unit == "ng/L"]/1000
      
      # Correct units
         GABA$Unit[GABA$Analyte == "fipronil desulfinyl amide" &
                      GABA$Matrix == "water"] <- "ppb"
         
  # Sediment: ppb, ng/g dw (equivalent units)
        GABA$Unit[GABA$Analyte == "fipronil desulfinyl amide" &
                           GABA$Matrix == "sediment"] <- "ppb"
```

#### fipronil sulfide - ppb


```r
# fipronil sulfide

  # Sediment:	ppb	and ng/g dw, and ug/kg	(equivalent units)
    GABA$Unit[GABA$Analyte == "fipronil sulfide" &
                           GABA$Matrix == "sediment"] <- "ppb"

  # Water: ng/L, ug/L, ppb
      # ppb = pg/L / (1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/L
        
      # Convert pg/L to ppb
        GABA$Result[GABA$Analyte == "fipronil sulfide" &
              GABA$Unit == "pg/L"] <- GABA$Result[
                GABA$Analyte == "fipronil sulfide" &
                GABA$Unit == "pg/L"] /(1000*1000)
        
      # Convert ng/L to ppb
        GABA$Result[GABA$Analyte == "fipronil sulfide" &
                  GABA$Unit == "ng/L"] <- GABA$Result[
                    GABA$Analyte == "fipronil sulfide" &
                    GABA$Unit == "ng/L"] /1000
      
      # Correct units
         GABA$Unit[GABA$Analyte == "fipronil sulfide" &
                      GABA$Matrix == "water"] <- "ppb"
```

fipronil sulfone


```r
# fipronil sulfone

  # Sediment:	ppb	and ng/g dw, and ug/kg	(equivalent units)
    GABA$Unit[GABA$Analyte == "fipronil sulfone" &
                           GABA$Matrix == "sediment"] <- "ppb"

  # Water: ng/L, ug/L, ppb
      # ppb = pg/L / (1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/L
        
      # Convert pg/L to ppb
        GABA$Result[GABA$Analyte == "fipronil sulfone" &
              GABA$Unit == "pg/L"] <- GABA$Result[
                GABA$Analyte == "fipronil sulfone" &
                GABA$Unit == "pg/L"] /(1000*1000)
        
      # Convert ng/L to ppb
        GABA$Result[GABA$Analyte == "fipronil sulfone" &
                  GABA$Unit == "ng/L"] <- GABA$Result[
                    GABA$Analyte == "fipronil sulfone" &
                    GABA$Unit == "ng/L"] /1000
      
      # Correct units
         GABA$Unit[GABA$Analyte == "fipronil sulfone" &
                      GABA$Matrix == "water"] <- "ppb"
         
# Correct names

GABA$Analyte[GABA$Analyte == "fipronil desulfinyl"] <- 
  "fipronil_desulfinyl"

GABA$Analyte[GABA$Analyte == "fipronil desulfinyl amide"] <-
    "fipronil_desulfinyl_amide"

GABA$Analyte[GABA$Analyte == "fipronil sulfide"] <- "fipronil_sulfide"

GABA$Analyte[GABA$Analyte == "fipronil sulfone"] <- "fipronil_sulfone"

# Remove if insufficient replication
  GABA <- GABA %>% 
    filter(!Analyte == "fipronil detrifluoromethylsulfinyl") %>%
    filter(!Analyte == "fipronil_amide")
```

#### **GABA Result**

```r
GABA %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```
## # A tibble: 10 x 5
## # Groups:   Analyte, Matrix [10]
##    Analyte                   Matrix   Unit      n      mean
##    <chr>                     <chr>    <chr> <int>     <dbl>
##  1 fipronil                  sediment ppb     321 0.00134  
##  2 fipronil                  water    ppb    1041 0.000545 
##  3 fipronil_desulfinyl       sediment ppb     351 0.00849  
##  4 fipronil_desulfinyl       water    ppb    1031 0.000174 
##  5 fipronil_desulfinyl_amide sediment ppb     248 0        
##  6 fipronil_desulfinyl_amide water    ppb     901 0.0000364
##  7 fipronil_sulfide          sediment ppb     150 0.0281   
##  8 fipronil_sulfide          water    ppb     273 0.000255 
##  9 fipronil_sulfone          sediment ppb     131 0.0425   
## 10 fipronil_sulfone          water    ppb     464 0.000553
```



```r
Wide.GABA.Waterdf <- GABA %>% filter(!Matrix == "sediment") %>%
  group_by(Date, Latitude, Longitude, Analyte, Matrix) %>%
  summarize(Subregion = first(Subregion),
            Mean = mean(Result, na.rm = T)) %>%
  pivot_wider(names_from = Analyte,
              names_repair = "check_unique",
              values_from = Mean) # Values to fill columns
```

```
## `summarise()` has grouped output by 'Date', 'Latitude', 'Longitude', 'Analyte'. You can override using the `.groups` argument.
```

```r
write.csv(x = Wide.GABA.Waterdf , file = "Data/Output/WideSubsets/GABA.Wide.water.csv", 
          row.names = F)
```

<br>


### Others: Glyphosate, Atrazine, Neonicitinoids  {.tabset}


```r
Other <- CEDENSURF %>% filter(SelectList == c("Glyphosate", "Atrazine", "Neon"))
```

```
## Warning in SelectList == c("Glyphosate", "Atrazine", "Neon"): longer object
## length is not a multiple of shorter object length
```

#### Neonicotinoids 

All measures are in water. 

hydroxy-imidacloprid (2 records total)
imidacloprid (228 records)
clothianidin (225 records)



```r
Other %>% filter(SelectList == "Neon") %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```


```r
  # Water: ng/L, ug/L, ppb

      # ppb = ng/L / 1000
      # ppb = ug/L

      # Convert ng/L to ppb
        Other$Result[Other$SelectList == "Neon" &
                  Other$Unit == "ng/L"] <- Other$Result[
                    Other$SelectList == "Neon" &
                    Other$Unit == "ng/L"] /1000
      
      # Correct units
         Other$Unit[Other$SelectList == "Neon"] <- "ppb"
         
# Remove records with insufficient replication
  Other <- Other %>% filter(!Analyte == "hydroxy-imidacloprid")
```

#### Glyphosate - ppb

(water only) 219 records, no conversion needed (equivalent units)


```r
Other %>% filter(SelectList == "Glyphosate") %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```r
# units ug/L and ppb (equivalent units)
  Other$Unit[Other$SelectList == "Glyphosate"] <- "ppb"
```

#### Atrazine

Atrazine (sed and water) - ppb
Atrazine degradate (water only) - ppb


```r
Other %>% filter(SelectList == "Atrazine") %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```r
# Water: ng/L, ug/L, ppb

      # ppb = ng/L / 1000
      # ppb = ug/L

      # Convert ng/L to ppb
        Other$Result[Other$SelectList == "Atrazine" &
                  Other$Unit == "ng/L"] <- Other$Result[
                    Other$SelectList == "Atrazine" &
                    Other$Unit == "ng/L"] /1000
      
      # Correct units
         Other$Unit[Other$SelectList == "Atrazine"] <- "ppb"
         
# Remove records with insufficient replication
  Other <- Other %>% filter(!Analyte == "atrazine degradate")
```

#### **Other Result**

```r
Other %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```
## # A tibble: 5 x 5
## # Groups:   Analyte, Matrix [5]
##   Analyte      Matrix   Unit      n       mean
##   <chr>        <chr>    <chr> <int>      <dbl>
## 1 atrazine     sediment ppb      85 0         
## 2 atrazine     water    ppb     961 0.00284   
## 3 clothianidin water    ppb     232 0.00000388
## 4 glyphosate   water    ppb     463 0.525     
## 5 imidacloprid water    ppb     299 0.00186
```



```r
Wide.Other.Waterdf <- Other %>% filter(!Matrix == "sediment") %>%
  group_by(Date, Latitude, Longitude, Analyte, Matrix) %>%
  summarize(Subregion = first(Subregion),
            Mean = mean(Result, na.rm = T)) %>%
  pivot_wider(names_from = Analyte,
              names_repair = "check_unique",
              values_from = Mean) # Values to fill columns
```

```
## `summarise()` has grouped output by 'Date', 'Latitude', 'Longitude', 'Analyte'. You can override using the `.groups` argument.
```

```r
write.csv(x = Wide.Other.Waterdf , file = "Data/Output/WideSubsets/Other.Wide.water.csv", 
          row.names = F)
```

<br>



### Late Additions: Herbicides

Chemicals that we have tox data for, but which were not in the original conceptual model. I need to talk with Allie about how to categorize them too; from what I can tell...

**records in water**
Triclopyr = Pyridine
Diuron
Linuron
Paraquat dichloride

**records in water and sediment**
Molinate = azepane (banned in US)
Propanil (very common herbicide)
Thiobencarb = monochlorobenzene 
Oxyfluorfen

**Records omitting**
Dinoseb = dinitrophenol (banned in US).
   Only 94 records. Omitted.
   



```r
Herbicide <- CEDENSURF %>% filter(SelectList == "Herbicide")

Herbicide %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```


```r
# Running conversions for all herbicides at once. Leaving in extra specification (SelectList == herbicide), in case I want to adapt this to run on the entire dataset. Don't see why not...

  # Water: ng/L, ug/L
      # ppb = pg/L / (1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/L
        
      # Convert ng/L to ppb
        Herbicide$Result[Herbicide$SelectList == "Herbicide" &
                  Herbicide$Unit == "ng/L"] <- Herbicide$Result[
                    Herbicide$SelectList == "Herbicide" &
                    Herbicide$Unit == "ng/L"] /1000

       # Convert pg/L to ppb
        Herbicide$Result[Herbicide$SelectList == "Herbicide" &
                  Herbicide$Unit == "pg/L"] <- Herbicide$Result[
                    Herbicide$SelectList == "Herbicide" &
                    Herbicide$Unit == "pg/L"] /(1000*1000)
      
      # Correct units
        Herbicide$Unit[Herbicide$SelectList == "Herbicide" &
                      Herbicide$Matrix == "water"] <- "ppb"
```


```r
# Name fix
Herbicide$Analyte[Herbicide$Analyte == 
        "paraquat dichloride"] <- "paraquat_dichloride"
```

#### **Herbicide Result**

```r
Herbicide %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```
## # A tibble: 13 x 5
## # Groups:   Analyte, Matrix [13]
##    Analyte             Matrix   Unit      n     mean
##    <chr>               <chr>    <chr> <int>    <dbl>
##  1 dinoseb             water    ppb     220 0.0594  
##  2 diuron              water    ppb    2750 0.154   
##  3 linuron             water    ppb    2349 0.00873 
##  4 molinate            sediment ppb     207 0       
##  5 molinate            water    ppb    1505 0.00497 
##  6 oxyfluorfen         sediment ppb     232 0.000172
##  7 oxyfluorfen         water    ppb    1434 0.00121 
##  8 paraquat_dichloride water    ppb     454 0.0106  
##  9 propanil            sediment ppb     247 0       
## 10 propanil            water    ppb    1435 0.00253 
## 11 thiobencarb         sediment ppb     247 0       
## 12 thiobencarb         water    ppb    2019 0.00262 
## 13 triclopyr           water    ppb     609 0.00668
```

### Late addition: OrganoChlorines

No organochlorines were included in the initial conceptual model, though we have good toxicity data for these compounds, evidence of environmental persistence, and recent discovery of tanks of DDT off the coast of CA. 

DDT has been banned a long time, whereas endosulfan was only recently phased out (last crop usages to end 2016)

DDT
DDE, DDD (degradates of DDT)
Endosulfan
Endosulfan sulfate (oxidized product of endosulfan)


```r
OrganoCh <- CEDENSURF %>% filter(SelectList == "OrganoCh")

OrganoCh %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```


```r
# Sediment:	ppb	and ng/g dw, and ug/kg	(equivalent units)
    OrganoCh %>% filter(Matrix == "sediment") %>% 
              distinct(Unit)
    
    OrganoCh$Unit[OrganoCh$Matrix == "sediment" & 
                    OrganoCh$Unit == "ng/g dw"] <- "ppb"
    
    OrganoCh$Unit[OrganoCh$Matrix == "sediment" & 
                    OrganoCh$Unit == "ug/Kg dw"] <- "ppb"
    
  # Water: ng/L, ug/L
      # ppb = pg/L / (1000*1000)
      # ppb = ng/L / 1000
      # ppb = ug/L
        
      # Convert ng/L to ppb
        OrganoCh$Result[OrganoCh$SelectList == "OrganoCh" &
                  OrganoCh$Unit == "ng/L"] <- OrganoCh$Result[
                    OrganoCh$SelectList == "OrganoCh" &
                    OrganoCh$Unit == "ng/L"] /1000

       # Convert pg/L to ppb
        OrganoCh$Result[OrganoCh$SelectList == "OrganoCh" &
                  OrganoCh$Unit == "pg/L"] <- OrganoCh$Result[
                    OrganoCh$SelectList == "OrganoCh" &
                    OrganoCh$Unit == "pg/L"] /(1000*1000)
      
      # Correct units
        OrganoCh$Unit[OrganoCh$SelectList == "OrganoCh" &
                      OrganoCh$Matrix == "water"] <- "ppb"
```


```r
# Name fix
OrganoCh$Analyte[OrganoCh$Analyte == 
        "endosulfan sulfate"] <- "endosulfan_sulfate"
```

#### **OrganoChloride Result**

```r
OrganoCh %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```
## # A tibble: 12 x 5
## # Groups:   Analyte, Matrix [12]
##    Analyte            Matrix   Unit      n     mean
##    <chr>              <chr>    <chr> <int>    <dbl>
##  1 ddd                sediment ppb     421 0.627   
##  2 ddd                water    ppb     992 0.000341
##  3 dde                sediment ppb     420 0.952   
##  4 dde                water    ppb    1044 0.000536
##  5 ddt                sediment ppb     350 0.440   
##  6 ddt                water    ppb     976 0.0220  
##  7 endosulfan         sediment ppb     140 0.0174  
##  8 endosulfan         water    ppb    1336 0.000591
##  9 endosulfan_sulfate sediment ppb      55 0       
## 10 endosulfan_sulfate water    ppb     424 0.00107 
## 11 pyridaben          sediment ppb     245 0       
## 12 pyridaben          water    ppb     607 0
```


<br>

### Late Additions: Fungicides  {.tabset}

Fungicides added due to presence in another tox paper

Dichloran
Myclobutanil
Triadimefon


```r
Fungi <- CEDENSURF %>% filter(SelectList == "Fungicide")

Fungi %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = round(mean(Result),2))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

#### Dichloran - ppb


```r
# dichloran (ppb, ng/L)

  # Water:	ng/L	5
      # ppb = ng/L / 1000
      # Convert ng/L to ppb
        Fungi$Result[Fungi$Analyte == "dichloran" &
                  Fungi$Unit == "ng/L"] <- Fungi$Result[
                    Fungi$Analyte == "dichloran" &
                    Fungi$Unit == "ng/L"] /1000
      # Correct units
       Fungi$Unit[Fungi$Analyte == "dichloran" &
                    Fungi$Matrix == "water"] <- "ppb"
       
Fungi %>% filter(Analyte== "dichloran") %>% distinct(Unit)
```

#### Myclobutanil - ppb


```r
# Myclobutanil (ppb, ng/L)

  # Sediment:	ppb	252

  # Water:	ng/L	144, ppb	472	
      # ppb = pg / (1000*1000)
      # ppb = ng/L / 1000
    
      # Convert ng/L to ppb
        Fungi$Result[Fungi$Analyte == "myclobutanil" &
                  Fungi$Unit == "ng/L"] <- Fungi$Result[
                    Fungi$Analyte == "myclobutanil" &
                    Fungi$Unit == "ng/L"] /1000

      # Correct units
       Fungi$Unit[Fungi$Analyte == "myclobutanil" &
                    Fungi$Matrix == "water"] <- "ppb"
       
Fungi %>% filter(Analyte== "myclobutanil") %>% distinct(Unit)
```

#### Triadimefon


```r
# Myclobutanil (ppb, ng/L)

  # Sediment:	ppb	264

  # Water:	ng/L	110, ppb	369	
      # ppb = pg / (1000*1000)
      # ppb = ng/L / 1000
    
      # Convert ng/L to ppb
        Fungi$Result[Fungi$Analyte == "triadimefon" &
                  Fungi$Unit == "ng/L"] <- Fungi$Result[
                    Fungi$Analyte == "triadimefon" &
                    Fungi$Unit == "ng/L"] /1000

      # Correct units
       Fungi$Unit[Fungi$Analyte == "triadimefon" &
                    Fungi$Matrix == "water"] <- "ppb"
       
Fungi %>% filter(Analyte== "triadimefon") %>% distinct(Unit)
```

#### **Fungi Result**

```r
Fungi %>%
  group_by(Analyte, Matrix, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte', 'Matrix'. You can override using the `.groups` argument.
```

```
## # A tibble: 5 x 5
## # Groups:   Analyte, Matrix [5]
##   Analyte      Matrix   Unit      n     mean
##   <chr>        <chr>    <chr> <int>    <dbl>
## 1 dichloran    water    ppb       8 0       
## 2 myclobutanil sediment ppb     247 0       
## 3 myclobutanil water    ppb    1002 0.000405
## 4 triadimefon  sediment ppb     247 0       
## 5 triadimefon  water    ppb     516 0
```


```r
Wide.Fungi.Waterdf <- Fungi %>% filter(!Matrix == "sediment") %>%
  group_by(Date, Latitude, Longitude, Analyte, Matrix) %>%
  summarize(Subregion = first(Subregion),
            Mean = mean(Result, na.rm = T)) %>%
  pivot_wider(names_from = Analyte,
              names_repair = "check_unique",
              values_from = Mean) # Values to fill columns
```

```
## `summarise()` has grouped output by 'Date', 'Latitude', 'Longitude', 'Analyte'. You can override using the `.groups` argument.
```

```r
write.csv(x = Wide.Fungi.Waterdf , file = "Data/Output/WideSubsets/Fungi.Wide.water.csv", 
          row.names = F)
```


<br>

## Save Intermediate

Compile and Save Modified Dataset (Long)

The file Data/Output/CEDENSURF_Limited_FixedUnits.csv contains all corrected units and analytes chosen for our model in long format. 


```r
Limited <- rbind(WQP, GABA, Metal, OrganoP, Pyre, Other, Herbicide, OrganoCh, Fungi)

length(unique(Limited$Analyte))
```

```
## [1] 51
```

```r
write.csv(x = Limited, 
          file = "Data/Output/CEDENSURF_Limited_FixedUnits.csv", 
          na = "", row.names = F)

# Save abbreviated template to use in 05_ToxUnits

ToxUnits <- Limited %>% distinct(Analyte, Matrix, Unit, SelectList)
write.csv(x = ToxUnits, 
          file = "Data/Output/ToxUnitsTemplate.csv", 
          na = "", row.names = F)
```

```r
Limited %>%
  group_by(SelectList, Unit) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'SelectList'. You can override using the `.groups` argument.
```

```
## # A tibble: 20 x 4
## # Groups:   SelectList [11]
##    SelectList  Unit         n        mean
##    <chr>       <chr>    <int>       <dbl>
##  1 Atrazine    ppb       1046    0.00261 
##  2 Fungicide   ppb       2020    0.000201
##  3 GABA        ppb       4911    0.00291 
##  4 Glyphosate  ppb        463    0.525   
##  5 Herbicide   ppb      13708    0.0353  
##  6 Metal       ug/Kg dw   840 9779.      
##  7 Metal       ug/L      4958    1.32    
##  8 Neon        ppb        531    0.00105 
##  9 OrganoCh    ppb       7010    0.120   
## 10 OrganoP     ppb      16131    0.0179  
## 11 Pyrethroids ppb       6744    0.337   
## 12 WQP         % dw       159    0.0890  
## 13 WQP         Deg C     6677   18.9     
## 14 WQP         mg/Kg dw    36 2144.      
## 15 WQP         mg/L     13002    6.04    
## 16 WQP         none      7234    7.69    
## 17 WQP         NTU       3117   30.6     
## 18 WQP         ppt        669    6.42    
## 19 WQP         ug/L        53  510.      
## 20 WQP         uS/cm      561  433.
```




<br/>

# Convert to Wide

## 3. Summarize into wide format

I used pivot_wider to summarize analyte results by date and subregion within each matrix. 

Because column names are the means by which Netica differentiates nodes, I made the wide format data from the water-matrix contain just the analyte as the column name, while the sediment data includes analyte_sediment as the column names

Allwater.Wide.csv contains the final summarized analytes in wide format (one row per date/location; one column per analyte)

Allsed.Wide.csv contains the final summarized analytes in wide format (one row per date/location; one column per analyte)


```r
## Subset: Water Matrix

# Convert long to wide
Wide.waterdf <- Limited %>% filter(!Matrix == "sediment") %>%
  group_by(Date, Latitude, Longitude, Analyte, Matrix) %>%
  summarize(Subregion = first(Subregion),
            Mean = mean(Result, na.rm = T)) %>%
  pivot_wider(names_from = Analyte,
              names_repair = "check_unique",
              values_from = Mean) # Values to fill columns
```

```
## `summarise()` has grouped output by 'Date', 'Latitude', 'Longitude', 'Analyte'. You can override using the `.groups` argument.
```

```r
# save document
write.csv(x = Wide.waterdf, file = "Data/Output/Allwater.Wide.csv", 
          row.names = F)
```


```r
## Subset: Water Matrix

# Convert long to wide
Wide.seddf <- Limited %>% filter(!Matrix == "water") %>%
  group_by(Date, Latitude, Longitude, Analyte, Matrix) %>%
  summarize(Subregion = first(Subregion),
            Mean = mean(Result, na.rm = T)) %>%
  pivot_wider(names_from = c(Analyte,Matrix),
              names_repair = "check_unique",
              values_from = Mean) # Values to fill columns
```

```
## `summarise()` has grouped output by 'Date', 'Latitude', 'Longitude', 'Analyte'. You can override using the `.groups` argument.
```

```r
# save document
write.csv(x = Wide.seddf, file = "Data/Output/Allsed.Wide.csv", 
          row.names = F)
```


