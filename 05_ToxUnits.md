---
title: "Toxic Units Approach"
author: "Erika W"
date: "5/1/2021"
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


## Intro

If we are to simplify the Netica model, we will need to summarize analytes by their functional category. 

Toxic units (TU) are used in the field of toxicology to quantify the interactions of toxicants in binary mixtures of chemicals.A toxic unit for a given compound is based on the concentration at which there is a 50% effect (ex. EC50) for a certain biological endpoint. One toxic unit is equal to the EC50 for a given endpoint for a specific biological effect over a given amount of time. Toxic units allow for the comparison of the individual toxicities of a binary mixture to the combined toxicity. This allows researchers to categorize mixtures as additive, synergistic or antagonistic. Synergism and antagonism are defined by mixtures that are more or less toxic than predicted by the sum of their toxic units.

For the purposes of simplicity, we will start by assuming that the chemicals are additive.

The process will be:

1. Load data. 

2. Convert to moles per volume

3. Convert concentrations to moles per volume

4. Convert to toxic units, using EC50 values? (If available/From Allie)

5. Summarize by RR/date for each analyte

6. Sum toxic units within each category

*THOUGHTS: I don't believe we should use analytes that were only measured in one region, because adding additional TU's for that region would falsly inflate that regions relative risk just by having more information. The alternative to omitting that data would be to estimate missing TU's for other regions by proxy (ie: land use)*

## Load Data

Start with the midpoint of 04_QAQC_Convert2Wide (long-format) because it is easiest to make these conversions *before* the conversion to wide-format:


```r
# Load CEDENSURF Limited Data (long format)

tmp <- tempfile()

CS_Limited <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDENSURF-mod/30YRS-OrigRR/Data/Output/CEDENSURF_Limited_FixedUnits.csv",
                   .token = gh_token(), 
                   .destfile = tmp)

Limited <- read_csv(tmp) # works for me!
```

```
## Rows: 30990 Columns: 34
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (24): Agency, Analyte, CollectionMethod, County, Data.source, Datum, ge...
## dbl   (9): Latitude, Longitude, LOQ, MDL, rb_number, Record_id, Result, Stud...
## date  (1): Date
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
# Check coverage for each analyte. 
Limited2<- Limited %>%
  group_by(Analyte,Matrix) %>%
  summarise(coverage = (length(unique(Subregion))),
            n = n(),
            mean = mean(Result)) 

Limited2

# How many analytes*matrix combinations are measured in at least 5 subregions?
Limited2 %>% filter(coverage >= 5) %>% nrow(.)

# 51 Out of 86
```

And tables containing Molar mass (Moles/g) of each analyte, and EC50 values for each analyte


```r
# Load CEDENSURF Limited Data (long format)

tmp <- tempfile()
MM <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDENSURF-mod/main/Data/MolarMass.csv",
                   .token = gh_token(), 
                   .destfile = tmp)

MM <- read_csv(tmp) # works for me!
```

```
## Rows: 38 Columns: 2
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr (1): Analyte
## dbl (1): MW
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# EC_Table <- fread("")
```
<br/>

## 2. Convert to Moles/Concentration

Those molar masses will be used to convert units of every chemical analyte (not water quality parameters) to micromoles per liter (water) or micromoles per kg of sediment. Aim is to have nM/ug

  1. Merge table with MM and CEDENSURF Limited data
  
  2. Divide by molar weight to obtain nano-Moles per volume

Each analyte is currently in micrograms per volume. That means that I can simply multiply each result by molar weight to obtain micro-Moles per volume

(μg/L) * (μM/ug) = μM/L

(ug/L) * (1000 ng/ug) * (nM/ng) = nM/L

**Starting Units**
- ppb
- μg/L
- μg/Kg dw

1 ppb = 1 μg/L = 1 μg/Kg

**Divide by MM then multiply by 1000 to convert to nM**
- g/Mole (μg/μM) * 1000 nM/μM

**Ending Units = nanomoles per liter (pM)**
Water:    nM/L
Sediment: nM/Kg dw


```r
# Left bind to merge MolarMass column into long data, then remove WQP parameters that won't undergo this process

# Merge
ToxUnits <- merge(Limited, MM, by = "Analyte") %>% filter(!SelectList == "WQP")

# Check that each analyte recieved MW - GOOD.
ToxUnits %>%
  group_by(Analyte) %>%
  summarise(n = n(),
            MW = mean(MW))
```

```
## # A tibble: 31 x 3
##    Analyte          n    MW
##    <chr>        <int> <dbl>
##  1 atrazine       575  216.
##  2 bifenthrin    1413  423.
##  3 chlorpyrifos  2399  351.
##  4 clothianidin   159  250.
##  5 cyfluthrin    1429  434.
##  6 dde             28  318 
##  7 ddt             12  354.
##  8 diazinon      2177  304.
##  9 diazoxon       233  288.
## 10 dinoseb         24  240.
## # ... with 21 more rows
```

```r
# Convert
ToxUnits<- ToxUnits %>% 
  
  # Convert to nM
  mutate(Result_M = Result*1000/MW) %>% 
  select(!Result) %>% mutate(Result = Result_M) %>%
  
  # Define units
  mutate(Unit = ifelse(Matrix == "water", 
                         "nM/L", Unit)) %>% 
  mutate(Unit = ifelse(Matrix == "sediment", 
                         "nM/Kg dw", Unit)) %>% 
  # Organize
  select(Analyte, Result, Unit, Matrix, Date, 
         Subregion, StationName, Latitude, Longitude,
         CollectionMethod, SelectList)
```

<br/>

# 3. Convert to relative tox units

Mikayla is compiling EC50 values for each analyte in the model. Once those are obtained, we can merge with the existing data and divide by X to obtain relative toxic units within each category.

Because the EC50 is organism specific, we will have a separate table output for each organism?



```r
### DONT HAVE EC50 YET, BUT WILL SOON

# Merge
ToxUnits2 <- merge(ToxUnits, EC50, by = "Analyte")

# Convert
ToxUnits<- ToxUnits %>% 
  
  # Divide by EC50 to convert to Tox Units. 
 
  mutate(ToxUnit = Result/EC50) # Multiply by 1000 if too small of numbers to record? Be sure to update mortality equations accordingly. 
```

<br/>


# 4. Compile and Save Intermediate


```r
# Add back on WQP Units? Makes more sense to keep fully separate, because will not sum WQP by the same. Will save together for this intermediate step. 

WQP <- Limited %>% 
  filter(SelectList == "WQP") %>%
  select(Analyte, Result, Unit, Matrix, Date, 
         Subregion, StationName, Latitude, Longitude,
         CollectionMethod, SelectList)
  

Limited_ToxUnits <- rbind(ToxUnits, WQP)

# Save?
write.csv(x = Limited_ToxUnits, 
          file = "Data/Output/CEDENSURFLimited_ToxUnits.csv",
          na = "", row.names = F)
```


# 5. Sum by location and date, within category

Each analyte must then be averaged by date*subregion combination, then toxic units summed within category. 

This wide output will not include WQP, because those parameters were retained with separate units and will not be summed by category.


```r
# Two analytes were not categorized in the conceptual model for some reason - ask why. If we want to group them with their chemical class, then remove eval = F in the header of this code block

# Glyphosate, an organic phosphorous compoud (phosphonate) is a broad spectrum herbicide. 

# Atrazine is also a broad spectrub herbicide

ToxUnits$SelectList[ToxUnits$Analyte == "glyphosate" | ToxUnits$Analyte == "atrazine"] <- "Herbicide"

unique(ToxUnits$SelectList)
```

In Wayne's email 4/5/2021 8:28 pm, he provided a data format for BN derivation. It recommended each case should be a combination of date * location within the risk region. 


```r
# TEST TEST TEST how does this work
# Method successful using raw data. Replace LIMITED with Tox df and re-test

# START: ToxUnits (n = 32237)

# Obtain average result for each analyte at a given location
Tox.mean <- ToxUnits %>% 
  group_by(Date, Analyte, Subregion, 
           Latitude, Longitude, Matrix, 
           SelectList, Unit) %>% 
  summarise(Mean = mean(Result, na.rm = T)) %>%
  ungroup()
```

```
## `summarise()` has grouped output by 'Date', 'Analyte', 'Subregion', 'Latitude', 'Longitude', 'Matrix', 'SelectList'. You can override using the `.groups` argument.
```

```r
# AFTER AVERAGE, n = 24807


# Sum those analytes at that location within each category
Tox.mean <- Tox.mean %>% 
  group_by(Date, Subregion, 
           Latitude, Longitude, 
           Matrix, SelectList, Unit) %>% 
  summarise(SumResults = sum(Mean))%>%
  ungroup()
```

```
## `summarise()` has grouped output by 'Date', 'Subregion', 'Latitude', 'Longitude', 'Matrix', 'SelectList'. You can override using the `.groups` argument.
```

```r
# AFTER SUM, n = 8925
# nrow(Tox.mean)

# Then, convert to WIDE
Wide.df <- Tox.mean %>%
  group_by(Date, Subregion, 
           Latitude, Longitude, 
           SelectList, Matrix, Unit) %>%
  summarize(Subregion = first(Subregion),
            Unit = first(Unit),
            SumToxUnit = SumResults) %>%
  
  pivot_wider(names_from = SelectList,
              names_repair = "check_unique",
              values_from = SumToxUnit) # Values to fill columns
```

```
## `summarise()` has grouped output by 'Date', 'Subregion', 'Latitude', 'Longitude', 'SelectList', 'Matrix'. You can override using the `.groups` argument.
```

```r
head(Wide.df)
```

```
## # A tibble: 6 x 14
## # Groups:   Date, Subregion, Latitude, Longitude, Matrix [6]
##   Date       Subregion        Latitude Longitude Matrix Unit  Herbicide  OrganoP
##   <date>     <chr>               <dbl>     <dbl> <chr>  <chr>     <dbl>    <dbl>
## 1 1995-01-09 Sacramento River     38.6     -122. water  nM/L          0  0      
## 2 1995-02-14 Suisun Bay           38.1     -122. water  nM/L         NA  1.91e-4
## 3 1995-02-15 Confluence           38.0     -122. water  nM/L         NA  3.71e-5
## 4 1995-02-15 Confluence           38.1     -122. water  nM/L         NA  1.29e+1
## 5 1995-04-18 Confluence           38.0     -122. water  nM/L         NA  6.85e-4
## 6 1995-04-18 Confluence           38.1     -122. water  nM/L         NA  6.49e+0
## # ... with 6 more variables: OrganoCh <dbl>, Pyrethroids <dbl>, Atrazine <dbl>,
## #   GABA <dbl>, Glyphosate <dbl>, Neon <dbl>
```

```r
sessionInfo()
```

```
## R version 4.1.0 (2021-05-18)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19043)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] gitcreds_0.1.1    gh_1.3.0          httr_1.4.2        forcats_0.5.1    
##  [5] stringr_1.4.0     dplyr_1.0.7       purrr_0.3.4       readr_2.0.0      
##  [9] tidyr_1.1.3       tibble_3.1.3      ggplot2_3.3.5     tidyverse_1.3.1  
## [13] sf_1.0-2          lubridate_1.7.10  data.table_1.14.0
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.7         class_7.3-19       assertthat_0.2.1   digest_0.6.27     
##  [5] utf8_1.2.2         R6_2.5.0           cellranger_1.1.0   backports_1.2.1   
##  [9] reprex_2.0.1       evaluate_0.14      e1071_1.7-8        pillar_1.6.2      
## [13] rlang_0.4.11       curl_4.3.2         readxl_1.3.1       rstudioapi_0.13   
## [17] jquerylib_0.1.4    rmarkdown_2.16     bit_4.0.4          munsell_0.5.0     
## [21] proxy_0.4-26       broom_0.7.9        compiler_4.1.0     modelr_0.1.8      
## [25] xfun_0.24          pkgconfig_2.0.3    htmltools_0.5.3    tidyselect_1.1.1  
## [29] fansi_0.5.0        crayon_1.4.1       tzdb_0.1.2         dbplyr_2.1.1      
## [33] withr_2.4.2        grid_4.1.0         jsonlite_1.7.2     gtable_0.3.0      
## [37] lifecycle_1.0.0    DBI_1.1.1          magrittr_2.0.1     units_0.7-2       
## [41] scales_1.1.1       KernSmooth_2.23-20 vroom_1.5.3        cli_3.0.1         
## [45] stringi_1.7.3      cachem_1.0.5       fs_1.5.0           xml2_1.3.2        
## [49] bslib_0.4.0        ellipsis_0.3.2     generics_0.1.0     vctrs_0.3.8       
## [53] tools_4.1.0        bit64_4.0.5        glue_1.4.2         hms_1.1.0         
## [57] parallel_4.1.0     fastmap_1.1.0      yaml_2.2.1         colorspace_2.0-2  
## [61] rvest_1.0.1        classInt_0.4-3     knitr_1.33         haven_2.4.3       
## [65] sass_0.4.2
```

