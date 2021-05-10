---
title: "Toxicological Units Approach"
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

Start with the midpoint of 04_QAQC_Convert2Wide (long-format) because it is easiest to make these conversions *before* the conversion to wide-format.


```r
# Load CEDENSURF Limited Data (long format)
Limited <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/CEDENSURF_Limited_FixedUnits.csv")

Limited %>%
  group_by(Analyte, Subregion) %>%
  summarise(n = n(),
            mean = mean(Result))
```

```
## `summarise()` has grouped output by 'Analyte'. You can override using the `.groups` argument.
```

```
## # A tibble: 154 x 4
## # Groups:   Analyte [28]
##    Analyte    Subregion            n      mean
##    <chr>      <chr>            <int>     <dbl>
##  1 atrazine   Central Delta      176  0.00548 
##  2 atrazine   Confluence          48  0.00208 
##  3 atrazine   North Delta        116  0.00189 
##  4 atrazine   Sacramento River   129  0.00548 
##  5 atrazine   South Delta         47  0.000832
##  6 atrazine   Suisun Bay          54  0.00370 
##  7 bifenthrin Central Delta      277  2.18    
##  8 bifenthrin Confluence         163 16.4     
##  9 bifenthrin North Delta        619  0.0836  
## 10 bifenthrin Sacramento River   403  0.0260  
## # ... with 144 more rows
```
Table containing Molar mass (Moles/g) of each analyte


```r
# Load CEDENSURF Limited Data (long format)
MM <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/MolarMass.csv")
```

## 2. Convert to Moles/Concentration

Those molar masses will be used to convert units of every chemical analyte (not water quality parameters) to micromoles per liter (water) or micromoles per kg of sediment. Aim is to have μM/ug

  1. Merge table with MM and CEDENSURF Limited data
  
  2. Divide by molar weight to obtain micro-Moles per volume

Each analyte is currently in micrograms per volume. That means that I can simply multiply each result by molar weight to obtain micro-Moles per volume

(μg/L) * (μM/ug) = μM/L

**Starting Units**
- ppb
- μg/L
- μg/Kg dw

1 ppb = 1 μg/L = 1 μg/Kg

**Divide by MM then multiply by 1000 to convert to nM**
- g/Mole (μg/μM) * 1000nM/μM

**Ending Units = nanomoles per liter (nM)**
Water:    nM/L
Sediment: nM/Kg dw


```r
# Left bind to merge MolarMass column into long data, then remove WQP parameters that won't undergo this process

# Merge
ToxUnits <- merge(Limited, MM, by = "Analyte") %>% filter(!SelectList == "WQP")

# Convert
ToxUnits<- ToxUnits %>% 
  
  # Convert to uM
  mutate(Result_M = Result/MolarMass*1000) %>% 
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



```r
# Add back on WQP Units
WQP <- Limited %>% filter(SelectList == "WQP")

ToxUnits <- rbind(ToxUnits, WQP)

# Save?
write.csv(x = ToxUnits, 
          file = "Data/Output/CEDENSURFLimited_ToxUnits.csv", 
          na = "", row.names = F)
```


