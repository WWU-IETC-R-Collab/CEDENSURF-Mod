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
Limited <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/CEDENSURF_Limited_FixedUnits.csv")
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
MM <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/MolarMass.csv")

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
## # A tibble: 37 x 3
##    Analyte          n    MW
##  * <chr>        <int> <dbl>
##  1 atrazine       594 216. 
##  2 bifenthrin    1540 423. 
##  3 cadmium        392 112. 
##  4 chlorpyrifos  2631 351. 
##  5 clothianidin   223 250. 
##  6 copper         678  63.6
##  7 cyfluthrin    1437 434. 
##  8 ddd            444 320  
##  9 dde            444 318  
## 10 ddt            421 354. 
## # ... with 27 more rows
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
ToxUnits <- merge(ToxUnits, EC50, by = "Analyte")

# Convert
ToxUnits<- ToxUnits %>% 
  
  # Divide by EC50 to convert to Tox Units. 
 
  mutate(ToxUnit = Result/EC50) # Multiply by 1000 if too small of numbers to record? Be sure to update mortality equations accordingly. 
```

<br/>


# 4. Compile and Save Intermediate


```r
# Add back on WQP Units? Makes more sense to keep fully separate, because will not sum WQP by the same. Will save together for this intermediate step. 

WQP <- Limited %>% filter(SelectList == "WQP")

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
## # A tibble: 6 x 15
## # Groups:   Date, Subregion, Latitude, Longitude, Matrix [6]
##   Date       Subregion Latitude Longitude Matrix Unit  OrganoCh Herbicide  Metal
##   <date>     <chr>        <dbl>     <dbl> <chr>  <chr>    <dbl>     <dbl>  <dbl>
## 1 2009-10-06 Central ~     38.0     -121. water  nM/L         0    NA     NA    
## 2 2009-10-06 Central ~     38.1     -122. water  nM/L         0     0      0.886
## 3 2009-10-06 Central ~     38.1     -121. water  nM/L         0    NA     NA    
## 4 2009-10-06 Central ~     38.1     -121. water  nM/L         0    NA     NA    
## 5 2009-10-06 Sacramen~     38.4     -122. water  nM/L        NA     0     NA    
## 6 2009-10-06 Sacramen~     38.4     -122. water  nM/L        NA     0.610 15.6  
## # ... with 6 more variables: OrganoP <dbl>, Pyrethroids <dbl>, Atrazine <dbl>,
## #   Glyphosate <dbl>, GABA <dbl>, Neon <dbl>
```
