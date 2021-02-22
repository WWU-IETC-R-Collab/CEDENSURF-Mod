---
title: "CEDENSURF_Merge"
author: "Erika W"
date: "2/18/2021"
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

# Load & Prep Data

### CEDEN

Two files - one with tox data, and one with wq data


```r
# Load CEDEN Data
CEDENMod_Tox <- fread("https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_Toxicity.csv")

CEDENMod_WQ <- fread("https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_WQ.csv")
```

### SURF

Two files - one with wq data, and one with SED data ?

```r
SURFMod_SED <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/SURFMod_SED.csv")

SURFMod_WQ <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/SURFMod_water.csv")
```

### Append with source


```r
CEDENMod_Tox$Source <- rep("CEDEN", times=nrow(CEDENMod_Tox))

CEDENMod_WQ$Source <- rep("CEDEN", times=nrow(CEDENMod_WQ))

SURFMod_SED$Source <- rep("SURF", times=nrow(SURFMod_SED))

SURFMod_WQ$Source <- rep("SURF", times=nrow(SURFMod_WQ))
```

# Assess duplication within each database

Due to reported combined efforts to translate CEDEN data to SURF and vice versa, and issues with replicates being retained in each dataset, careful detection and elimination of duplicates should precede any analysis.

## CEDEN Data {.tabset}

Due to the structure of the tox data, I have a feeling it involves biological assays that were then related to WQ data sampled on that date (and already present in the CEDEN_WQ dataset. 

There are 117144 records in the WQ dataset
and 60637 in the Tox dataset. 

### CEDEN Tox 

In the tox dataset, there were several records related to a single species assessment - by creating a group identifier by the location, date, and organism assessed, I calculated that there were 2430 unique samples.

Within the Tox dataset, 11 different species are represented.

#### Exact duplicates

duplicated() produces a vector identifying which records are duplicates. This can be appended to the original dataset for further investigation. The first record is marked FALSE, while subsequent matches are marked TRUE


```r
# add index column identifying which entries are duplicated

CEDENMod_Tox$DupCheck <- duplicated(CEDENMod_Tox)

summary(CEDENMod_Tox$DupCheck)
```

```
##    Mode   FALSE    TRUE 
## logical   42363   18274
```

Is it only some programs that have duplication? 9/10 programs do.


```r
length(unique(CEDENMod_Tox$Program)) # 10
```

```
## [1] 10
```

```r
unique(CEDENMod_Tox$Program[CEDENMod_Tox$DupCheck == "TRUE"])
```

```
## [1] "Irrigated Lands Regulatory Program"                  
## [2] "Surface Water Ambient Monitoring Program"            
## [3] "Delta Monitoring RWQCB5"                             
## [4] "SF Bay Regional Monitoring for Water Quality"        
## [5] "Sacramento-San Joaquin River Delta Data"             
## [6] "SF Bay STLS Monitoring"                              
## [7] "BASMAA Regional Monitoring Coalition"                
## [8] "Delta Regional Monitoring Program"                   
## [9] "California Department of Transportation NPDES Permit"
```

#### Alternative (looser) assessment of duplication

If we assume that some columns may differ due to differences in data loading (ie: originally submitted to SURF and brought to CEDEN vs originally loaded to CEDEN), then we may want to use a looser structure to detect duplicates. 


```r
NoDup_Tox<- distinct(CEDENMod_Tox, Date, StationName, Analyte, CollectionMethod, Result, .keep_all= TRUE)

nrow(CEDENMod_Tox) - nrow(NoDup_Tox)
```

```
## [1] 25729
```

By assuming that records in the same location on the same date,  measuring the same analyte via the same collection method, and obtaining the same result are duplicates, we find almost 50% more duplicates. 
25729 records, to be specific. 

That is 42.4311889% of the dataset.

Including Organism name in the list of differentiating variables, it returns almost the exact same count of duplicates (18274) as exact duplication (18297) - indicating that a major source of WQ duplication is repeating WQ parameters for multiple species assessed.


```r
# Remove duplicate rows of the dataframe using multiple variables & ORGANISM NAME

nrow(CEDENMod_Tox) - nrow(distinct(CEDENMod_Tox, Date, StationName, CollectionMethod, OrganismName, Analyte, Result, .keep_all= TRUE))
```

```
## [1] 18274
```

Since we are using the df for the water parameters and not the associated organism survival, it's more useful to remove duplicate WQ entries regardless of the organism. We can also remove records that assess 'survival' and 'biomass' (since we aren't using this for the biotic parameters in our model).


```r
NoDup_Tox <- NoDup_Tox[NoDup_Tox$Analyte != "Survival"]

NoDup_Tox <- NoDup_Tox[NoDup_Tox$Analyte != "Biomass (wt/orig indiv)"]

# nrow = 31859
```

We're left with only 32011 unique, useful records in the tox dataset - or 52.7912001 % of the original tox data remaining

<br>

### CEDEN WQ

#### Exact duplicates

duplicated() produces a vector identifying which records are duplicated. This can be appended to the original dataset for further investigation.


```r
# add index column identifying which entries are duplicated

CEDENMod_WQ$DupCheck <- duplicated(CEDENMod_WQ)

summary(CEDENMod_WQ$DupCheck)
```

```
##    Mode   FALSE    TRUE 
## logical  116487     657
```

This method identified only 657 exact duplicates in the entire WQ dataset.

Only 9/17 programs present these exact duplicates


```r
length(unique(CEDENMod_WQ$Program)) # 17
```

```
## [1] 17
```

```r
unique(CEDENMod_WQ$Program[CEDENMod_WQ$DupCheck == "TRUE"])
```

```
## [1] "Delta Monitoring RWQCB5"                             
## [2] "Sacramento-San Joaquin River Delta Data"             
## [3] "SF Bay Regional Monitoring for Water Quality"        
## [4] "Surface Water Ambient Monitoring Program"            
## [5] "Suisun Bay Monitoring Project"                       
## [6] "SF Bay STLS Monitoring"                              
## [7] "California Department of Transportation NPDES Permit"
## [8] "Irrigated Lands Regulatory Program"                  
## [9] "American Rivers Restoration"
```

#### Looser assessment of Duplication

Utilizing the distinct() function to assume that records in the same location on the same date, measuring the same analyte via the same collection method and obtaining the same result are duplicates, we find 1536 duplicate records.

That is more than double the number of exact duplicates found, yet still only 1.3112067% of the entire WQ dataset.


```r
# Remove duplicate rows of the dataframe using multiple variables

NoDupWQ <- distinct(CEDENMod_WQ, Date, Analyte, StationName, CollectionMethod, Result, .keep_all= TRUE)

nrow(CEDENMod_WQ) - nrow(NoDupWQ)
```

```
## [1] 1536
```
<br>

<br>

## Merging CEDEN data

After dealing with duplication WITHIN the CEDEN tox and wq datasets, there were only 9 duplicate records found following the merged data. (75 if Collection Method is not a requirement for establishing duplication)


```r
# Strip column with duplicate check from both, and organism from tox
NoDup_Tox <- select(NoDup_Tox, -c(DupCheck, OrganismName))
NoDupWQ <- select(NoDupWQ, -DupCheck)

# Vector of column names to compare
WQ <- names(NoDupWQ)
TOX <- names(NoDup_Tox)

#Add missing columns to CEDEN WQ
DIF<- setdiff(TOX, WQ) # gives items in T that are not in W
NoDupWQ[, DIF] <- NA
```

```
## Warning in `[<-.data.table`(`*tmp*`, , DIF, value = NA): length(LHS)==0; no
## columns to delete or assign RHS to.
```

```r
#Add missing columns to CEDEN TOX
DIF<- setdiff(WQ, TOX) # gives items in W that are not in T
NoDup_Tox[, DIF] <- NA

# Finishing touches before merge; order columns to match - is this really necessary for merge? IDK

WQ <- sort(names(NoDupWQ))
TOX <- sort(names(NoDup_Tox))

NoDup_Tox <- NoDup_Tox %>% select(TOX)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(TOX)` instead of `TOX` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
NoDupWQ <- NoDupWQ %>% select(WQ)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(WQ)` instead of `WQ` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
# Check once all columns have perfect matches?
tibble(SURF = names(NoDup_Tox), CEDEN = names(NoDupWQ))
```

```
## # A tibble: 21 x 2
##    SURF             CEDEN           
##    <chr>            <chr>           
##  1 Analyte          Analyte         
##  2 CollectionMethod CollectionMethod
##  3 Date             Date            
##  4 Datum            Datum           
##  5 geometry         geometry        
##  6 Latitude         Latitude        
##  7 LocationCode     LocationCode    
##  8 Longitude        Longitude       
##  9 MatrixName       MatrixName      
## 10 MDL              MDL             
## # ... with 11 more rows
```

```r
# MERGE
CEDEN_ALL <- rbind(NoDupWQ,NoDup_Tox)
```


```r
# Ideas from: https://www.datasciencemadesimple.com/remove-duplicate-rows-r-using-dplyr-distinct-function/

# Remove duplicate rows of the dataframe using multiple variables

CEDEN_ALL_DupChecked <- distinct(CEDEN_ALL, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(CEDEN_ALL)-nrow(CEDEN_ALL_DupChecked)
```

```
## [1] 9
```
### Duplication Errors? {.tabset}

#### Queries
Are there duplicates retained by restricting to unique Collection Methods, which should not be?

There are 401 extra records retained when using Collection method to differentiate identical results. 343 of these duplicate records were not zero-result values, and therefore represent records of concern.


```r
CEDEN_ALL_DupChecked <- distinct(CEDEN_ALL, Date, Analyte, 
                                 CollectionMethod, StationName, 
                                 Result, .keep_all= TRUE)

CEDEN_ALL_Check1 <- distinct(CEDEN_ALL, Date, Analyte, 
                              StationName, Result, 
                             .keep_all= TRUE)

nrow(CEDEN_ALL_DupChecked) - nrow(CEDEN_ALL_Check1)
```

```
## [1] 401
```

```r
# ID columns retained only when use Collection Method also

DIF<- setdiff(CEDEN_ALL_DupChecked, CEDEN_ALL_Check1) # gives items in A that are not in B
nrow(DIF)
```

```
## [1] 401
```
Restricting the duplicates of concern to those which are non-zero values... It appears that there are 6 different collection methods represented in the duplicated data.

Sleuthing what is going on:

1. Many are duplication within a parent project, with one entry having sample method marked "not recorded" and the other defined. 

2. Two projects hold the majority of the duplication: CA Dept of Transit NPDES Monitoring 2014-2015 (153), and San Joaquin County Delta Water Quality Coalition (105)

3. Duplication within the Regional Monitoring Program - Status and Trends deserves closer investigation. Those samples have identical, >0 results for samples labeled with:

+ Collection_Method = Sediment_Core & Matrix = sediment
+ Collection_Method = Sediment_Grab & Matrix = overlyingwater

**--> Question** Does it make sense to have a sample defined as a sediment grab method + matrix of overlying water? Does it make sense for these samples to have identical results?


```r
# 343 duplicates had non-zero results

DIF <- DIF %>% filter(., Result != "0") 
nrow(DIF)
```

```
## [1] 343
```

```r
# Summary of collection methods which differentiate these duplicates. 

DIF$CollectionMethod <- as.factor(DIF$CollectionMethod)
summary(DIF$CollectionMethod)
```

```
## Auto sampler automatically triggered      Auto sampler manually triggered 
##                                   18                                   23 
##                         Not Recorded                        Sediment_Core 
##                                   79                                   18 
##                        Sediment_Grab                           Water_Grab 
##                                   86                                  119
```

```r
# Summary of parent projects represented in the duplicated data

DIF$ParentProject <- as.factor(DIF$ParentProject)
summary(DIF$ParentProject)
```

```
##                  BASMAA RMC Monitoring in WY2012 
##                                                2 
##                  BASMAA RMC Monitoring in WY2017 
##                                                3 
##    CA Dept of Transit NPDES Monitoring 2014-2015 
##                                              153 
##           Delta Island Monitoring Project RWQCB5 
##                                                3 
##               Delta RMP - Current Use Pesticides 
##                                               10 
##  Regional Monitoring Program - Status and Trends 
##                                               27 
##        Sacramento Valley Water Quality Coalition 
##                                               33 
## San Joaquin County Delta Water Quality Coalition 
##                                              105 
##                                 SWAMP Monitoring 
##                                                4 
##                            SWAMP RWB5 Monitoring 
##                                                2 
##                    SWAMP Stream Pollution Trends 
##                                                1
```

```r
# How does number of "not recorded" methods in these duplications compare to those in the entire df?

paste("Collection Method = Not Recorded in...")
```

```
## [1] "Collection Method = Not Recorded in..."
```

```r
tibble(Entire = nrow(CEDEN_ALL_DupChecked[CEDEN_ALL_DupChecked$CollectionMethod== "Not Recorded"]), Dup = nrow(DIF[DIF$CollectionMethod== "Not Recorded"]))
```

```
## # A tibble: 1 x 2
##   Entire   Dup
##    <int> <int>
## 1    307    79
```

#### Printout of duplicated data

Again, these duplicates are retained in the current methodology due to differences between Collection Method. The question is whether they are truly different records, or whether we need to further refine the selection criteria.


```r
# Bring back original data associated with each duplicate identified

Dups <- vector("list") # save empty list where each subset be saved

for (i in 1:343){
  Dups[[i]] <- CEDEN_ALL %>% filter(., Analyte == DIF$Analyte[i],
                                         Date == DIF$Date[i], 
                                         StationName == DIF$StationName[i],
                                    Result == DIF$Result[i])
  }

Example <- do.call(rbind, Dups)
Example
```

```
##                               Analyte                     CollectionMethod
##   1:         Benz(a)anthracene, Total                         Not Recorded
##   2:         Benz(a)anthracene, Total                           Water_Grab
##   3:                  Aluminum, Total Auto sampler automatically triggered
##   4:                  Aluminum, Total                         Not Recorded
##   5:                      Zinc, Total Auto sampler automatically triggered
##  ---                                                                      
## 682: Ammonia as NH3, Unionized, Total                        Sediment_Grab
## 683: Ammonia as NH3, Unionized, Total                        Sediment_Core
## 684: Ammonia as NH3, Unionized, Total                        Sediment_Grab
## 685:                      Temperature                        Sediment_Grab
## 686:                      Temperature                        Sediment_Core
##            Date Datum                       geometry Latitude LocationCode
##   1: 2014-02-26 NAD83 c(-121.49340057373, 38.456501)  38.4565 Not Recorded
##   2: 2014-02-26 NAD83 c(-121.49340057373, 38.456501)  38.4565 Not Recorded
##   3: 2014-02-26 NAD83 c(-121.49340057373, 38.456501)  38.4565 Not Recorded
##   4: 2014-02-26 NAD83 c(-121.49340057373, 38.456501)  38.4565 Not Recorded
##   5: 2014-02-26 NAD83 c(-121.49340057373, 38.456501)  38.4565 Not Recorded
##  ---                                                                      
## 682: 2012-04-23 NAD83               c(-122, 38.0877)  38.0877   OpenWater1
## 683: 2012-04-23 NAD83               c(-122, 38.0877)  38.0877   OpenWater1
## 684: 2012-04-23 NAD83               c(-122, 38.0877)  38.0877   OpenWater1
## 685: 2012-04-23 NAD83           c(-122.043, 38.0882)  38.0882   OpenWater1
## 686: 2012-04-23 NAD83           c(-122.043, 38.0882)  38.0882   OpenWater1
##      Longitude     MatrixName     MDL
##   1: -121.4934    samplewater   0.028
##   2: -121.4934    samplewater   0.028
##   3: -121.4934    samplewater 200.000
##   4: -121.4934    samplewater 200.000
##   5: -121.4934    samplewater  14.000
##  ---                                 
## 682: -122.0000       sediment      NA
## 683: -122.0000 overlyingwater      NA
## 684: -122.0000       sediment      NA
## 685: -122.0430       sediment      NA
## 686: -122.0430 overlyingwater      NA
##                                        ParentProject
##   1:   CA Dept of Transit NPDES Monitoring 2014-2015
##   2:   CA Dept of Transit NPDES Monitoring 2014-2015
##   3:   CA Dept of Transit NPDES Monitoring 2014-2015
##   4:   CA Dept of Transit NPDES Monitoring 2014-2015
##   5:   CA Dept of Transit NPDES Monitoring 2014-2015
##  ---                                                
## 682: Regional Monitoring Program - Status and Trends
## 683: Regional Monitoring Program - Status and Trends
## 684: Regional Monitoring Program - Status and Trends
## 685: Regional Monitoring Program - Status and Trends
## 686: Regional Monitoring Program - Status and Trends
##                                                   Program
##   1: California Department of Transportation NPDES Permit
##   2: California Department of Transportation NPDES Permit
##   3: California Department of Transportation NPDES Permit
##   4: California Department of Transportation NPDES Permit
##   5: California Department of Transportation NPDES Permit
##  ---                                                     
## 682:         SF Bay Regional Monitoring for Water Quality
## 683:         SF Bay Regional Monitoring for Water Quality
## 684:         SF Bay Regional Monitoring for Water Quality
## 685:         SF Bay Regional Monitoring for Water Quality
## 686:         SF Bay Regional Monitoring for Water Quality
##                                            Project rb_number    regional_board
##   1: CA Dept of Transit NPDES Monitoring 2013-2014         5    Central Valley
##   2: CA Dept of Transit NPDES Monitoring 2013-2014         5    Central Valley
##   3: CA Dept of Transit NPDES Monitoring 2013-2014         5    Central Valley
##   4: CA Dept of Transit NPDES Monitoring 2013-2014         5    Central Valley
##   5: CA Dept of Transit NPDES Monitoring 2013-2014         5    Central Valley
##  ---                                                                          
## 682:                    2012 RMP Status and Trends         2 San Francisco Bay
## 683:                    2012 RMP Status and Trends         2 San Francisco Bay
## 684:                    2012 RMP Status and Trends         2 San Francisco Bay
## 685:                    2012 RMP Status and Trends         2 San Francisco Bay
## 686:                    2012 RMP Status and Trends         2 San Francisco Bay
##        Result Source StationCode              StationName        Subregion
##   1: 2.80e-02  CEDEN       3-217 28-ft RVTS Strip Station Sacramento River
##   2: 2.80e-02  CEDEN       3-217 28-ft RVTS Strip Station Sacramento River
##   3: 6.30e+04  CEDEN       3-217 28-ft RVTS Strip Station Sacramento River
##   4: 6.30e+04  CEDEN       3-217 28-ft RVTS Strip Station Sacramento River
##   5: 3.30e+03  CEDEN       3-217 28-ft RVTS Strip Station Sacramento River
##  ---                                                                      
## 682: 1.00e-02  CEDEN      SU128S      Suisun Bay (SU128S)       Suisun Bay
## 683: 3.00e-02  CEDEN      SU128S      Suisun Bay (SU128S)       Suisun Bay
## 684: 3.00e-02  CEDEN      SU128S      Suisun Bay (SU128S)       Suisun Bay
## 685: 1.54e+01  CEDEN      SU131S      Suisun Bay (SU131S)       Suisun Bay
## 686: 1.54e+01  CEDEN      SU131S      Suisun Bay (SU131S)       Suisun Bay
##       Unit
##   1:  ug/L
##   2:  ug/L
##   3:  ug/L
##   4:  ug/L
##   5:  ug/L
##  ---      
## 682:  mg/L
## 683:  mg/L
## 684:  mg/L
## 685: Deg C
## 686: Deg C
```

### Splitting Analyte Column

Because of formatting differences between the amount of data recorded under "Analyte" in CEDEN compared to the "Chemical_name" in SURF (which will be renamed Analyte), we opted to split the data in CEDEN's analyte column into two columns: 
Analyte (Chemical Name), and Analyte_Type (ie: total or particulate)


```r
CEDEN_ALL_DupChecked <- CEDEN_ALL_DupChecked %>%
  separate(Analyte, into = c("Analyte", "Analyte_type"), sep = ", " , extra = "merge")
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 21820 rows [2, 3,
## 4, 5, 7, 8, 9, 10, 11, 12, 14, 19, 20, 22, 23, 24, 28, 94, 118, 144, ...].
```

```r
# sort(unique(CEDEN_ALL_DupChecked$Analyte))
head(sort(unique(CEDEN_ALL_DupChecked$Analyte))) # 908 unique Analytes total
```

```
## [1] ""                                          
## [2] "1,2-bis(2,4,6- tribromophenoxy)ethane"     
## [3] "2-ethyl-1-hexyl-2,3,4,5-tetrabromobenzoate"
## [4] "2-Ethylhexyl-diphenyl phosphate"           
## [5] "2,4,6-tribromophenyl allyl ether"          
## [6] "Abamectin"
```

```r
# Looks like requiring the separation from extra to analyte to contain a comma and a space allowed the full names to be retained. Without that, the separation led to an analyte "1" which should have been 1,2-bis(2,4,6- tribromophenoxy)ethane, etc.
```


```r
# How many records available via full CEDEN merge?
nrow(CEDEN_ALL_DupChecked)
```

```
## [1] 147610
```
In the end, there are 147610 unique records available through the CEDEN datasets. 

<br>

<br>


## SURF data {.tabset}

There are 129323 records in the WQ dataset
and 36027 in the SED dataset. 

### SURF Water

#### Data Prep

**Correct mismatched column names**

Rather than using Steven's method of renaming using a list (which is dependent on an expected number and order of column names), I renamed each column individually


```r
SURFMod_WQ$Unit <- "ppb"

SURFMod_WQ <- SURFMod_WQ %>% rename(Date = Sample_date,
          Analyte = Chemical_name, 
          Result = Concentration..ppb., 
          CollectionMethod = Sample_type, 
          StationCode = Site_code,
          StationName = Site_name,
          MDL = Method_detection_level..ppb.,
          LOQ = Level_of_quantification..ppb.)
```

#### Exact duplicates

duplicated() produces a vector identifying which records are duplicated. This can be appended to the original dataset for further investigation.


```r
# add index column identifying which entries are duplicated

SURFMod_WQ$DupCheck <- duplicated(SURFMod_WQ)

summary(SURFMod_WQ$DupCheck)
```

```
##    Mode   FALSE 
## logical  129323
```

Fantastic. There are no exact duplicates within the SURF water data - even when we include the CEDEN allowed in SURF

#### Looser assessment of Duplication

Utilizing the distinct() function to assume that records in the same location on the same date, measuring the same analyte and obtaining the same result are duplicates, we find 17756 (there were only 5,676 w/o CEDEN data) duplicate records.


```r
# Remove duplicate rows of the dataframe using multiple variables

NoDupWQ <- distinct(SURFMod_WQ, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(SURFMod_WQ) - nrow(NoDupWQ)
```

```
## [1] 17756
```
<br>

<br>

### SURF sediment

#### Data Prep

More investigating will probably be better, but for now we can simply remove those data defined in SURF as sourced from CEDEN

**Correct mismatched column names**

```r
SURFMod_SED$Unit <- "ppb"

SURFMod_SED <- SURFMod_SED %>% rename(Date = Sample_date,
          Analyte = Chemical_name, 
          Result = Concentration..ppb., 
          CollectionMethod = Sample_type, 
          StationCode = Site_code,
          StationName = Site_name,
          MDL = Method_detection_level..ppb.,
          LOQ = Level_of_quantification..ppb.)
```

#### Exact duplicates

duplicated() produces a vector identifying which records are duplicated. This can be appended to the original dataset for further investigation.


```r
# add index column identifying which entries are duplicated

SURFMod_SED$DupCheck <- duplicated(SURFMod_SED)

summary(SURFMod_SED$DupCheck)
```

```
##    Mode   FALSE 
## logical   36027
```

Fantastic. There are no exact duplicates within the SURF sediment data, even with CEDEN also contained.

#### Looser assessment of Duplication

Utilizing the distinct() function to assume that records in the same location on the same date, measuring the same analyte and obtaining the same result are duplicates, we find only 4,120 (was 784 without CEDEN sources)


```r
# Remove duplicate rows of the dataframe using multiple variables

NoDupSED <- distinct(SURFMod_SED, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(SURFMod_SED) - nrow(NoDupSED)
```

```
## [1] 4120
```
<br>

<br>

## Merge SURF df


```r
# Strip column with duplicate check from both
NoDupSED <- select(NoDupSED, -DupCheck)
NoDupWQ <- select(NoDupWQ, -DupCheck)

# Vector of column names to compare
WQ <- names(NoDupWQ)
SED <- names(NoDupSED)

#Add missing columns to CEDEN WQ
DIF<- setdiff(SED, WQ) # gives items in S that are not in W
NoDupWQ[, DIF] <- NA

#Add missing columns to CEDEN SED
DIF<- setdiff(WQ, SED) # gives items in W that are not in S
NoDupSED[, DIF] <- NA
```

```
## Warning in `[<-.data.table`(`*tmp*`, , DIF, value = NA): length(LHS)==0; no
## columns to delete or assign RHS to.
```

```r
# Finishing touches before merge; order columns to match - is this really necessary for merge? IDK

WQ <- sort(names(NoDupWQ))
SED <- sort(names(NoDupSED))

NoDupSED <- NoDupSED %>% select(SED)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(SED)` instead of `SED` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
NoDupWQ <- NoDupWQ %>% select(WQ)

# Check once all columns have perfect matches?
tibble(SURF = names(NoDupSED), CEDEN = names(NoDupWQ))
```

```
## # A tibble: 22 x 2
##    SURF             CEDEN           
##    <chr>            <chr>           
##  1 Agency           Agency          
##  2 Analyte          Analyte         
##  3 CollectionMethod CollectionMethod
##  4 County           County          
##  5 Data.source      Data.source     
##  6 Date             Date            
##  7 geometry         geometry        
##  8 Latitude         Latitude        
##  9 Longitude        Longitude       
## 10 LOQ              LOQ             
## # ... with 12 more rows
```

```r
# MERGE
SURF_ALL <- rbind(NoDupWQ,NoDupSED)
```

### Duplication between SURF sets

ZERO duplication found between the SED and WQ datasets, assuming duplicates would have to have the exact same Location, Date, Analyte, Collection Method, and Result.


```r
# Ideas from: https://www.datasciencemadesimple.com/remove-duplicate-rows-r-using-dplyr-distinct-function/

# Remove duplicate rows of the dataframe using multiple variables

SURF_ALL_DupChecked <- distinct(SURF_ALL, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(SURF_ALL)-nrow(SURF_ALL_DupChecked)
```

```
## [1] 0
```

### Request: Examples of SURF duplicates when Collection Method is omitted? {.tabset}

#### With CEDEN Data

Able to make df of "duplicate" entries for further inspection. Though **ALL WERE DUE TO RESULT == 0 WITHOUT CEDEN INCLUDED**

When retaining data sourced from CEDEN, there are some instances of results != 0 with identical results.


```r
# Create DF limited to only one record per date,loc,analyte,result combo

SURF_ALL_Check1 <- distinct(SURF_ALL, Date, Analyte, StationName, Result, .keep_all= TRUE)

# ID columns retained only when use Collection Method also

DIF<- setdiff(SURF_ALL_DupChecked, SURF_ALL_Check1) # gives items in A that are not in B

# Produce vector of all different "Result" values in the duplicated data

length(unique(SURF_ALL$Result)) # how many different "result" values are represented in the entire combined SURF dataset
```

```
## [1] 2467
```

```r
unique(DIF$Result) # All unique "result" values within the duplicates that were removed.
```

```
##  [1] 0.0000 0.9000 0.0036 0.0160 0.0100 0.0140 0.0250 0.0130 0.0032 0.0270
## [11] 0.0430 0.0024 0.0029 0.1500 0.0470
```

These duplicates have some commonalities;

1. they are all brought into SURF from "CEDEN" (except for one "OTHER")

2. they have different "collection methods", typically one with a collection method specified (Study CD 523), and one without the method specified (Study CD 305)

3. Duplicates are almost all between Study CD's 523 & 305. Only one pair of replicates were due to Study CD's 451 & 80. 


```r
DIF <- DIF %>% filter(., Result != "0") # Only 19 duplicates were removed that had non-0-result values

# Bring back original data associated with each duplicate identified

Dups <- vector("list") # save empty list where each subset be saved

for (i in 1:19){
  Dups[[i]] <- SURF_ALL %>% filter(., Analyte == DIF$Analyte[i],
                                         Date == DIF$Date[i], 
                                         StationName == DIF$StationName[i])
  }

Example <- do.call(rbind, Dups)
Example
```

```
##                                     Agency            Analyte
##  1:        Aquatic Toxicology LaboratoryÂ              diuron
##  2:     Sacramento River Watershed Program             diuron
##  3:                      ADH Environmental       tetramethrin
##  4: San Francisco Estuary Institute (SFEI)       tetramethrin
##  5:                      ADH Environmental   fipronil sulfone
##  6: San Francisco Estuary Institute (SFEI)   fipronil sulfone
##  7:                      ADH Environmental           fipronil
##  8: San Francisco Estuary Institute (SFEI)           fipronil
##  9:                      ADH Environmental   fipronil sulfone
## 10: San Francisco Estuary Institute (SFEI)   fipronil sulfone
## 11:                      ADH Environmental         cyfluthrin
## 12: San Francisco Estuary Institute (SFEI)         cyfluthrin
## 13:                      ADH Environmental   fipronil sulfone
## 14: San Francisco Estuary Institute (SFEI)   fipronil sulfone
## 15:                      ADH Environmental lambda cyhalothrin
## 16: San Francisco Estuary Institute (SFEI) lambda cyhalothrin
## 17:                      ADH Environmental         bifenthrin
## 18: San Francisco Estuary Institute (SFEI)         bifenthrin
## 19:                      ADH Environmental         bifenthrin
## 20: San Francisco Estuary Institute (SFEI)         bifenthrin
## 21:                      ADH Environmental         cyfluthrin
## 22: San Francisco Estuary Institute (SFEI)         cyfluthrin
## 23:                      ADH Environmental         cyfluthrin
## 24: San Francisco Estuary Institute (SFEI)         cyfluthrin
## 25:                      ADH Environmental           fipronil
## 26: San Francisco Estuary Institute (SFEI)           fipronil
## 27:                      ADH Environmental           fipronil
## 28: San Francisco Estuary Institute (SFEI)           fipronil
## 29:                      ADH Environmental   fipronil sulfone
## 30: San Francisco Estuary Institute (SFEI)   fipronil sulfone
## 31:                      ADH Environmental         bifenthrin
## 32: San Francisco Estuary Institute (SFEI)         bifenthrin
## 33:                      ADH Environmental         cyfluthrin
## 34: San Francisco Estuary Institute (SFEI)         cyfluthrin
## 35:                      ADH Environmental           fipronil
## 36: San Francisco Estuary Institute (SFEI)           fipronil
## 37:                      ADH Environmental         bifenthrin
## 38: San Francisco Estuary Institute (SFEI)         bifenthrin
##                                     Agency            Analyte
##              CollectionMethod       County Data.source       Date
##  1:       Sample-type unknown       Solano       CEDEN 2000-02-16
##  2:               Grab sample       Solano       Other 2000-02-16
##  3: Single whole water sample Contra Costa       CEDEN 2013-04-05
##  4:                           Contra Costa       CEDEN 2013-04-05
##  5:       Sample-type unknown Contra Costa       CEDEN 2012-12-01
##  6:                           Contra Costa       CEDEN 2012-12-01
##  7:       Sample-type unknown Contra Costa       CEDEN 2012-11-29
##  8:                           Contra Costa       CEDEN 2012-11-29
##  9:       Sample-type unknown Contra Costa       CEDEN 2012-11-29
## 10:                           Contra Costa       CEDEN 2012-11-29
## 11: Single whole water sample Contra Costa       CEDEN 2012-12-01
## 12:                           Contra Costa       CEDEN 2012-12-01
## 13:       Sample-type unknown Contra Costa       CEDEN 2012-12-22
## 14:                           Contra Costa       CEDEN 2012-12-22
## 15: Single whole water sample Contra Costa       CEDEN 2012-12-01
## 16:                           Contra Costa       CEDEN 2012-12-01
## 17: Single whole water sample Contra Costa       CEDEN 2013-04-05
## 18:                           Contra Costa       CEDEN 2013-04-05
## 19: Single whole water sample Contra Costa       CEDEN 2012-12-22
## 20:                           Contra Costa       CEDEN 2012-12-22
## 21: Single whole water sample Contra Costa       CEDEN 2013-04-05
## 22:                           Contra Costa       CEDEN 2013-04-05
## 23: Single whole water sample Contra Costa       CEDEN 2012-11-29
## 24:                           Contra Costa       CEDEN 2012-11-29
## 25:       Sample-type unknown Contra Costa       CEDEN 2012-12-01
## 26:                           Contra Costa       CEDEN 2012-12-01
## 27:       Sample-type unknown Contra Costa       CEDEN 2012-12-22
## 28:                           Contra Costa       CEDEN 2012-12-22
## 29:       Sample-type unknown Contra Costa       CEDEN 2013-04-05
## 30:                           Contra Costa       CEDEN 2013-04-05
## 31: Single whole water sample Contra Costa       CEDEN 2012-12-01
## 32:                           Contra Costa       CEDEN 2012-12-01
## 33: Single whole water sample Contra Costa       CEDEN 2012-12-22
## 34:                           Contra Costa       CEDEN 2012-12-22
## 35:       Sample-type unknown Contra Costa       CEDEN 2013-04-05
## 36:                           Contra Costa       CEDEN 2013-04-05
## 37: Single whole water sample Contra Costa       CEDEN 2012-11-29
## 38:                           Contra Costa       CEDEN 2012-11-29
##              CollectionMethod       County Data.source       Date
##                   geometry Latitude Longitude    LOQ        MDL Record_id
##  1:    c(-121.6593, 38.19) 38.19000 -121.6593 0.4000 -9.990e+02   1616574
##  2:    c(-121.6593, 38.19) 38.19000 -121.6593 0.4000         NA    104422
##  3: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  1.324e-04   1105972
##  4: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  1.324e-04    464883
##  5: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04   1206612
##  6: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04    472857
##  7: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04   1206615
##  8: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04    478238
##  9: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04   1206611
## 10: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04    465500
## 11: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  2.000e-04   1105938
## 12: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  2.000e-04    470161
## 13: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04   1206613
## 14: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04    479675
## 15: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  2.000e-04   1105942
## 16: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  2.000e-04    470985
## 17: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  6.620e-05   1105936
## 18: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  6.620e-05    470154
## 19: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0017  3.000e-04   1105935
## 20: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0017  3.000e-04    470960
## 21: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  1.324e-04   1105940
## 22: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  1.324e-04    469790
## 23: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  2.000e-04   1105937
## 24: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  2.000e-04    469794
## 25: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04   1206616
## 26: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04    479007
## 27: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04   1206617
## 28: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04    479721
## 29: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04   1206614
## 30: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04    465543
## 31: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  1.000e-04   1105934
## 32: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  1.000e-04    470522
## 33: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0017  7.000e-04   1105939
## 34: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0017  7.000e-04    470532
## 35: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04   1206618
## 36: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020  5.000e-04    465162
## 37: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  1.000e-04   1105933
## 38: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015  1.000e-04    464882
##                   geometry Latitude Longitude    LOQ        MDL Record_id
##     Result Source StationCode
##  1: 0.9000   SURF       48_12
##  2: 0.9000   SURF       48_12
##  3: 0.0036   SURF        07_4
##  4: 0.0036   SURF        07_4
##  5: 0.0160   SURF        07_4
##  6: 0.0160   SURF        07_4
##  7: 0.0100   SURF        07_4
##  8: 0.0100   SURF        07_4
##  9: 0.0140   SURF        07_4
## 10: 0.0140   SURF        07_4
## 11: 0.0250   SURF        07_4
## 12: 0.0250   SURF        07_4
## 13: 0.0130   SURF        07_4
## 14: 0.0130   SURF        07_4
## 15: 0.0032   SURF        07_4
## 16: 0.0032   SURF        07_4
## 17: 0.0270   SURF        07_4
## 18: 0.0270   SURF        07_4
## 19: 0.0430   SURF        07_4
## 20: 0.0430   SURF        07_4
## 21: 0.0024   SURF        07_4
## 22: 0.0024   SURF        07_4
## 23: 0.0029   SURF        07_4
## 24: 0.0029   SURF        07_4
## 25: 0.0130   SURF        07_4
## 26: 0.0130   SURF        07_4
## 27: 0.0100   SURF        07_4
## 28: 0.0100   SURF        07_4
## 29: 0.0140   SURF        07_4
## 30: 0.0140   SURF        07_4
## 31: 0.1500   SURF        07_4
## 32: 0.1500   SURF        07_4
## 33: 0.0024   SURF        07_4
## 34: 0.0024   SURF        07_4
## 35: 0.0100   SURF        07_4
## 36: 0.0100   SURF        07_4
## 37: 0.0470   SURF        07_4
## 38: 0.0470   SURF        07_4
##     Result Source StationCode
##                                                  StationName Study_cd
##  1:                            Cache Slough near Ryers Ferry      451
##  2:                            Cache Slough near Ryers Ferry       80
##  3: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
##  4: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
##  5: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
##  6: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
##  7: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
##  8: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
##  9: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 10: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 11: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 12: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 13: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 14: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 15: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 16: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 17: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 18: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 19: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 20: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 21: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 22: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 23: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 24: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 25: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 26: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 27: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 28: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 29: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 30: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 31: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 32: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 33: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 34: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 35: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 36: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
## 37: Marsh Creek at Cypress Rd bridge (trib to western Delta)      523
## 38: Marsh Creek at Cypress Rd bridge (trib to western Delta)      305
##                                                  StationName Study_cd
##                                                                                                     Study_description
##  1: Sacramento River Watershed Program , Sacramento River Watershed Program , Sacramento River Watershed Program 2000
##  2:  Sacramento River Watershed Program data, 1998-2002, monitoring years 1-4.(No pesticide data collected for 1998).
##  3:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
##  4: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
##  5:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
##  6: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
##  7:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
##  8: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
##  9:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 10: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 11:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 12: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 13:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 14: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 15:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 16: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 17:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 18: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 19:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 20: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 21:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 22: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 23:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 24: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 25:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 26: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 27:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 28: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 29:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 30: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 31:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 32: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 33:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 34: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 35:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 36: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
## 37:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
## 38: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
##                                                                                                     Study_description
##                Study_weblink   Subregion Total organic carbon (%) Unit
##  1: http://www.sacriver.org/ North Delta                       NA  ppb
##  2:                       NA North Delta                       NA  ppb
##  3:    http://www.ceden.org/  Confluence                       NA  ppb
##  4:    http://www.ceden.org/  Confluence                       NA  ppb
##  5:    http://www.ceden.org/  Confluence                       NA  ppb
##  6:    http://www.ceden.org/  Confluence                       NA  ppb
##  7:    http://www.ceden.org/  Confluence                       NA  ppb
##  8:    http://www.ceden.org/  Confluence                       NA  ppb
##  9:    http://www.ceden.org/  Confluence                       NA  ppb
## 10:    http://www.ceden.org/  Confluence                       NA  ppb
## 11:    http://www.ceden.org/  Confluence                       NA  ppb
## 12:    http://www.ceden.org/  Confluence                       NA  ppb
## 13:    http://www.ceden.org/  Confluence                       NA  ppb
## 14:    http://www.ceden.org/  Confluence                       NA  ppb
## 15:    http://www.ceden.org/  Confluence                       NA  ppb
## 16:    http://www.ceden.org/  Confluence                       NA  ppb
## 17:    http://www.ceden.org/  Confluence                       NA  ppb
## 18:    http://www.ceden.org/  Confluence                       NA  ppb
## 19:    http://www.ceden.org/  Confluence                       NA  ppb
## 20:    http://www.ceden.org/  Confluence                       NA  ppb
## 21:    http://www.ceden.org/  Confluence                       NA  ppb
## 22:    http://www.ceden.org/  Confluence                       NA  ppb
## 23:    http://www.ceden.org/  Confluence                       NA  ppb
## 24:    http://www.ceden.org/  Confluence                       NA  ppb
## 25:    http://www.ceden.org/  Confluence                       NA  ppb
## 26:    http://www.ceden.org/  Confluence                       NA  ppb
## 27:    http://www.ceden.org/  Confluence                       NA  ppb
## 28:    http://www.ceden.org/  Confluence                       NA  ppb
## 29:    http://www.ceden.org/  Confluence                       NA  ppb
## 30:    http://www.ceden.org/  Confluence                       NA  ppb
## 31:    http://www.ceden.org/  Confluence                       NA  ppb
## 32:    http://www.ceden.org/  Confluence                       NA  ppb
## 33:    http://www.ceden.org/  Confluence                       NA  ppb
## 34:    http://www.ceden.org/  Confluence                       NA  ppb
## 35:    http://www.ceden.org/  Confluence                       NA  ppb
## 36:    http://www.ceden.org/  Confluence                       NA  ppb
## 37:    http://www.ceden.org/  Confluence                       NA  ppb
## 38:    http://www.ceden.org/  Confluence                       NA  ppb
##                Study_weblink   Subregion Total organic carbon (%) Unit
```


```r
unique(Example$Data.source)
```

```
## [1] "CEDEN" "Other"
```

```r
unique(Example$Study_cd)
```

```
## [1] 451  80 523 305
```

Given this info, it is worthwhile to ask if there are any unique entries by Study_cd 305, or if all are duplications of more complete data by Study_523.

**ALL ENTRIES IN SURF BY STUDY CD 305 ARE DUPS**


```r
length(SURF_ALL$Study_cd[SURF_ALL$Study_cd == "305"]) # Number of entries by this Agency/Program in all SURF data
```

```
## [1] 64
```

```r
length(Example$Study_cd[SURF_ALL$Study_cd == "305"]) # Number of entries that were part of the duplication findings
```

```
## [1] 64
```

#### Without CEDEN 

ZERO duplication found between the SED and WQ datasets, assuming duplicates would have to have the exact same Location, Date, Analyte, Collection Method, and Result.


```r
# Ideas from: https://www.datasciencemadesimple.com/remove-duplicate-rows-r-using-dplyr-distinct-function/

# Filter out data sourced from CEDEN

SURF_ALL_NC <- filter(SURF_ALL, Data.source != "CEDEN")
# Remove duplicate rows of the dataframe using multiple variables

SURF_ALL_DupCheckedNC <- distinct(SURF_ALL_NC, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(SURF_ALL_NC)-nrow(SURF_ALL_DupCheckedNC)
```

```
## [1] 0
```

*Note* I had started without 'Collection Method' in the distinct() query, but added it as an additional requirement for establishing duplication because without that record, the difference between results for water vs sediment were not recognized and **26127** records were considered duplicates following the merging of the SURF data.

This is still surprising to me, because that means the RESULT concentration was identical for both sediment and water samples. 

By creating a df of "duplicate" entries for further inspection. Findings confirm that
**ALL ARE DUE TO RESULT == 0**


```r
# Create DF limited to only one record per date,loc,analyte,result combo

SURF_ALL_Check1 <- distinct(SURF_ALL_NC, Date, Analyte, StationName, Result, .keep_all= TRUE)

# ID columns retained only when use Collection Method also

DIF<- setdiff(SURF_ALL_DupCheckedNC, SURF_ALL_Check1) # gives items in A that are not in B

# Produce vector of all different "Result" values in the duplicated data

length(unique(SURF_ALL_NC$Result)) # how many different "result" values are represented in the entire combined SURF dataset
```

```
## [1] 1210
```

```r
unique(DIF$Result) # All unique "result" values within the duplicates that were removed.
```

```
## [1] 0
```

### Result of SURF Merge

Due to duplication found above, remove study 305 that was entirely duplication prior to next steps (Because this duplication was not able to be removed by simply filtering on date, loc, analyte, collection method, and result)


```r
SURF_ALL_DupChecked <- filter(SURF_ALL_DupChecked, Study_cd != "305")

nrow(SURF_ALL_DupChecked)
```

```
## [1] 143410
```
There are 143,000 unique records available through SURF.

<br>

<br>

# Merge SURF and CEDEN
Now that each dataset has been independently inspected for duplication *within* each dataset, they can be merged and searched for duplication *between* the datasets.

### Data prep

**Match columns to CEDEN data**


```r
C <- names(CEDEN_ALL_DupChecked)
S <- names(SURF_ALL_DupChecked)

DIF<- setdiff(S, C) # gives items in S that are not in C
```

Columns that were in SURF and not CEDEN:
Agency, County, Data.source, LOQ, Record_id, Study_cd, Study_description, Study_weblink, Total organic carbon (%)


```r
#Add missing columns to CEDEN
CEDEN_ALL_DupChecked[, DIF] <- NA
```


```r
#Add missing columns to SURF
DIF<- setdiff(C, S) # gives items in C that are not in S
SURF_ALL_DupChecked[, DIF] <- NA
```

Columns that were in CEDEN and not SURF:
` r DIF`

### Merge

Reorder columns to align, then merge with rbind()


```r
# Finishing touches before merge; order columns to match - is this really necessary for merge? IDK

C <- sort(names(CEDEN_ALL_DupChecked)) # 908
S <- sort(names(SURF_ALL_DupChecked)) # 327

SURF_ALL_DupChecked <- SURF_ALL_DupChecked %>% select(S)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(S)` instead of `S` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
CEDEN_ALL_DupChecked <- CEDEN_ALL_DupChecked %>% select(C)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(C)` instead of `C` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
# Check once all columns have perfect matches?
tibble(SURF = names(SURF_ALL_DupChecked), CEDEN = names(CEDEN_ALL_DupChecked))
```

```
## # A tibble: 31 x 2
##    SURF             CEDEN           
##    <chr>            <chr>           
##  1 Agency           Agency          
##  2 Analyte          Analyte         
##  3 Analyte_type     Analyte_type    
##  4 CollectionMethod CollectionMethod
##  5 County           County          
##  6 Data.source      Data.source     
##  7 Date             Date            
##  8 Datum            Datum           
##  9 geometry         geometry        
## 10 Latitude         Latitude        
## # ... with 21 more rows
```

```r
# MERGE
CEDENSURF <- rbind(SURF_ALL_DupChecked, CEDEN_ALL_DupChecked)
```

### Check for duplicates

None found! Great start.


```r
# Ideas from: https://www.datasciencemadesimple.com/remove-duplicate-rows-r-using-dplyr-distinct-function/

# Remove duplicate rows of the dataframe using multiple variables

CEDENSURF_DupChecked <- distinct(CEDENSURF, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(CEDENSURF)-nrow(CEDENSURF_DupChecked)
```

```
## [1] 15296
```
# Next Steps

### Naming conventions

We should check whether splitting the Analyte column in CEDEN helped to unify the nomenclature between datasets, before we can confidently suggest there is no more duplication.

The lists of analytes are MUCH closer to eachother using this method, though it isn't perfect. Some further generalizations seem to exist in SURF Chemical_names, and more analytes are recorded in CEDEN

```r
analyte_C <- sort(unique(CEDENSURF$Analyte[CEDENSURF$Source == "CEDEN"]))
# 908 (was >1800 before splitting the column)


analyte_S <- sort(unique(CEDENSURF$Analyte[CEDENSURF$Source == "SURF"]))
# 307

DIF <- setdiff(analyte_C, analyte_S)
# Main things in C and not in S:
# PCH as ... three dif, 
# PCB - MANY differnt numbers, 
# PBDE = MANY
# Dechlorane - several
# Two each for DDD, DDE, and DDT

# Funny analytes to note: Young/female
```

There are ` r length(analyte_C) ` different analytes in the CEDEN data, while only 327 in the SURF.

It appears that SURF gives the simple analyte name, while CEDEN retains additional information in the analyte name specifying 'total' vs 'particulate' vs 
'dissolved'.

For the purposes of this merged dataset, it may make sense to simplify the analyte names from CEDEN, and allow the original (unmerged with SURF) CEDEN data to be used in situations where that additional information is required.

For example, a subset of "Analytes" in each database follow:

```r
# Show example of differences
tibble(CEDEN = c(analyte_C[20:40]), SURF = c(analyte_S[10:30]))
```

```
## # A tibble: 21 x 2
##    CEDEN               SURF                    
##    <chr>               <chr>                   
##  1 Alkalinity as CaCO3 acephate                
##  2 Allethrin           acet                    
##  3 Aluminum            acetamiprid             
##  4 Ametryn             acibenzolar-s-methyl    
##  5 Aminocarb           acifluorfen, sodium salt
##  6 Ammonia as N        alachlor                
##  7 Ammonia as NH3      aldicarb                
##  8 Ammonium as N       aldicarb sulfone        
##  9 AnalysisWeight      aldicarb sulfoxide      
## 10 Anatoxin-A          aldrin                  
## # ... with 11 more rows
```

### Wayne Q: Differences in duplication by year?{.tabset}

There is definitely a more streamlined way to do this, but for expediency... These tables contain the total count of **Exact** duplicates identified in each dataframe. Recall from above that there were more duplicates found usign the final method, but this will give us an initial picture. 

Because there were no exact duplicates for the SURF df, I have not compared their duplication by year.

#### CEDEN WQ

```r
CEDENMod_WQ$year <- format(as.Date(CEDENMod_WQ$Date, format="%d/%m/%Y"),"%Y")

aggregate(DupCheck~year,FUN=length,data=CEDENMod_WQ[CEDENMod_WQ$DupCheck=="TRUE",])
```

```
##   year DupCheck
## 1 2010       79
## 2 2011      193
## 3 2012       76
## 4 2013       21
## 5 2014      140
## 6 2015       40
## 7 2016        7
## 8 2017       84
## 9 2018       17
```

#### CEDEN Tox

2015 and 2016 were far worse than other years. 


```r
CEDENMod_Tox$year <- format(as.Date(CEDENMod_Tox$Date, format="%d/%m/%Y"),"%Y")

aggregate(DupCheck~year,FUN=length,data=CEDENMod_Tox[CEDENMod_Tox$DupCheck=="TRUE",])
```

```
##    year DupCheck
## 1  2009      139
## 2  2010     2162
## 3  2011     1095
## 4  2012      734
## 5  2013     1070
## 6  2014     1238
## 7  2015     3273
## 8  2016     4774
## 9  2017     1660
## 10 2018     1098
## 11 2019     1031
```
