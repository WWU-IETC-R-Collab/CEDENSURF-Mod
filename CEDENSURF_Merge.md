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

Utilizign the distinct() function to assume that records in the same location on the same date, measuring the same analyte via the same collection method and obtaining the same result are duplicates, we find 1536 duplicate records.

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
<br>

<br>


## SURF data {.tabset}


```r
rm(list=setdiff(ls(), c("CEDENMod_WQ", "CEDENMod_Tox", "CEDEN_ALL_DupChecked","SURFMod_WQ", "SURFMod_SED")))
```

There are 129323 records in the WQ dataset
and 36027 in the SED dataset. 

### SURF Water

#### Data Prep

More investigating will probably be better, but for now we can simply remove those data defined in SURF as sourced from CEDEN


```r
SURFMod_WQ <- filter(SURFMod_WQ, Data.source != "CEDEN")
```

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
## logical   82846
```

Fantastic. There are no exact duplicates within the SURF water data. 

#### Looser assessment of Duplication

Utilizing the distinct() function to assume that records in the same location on the same date, measuring the same analyte and obtaining the same result are duplicates, we find 5,676 duplicate records.


```r
# Remove duplicate rows of the dataframe using multiple variables

NoDupWQ <- distinct(SURFMod_WQ, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(SURFMod_WQ) - nrow(NoDupWQ)
```

```
## [1] 5662
```
<br>

<br>

## SURF sediment

#### Data Prep

More investigating will probably be better, but for now we can simply remove those data defined in SURF as sourced from CEDEN


```r
SURFMod_SED <- filter(SURFMod_SED, Data.source != "CEDEN")
```

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
## logical   28817
```

Fantastic. There are no exact duplicates within the SURF sediment data. 

#### Looser assessment of Duplication

Utilizing the distinct() function to assume that records in the same location on the same date, measuring the same analyte and obtaining the same result are duplicates, we find only 784 - not bad.


```r
# Remove duplicate rows of the dataframe using multiple variables

NoDupSED <- distinct(SURFMod_SED, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(SURFMod_SED) - nrow(NoDupSED)
```

```
## [1] 784
```

<br>

<br>

## Merge SURF df

ZERO duplication found between the SED and WQ datasets, assuming duplicates would have to have the exact same Location, Date, Analyte, Collection Method, and Result.

*Note* I had started without 'Collection Method' in the distinct() query, but added it as an additional requirement for establishing duplication because without that record, the difference between results for water vs sediment were not recognized and **26127** records were considered duplicates following the merging of the SURF data.

This is still surprising to me, because that means the RESULT concentration was identical for both sediment and water samples. Are they truly sampled separately and exactly the same? By including "SampleMethod" to differentiate water and sediment samples, 


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


```r
# Ideas from: https://www.datasciencemadesimple.com/remove-duplicate-rows-r-using-dplyr-distinct-function/

# Remove duplicate rows of the dataframe using multiple variables

SURF_ALL_DupChecked <- distinct(SURF_ALL, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(SURF_ALL)-nrow(SURF_ALL_DupChecked)
```

```
## [1] 0
```

<br>

<br>

# Merge SURF and CEDEN


```r
rm(list=setdiff(ls(), c("CEDENMod_WQ", "CEDENMod_Tox", "CEDEN_ALL_DupChecked","SURFMod_WQ", "SURFMod_SED", "SURF_ALL_DupChecked")))
```

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

C <- sort(names(CEDEN_ALL_DupChecked))
S <- sort(names(SURF_ALL_DupChecked))

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
## # A tibble: 30 x 2
##    SURF             CEDEN           
##    <chr>            <chr>           
##  1 Agency           Agency          
##  2 Analyte          Analyte         
##  3 CollectionMethod CollectionMethod
##  4 County           County          
##  5 Data.source      Data.source     
##  6 Date             Date            
##  7 Datum            Datum           
##  8 geometry         geometry        
##  9 Latitude         Latitude        
## 10 LocationCode     LocationCode    
## # ... with 20 more rows
```

```r
# MERGE
CEDENSURF <- rbind(CEDEN_ALL_DupChecked, SURF_ALL_DupChecked)
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
## [1] 0
```
# Next Steps

Next, we need to find a way to unify nomenclature between the datasets, before we can confidently suggest there is no more duplication. For example, there are differences in analyte nomenclature between the databases:


```r
analyte_C <- sort(unique(CEDENSURF$Analyte[CEDENSURF$Source == "CEDEN"]))
# 1808

analyte_S <- sort(unique(CEDENSURF$Analyte[CEDENSURF$Source == "SURF"]))
# 307
```

There are ` r length(analyte_C) ` different analytes in the CEDEN data, while only 307 in the SURF.

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
##    CEDEN                             SURF                    
##    <chr>                             <chr>                   
##  1 AFDM_Algae, Particulate           acephate                
##  2 Alachlor, Dissolved               acetamiprid             
##  3 Alachlor, Particulate             acibenzolar-s-methyl    
##  4 Alachlor, Total                   acifluorfen, sodium salt
##  5 Aldicarb, Total                   alachlor                
##  6 Aldrin, Total                     aldicarb                
##  7 Alkalinity as CaCO3, Not Recorded aldicarb sulfone        
##  8 Alkalinity as CaCO3, Total        aldicarb sulfoxide      
##  9 Allethrin, Dissolved              aldrin                  
## 10 Allethrin, Particulate            allethrin               
## # ... with 11 more rows
```

