---
title: "CEDENSURF Merge - Official Short"
author: "Erika W"
date: "2/23/2021"
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

# Load Data

### CEDEN

Two files - one with tox data, and one with wq data


```r
# Load CEDEN Data
CEDENMod_Tox <- fread("https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_Toxicity.csv")

CEDENMod_WQ <- fread("https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_WQ.csv")
```

### SURF

Two files - one with wq data, and one with sediment data

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

# Remove duplication within each

Due to reported combined efforts to translate CEDEN data to SURF and vice versa, and issues with replicates being retained in each dataset, careful detection and elimination of duplicates should precede any analysis.

## CEDEN Data

There are 117144 records in the original WQ dataset

and 60637 in the original Tox dataset. 

### CEDEN Tox 

Removing exact duplicates via duplicated() misses duplication of wq data due to multiple species assessments, different sources of data upload, etc. 

Instead, we used distinct() which allows us to assume that records in the same location on the same date,  measuring the same analyte via the same collection method, and obtaining the same result are duplicates. This method deletes almost 50% more records than duplicated() 


```r
# Remove duplicate entries, determined as those matching all 5 columns names within distinct().

# Since we are using the Tox database for the water parameters, not the associated organism survival, we remove duplicate WQ entries regardless of the organism assessed.

NoDup_Tox<- distinct(CEDENMod_Tox, Date, StationName, Analyte, CollectionMethod, Result, .keep_all= TRUE) 

# How many duplicate entries were identified and removed?

nrow(CEDENMod_Tox) - nrow(NoDup_Tox)
```

```
## [1] 25729
```

Because of the way we are going to use this data, we are essentially scavenging the WQ parameters associated with each of these tox tests. It is worth considering whether it is worth using this database at all for this purpose;

+ There are only 23 analytes listed, at least 4 are specifically regarding the survival / condition of the organisms being tested

+ Theoretically, the associated WQ data should have already been recorded in the appropriate database.


```r
# How many analytes are in this database?
length(unique(NoDup_Tox$Analyte))
```

```
## [1] 23
```

```r
# We can also remove records that assess organism status (since we aren't using this for the biotic parameters in our model).

NoDup_Tox <- NoDup_Tox %>% filter(Analyte != "Survival") %>%
  filter(Analyte != "Biomass (wt/orig indiv)") %>%
  filter(Analyte != "Young/female") %>%
  filter(Analyte != "Total Cell Count") %>%
  select(-OrganismName)
```

We're left with only 27872 unique, useful records in the tox dataset - or 45.9653347 % of the original tox data.

<br>

### CEDEN WQ

Utilizing the distinct() function to assume that records in the same location on the same date, measuring the same analyte via the same collection method and obtaining the same result are duplicates, we find 1536 duplicate records.

That is more than double the number of exact duplicates found, yet still only 1.3112067% of the entire WQ dataset.


```r
# Remove duplicate rows of the dataframe using multiple variables

NoDup_WQ <- distinct(CEDENMod_WQ, Date, Analyte, StationName, CollectionMethod, Result, .keep_all= TRUE)

nrow(CEDENMod_WQ) - nrow(NoDup_WQ)
```

```
## [1] 1536
```
<br>

<br>

## Merging CEDEN data

After dealing with duplication WITHIN the CEDEN tox and wq datasets, there were only 9 duplicate records found following the merged data. (75 if Collection Method is not a requirement for establishing duplication)


```r
# Vector of column names to compare
WQ <- names(NoDup_WQ)
TOX <- names(NoDup_Tox)

#Add missing columns to CEDEN WQ
DIF<- setdiff(TOX, WQ) # gives items in T that are not in W
NoDup_WQ[, DIF] <- NA
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

WQ <- sort(names(NoDup_WQ))
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
NoDup_WQ <- NoDup_WQ %>% select(WQ)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(WQ)` instead of `WQ` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
# Check once all columns have perfect matches?
# tibble(SURF = names(NoDup_Tox), CEDEN = names(NoDup_WQ))

# MERGE
CEDEN_ALL <- rbind(NoDup_WQ,NoDup_Tox)
```

### Remove duplicates 


```r
# Remove duplicate rows of the dataframe using multiple variables

CEDEN_ALL_DupChecked <- distinct(CEDEN_ALL, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(CEDEN_ALL)-nrow(CEDEN_ALL_DupChecked)
```

```
## [1] 9
```

### Further refine

Further assessment revealed problematic data duplication that was not caught when requiring Collection Methods to be equal. We corrected the majority of these errors by:

1. Removing records where Collection Method = "Not Recorded" (Of 305 samples labeled "not recorded" in the entire CEDEN dataset, 153 were duplicated data with non-zero results)

2. (Add another bullet & code if we take action on Sediment Core vs Grab issues)


```r
# While there were 307 entries marked "Not Recorded" in the entire combined dataset, 153 were identified as duplicated data. To err on the safe side, we removed all records for which the Collection Method was "Not Recorded"

CEDEN_ALL_DupChecked <- filter(CEDEN_ALL_DupChecked, CollectionMethod != "Not Recorded")

# How to check to see how many more duplicates (of non-zero results) would have been removed if we don't restrict to identical "CollectionMethod" values

Check <- distinct(CEDEN_ALL, Date, Analyte, 
              StationName, Result, 
              .keep_all= TRUE)

nrow(CEDEN_ALL_DupChecked[CEDEN_ALL_DupChecked$Result != 0]) - nrow(Check[Check$Result != 0])
```

```
## [1] 83
```

### Fix nomenclature

**Split Analyte Column**

Because of formatting differences between the amount of data recorded under "Analyte" in CEDEN compared to the "Chemical_name" in SURF (which will be renamed Analyte), we opted to split the data in CEDEN's analyte column into two columns: 

Analyte (Chemical Name), and Analyte_Type (ie: total or particulate)

Using separate() to split the column and requiring the separation to be a comma and space ", " seems to have worked well, except for one name which appears to be empty

**Convert to lower-case**

SURF chemicals are all lowercase. We can make all letters in the CEDEN data lowercase using tolower() so that they will be compatible.


```r
# Split Analyte column

CEDEN_ALL_DupChecked <- CEDEN_ALL_DupChecked %>%
  separate(Analyte, into = c("Analyte", "Analyte_type"), sep = ", " , extra = "merge")
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 17681 rows [2, 3,
## 4, 5, 7, 8, 9, 10, 11, 12, 14, 19, 20, 22, 23, 24, 28, 94, 118, 144, ...].
```

```r
# Convert to lowercase

CEDEN_ALL_DupChecked$Analyte <- tolower(CEDEN_ALL_DupChecked$Analyte)

# Preview
head(sort(unique(CEDEN_ALL_DupChecked$Analyte))) # 908 unique Analytes total
```

```
## [1] ""                                          
## [2] "1,2-bis(2,4,6- tribromophenoxy)ethane"     
## [3] "2-ethyl-1-hexyl-2,3,4,5-tetrabromobenzoate"
## [4] "2-ethylhexyl-diphenyl phosphate"           
## [5] "2,4,6-tribromophenyl allyl ether"          
## [6] "abamectin"
```

```r
# Looks like requiring the separation from extra to analyte to contain a comma and a space allowed the full names to be retained. Without that, the separation led to an analyte "1" which should have been 1,2-bis(2,4,6- tribromophenoxy)ethane, etc.
```

This simplification of Analyte name would lead to 15,290 more records being considered "duplication" using our current method, mostly (14,776) containing a zero-result. Because these differences in analytes are not retained in the SURF dataset, it makes sense to condense them prior to merging the databases.


```r
Check <- distinct(CEDEN_ALL_DupChecked, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(CEDEN_ALL_DupChecked) - nrow(Check)

DIF<- setdiff(CEDEN_ALL_DupChecked, Check)
length(DIF$Result[DIF$Result == "0"])
```


```r
CEDEN_ALL_DupChecked <- distinct(CEDEN_ALL_DupChecked, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)
```

### CEDEN merge result

Using these QA/QC methods, 127874 unique records are available through the CEDEN datasets. 

<br>

<br>

## SURF data

There are 129323 records in the WQ dataset
and 36027 in the SED dataset. 

There were no exact duplicates in either the WQ or SED data from SURF. Far fewer duplicates were located using our flexible methods than in the CEDEN dataset.

### Data Prep

We renamed columns with analogous data to match CEDEN column names.


```r
### SURF WATER

# Move units from embedded in Result column name to their own column
SURFMod_WQ$Unit <- "ppb"

# Rename columns with analogous data to match CEDEN column names.
SURFMod_WQ <- SURFMod_WQ %>% rename(Date = Sample_date,
          Analyte = Chemical_name, 
          Result = Concentration..ppb., 
          CollectionMethod = Sample_type, 
          StationCode = Site_code,
          StationName = Site_name,
          MDL = Method_detection_level..ppb.,
          LOQ = Level_of_quantification..ppb.)

### SURF SEDIMENT

# Move units from embedded in Result column name to their own column
SURFMod_SED$Unit <- "ppb"

# Rename columns with analogous data to match CEDEN column names.
SURFMod_SED <- SURFMod_SED %>% rename(Date = Sample_date,
          Analyte = Chemical_name, 
          Result = Concentration..ppb., 
          CollectionMethod = Sample_type, 
          StationCode = Site_code,
          StationName = Site_name,
          MDL = Method_detection_level..ppb.,
          LOQ = Level_of_quantification..ppb.)
```

### Water: Remove duplicates

Used distinct() to remove duplicates (records in the same location on the same date, measuring the same analyte via the same collection method, and obtaining the same result)


```r
# Remove duplicate rows of the dataframe using multiple variables

NoDup_WQ <- distinct(SURFMod_WQ, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)
```
We find 17756, 5,676 of which come from the SURF data sourced from CEDEN

<br>

<br>

### Sediment: Remove duplicates

Used distinct() to remove records in the same location on the same date, measuring the same analyte via the same collection method.


```r
# Remove duplicate rows of the dataframe using multiple variables

NoDup_SED <- distinct(SURFMod_SED, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)
```

Find only `nrow(SURFMod_SED) - nrow(NoDup_SED)` duplicates, 3336 of which are data sourced from CEDEN

<br>

<br>

## Merge SURF df


```r
# Vector of column names to compare
WQ <- names(NoDup_WQ)
SED <- names(NoDup_SED)

#Add missing columns to CEDEN WQ
DIF<- setdiff(SED, WQ) # gives items in S that are not in W
NoDup_WQ[, DIF] <- NA

#Add missing columns to CEDEN SED
DIF<- setdiff(WQ, SED) # gives items in W that are not in S
NoDup_SED[, DIF] <- NA
```

```
## Warning in `[<-.data.table`(`*tmp*`, , DIF, value = NA): length(LHS)==0; no
## columns to delete or assign RHS to.
```

```r
# Finishing touches before merge; order columns to match - is this really necessary for merge? IDK

WQ <- sort(names(NoDup_WQ))
SED <- sort(names(NoDup_SED))

NoDup_SED <- NoDup_SED %>% select(SED)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(SED)` instead of `SED` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
NoDup_WQ <- NoDup_WQ %>% select(WQ)

# Check once all columns have perfect matches?
# tibble(SURF = names(NoDup_SED), CEDEN = names(NoDup_WQ))

# MERGE
SURF_ALL <- rbind(NoDup_WQ,NoDup_SED)
```

### Duplication between SURF sets

ZERO duplication found between the SED and WQ datasets, assuming duplicates would have to have the exact same Location, Date, Analyte, Collection Method, and Result.


```r
# Remove duplicate rows of the dataframe using multiple variables

SURF_ALL_DupChecked <- distinct(SURF_ALL, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(SURF_ALL)-nrow(SURF_ALL_DupChecked)
```

```
## [1] 0
```

### Further Refine

We further investigated instances of duplicated entries retained using these methods which differed only in their collection methods, specifically targeting records with identical results != 0.

All but one were corrected by removing records by Study_cd 305, which was an exact replicate of Study_cd 523 except missing collection methods.


```r
SURF_ALL_DupChecked <- filter(SURF_ALL_DupChecked, Study_cd != "305")
```


```r
# Create DF limited to only one record per date,loc,analyte,result combo

Check <- distinct(SURF_ALL, Date, Analyte, StationName, Result, .keep_all= TRUE)

DIF <- setdiff(SURF_ALL_DupChecked, Check)

# How many duplicates of concern are retained after this correction?

length(DIF$Result[DIF$Result != "0"])
```

### Result of SURF Merge

There are 143410 unique records available through SURF.


```r
# Create a subset of SURF data that excludes data sourced from CEDEN

SURF_ALL_NC <- filter(SURF_ALL_DupChecked, Data.source != "CEDEN")
```

That said, 45064 of these records are listed as having been sourced from CEDEN.

In theory only 98346 unique records will be contributed through the SURF dataset. Rather than filter these out ahead of the merge, I am retaining them and then using the identification of those records as a test to see whether there are other differentiating factors (such as persisting differences in naming) between the merged dataset that will inhibit our analyses

<br>

<br>

# Merge SURF and CEDEN

Now that each dataset has been independently inspected for duplication *within* each dataset, they can be merged and searched for duplication *between* the datasets.


```r
## Data Prep - Match Columns between DF prior to merge

C <- names(CEDEN_ALL_DupChecked)
S <- names(SURF_ALL_DupChecked)

DIF<- setdiff(S, C) # gives items in S that are not in C

#Add missing columns to CEDEN
CEDEN_ALL_DupChecked[, DIF] <- NA

#Add missing columns to SURF
DIF<- setdiff(C, S) # gives items in C that are not in S
SURF_ALL_DupChecked[, DIF] <- NA

# Re-order columns to align
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
# tibble(SURF = names(SURF_ALL_DupChecked), CEDEN = names(CEDEN_ALL_DupChecked))

## MERGE ##

CEDENSURF <- rbind(SURF_ALL_DupChecked, CEDEN_ALL_DupChecked)
```

### Check for duplicates

Even retaining the CEDEN data in SURF prior to merge, we locate no duplicates using this method. There are likely still issues with matching nomenclature (ie: Anaylte names) between the data sources preventing this.


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

### Naming conventions

We should check whether splitting the Analyte column in CEDEN helped to unify the nomenclature between datasets, before we can confidently suggest there is no more duplication.

The lists of analytes are MUCH closer to the same length using this method, though still all names in CEDEN are different than those in SURF (908 unique analytes in CEDEN, and 908 in the difference between CEDEN and SURF)

For example, a subset of "Analytes" in each database follow:


```r
## Example analytes observed in each dataset

analyte_C <- sort(unique(CEDENSURF$Analyte[CEDENSURF$Source == "CEDEN"]))
# 908 (was >1800 before splitting the column)

analyte_S <- sort(unique(CEDENSURF$Analyte[CEDENSURF$Source == "SURF"]))
# 307

# Show example of differences
tibble(CEDEN = c(analyte_C[20:40]), SURF = c(analyte_S[10:30]))
```

```
## # A tibble: 21 x 2
##    CEDEN               SURF                    
##    <chr>               <chr>                   
##  1 alkalinity as caco3 acephate                
##  2 allethrin           acet                    
##  3 aluminum            acetamiprid             
##  4 ametryn             acibenzolar-s-methyl    
##  5 aminocarb           acifluorfen, sodium salt
##  6 ammonia as n        alachlor                
##  7 ammonia as nh3      aldicarb                
##  8 ammonium as n       aldicarb sulfone        
##  9 analysisweight      aldicarb sulfoxide      
## 10 anatoxin-a          aldrin                  
## # ... with 11 more rows
```
