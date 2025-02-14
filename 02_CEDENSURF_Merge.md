---
title: "CEDENSURF Merge"
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
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(data.table)
library(lubridate)
library(sf)
library(tidyverse)
```

# Intro

This markdown covers the process to identify duplication within the CEDEN and SURF datasets, and to merge those datasets together to make a quality-assured database for use in our analyses.

**Output/Result:** CEDENSURFMod.csv - a combined and quality controlled dataset from the CEDEN and SURF databases.

# Load Data  {.tabset}

## CEDEN

**CEDEN** Data was acquired from https://ceden.waterboards.ca.gov/AdvancedQueryTool on January 29 2020 for the Central Valley and San Francisco Bay regions, and spatially queried to the USFE project area and modified via methods described at: https://github.com/WWU-IETC-R-Collab/CEDEN-mod/blob/main/CEDEN_ModMaster.md

This original data set can be found within the IETC Tox Box at: Upper San Francisco Project\Data & Analyses\Original\CEDEN.


```r
# Load Data
# Append with source
# Identify sample matrix

CEDENMod_Tox <- fread("https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_Toxicity.csv") %>% 
      mutate(Source = "CEDEN")%>%
      mutate(Matrix = "water") # 3 of 4 values in MatrixName reference water, so starting with that and then editing those that are sediment values to read sediment

CEDENMod_Tox$Matrix[CEDENMod_Tox$MatrixName == "sediment"] <- "sediment"

CEDENMod_WQ <- fread("https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_WQ.csv") %>% 
      mutate(Source = "CEDEN")%>% 
      mutate(Matrix = "water")

# SedimentGrab should have Matrix == "sediment".
CEDENMod_WQ$Matrix[CEDENMod_WQ$CollectionMethod == "Sediment_Grab"]<- "sediment"
```
Two files - one with tox data, and one with wq data

CEDEN water data contains 122489 records, between 2009-10-06 to 2019-09-26

CEDEN tox data contains 60531 records, between 2009-10-06 to 2019-09-25

<br> 

## SURF

**SURF** Data was acquired at the DPR SURF database web page as CSVs via FTP download on 2/17/2021, then spatially and temporally restricted via methods outlined in https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/blob/main/CEDENSURF.md prior to this work.

This original data set can be found within the IETC Tox Box at: Upper San Francisco Project\Data & Analyses\Original\SURF.


```r
# Load Data
# Append with source
# Identify sample matrix

SURFMod_SED <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/SURFMod_SED.csv") %>% 
      mutate(Source = "SURF")%>% 
      mutate(Matrix = "sediment")

SURFMod_WQ <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/SURFMod_water.csv") %>% 
      mutate(Source = "CEDEN")%>% 
      mutate(Matrix = "water")
```

Two files - one with wq data, and one with sediment data

SURF water contains 91021 records, from 2009-10-06 to 2019-09-17

SURF sediment contains 35346 records, from from NA to 2019-09-17

<br>

<br>

# Data prep  {.tabset}

Due to reported combined efforts to translate CEDEN data to SURF and vice versa, and issues with replicates being retained in each dataset, careful detection and elimination of duplicates should precede any analysis.

<br>

## CEDEN

#### 1. Remove duplicates

Removing exact duplicates via duplicated() misses duplication of wq data due to multiple species assessments, different sources of data upload, etc. 

Instead, we used distinct() which allows us to assume that records in the same location on the same date,  measuring the same analyte via the same collection method, and obtaining the same result are duplicates. This method deletes almost 50% more records than duplicated() 


```r
# Remove duplicate entries, determined as those matching all 5 columns names within distinct().

# Since we are using the Tox database for the water parameters, not the associated organism survival, we remove duplicate WQ entries regardless of the organism assessed.

NoDup_Tox<- distinct(CEDENMod_Tox, Date, StationName, Analyte, CollectionMethod, Matrix, Result, .keep_all= TRUE) 

# How many duplicate entries were identified and removed?

nrow(CEDENMod_Tox) - nrow(NoDup_Tox) # 23,518
```


```r
# Remove duplicate rows of the dataframe using multiple variables

NoDup_WQ <- distinct(CEDENMod_WQ, Date, Analyte, StationName, CollectionMethod, Matrix, Result, .keep_all= TRUE)

nrow(CEDENMod_WQ) - nrow(NoDup_WQ) # 1661
```
<br>

#### 2. Remove irrelevant data

Since we are using the Tox database for the water parameters, not the associated organism survival, we can also remove records that assess organism status.


```r
# We can also remove records that assess organism status (since we aren't using this for the biotic parameters in our model).

NoDup_Tox <- NoDup_Tox %>% filter(Analyte != "Survival") %>%
  filter(Analyte != "Biomass (wt/orig indiv)") %>%
  filter(Analyte != "Young/female") %>%
  filter(Analyte != "Total Cell Count") %>%
  filter(Analyte != "Mean Percent Normal Alive") %>%
  filter(Analyte != "Ash Free Dry Mass") %>%
  filter(Analyte != "Growth (wt/surv indiv)") %>%
  filter(Analyte != "Growth (ash-free dry wt/surv indiv)") %>%
  filter(Analyte != "Weight") %>%
  filter(Analyte != "") %>%
  select(-OrganismName)

# 28736 records
```

<br>

After CEDEN data prep, there are 28731 unique, useful records in the tox dataset, and 120828 unique records in the WQ dataset.

<br>

#### 3. Merge CEDEN df

After dealing with duplication WITHIN the CEDEN tox and wq datasets, there were only 9 duplicate records found following the merged data. (75 if Collection Method is not a requirement for establishing duplication)


```r
# Vector of column names to compare
WQ <- names(NoDup_WQ)
TOX <- names(NoDup_Tox)

# Add missing columns to CEDEN WQ: SKIP, there were no missing columns
# DIF<- setdiff(TOX, WQ) ## gives items in T that are not in W
# NoDup_WQ[, DIF] <- NA 

#Add missing columns to CEDEN TOX
DIF<- setdiff(WQ, TOX) # gives items in W that are not in T
NoDup_Tox[, DIF] <- NA

# Finishing touches before merge; order columns to match - is this really necessary for merge? IDK

WQ <- sort(names(NoDup_WQ))
TOX <- sort(names(NoDup_Tox))

NoDup_Tox <- NoDup_Tox %>% select(all_of(TOX))
NoDup_WQ <- NoDup_WQ %>% select(all_of(WQ))

# Check once all columns have perfect matches?
# tibble(SURF = names(NoDup_Tox), CEDEN = names(NoDup_WQ))

# MERGE
CEDEN_ALL <- rbind(NoDup_WQ,NoDup_Tox)
```
<br>

#### 4. Further refine: Remove duplicates


```r
# Remove duplicate rows of the dataframe using multiple variables

CEDEN_ALL_DupChecked <- distinct(CEDEN_ALL, Date, Analyte, CollectionMethod, StationName,Matrix, Result, .keep_all= TRUE)
```

<br>

**Problematic duplicates**

Further assessment revealed problematic data duplication that was not caught when requiring Collection Methods to be equal. We corrected the majority of these errors by:

1. Removing records where Collection Method = "Not Recorded" (Of 305 samples labeled "not recorded" in the entire CEDEN dataset, 153 were duplicated data with non-zero results)

2. *(Add another bullet & code if we take action on Sediment Core vs Grab issues)*


```r
# While there were 307 entries marked "Not Recorded" in the entire combined dataset, 153 were identified as duplicated data. To err on the safe side, we removed all records for which the Collection Method was "Not Recorded"

CEDEN_ALL_DupChecked <- filter(CEDEN_ALL_DupChecked, CollectionMethod != "Not Recorded")
```

<br>

#### 5. Further refine: Fix nomenclature

**Split Analyte Column**

Because of formatting differences between the amount of data recorded under "Analyte" in CEDEN compared to the "Chemical_name" in SURF (which will be renamed Analyte), we opted to split the data in CEDEN's analyte column into two columns: 

Analyte (Chemical Name), and Analyte_Type (ie: total or particulate)

Using separate() to split the column and requiring the separation to be a comma and space ", " seems to have worked well, except for one name which appears to be empty

<br>

**Convert to lower-case**

SURF chemicals are all lowercase. We can make all letters in the CEDEN data lowercase using tolower() so that they will be compatible.

Preview of analyte names:

```r
# Split Analyte column

CEDEN_ALL_DupChecked <- CEDEN_ALL_DupChecked %>%
  separate(Analyte, into = c("Analyte", "Analyte_type"), sep = ", " , extra = "merge")

# Convert to lowercase

CEDEN_ALL_DupChecked$Analyte <- tolower(CEDEN_ALL_DupChecked$Analyte)

# Preview
head(sort(unique(CEDEN_ALL_DupChecked$Analyte))) # 908 unique Analytes total
```

```
## [1] "1,2-bis(2,4,6- tribromophenoxy)ethane"     
## [2] "2-ethyl-1-hexyl-2,3,4,5-tetrabromobenzoate"
## [3] "2-ethylhexyl-diphenyl phosphate"           
## [4] "2,4,6-tribromophenyl allyl ether"          
## [5] "abamectin"                                 
## [6] "acenaphthene"
```

```r
# Looks like requiring the separation from extra to analyte to contain a comma and a space allowed the full names to be retained. Without that, the separation led to an analyte "1" which should have been 1,2-bis(2,4,6- tribromophenoxy)ethane, etc.
```

This simplification of Analyte name would lead to more records being considered "duplication" using our current method, 90% of which containing a zero-result. Because these differences in analytes are not retained in the SURF dataset, it makes sense to condense them (remove duplicates) prior to merging the databases.

Removal of these simplified duplicates also eliminates the utility of retaining the "Analyte Type" column. For example, a reading of Analyte X with Type = Total Type = Suspended have the exact same result. Removing duplicates would keep the first record (Analyte = X, Type = Total) and remove the second (Analyte X, Type = S). In the dataframe, if you are trying to reason backwards to the meaning of that remaining meaning (Analyte X, Type = Total), you're missing the other half of the story (Type = Sus too). So, to avoid improper interpretation of this dataframe, Analyte Type should be removed. 


```r
CEDEN_ALL_DupChecked <- distinct(CEDEN_ALL_DupChecked, Date, Analyte, CollectionMethod, StationName, Matrix, Result, .keep_all= TRUE) %>%
  select(-Analyte_type)
```

<br>

#### **CEDEN merge result**

Using these QA/QC methods, 133926 unique records are available through the CEDEN datasets. 

<br>

<br>

## SURF

There were no exact duplicates in either the WQ or SED data from SURF. Far fewer duplicates were located using our flexible methods than in the CEDEN dataset.

<br>

#### 1. Remove duplicates

We used distinct() to remove records in the same location on the same date, measuring the same analyte via the same collection method which had identical results.


```r
# Remove duplicate rows of the dataframe using multiple variables

# SURF Water
NoDup_WQ <- distinct(SURFMod_WQ, Date, Analyte, CollectionMethod, StationName, Matrix, Result, .keep_all= TRUE)

# SURF Sediment
NoDup_SED <- distinct(SURFMod_SED, Date, Analyte, CollectionMethod, StationName, Matrix, Result, .keep_all= TRUE)
```

This results in 75688 unique records in the WQ dataset
and 31266 unique records in the SED dataset, prior to merging.

<br>

#### 2. Merge SURF df


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

# Finishing touches before merge; order columns to match - is this really necessary for merge? IDK

WQ <- sort(names(NoDup_WQ))
SED <- sort(names(NoDup_SED))

NoDup_SED <- NoDup_SED %>% select(all_of(SED))
NoDup_WQ <- NoDup_WQ %>% select(all_of(WQ))

# Check once all columns have perfect matches?
# tibble(SURF = names(NoDup_SED), CEDEN = names(NoDup_WQ))

# MERGE
SURF_ALL <- rbind(NoDup_WQ,NoDup_SED)
```
<br>

#### 3. Further refine: Duplication between SURF sets

ZERO duplication found between the SED and WQ datasets, assuming duplicates would have to have the exact same Location, Date, Analyte, Collection Method, and Result.


```r
# Remove duplicate rows of the dataframe using multiple variables

SURF_ALL_DupChecked <- distinct(SURF_ALL, Date, Analyte, CollectionMethod, StationName, Matrix, Result, .keep_all= TRUE)

nrow(SURF_ALL)-nrow(SURF_ALL_DupChecked)
```

```
## [1] 0
```

We further investigated instances of duplicated entries retained using these methods which differed only in their collection methods, specifically targeting records with identical results != 0.

All but one were corrected by removing records by Study_cd 305, which was an exact replicate of Study_cd 523 except missing collection methods.


```r
SURF_ALL_DupChecked <- filter(SURF_ALL_DupChecked, Study_cd != "305")
```

<br>

#### **SURF merge result**

There are 106890 unique records available through SURF.


```r
# Create a subset of SURF data that excludes data sourced from CEDEN

SURF_ALL_NC <- filter(SURF_ALL_DupChecked, Data.source != "CEDEN")
```

That said, 28100 of these records are listed as having been sourced from CEDEN.

In theory only 78790 unique records will be contributed through the SURF dataset. Rather than filter these out ahead of the merge, I am retaining them and then using the identification of those records as a test to see whether there are other differentiating factors (such as persisting differences in naming) between the merged dataset that will inhibit our analyses

<br>

<br>

# Merge SURF and CEDEN  {.tabset}

Now that each dataset has been independently inspected for duplication *within* the dataset, they can be merged and searched for duplication *between* the datasets.


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

SURF_ALL_DupChecked <- SURF_ALL_DupChecked %>% select(all_of(S))
CEDEN_ALL_DupChecked <- CEDEN_ALL_DupChecked %>% select(all_of(C))

# Check once all columns have perfect matches?
# tibble(SURF = names(SURF_ALL_DupChecked), CEDEN = names(CEDEN_ALL_DupChecked))

## MERGE ##

CEDENSURF <- rbind(CEDEN_ALL_DupChecked, SURF_ALL_DupChecked)

write_csv(CEDENSURF, "IssueDocumentation/CEDENSURF_IssueInvestigation.csv") # Note: coerces empty data fields to NA
```

There are 240816 total records in the initial merge of CEDEN with SURF.

Due to initial barriers to removing duplicates between the datasets (see below), I will simply filter out data identified as being sourced from CEDEN within SURF to eliminate duplicates. This is not an ideal solution though, because there is a large amount of data identified as coming from CEDEN which is not present in our CEDEN WQ data (again, see below).


```r
SURFMod_NC <- filter(SURF_ALL_DupChecked, Data.source != "CEDEN")

CEDENSURFModNC <- rbind(SURFMod_NC, CEDEN_ALL_DupChecked)

#  THE CURRENT CEDENSURFMOD CSV, THAT SAID< NOW CAN DETECT DUPLICATION BETWEEN DATASETS> PREFER TO WRITE FROM FINISHED METHODS BELOW. write_csv(CEDENSURFModNC, "Data/Output/CEDENSURFMod.csv") # Note: coerces empty data fields to NA
```

<br>

# Next Steps

#### 1. Investigate barriers to duplicate removal {.tabset}
*(area of active investigation)*

Because the station names differ between these databases, we used Lat and Long in lieu of StationName to detect duplicates.

This only works if the projection and rounding of latitude and longitude have been made consistent both within and between the datasets (see linked protocols for our CEDENMod and SURFMod data preparation).

It seems to only detect 11 duplicates, while there are 28100 labeled in SURF as having come from CEDEN. Some may be unique, and some may be additional duplicates that we are not catching. 

Because all values in SURF are in units PPB, while units of records in CEDEN vary, those results cannot be expected to be identical. 

We must assume that records of the same analyte collected by the same method on the same date at the same station are duplicates. Through this, we find 30,608 duplicates - just over the number expected given those labeled as from CEDEN.


```r
# Remove duplicate rows of the dataframe using multiple variables

CEDENSURF_DupChecked <- distinct(CEDENSURF, Date, Analyte, CollectionMethod, Latitude, Longitude, Matrix, .keep_all= TRUE)

nrow(CEDENSURF)-nrow(CEDENSURF_DupChecked)

#THIS IS PREFERRED: 
write_csv(CEDENSURF_DupChecked, "Data/Output/CEDENSURFMod.csv") # Note: coerces empty data fields to NA
```
**Causes of these records being retained include:**

A. No match actually exists in CEDEN. Record SHOULD be retained.

B. CEDEN and SURF have different naming protocols - both Station Name and Station Code differ for the same sites.

C. Station Latitude and Longitude appear to be consistent between the databases, yet the projection to geometry results in different outcomes.

D. Results are in different units. Important to remember for future analyses. Need to convert? Can all be converted to ppb (except temperature?)

*See the IssueDocumentation.rmd for examples and details about each of these issues.*

**Current conclusions:**

A. Minimal instances, low priority issue

B. Discovered that blank results in CEDEN are usually paired with a ResultQualCode "ND" meaning that they were tested for and not detected. Replacing with a 0 would therefore be a more appropriate approach than our current method of removal.

C. Should review the projection method between these datasets, and the code to convert to shapefile, and locate potential sources of discrepancy.

D. We should go through and unify these as much as possible, or folks should simply be aware of this and convert within their subsets prior to analysis. May have to happen manually (ie: in original Mod documents for each dataset, subset these units, run conversion on result)

In SURF, rather than a "unit" column, their results column was "concentration..ppb" - implying that all results were converted prior to storing in the database.


```r
unique(CEDENSURF$Unit)
```

```
##  [1] "mg/L"       "NTU"        "ug/L"       "pg/L"       "ng/L"      
##  [6] "uS/cm"      "Deg C"      "none"       "%"          "mg/Kg dw"  
## [11] "ug/Kg dw"   "mL"         "% vol"      "% dw"       "g"         
## [16] "ppt"        "m"          "mg/m3"      "psu"        "ng/g dw"   
## [21] "% ww"       "cfs"        "MPN/100 mL" "1/cm"       "umhos/cm"  
## [26] "gc/mL"      "m/s"        "mg/m2"      "g/m2"       "ueq/L"     
## [31] "CU"         "pCi/L"      "ft/s"       "oocysts/L"  "cysts/L"   
## [36] "mf/L"       "mL/L/hr"    "ppb"
```

<br>

#### 2. Decide whether to retain composite samples

If we decide to remove them, determine how to identify.

In CEDEN, might be able to locate by CollectionMethod OR CollectionDeviceDescription

ie: "7 day auto sampler" and "AutoSampler" collection methods may indicate composite over time, or "depth-integrating" collection device description may indivate composite over depths.

