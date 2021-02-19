---
title: "CEDENSURF_Merge"
author: "Erika W"
date: "2/18/2021"
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

## Load Data

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

## Remove duplicate data

More investigating will probably be better, but for now we can simply remove those data defined in SURF as sourced from CEDEN


```r
SURFMod_WQ <- filter(SURFMod_WQ, Data.source != "CEDEN")
```

## Merge Water Quality Sets

### Correct mismatched column names

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

### Match columns

To do the merge even though there are columns without matches in each dataset, we can give both datasets identical columns and allow them to be filled with NA's when those columns are not actually present.


```r
C <- names(CEDENMod_WQ)
S <- names(SURFMod_WQ)

DIF<- setdiff(S, C) # gives items in S that are not in C
```

Columns that were in SURF and not CEDEN:
County, LOQ, Study_cd, Study_description, Study_weblink, Data.source, Agency, Record_id


```r
#Add missing columns to CEDEN

CEDENMod_WQ[, DIF] <- NA
```



```r
#Add missing columns to SURF

DIF<- setdiff(C, S) # gives items in C that are not in S

SURFMod_WQ[, DIF] <- NA
```

Columns that were in CEDEN and not SURF:
` r DIF`

### Merge

Reorder columns so columns align, then merge with rbind()


```r
# Finishing touches before merge; order columns to match - is this really necessary for merge? IDK

C <- sort(names(CEDENMod_WQ))
S <- sort(names(SURFMod_WQ))

SURFMod_WQ <- SURFMod_WQ %>% select(S)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(S)` instead of `S` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
CEDENMod_WQ <- CEDENMod_WQ %>% select(C)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(C)` instead of `C` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
# Check once all columns have perfect matches?
tibble(SURF = names(SURFMod_WQ), CEDEN = names(CEDENMod_WQ))
```

```
## # A tibble: 29 x 2
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
## # ... with 19 more rows
```

```r
# MERGE

CEDENSURF_WQ <- rbind(CEDENMod_WQ, SURFMod_WQ)
```

## Merge Joined water data with SURF sediment data


### Correct mismatched column names

Rather than using Steven's method of renaming using a list (which is dependent on an expected number and order of column names), I renamed each column individually


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

### Match columns

To do the merge even though there are columns without matches in each dataset, we can give both datasets identical columns and allow them to be filled with NA's when those columns are not actually present.


```r
C <- names(CEDENSURF_WQ)
S <- names(SURFMod_SED)

#Add missing columns to CEDEN

DIF<- setdiff(S, C) # gives items in S that are not in C

CEDENSURF_WQ[, DIF] <- NA
```

Columns that were in SURF sediment and not in the water tables:
Total organic carbon (%)



```r
#Add missing columns to SURF

DIF<- setdiff(C, S) # gives items in C that are not in S

SURFMod_SED[, DIF] <- NA
```


Columns that were in the water tables and not in SURF sediment:

Datum, LocationCode, MatrixName, ParentProject, Program, Project, rb_number, regional_board

### Merge

Reorder columns so columns align, then merge with rbind()


```r
# Finishing touches before merge; order columns to match - is this really necessary for merge? IDK

C <- sort(names(CEDENSURF_WQ))
S <- sort(names(SURFMod_SED))

SURFMod_SED <- SURFMod_SED %>% select(S)

CEDENSURF_WQ <- CEDENSURF_WQ %>% select(C)

# Check once all columns have perfect matches?

tibble(SURF = names(SURFMod_SED), CEDEN = names(CEDENSURF_WQ))
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

CEDENSURF_WQSED <- rbind(CEDENSURF_WQ, SURFMod_SED)
```

## Remove duplicate data

### Removal from final merged document

distinct() allows us to remove data that matches in specific columns, following the merge; like Date, StationName, Analyte, and Result. It keeps the first record and removes all subsequent matches. 


```r
# Ideas from: https://www.datasciencemadesimple.com/remove-duplicate-rows-r-using-dplyr-distinct-function/

# Records in merged data frame
nrow(CEDENSURF_WQSED)
```

```
## [1] 236017
```

```r
# Remove duplicate rows of the dataframe using multiple variables

CEDENSURF_DupChecked <- distinct(CEDENSURF_WQSED, Date, Analyte, StationName, Result, .keep_all= TRUE)

# Records in checked data
nrow(CEDENSURF_DupChecked)
```

```
## [1] 198374
```

Assuming records that have the exact same station name, date, analyte, and result are duplicates, there were 37643 duplicates in the merged data

## Investigating data first (late)

Due to the structure of the tox data, I have a feeling it involves biological assays that were then related to WQ data sampled on that date (and already present in the CEDEN_WQ dataset)

I can look to see if this is true by merging the two ceden datasets following the same method as before, and assessing differences in sample length. 

### Repeat Merge with only CEDEN data frames

Start with original Mod Data (pre merge)


```r
# Load CEDEN Data
CEDENMod_Tox <- fread("https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_Toxicity.csv")

CEDENMod_WQ <- fread("https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_WQ.csv")
```

```r
WQ <- names(CEDENMod_WQ)
TOX <- names(CEDENMod_Tox)

DIF<- setdiff(TOX, WQ) # gives items in T that are not in W

#Add missing columns to CEDEN WQ

CEDENMod_WQ[, DIF] <- NA

#Add missing columns to CEDEN TOX

DIF<- setdiff(WQ, TOX) # gives items in W that are not in T

CEDENMod_Tox[, DIF] <- NA

# Finishing touches before merge; order columns to match - is this really necessary for merge? IDK

WQ <- sort(names(CEDENMod_WQ))
TOX <- sort(names(CEDENMod_Tox))

CEDENMod_Tox <- CEDENMod_Tox %>% select(TOX)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(TOX)` instead of `TOX` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
CEDENMod_WQ <- CEDENMod_WQ %>% select(WQ)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(WQ)` instead of `WQ` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
# Check once all columns have perfect matches?
tibble(SURF = names(CEDENMod_Tox), CEDEN = names(CEDENMod_WQ))
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

CEDEN_ALL <- rbind(CEDENMod_WQ,CEDENMod_Tox)
```

### Detect differences

There were 117144 records in the WQ dataset
and 60637 in the Tox dataset. 

In the tox dataset, there were several records related to a single species assessment - by creating a group identifier by the location, date, and organism assessed, I calculated that there were 2430 unique samples.

Within the Tox dataset, 11 different species are represented.


```r
CEDENMod_Tox$ID <- paste(CEDENMod_Tox$Date, CEDENMod_Tox$StationCode, CEDENMod_Tox$OrganismName, sep= ",")

length(unique(CEDENMod_Tox$ID))
```

```
## [1] 2430
```

```r
# Ideas from: https://www.datasciencemadesimple.com/remove-duplicate-rows-r-using-dplyr-distinct-function/

# Remove duplicate rows of the dataframe using multiple variables

CEDEN_ALL_DupChecked <- distinct(CEDEN_ALL, Date, Analyte, StationName, Result, .keep_all= TRUE)

nrow(CEDEN_ALL_DupChecked)
```

```
## [1] 149998
```
Assuming records that have the exact same station name, date, analyte, and result are duplicates, there were 27783 duplicates in the merged data, leaving 149998 total records in the merged df.

That suggests that only 32854 unique records were brought over from the tox dataset

### Further refining - what is left?

If we remove records that assess 'survival' and 'biomass' (since we aren't using this for biotic parameters in our model)...


```r
CEDEN_ALL_DupChecked <- CEDEN_ALL_DupChecked[CEDEN_ALL_DupChecked$Analyte != "Survival"]

CEDEN_ALL_DupChecked <- CEDEN_ALL_DupChecked[CEDEN_ALL_DupChecked$Analyte != "Biomass (wt/orig indiv)"]
```

We're left with 30065 unique records brought over out of all 60637 records in the Tox dataset 
