---
title: "Analyte Names"
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

## Load Data

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

## Merge within source {.tabset}

To facilitate ID of analytes in each source, I merged the CEDEN sets here and the SURF sets (WITHOUT ADDRESSING DUPLICATION - since that shouldn't matter for these purposes. 

If you want to see CEDEN tox vs wq analyte lists separately, we can also do that.

### CEDEN

```r
# Vector of column names to compare
WQ <- names(CEDENMod_WQ)
TOX <- names(CEDENMod_Tox)

#Add missing columns to CEDEN WQ
DIF<- setdiff(TOX, WQ) # gives items in T that are not in W
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
# If rbind doesnt work, Check here to ensure columns have perfect matches
# tibble(Tox = names(CEDENMod_Tox), WQ = names(CEDENMod_WQ))

# MERGE
CEDEN_ALL <- rbind(CEDENMod_WQ,CEDENMod_Tox)
```

### SURF

```r
# Vector of column names to compare
WQ <- names(SURFMod_WQ)
SED <- names(SURFMod_SED)

#Add missing columns to SURF WQ
DIF<- setdiff(SED, WQ) # gives items in T that are not in W
SURFMod_WQ[, DIF] <- NA

#Add missing columns to SURF SED
DIF<- setdiff(WQ, SED) # gives items in W that are not in T
SURFMod_SED[, DIF] <- NA
```

```
## Warning in `[<-.data.table`(`*tmp*`, , DIF, value = NA): length(LHS)==0; no
## columns to delete or assign RHS to.
```

```r
# Finishing touches before merge; order columns to match - is this really necessary for merge? IDK

WQ <- sort(names(SURFMod_WQ))
SED <- sort(names(SURFMod_SED))

SURFMod_SED <- SURFMod_SED %>% select(SED)
```

```
## Note: Using an external vector in selections is ambiguous.
## i Use `all_of(SED)` instead of `SED` to silence this message.
## i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
## This message is displayed once per session.
```

```r
SURFMod_WQ <- SURFMod_WQ %>% select(WQ)

# If rbind doesnt work, Check here to ensure columns have perfect matches
# tibble(SED = names(SURFMod_SED), WQ = names(SURFMod_WQ))

# MERGE
SURF_ALL <- rbind(SURFMod_WQ,SURFMod_SED)

# Correct SURF column names to match CEDEN
SURF_ALL$Unit <- "ppb"

SURF_ALL <- SURF_ALL %>% rename(Date = Sample_date,
          Analyte = Chemical_name, 
          Result = Concentration..ppb., 
          CollectionMethod = Sample_type, 
          StationCode = Site_code,
          StationName = Site_name,
          MDL = Method_detection_level..ppb.,
          LOQ = Level_of_quantification..ppb.)
```

## Fixing CEDEN nomenclature

**Split Analyte Column**

Because of formatting differences between the amount of data recorded under "Analyte" in CEDEN compared to the "Chemical_name" in SURF (which will be renamed Analyte), we opted to split the data in CEDEN's analyte column into two columns: 

Analyte (Chemical Name), and Analyte_Type (ie: total or particulate)

Using separate() to split the column and requiring the separation to be a comma and space ", " seems to have worked well, except for one name which appears to be empty

**Convert to lower-case**

SURF chemicals are all lowercase. We can make all letters in the CEDEN data lowercase using tolower() so that they will be compatible.


```r
# Split Analyte column

CEDEN_ALL_Renamed <- CEDEN_ALL %>%
  separate(Analyte, into = c("Analyte", "Analyte_type"), sep = ", " , extra = "merge")
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 41530 rows [2, 3,
## 4, 5, 7, 8, 9, 10, 11, 12, 14, 19, 20, 22, 23, 24, 28, 94, 118, 144, ...].
```

```r
# Convert to lowercase

CEDEN_ALL_Renamed$Analyte <- tolower(CEDEN_ALL_Renamed$Analyte)

# Preview
head(sort(unique(CEDEN_ALL_Renamed$Analyte))) # 908 unique Analytes total
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

## Unique analyte names

### Identify unique analyte names

Create vectors that contain unique analyte names from each DF; sorted alphabetically

```r
CEDEN_Orig_Analytes <- sort(unique(CEDEN_ALL$Analyte)) #1810 unique analytes before we split the names from modifying text

CEDEN_Analytes <- sort(unique(CEDEN_ALL_Renamed$Analyte)) #910 unique analytes

SURF_Analytes <- sort(unique(SURF_ALL$Analyte)) #327
```

You can also look at analytes that are specifically in CEDEN and not in SURF using this code:

```r
DIF <- setdiff(CEDEN_Analytes, SURF_Analytes)

## Print DIF to see analytes in C and not in S

# Main things I noticed....
# PCH as ... three dif, 
# PCB - MANY differnt numbers, 
# PBDE = MANY
# Dechlorane - several
# Two each for DDD, DDE, and DDT

# Funny analytes to note: Young/female
```

### Create table of names {.tabset}

Both these output tables use the "corrected" naming of the CEDEN data. If you want to compare the original names, the vector "CEDEN_Orig_Analytes" contains those.


#### Format 1

If you want a single column of analyte names, and another identifying source (CEDEN vs SURF) run this code and remove the hashtag to write an output table.


```r
# DF of unique analytes for each

CE <- distinct(CEDEN_ALL_Renamed, Analyte, Source)
CE$Analyte <- CEDEN_Analytes # Easiest way to rearrange them alphabetically

SE <- distinct(SURF_ALL, Analyte, Source)
SE$Analyte <- SURF_Analytes # Easiest way to rearrange them alphabetically

#Unite into one DF
Analytes <- rbind(CE,SE)

# Run this code to save this file
write.csv(x = Analytes, file = "CEDENSURF_Analytes_Format1.csv", row.names = F)
```


#### Format 2

If you want a table of SURF and CEDEN analytes in adjacent columns like below, run this code and remove the hashtag to write an output table.


```r
# Example of differences
tibble(CEDEN = CEDEN_Analytes[2:11], SURF = SURF_Analytes[1:10])
```

```
## # A tibble: 10 x 2
##    CEDEN                                      SURF                         
##    <chr>                                      <chr>                        
##  1 1,2-bis(2,4,6- tribromophenoxy)ethane      1,4-dichlorobenzene (p-dcb0  
##  2 2-ethyl-1-hexyl-2,3,4,5-tetrabromobenzoate 2,4-d                        
##  3 2-ethylhexyl-diphenyl phosphate            2,4-db                       
##  4 2,4,6-tribromophenyl allyl ether           2,4,5-t                      
##  5 abamectin                                  3-hydroxycarbofuran          
##  6 acenaphthene                               4-hydroxy chlorothalonil     
##  7 acenaphthenes                              4-hydroxy molinate           
##  8 acenaphthylene                             4(2,4-db), dimethylamine salt
##  9 acetamiprid                                abamectin                    
## 10 acetone                                    acephate
```

```r
# Create empty DF with correct dimensions
Analytes <- setNames(data.frame(matrix(ncol = 2, nrow = 910)), c("CEDEN", "SURF"))

# Add in data
Analytes$CEDEN <- CEDEN_Analytes

Analytes$SURF[1:327] <- SURF_Analytes

# Run this code to save this file
# write.csv(x = Analytes, file = "CEDENSURF_Analytes_Format2.csv", row.names = F)
```

