---
title: "Adding Categories for Conceptual Model"
author: "Erika W"
date: "3/9/2021"
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

This rmd produces a limited combined dataset, retaining only those analytes identified within our conceptual model, and including a new column "SelectionList" by which analytes can be subsetted according to their role in the conceptual model.

There are two approaches that should work - whichever is most compatible with needs for later updates, or future documentation. 

## Data Splitting

### Method 1: Lists

If it's most simple to have a transparent list of analytes included in each category, documented within the R code, this approach will work. The code is a little longer. 

#### Load Data

Start with merged CEDEN and SURF data, with duplicates found and removed per the QA/QC protocol outlined at: https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/blob/main/CEDENSURF_Merge.md


```r
# Load CEDEN Data
CEDENSURF <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/CEDENSURFMod.csv")

# Create empty column to fill with values

CEDENSURF$SelectList <- NA

# Keep a record (DF) of unique analytes?

AnalyteList <- sort(unique(CEDENSURF$Analyte)) #1053 records
```

#### Append from Selection Lists


```r
## Used grepl() to help find all Analytes that matched the conceptual model, ie: AnalyteList[(grepl("nitr", AnalyteList))

## Water Quality Parameters
            
WQP_SelectList <- c("temperature", "ph", "oxygen",
                    "electricalconductivity", 
                    "nitrate as n", "nitrite as n", "nitrogen",
                    "nitrate + nitrite as n", "phosphorus as p",
                    "turbidity","settleable solids", 
                    "suspended sediment concentration", 
                    "salinity","sodium")


CEDENSURF$SelectList[CEDENSURF$Analyte %in% WQP_SelectList] <- "WQP"

# Metals

Metal_SelectList <- c("mercury", "selenium", "copper", "cadmium") # # Only one record of mercury (ii)r; chose to omit. # did not find methylmercury in list. 
# Others may want? "silver", 


CEDENSURF$SelectList[CEDENSURF$Analyte %in% Metal_SelectList] <- "Metal"

## Organophosphates

ORG_SelectList <- c("malathion", 
                    "diazinon", "diazinon degradate", 
                    "diazinon oxon", "diazoxon",
                    "chlorpyrifos")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% ORG_SelectList] <- "OrganoP"

## Neonicotinoids

Neon_SelectList <- c("hydroxy-imidacloprid", "imidacloprid")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% Neon_SelectList] <- "Neon"

## Pyrethroids

Pyre_SelectList <- c("bifenthrin", "cyfluthrin", 
                     "esfenvalerate", "esfenvalerate/fenvalerate",
                     "esfenvalerate/fenvalerate-1",
                     "esfenvalerate/fenvalerate-2")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% Pyre_SelectList] <- "Pyrethroids"

## GABA inhibitors

GABA_SelectList <- AnalyteList[grepl('fipronil', AnalyteList)] # THIS IS THE SAME AS: <- c("desulfinyl fipronil", "desulfinyl fipronil amide", "fipronil", "fipronil amide", "fipronil desulfinyl", "fipronil desulfinyl amide", "fipronil detrifluoromethylsulfinyl", "fipronil sulfide","fipronil sulfone")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% GABA_SelectList] <- "GABA"

## Glyphosate (it's only one!)

CEDENSURF$SelectList[CEDENSURF$Analyte == "glyphosate"] <- "Glyphosate"

## Atrazine

Atraz_SelectList <-  c("atrazine", "atrazine degradate")
# Chose to omit single records: "desethyl-atrazine""desisopropyl-atrazine", "hydroxyatrazine","hydroxyatrazine, 2-" )

CEDENSURF$SelectList[CEDENSURF$Analyte %in% Atraz_SelectList] <- "Atrazine"


## Late Additions bc available tox data

      # Tox available for DDT / Dichlorodiphenyltrichloroethane

        ## Find names:
        # AnalyteList[(grepl("ddt", AnalyteList))]
        ## make names consistent
        CEDENSURF$Analyte[CEDENSURF$Analyte == "ddt(o,p')" |
                        CEDENSURF$Analyte == "ddt(p,p')"] <- "ddt"

      # Tox available for dinoseb aka dinitrophenol
        ## Find names:
        AnalyteList[(grepl("dinitroph", AnalyteList))]
```

```
## [1] "dinitrophenol"
```

```r
        ## make names consistent
        CEDENSURF$Analyte[CEDENSURF$Analyte == "dinoseb" |
                        CEDENSURF$Analyte == "dinitrophenol"] <- "dinoseb"
        
      # Tox available for triclopyr / garlon (only one name)
      # Tox available for thiobencarb (only one name)
      # Tox available for molinate (only one name). 
          # Too few records of degradate (4-hydroxy molinate) to include
      
Late_SelectList <- c("ddt","thiobencarb", "dinoseb", "triclopyr", "molinate")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% Late_SelectList] <- "Late"
```

#### Save categories assigned to analytes


```r
## Save Analyte Table showing assigned categories

AnalyteTable <- CEDENSURF %>% select(Analyte, SelectList) %>% distinct(Analyte, .keep_all = T) # 44 records

write.csv(x = AnalyteTable, 
          file = "Data/Output/CEDENSURF_AnalytesCategorized.csv", 
          na = "", row.names = F)
```

### Save Categorized, Limited CEDENSURF 


```r
## Remove rows with irrelevant Analytes?
CEDENSURF<- CEDENSURF %>% filter(!is.na(SelectList))
```

The result is 39831 records, all appended with appropriate selection categories according to the conceptual model


```r
head(CEDENSURF %>% select(Date, Analyte, Result, Unit, StationName, SelectList))
```

```
##          Date                          Analyte   Result Unit
## 1: 2009-10-14                        turbidity 15.40000  NTU
## 2: 2009-10-14                        turbidity 10.40000  NTU
## 3: 2009-10-15                        turbidity 21.70000  NTU
## 4: 2009-10-16                          mercury  0.00023 ug/L
## 5: 2009-10-16 suspended sediment concentration 11.00000 mg/L
## 6: 2009-10-21                        turbidity 15.90000  NTU
##                                                            StationName
## 1:                                    Montezuma Slough at Nurse Slough
## 2: Suisun Bay, off Chipps Island, opposite Sacramento North ferry slip
## 3:                                                Suisun at Rush Ranch
## 4:                                                   Mallard Island-MI
## 5:                                                   Mallard Island-MI
## 6:                                Sacramento River at Point Sacramento
##    SelectList
## 1:        WQP
## 2:        WQP
## 3:        WQP
## 4:      Metal
## 5:        WQP
## 6:        WQP
```


```r
write.csv(x = CEDENSURF, file = "Data/Output/CEDENSURF_Limited.csv", 
          row.names = F)
```


### Method 2: Table

#### Load Data

Two files:

1. Merged CEDEN and SURF data, with duplicates found and removed per the QA/QC protocol outlined at: https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/blob/main/CEDENSURF_Merge.md

2. Table containing analyte names and selection categories. This would be made externally, but for this example I'll use a "mock table" adapted from my output above. 

A suggested structure of this table is:

```r
# Load external table (Mock Data)

AnalyteTable <- CEDENSURF %>% select(Analyte, SelectList) %>% distinct(Analyte, .keep_all = T) # 44 records

head(AnalyteTable)
```

```
##                             Analyte SelectList
## 1:                        turbidity        WQP
## 2:                          mercury      Metal
## 3: suspended sediment concentration        WQP
## 4:                           oxygen        WQP
## 5:                      temperature        WQP
## 6:                               ph        WQP
```

```r
# Load CEDEN Data
CEDENSURF2 <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/CEDENSURFMod.csv")
```

With this, we could append the category to the original data using a merge


```r
CEDENSURF2 <- merge(x = CEDENSURF2, y = AnalyteTable) # removes rows of x with no match in Y

head(CEDENSURF2 %>% select(Date, Analyte, Result, StationName, SelectList))
```

```
##          Date  Analyte Result                              StationName
## 1: 2011-04-05 atrazine      0                               Roe Island
## 2: 2011-04-05 atrazine      0 Grizzly Bay at Dolphin nr. Suisun Slough
## 3: 2011-04-05 atrazine      0                                Pittsburg
## 4: 2011-04-05 atrazine      0                                Avon Pier
## 5: 2011-04-05 atrazine      0                            Middle Ground
## 6: 2011-04-12 atrazine      0 Grizzly Bay at Dolphin nr. Suisun Slough
##    SelectList
## 1:   Atrazine
## 2:   Atrazine
## 3:   Atrazine
## 4:   Atrazine
## 5:   Atrazine
## 6:   Atrazine
```

The result is 39831 records, all appended with appropriate selection categories according to the conceptual model

<br>


