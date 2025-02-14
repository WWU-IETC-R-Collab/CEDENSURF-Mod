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


```r
# packages b/c accessing private repo

library(httr)
library(tidyverse)
library(gh)
library(gitcreds)
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
# Previous way to load data - does not work for private repo

# Load CEDEN Data
CEDENSURF <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/CEDENSURFMod.csv")
```



```r
# Load Data - Compatible with private repository

tmp <- tempfile()

CEDENSURF <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDENSURF-mod/main/Data/Output/CEDENSURFMod.csv",
                   .token = gh_token(), 
                   .destfile = tmp)

CEDENSURF <- read_csv(tmp) # works for me!
```

```
## Rows: 210208 Columns: 33
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (22): Agency, Analyte, CollectionMethod, County, Data.source, Datum, ge...
## dbl  (10): Latitude, Longitude, LOQ, MDL, rb_number, Record_id, Result, RL, ...
## date  (1): Date
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```



```r
# Create empty column to fill with values

CEDENSURF$SelectList <- NA

# Keep a record (DF) of unique analytes?

AnalyteList <- sort(unique(CEDENSURF$Analyte)) #1053 records
```

#### Append from Selection Lists


Defined analyte categories based on the conceptual model provided

```r
## Used grepl() to help find all Analytes that matched the conceptual model, ie: AnalyteList[(grepl("nitr", AnalyteList))

## Water Quality Parameters
            
WQP_SelectList <- c("temperature", "ph", "oxygen",
                    "electricalconductivity", 
                    "nitrate as n", "nitrite as n",
                    "nitrogen",
                    "nitrate + nitrite as n",
                    "phosphorus as p",
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
                    "chlorpyrifos", "dichlorvos", 
                    "phorate")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% ORG_SelectList] <- "OrganoP"


## Neonicotinoids

Neon_SelectList <- c("hydroxy-imidacloprid",
                     "imidacloprid", 
                     "clothianidin")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% Neon_SelectList] <- "Neon"

## Pyrethroids

Pyre_SelectList <- c("bifenthrin", "cyfluthrin", 
                     "esfenvalerate", 
                     "esfenvalerate/fenvalerate",
                     "esfenvalerate/fenvalerate-1",
                     "esfenvalerate/fenvalerate-2")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% Pyre_SelectList] <- "Pyrethroids"

## GABA inhibitors

GABA_SelectList <- AnalyteList[grepl('fipronil', AnalyteList)] # THIS IS THE SAME AS: <- c("desulfinyl fipronil", "desulfinyl fipronil amide", "fipronil", "fipronil amide", "fipronil desulfinyl", "fipronil desulfinyl amide", "fipronil detrifluoromethylsulfinyl", "fipronil sulfide","fipronil sulfone")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% GABA_SelectList] <- "GABA"

## Glyphosate: In conceptual model alone, though it's a herbicide (organophosphorous compound)

CEDENSURF$SelectList[CEDENSURF$Analyte == "glyphosate"] <- "Glyphosate"

## Atrazine: In conceptual model alone, though it's a herbicide

Atraz_SelectList <-  c("atrazine", "atrazine degradate")
# Chose to omit single records: "desethyl-atrazine""desisopropyl-atrazine", "hydroxyatrazine","hydroxyatrazine, 2-" )

CEDENSURF$SelectList[CEDENSURF$Analyte %in% Atraz_SelectList] <- "Atrazine"
```

We later added additional categories to account for chemicals that were identified in the Year 1 Report as problematic and / or exceeding USEPA Aquatic Life Benchmarks:



```r
## Late Addition bc in YEAR 1 REPORT

    # Neonicotinoid: clothianidin (n = 600), organophoshphate dichlorvos (n= 460), and phorate (n = 1000) - see above

    # Linuron (n = 10226)
    # oxyfluorfen (n = 1358)
    # paraquat dichloride (n=227)
    # pyridaben (n = 744)

    # DDT / Dichlorodiphenyltrichloroethane (and Tox available)

        ## Find names:
        AnalyteList[(grepl("dd", AnalyteList))]
```

```
##  [1] "ddd(o,p')"                   "ddd(p,p')"                  
##  [3] "dde(o,p')"                   "dde(p,p')"                  
##  [5] "ddmu(p,p')"                  "ddt(o,p')"                  
##  [7] "ddt(p,p')"                   "ddvp"                       
##  [9] "dechlorane plus mono adduct" "hpcdd"                      
## [11] "hxcdd"                       "ocdd"                       
## [13] "pecdd"                       "tcdd"
```

```r
        ## make names consistent
        CEDENSURF$Analyte[CEDENSURF$Analyte == "ddt(o,p')" |
                        CEDENSURF$Analyte == "ddt(p,p')"] <- "ddt" # 421

        CEDENSURF$Analyte[CEDENSURF$Analyte == "ddd(o,p')" |
                        CEDENSURF$Analyte == "ddd(p,p')"] <- "ddd" # 444
        
        CEDENSURF$Analyte[CEDENSURF$Analyte == "dde(o,p')" |
                        CEDENSURF$Analyte == "dde(p,p')"] <- "dde"  # 444      
    # Endosulfan has two isomers (endo and exosulfan, or endosulfan i and  endosulfan ii). It is acutely toxic with a high potential to bioaccumulate, and therefore is being phased out of agriculture.
    AnalyteList[(grepl("duran", AnalyteList))]
```

```
## character(0)
```

```r
    CEDENSURF$Analyte[CEDENSURF$Analyte == "alpha-endosulfan" |
                        CEDENSURF$Analyte == "exosulfan"|
                        CEDENSURF$Analyte == "endosulfan ii"|
                        CEDENSURF$Analyte == "endosulfan i"] <- "endosulfan"
    

# Herbicides tested > USEPA Aquatic Life Benchmarks: Diuron, thiobencarb, propanil, 2,4-d, thiobencarb, glyphosate, atrazine
      
      #AnalyteList[(grepl("propanil", AnalyteList))]
      # nrow(CEDENSURF[CEDENSURF$Analyte == "diuron"]) #1729
      # nrow(CEDENSURF[CEDENSURF$Analyte == "thiobencarb"]) # 1300
      # nrow(CEDENSURF[CEDENSURF$Analyte == "propanil"]) # 1057
      # nrow(CEDENSURF[CEDENSURF$Analyte == "2,4-d"]) #96 - insufficient coverage
     
     
## Late Additions bc available tox data

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

      # Tox available for molinate (only one name). 
          # nrow(CEDENSURF[CEDENSURF$Analyte == "molinate"]) # 773
          # Too few records of degradate (4-hydroxy molinate) to include
        
## Create new subsections for these Herbicides and Organochlorines     
Herb_SelectList <- c("thiobencarb", "dinoseb",
                     "triclopyr", "molinate", 
                     "diuron", "propanil", 
                     "linuron",
                     "oxyfluorfen", 
                     "paraquat dichloride")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% Herb_SelectList] <- "Herbicide"

OrganoCh_SelectList <- c("ddt","ddd","dde", 
                         "endosulfan","endosulfan sulfate",
                         "pyridaben")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% OrganoCh_SelectList] <- "OrganoCh"
```

We also added 3 chemicals which were included in a paper assessing how salinity alters toxicity to a model fish species. The fourth was not present in this dataset. (https://www.mdpi.com/2305-6304/9/5/114)


```r
TEMPORARY EXPLORATIONS FOR ADDING NEW COMPOUNDS - EMMA REQUEST

AnalyteList[grepl("penz", AnalyteList)]

# 1. DICLORAN - fungicide. Use "dichloran". Only 5 records.

length(CEDENSURF$Result[CEDENSURF$Analyte == "dichloran"])

# 2. Myclobutanil = in list and a fungicide as well. 868 Records

length(CEDENSURF$Result[CEDENSURF$Analyte == "myclobutanil"])

# 3. Penconazole - not in list. nor pseudonyms

# 4. Triadimefon - in list as "triadimefon". 743 Records

length(CEDENSURF$Result[CEDENSURF$Analyte == "triadimefon"])
```


```r
Fungicide_SelectList <- c("myclobutanil", "triadimefon","dichloran")

CEDENSURF$SelectList[CEDENSURF$Analyte %in% Fungicide_SelectList] <- "Fungicide"
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

The result is 51373 records, all appended with appropriate selection categories according to the conceptual model


```r
head(CEDENSURF %>% select(Date, Analyte, Result, Unit, StationName, SelectList))
```

```
## # A tibble: 6 x 6
##   Date       Analyte                            Result Unit  StationName SelectList
##   <date>     <chr>                               <dbl> <chr> <chr>       <chr>     
## 1 2009-10-14 turbidity                        15.4     NTU   Montezuma ~ WQP       
## 2 2009-10-14 turbidity                        10.4     NTU   Suisun Bay~ WQP       
## 3 2009-10-15 turbidity                        21.7     NTU   Suisun at ~ WQP       
## 4 2009-10-16 mercury                           0.00023 ug/L  Mallard Is~ Metal     
## 5 2009-10-16 suspended sediment concentration 11       mg/L  Mallard Is~ WQP       
## 6 2009-10-21 turbidity                        15.9     NTU   Sacramento~ WQP
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
## # A tibble: 6 x 2
##   Analyte                          SelectList
##   <chr>                            <chr>     
## 1 turbidity                        WQP       
## 2 mercury                          Metal     
## 3 suspended sediment concentration WQP       
## 4 oxygen                           WQP       
## 5 temperature                      WQP       
## 6 ph                               WQP
```

```r
# Load CEDEN Data

tmp <- tempfile()

CEDENSURF <- gh("https://raw.githubusercontent.com/WWU-IETC-R-Collab/CEDENSURF-mod/main/Data/Output/CEDENSURFMod.csv",
                   .token = gh_token(), 
                   .destfile = tmp)

CEDENSURF2 <- read_csv(tmp)
```

```
## Rows: 210208 Columns: 33
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (22): Agency, Analyte, CollectionMethod, County, Data.source, Datum, ge...
## dbl  (10): Latitude, Longitude, LOQ, MDL, rb_number, Record_id, Result, RL, ...
## date  (1): Date
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# Following code is the old way to load data / pre private repository
# CEDENSURF2 <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/CEDENSURFMod.csv")
```

With this, we could append the category to the original data using a merge


```r
CEDENSURF2 <- merge(x = CEDENSURF2, y = AnalyteTable) # removes rows of x with no match in Y

head(CEDENSURF2 %>% select(Date, Analyte, Result, StationName, SelectList))
```

```
##         Date  Analyte Result
## 1 2011-04-26 atrazine      0
## 2 2013-08-20 atrazine      0
## 3 2012-03-27 atrazine      0
## 4 2013-01-15 atrazine      0
## 5 2017-06-28 atrazine      0
## 6 2015-06-04 atrazine      0
##                                                  StationName SelectList
## 1                                              Middle Ground   Atrazine
## 2                                   Empire Tract @ 8 Mile Rd   Atrazine
## 3 Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7   Atrazine
## 4                     Bishop Cut at Eight Mile Rd (in Delta)   Atrazine
## 5                Cache Slough at Ryer Island Road (in Delta)   Atrazine
## 6               Sacramento River at Freeport (USGS-11447650)   Atrazine
```

The result is  records, all appended with appropriate selection categories according to the conceptual model

<br>


