---
title: "IssuesDocumentation"
author: "Erika W"
date: "3/2/2021"
output:
  html_document:
    code_download: true
    keep_md: true
    code_folding: hide
    toc: true
    toc_float: true
    toc_depth: 4
    theme: lumen
---




```r
rm(list=setdiff(ls(), c("CEDENSURF", "CEDENSURF_DupChecked", "CEDENSURFModNC")))

library(data.table)
library(lubridate)
library(sf)
library(tidyverse)
```

### Overview

This document is to contain QA/QC issues that have come up during the merge efforts between CEDEN and SURF

### Load data


```r
# Load data

# original CEDEN WQ data

CEDEN_OriginalWQ <- fread("Data/CEDEN_WQ_20212259571.txt", quote = "") %>%
 filter(between(SampleDate, 
        as_date("2009-10-01"),as_date("2019-09-30"))) %>%
  mutate(Result = as.numeric(Result)) # Converts blanks to NA
```

```
## Warning: Problem with `mutate()` input `Result`.
## i NAs introduced by coercion
## i Input `Result` is `as.numeric(Result)`.
```

```r
# Ceden and SURF WQ data

SURFMod_WQ <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/SURFMod_water.csv")

CEDENMod_WQ <- fread("https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_WQ.csv")

# Original Merged output (With Dups)

CEDENSURF <- fread("Data/Output/CEDENSURF_IssueInvestigation.csv")

# De-dup Merged output (as of 3/5/2021)

CEDENSURFNC <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/CEDENSURFMod.csv")
```
### Issues & Investigation

<br>

#### 1. *Resolved* Non-results in CEDEN (NA) appear in SURF as "0" result concentration.

Conclusion: Our strategy to reduce CEDEN was removing valid records. NA results paired with qual codes ND or DNQ replaced with 0's as in SURF.

This one was FIXED by the updates to CEDENMod.rmd

**Glyphosate in SURF-CEDEN data, but not in CEDEN data.**

An example record from SURF cited as coming from CEDEN:

```r
A <- CEDENSURF %>% filter(grepl('Grizzly', StationName)) %>%
  filter(grepl('Dolphin', StationName)) %>%
  filter(Source == "SURF") %>%
  filter(Data.source == "CEDEN")%>%
  filter(Analyte == "glyphosate")%>%
  select(Agency, Date, StationName, Analyte, Result)

A[1]
```

```
##                     Agency       Date
## 1: Michael L. Johnson, LLC 2012-05-08
##                                                   StationName    Analyte Result
## 1: Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7 glyphosate      0
```

```r
# Formerly, this would have returned no record :)

A <- CEDENSURF %>% filter(grepl('Grizzly', StationName)) %>%
  filter(grepl('Dolphin', StationName)) %>%
  filter(Source == "CEDEN") %>%
  filter(Analyte == "glyphosate")%>%
  select(Agency, Date, StationName, Analyte, Result)

A[1]
```

```
##     Agency       Date                              StationName    Analyte
## 1: MLJ-LLC 2011-04-05 Grizzly Bay at Dolphin nr. Suisun Slough glyphosate
##    Result
## 1:      0
```


<br>

#### 2. CEDEN-SURF data not in CEDEN 

Conclusion: Possibly indicative that data for certain areas of our study region may be omitted in the CEDEN data download. Alternatively, this project was incorrectly labeled as sourced from CEDEN?

SURF Name: "Toe Drain Nr Babel Slough Nr Freeport Ca"
SURF Code: "57_58"

Could not ID this location in CEDEN data, using grepl() to allow approximate naming. None with "Babel" and T in StationName


```r
# From SURF, identified as coming from CEDEN = 657
CS <- CEDENSURF %>% filter(grepl('Toe', StationName)) %>%
  filter(grepl('Babel', StationName)) %>%
  filter(Data.source == "CEDEN")%>%
  filter(Source == "SURF")

# Direct from CEDEN = 0
C <- CEDENSURF %>%
  filter(grepl('Babel', StationName)) %>%
  filter(Source == "CEDEN")
```

There are 657 records at this site from SURF, all from the same agency, and identified as coming from CEDEN. 

Here is one example record:


```r
nrow(CS)
```

```
## [1] 657
```

```r
unique(CS$Agency)
```

```
## [1] "USGS California Water Science Center"
```

```r
CS[1]
```

```
##                                  Agency      Analyte      CollectionMethod
## 1: USGS California Water Science Center pyrimethanil Filtered water sample
##    County Data.source       Date Datum                   geometry Latitude
## 1:   Yolo       CEDEN 2016-07-28  <NA> c(-121.588225, 38.4747806) 38.47478
##    LocationCode Longitude    LOQ MatrixName  MDL ParentProject Program Project
## 1:         <NA> -121.5882 0.0041       <NA> -999          <NA>    <NA>    <NA>
##    rb_number Record_id regional_board Result ResultQualCode RL Source
## 1:        NA   1805112           <NA>      0           <NA> NA   SURF
##    StationCode                              StationName Study_cd
## 1:       57_58 Toe Drain Nr Babel Slough Nr Freeport Ca      747
##                                                                                                Study_description
## 1: SFCWA YoloBypassFoodWeb , State and Federal Contractors Water Agency , SFCWA YoloBypassFoodWeb PEST 2015-2016
##            Study_weblink   Subregion Total organic carbon (%) Unit
## 1: http://www.ceden.org/ North Delta                       NA  ppb
```

These aren't in CEDENMod nor in the original water data...


```r
C_OG <- CEDEN_OriginalWQ %>%
  filter(grepl('Babel', StationName)) %>%
  select(SampleDate, StationName, Analyte, Result, ResultQualCode, SampleComments)
```


<br>

#### 3. Naming inconsistencies between SURF and CEDEN

Conclusion: It appears that SURF has appended station names from CEDEN with extra info. 

Solution: Filter using latitude and longitude

**Naming:**

*SURF: "Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7"*
*SURF Code: "48_52"*

*CEDEN: "Grizzly Bay at Dolphin nr. Suisun Slough"*
*CEDEN Code: "207SNB0D7"*

and 

*SURF Name: "Sacramento River at Freeport (USGS-11447650)"*
*SURF Code: "34_5"*

*CEDEN Name: "Sacramento River at Freeport, CA"*
*CEDEN Code: 11447650*

**Projections/geometry**

```r
A <- CEDENSURF %>% filter(grepl('Grizzly', StationName)) %>%
  filter(grepl('Dolphin', StationName)) %>%
  filter(Source == "SURF") %>%
  filter(Data.source == "CEDEN")%>%
  select(StationName, StationCode, Latitude, Longitude, geometry)

B <- CEDENSURF %>% filter(grepl('Grizzly', StationName)) %>%
  filter(grepl('Dolphin', StationName)) %>%
  filter(Source == "CEDEN")%>%
  select(StationName, StationCode, Latitude, Longitude, geometry)

rbind(A[1,], B[1,])
```

```
##                                                   StationName StationCode
## 1: Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7       48_52
## 2:                   Grizzly Bay at Dolphin nr. Suisun Slough   207SNB0D7
##    Latitude Longitude                       geometry
## 1: 38.11708 -122.0397        c(-122.03972, 38.11708)
## 2: 38.11708 -122.0397 c(-122.03971862793, 38.117081)
```


<br>

#### 4. Data in SURF-CEDEN not in CEDEN, and vice versa

If we subset just the Analytes that occur in CEDEN-SURF and not in our CEDEN set, we can hone in on some specific agencies and/or sites that account for the majority of the discrepancies.


```r
CS <- CEDENSURF %>%
  filter(Data.source == "CEDEN")%>%
  filter(Source == "SURF")

# Direct from CEDEN
C <- CEDENSURF %>%
  filter(Source == "CEDEN")

DIF<- setdiff(C, CS) #Ceden not in ceden-SURF

CEDnotCeSu<- unique(setdiff(C$Analyte, CS$Analyte))

DIF<- setdiff(CS, C) #Analytes in ceden-surf and not in ceden

CeSUnotC<- unique(setdiff(CS$Analyte, C$Analyte))
```

There are only 28 analytes in SURF listed as 'from CEDEN' which do not occur in the CEDEN data. These are:

```r
length(CeSUnotC)
```

```
## [1] 28
```

```r
CeSUnotC
```

```
##  [1] "desulfinyl fipronil amide"                    
##  [2] "s,s,s-tributyl phosphorotrithioate"           
##  [3] "carbendazim (methyl 2-benzimidazolecarbamate)"
##  [4] "chlorthal-dimethyl"                           
##  [5] "methyl parathion"                             
##  [6] "chlorpyrifos-methyl"                          
##  [7] "s-cypermethrin"                               
##  [8] "lambda cyhalothrin"                           
##  [9] "cyhalofop butyl"                              
## [10] "benefin"                                      
## [11] "azinphos-methyl-oa"                           
## [12] "desulfinyl fipronil"                          
## [13] "azinphos-methyl"                              
## [14] "chlorpyrifos oa"                              
## [15] "endosulfan"                                   
## [16] "paraquat dichloride"                          
## [17] "bentazon, sodium salt"                        
## [18] "ddvp"                                         
## [19] "lindane (gamma-bhc)"                          
## [20] "diquat dibromide"                             
## [21] "sulprofos"                                    
## [22] "trichlorophon"                                
## [23] "dea"                                          
## [24] "pyrethrins"                                   
## [25] "acet"                                         
## [26] "hydroxyatrazine, 2-"                          
## [27] "swep"                                         
## [28] "alpha-endosulfan"
```

Conversely, there are 823analytes in CEDEN that do not occur in the CEDEN-SURF set, most of which are likely naming issues (ie; from the splitting of analyte from type in CEDEN), which should be corrected.

For example:

```r
length(CEDnotCeSu)
```

```
## [1] 823
```

```r
head(CEDnotCeSu)
```

```
## [1] "ammonia as n"   "ammonia as nh3" "turbidity"      "mercury"       
## [5] "pcb 008"        "pcb 052"
```



```r
# Summarize by agency - sort descending (agency with the most in CS not in C at top)

SumDifA <- DIF %>% group_by(Agency) %>%
  summarise(n = length(Latitude), 
            n_results = length(unique(Result))) %>%
  arrange(desc(n))

head(SumDifA)
```

```
## # A tibble: 6 x 3
##   Agency                                                             n n_results
##   <chr>                                                          <int>     <int>
## 1 Michael L. Johnson, LLC                                         8929       218
## 2 USGS Pesticide Fate Research Group (PFRG), Organic Chemistry ~  8384       251
## 3 Pacific Ecorisk                                                 3447       229
## 4 USGS California Water Science Center                            2348       231
## 5 Sacramento Regional County Sanitation District                  1406         9
## 6 University of California Davis-Granite Canyon                   1360       235
```


```r
# Summarize by station - sort descending (agency with the most in CS not in C at top)

SumDif <- DIF %>% group_by(StationName) %>%
  summarise(n = length(Latitude), 
            n_results = length(unique(Result))) %>%
  arrange(desc(n))

head(SumDif)
```

```
## # A tibble: 6 x 3
##   StationName                                              n n_results
##   <chr>                                                <int>     <int>
## 1 Ulatis Creek at Brown Road                            3990       218
## 2 Mokelumne River at New Hope Rd Bridge (in Delta)      1981        28
## 3 Sacramento River at Hood Monitoring Station Platform  1942        46
## 4 San Joaquin R at Buckley Cove. CEDEN: 544LSAC13       1772       105
## 5 Walthall Slough                                       1595        46
## 6 Freeport                                              1305         9
```
There are 13 agencies represented, and 13 stations represented in these discrepancies.

**Let's take Ulatis Creek, n = 3,990**

Only half of the instances are with Result = 0; 4624/8667


```r
## Preview data, anything in common?

DIF %>%
  filter(grepl('Ulatis', StationName)) %>%
  filter(grepl('Creek', StationName)) %>%
  filter(grepl('Brown', StationName)) %>% 
  nrow(.)
```

```
## [1] 3990
```

Subsetting from the merged dataset to see if the thousands of non-detects at this station are retained in CEDEN, we find that there are only 17 0-results at this station in the CEDEN data, compared with the CEDEN-SURF data.


```r
CS <- CEDENSURF %>%
  filter(Data.source == "CEDEN") %>%
  filter(Source == "SURF") %>%
  filter(grepl('Ulatis', StationName)) %>%
  filter(grepl('Creek', StationName)) %>%
  filter(grepl('Brown', StationName)) %>%
  filter(Result == "0")

C <- CEDENSURF %>%
  filter(Source == "CEDEN") %>%
  filter(grepl('Ulatis', StationName)) %>%
  filter(grepl('Creek', StationName))%>%
  filter(grepl('Brown', StationName))%>%
  filter(Result == "0")
```

In the original data at this site, there are 7492 NA results at this site, which were removed during our CEDEN data prep process. Within these, 7462 are ND qualcodes (indicating non-detect, or results below the MDL), and only 32 with NR qualcodes (which indicates the test failed)


```r
C_OG <- CEDEN_OriginalWQ %>% 
  filter(grepl('Ulatis', StationName)) %>%
  filter(grepl('Creek', StationName)) %>%
  filter(grepl('Brown', StationName)) %>%
  filter(is.na(Result)) %>%
  select(SampleDate, StationName, Analyte, Result, ResultQualCode, SampleComments)

head(C_OG)
```

```
##    SampleDate                StationName                     Analyte Result
## 1: 2010-01-19 Ulatis Creek at Brown Road    Oxygen, Dissolved, Total     NA
## 2: 2010-01-19 Ulatis Creek at Brown Road                 Temperature     NA
## 3: 2010-01-19 Ulatis Creek at Brown Road SpecificConductivity, Total     NA
## 4: 2010-01-19 Ulatis Creek at Brown Road             Aldicarb, Total     NA
## 5: 2010-01-19 Ulatis Creek at Brown Road          Fluometuron, Total     NA
## 6: 2010-01-19 Ulatis Creek at Brown Road          Mexacarbate, Total     NA
##    ResultQualCode SampleComments
## 1:             NR               
## 2:             NR               
## 3:             NR               
## 4:             ND               
## 5:             ND               
## 6:             ND
```

```r
# How many ND?

CEDEN_OriginalWQ %>% 
  filter(grepl('Ulatis', StationName)) %>%
  filter(grepl('Creek', StationName)) %>%
  filter(grepl('Brown', StationName)) %>%
  filter(is.na(Result)) %>%
  filter(ResultQualCode == "ND") %>%
  nrow(.)
```

```
## [1] 7460
```

```r
# How many NR?

CEDEN_OriginalWQ %>% 
  filter(grepl('Ulatis', StationName)) %>%
  filter(grepl('Creek', StationName)) %>%
  filter(grepl('Brown', StationName)) %>%
  filter(is.na(Result)) %>%
  filter(ResultQualCode == "NR") %>%
  nrow(.)
```

```
## [1] 32
```


<br>

#### 5. Differences in geometry btwn SURF and CEDEN

Same station at Grizzly Bay; despite same Lat and Long coordinates listed in SURF (1998 and 2012), compared to CEDEN (2010), the shapefile conversion to geometry is different.


```r
rbind(A[1,], B[1,])
```

```
##                                                   StationName StationCode
## 1: Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7       48_52
## 2:                   Grizzly Bay at Dolphin nr. Suisun Slough   207SNB0D7
##    Latitude Longitude                       geometry
## 1: 38.11708 -122.0397        c(-122.03972, 38.11708)
## 2: 38.11708 -122.0397 c(-122.03971862793, 38.117081)
```



<br>

#### 6. Results in SURF vs CEDEN

I'm interested, if SURF changed ND's in CEDEN from blank --> 0, would they also have changed values listed that were below MDL to 0? (Because theoretically also labeled ND)

Selected data in original set at one site, with qual code ND, and result above zero, then looked for that data in SURF by filtering on a specific analyte.

Prodiamine? No, also not found under value = 0 in C-S.
Penoxsulam? No, but YES in CS as result = 0. 
Fluridone?  Not in CEDENSURF with results 1-4, but YES in CEDENSURF 

*NOTE: Results in CEDEN are in ng/L whereas in CEDENSURF they're in PPB. Some results < MDL may be recorded as 0 in SURF*

**Example 1**

```r
# Subset records at one station with ND code and results above 0 (btwn 1-4)

C <- CEDEN_OriginalWQ %>% 
  filter(grepl('Ulatis', StationName)) %>%
  filter(grepl('Creek', StationName)) %>%
  filter(grepl('Brown', StationName)) %>%
  filter(ResultQualCode == "ND") %>%
  filter(between(Result,1,4) ) 

CSR <- CEDENSURF %>%
  filter(Data.source == "CEDEN") %>%
  filter(Source == "SURF") %>%
  filter(grepl('Ulatis', StationName)) %>%
  filter(grepl('Creek', StationName)) %>%
  filter(grepl('Brown', StationName)) %>%
  filter(between(Result,1,4) )

## Look for that ND record in SURF as ANY record with results in the appropriate range and with that analyte name. Not found. 

# Matches in Ceden OG
C2<- C %>%
  filter(grepl('Fluridone', Analyte)) %>%
  select(Agency = SubmittingAgency, StationName, Date = SampleDate, CollectionMethod = CollectionMethodName, Analyte, Result, Unit, ResultQualCode)

# Example: Delta RMP, on 2016-02-17
C2[2,]
```

```
##            Agency                StationName       Date CollectionMethod
## 1: USGS-PFRG-OCRL Ulatis Creek at Brown Road 2016-02-17       Water_Grab
##                 Analyte Result Unit ResultQualCode
## 1: Fluridone, Dissolved      2 ng/L             ND
```

```r
# Matches in Ceden-sourced Surf data - none
CSR2<- CSR %>%
  filter(grepl('fluridone', Analyte))

# Matches in the CEDENSURF data allowing  another result value?
CS <- CEDENSURF %>%
  filter(Data.source == "CEDEN") %>%
  filter(Source == "SURF") %>%
  filter(grepl('Ulatis', StationName)) %>%
  filter(grepl('Creek', StationName)) %>%
  filter(grepl('Brown', StationName)) %>%
  filter(grepl('fluridone', Analyte))

CS2<- CS %>%
  filter(Date == "2016-02-17") %>%
  select(Agency, StationName, Date,CollectionMethod, Analyte, Result, Unit)

rbind(C2[2,], CS2, fill = TRUE)
```

```
##                                                                                     Agency
## 1:                                                                          USGS-PFRG-OCRL
## 2: USGS Pesticide Fate Research Group (PFRG), Organic Chemistry Research Laboratory (OCRL)
##                   StationName       Date      CollectionMethod
## 1: Ulatis Creek at Brown Road 2016-02-17            Water_Grab
## 2: Ulatis Creek at Brown Road 2016-02-17 Filtered water sample
##                 Analyte Result Unit ResultQualCode
## 1: Fluridone, Dissolved      2 ng/L             ND
## 2:            fluridone      0  ppb           <NA>
```
This shows two samples that are likely duplicates, one "from ceden" in SURF, and the other from the original CEDEN file. The original CEDEN file has methods further described under "prep preservation name" = "lab filtered", which could explain the differences in collection method nomenclature.

**Example 2**

Lets try a new station with ND results - Freeport? It was actually really hard to find other examples! I may have been lucky with my first station/process.  Can come back to this. 



<br>

### Up next: 

#### 1. Fixing name errors in CEDEN data (following automated split)

We can probably focus on the setdiff list above: find those names that are clearly nonsensical, then refer back to the original data to determine what they were meant to be.

#### 2. Whether to retain or remove composite samples

If we decide to remove them, determine how to identify.

In CEDEN, might be able to locate by CollectionMethod OR CollectionDeviceDescription

ie: "7 day auto sampler" and "AutoSampler" collection methods may indicate composite over time, or "depth-integrating" collection device description may indivate composite over depths.
