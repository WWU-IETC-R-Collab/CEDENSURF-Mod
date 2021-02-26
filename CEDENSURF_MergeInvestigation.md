---
title: "Merge Investigation"
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

# Remove Duplicates

Due to reported combined efforts to translate CEDEN data to SURF and vice versa, and issues with replicates being retained in each dataset, careful detection and elimination of duplicates should precede any merge and analysis.

## CEDEN Data

The tox data seems to be focused on associating WQ parameters with specific biological assays. These WQ parameters may already be present in the CEDEN_WQ dataset. 

There are 60429 records in the WQ dataset
and 60531 in the Tox dataset. 

### CEDEN Tox 

In the tox dataset, there were several records related to each species assessment - by creating a group identifier by the location, date, and organism assessed, I calculated that there were ~ 2430 unique biological assays.11 different species are represented.

#### Exact duplicates

duplicated() produces a vector identifying which records are duplicates. This can be appended to the original dataset for further investigation. The first record is marked FALSE, while subsequent matches are marked TRUE


```r
# add index column identifying which entries are duplicated

CEDENMod_Tox$DupCheck <- duplicated(CEDENMod_Tox)

summary(CEDENMod_Tox$DupCheck)
```

```
##    Mode   FALSE    TRUE 
## logical   44686   15845
```

Is it only some programs that have duplication? 9/10 programs do.


```r
length(unique(CEDENMod_Tox$Program)) # 10
```

```
## [1] 11
```

```r
unique(CEDENMod_Tox$Program[CEDENMod_Tox$DupCheck == "TRUE"])
```

```
##  [1] "SF Bay Regional Monitoring for Water Quality"
##  [2] "BASMAA Regional Monitoring Coalition"        
##  [3] "Irrigated Lands Regulatory Program"          
##  [4] "Surface Water Ambient Monitoring Program"    
##  [5] "Delta Monitoring RWQCB5"                     
##  [6] "Sacramento-San Joaquin River Delta Data"     
##  [7] "SF Bay STLS Monitoring"                      
##  [8] "Delta Regional Monitoring Program"           
##  [9] "CA Department of Transportation NPDES Permit"
## [10] "State Water Contractors Science Program"
```

#### Alternative (looser) assessment of duplication

If we assume that some columns may differ due to differences in data loading, then we may want to use a looser structure to detect duplicates. 

By assuming that records in the same location on the same date,  measuring the same analyte via the same collection method, and obtaining the same result are duplicates, we find almost 50% more duplicates. 
25729 records, to be specific. 

That is 42.5054931% of the dataset.


```r
NoDup_Tox<- distinct(CEDENMod_Tox, Date, StationName, Analyte, CollectionMethod, Result, .keep_all= TRUE)

nrow(CEDENMod_Tox) - nrow(NoDup_Tox)
```

```
## [1] 23518
```

Since we are using the df for the water parameters and not the associated organism survival, we can remove the organism column, and also remove records that assess biotic parameters like 'survival' and 'biomass'.


```r
NoDup_Tox <- NoDup_Tox %>% filter(Analyte != "Survival") %>%
  filter(Analyte != "Biomass (wt/orig indiv)") %>%
  filter(Analyte != "Young/female") %>%
  filter(Analyte != "Total Cell Count") %>%
  select(-OrganismName)
```

We're left with only 29695 unique, useful records in the tox dataset - or 49.1% of the original tox data remaining

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
## logical   59930     499
```

This method identified only 657 exact duplicates in the entire WQ dataset.

Only 9/17 programs present these exact duplicates


```r
length(unique(CEDENMod_WQ$Program)) # 17
```

```
## [1] 18
```

```r
unique(CEDENMod_WQ$Program[CEDENMod_WQ$DupCheck == "TRUE"])
```

```
## [1] "SF Bay Regional Monitoring for Water Quality"
## [2] "Suisun Bay Monitoring Project"               
## [3] "Delta Monitoring RWQCB5"                     
## [4] "Sacramento-San Joaquin River Delta Data"     
## [5] "Surface Water Ambient Monitoring Program"    
## [6] "SF Bay STLS Monitoring"                      
## [7] "CA Department of Transportation NPDES Permit"
## [8] "Irrigated Lands Regulatory Program"          
## [9] "American Rivers Restoration"
```

#### Looser assessment of Duplication

Utilizing the distinct() function to assume that records in the same location on the same date, measuring the same analyte via the same collection method and obtaining the same result are duplicates, we find 1536 duplicate records.

That is more than double the number of exact duplicates found, yet still only 2.5% of the entire WQ dataset.


```r
# Remove duplicate rows of the dataframe using multiple variables

NoDupWQ <- distinct(CEDENMod_WQ, Date, Analyte, StationName, CollectionMethod, Result, .keep_all= TRUE)

nrow(CEDENMod_WQ) - nrow(NoDupWQ)
```

```
## [1] 1336
```
<br>

<br>

## Merging CEDEN data

After dealing with duplication WITHIN the CEDEN tox and wq datasets, there were only 9 duplicate records found following the merged data. (75 if Collection Method is not a requirement for establishing duplication)


```r
# Strip column with duplicate check from both, and organism from tox
NoDup_Tox <- select(NoDup_Tox, -c(DupCheck))
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
# tibble(SURF = names(NoDup_Tox), CEDEN = names(NoDupWQ))

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
## [1] 386
```
Restricting the duplicates of concern to those which are non-zero values... It appears that there are 6 different collection methods represented in the duplicated data.

Sleuthing what is going on:

1. Many are duplication within a parent project, with one entry having sample method marked "not recorded" and the other defined. While the entire CEDEN dataset has 305 samples labeled "not recorded", 153 of these are likely duplicated data.

**Question** Is it worth simply removing all records for which the sample method is not recorded?

2. Two projects hold the majority of the duplication: CA Dept of Transit NPDES Monitoring 2014-2015 (153), and San Joaquin County Delta Water Quality Coalition (105)

3. Duplication within the Regional Monitoring Program - Status and Trends deserves closer investigation. Those samples have identical, >0 results for samples labeled with:

+ Collection_Method = Sediment_Core & Matrix = sediment
+ Collection_Method = Sediment_Grab & Matrix = overlyingwater

**--> Question** Does it make sense to have a sample defined as a sediment grab method + matrix of overlying water? Does it make sense for these samples to have identical results?


```r
# Find columns retained only when restricting Collection Method also

DIF<- setdiff(CEDEN_ALL_DupChecked, CEDEN_ALL_Check1) # gives items in A that are not in B


# Restrict to non-zero results (concern for duplication)

DIF <- DIF %>% filter(., Result != "0") 
nrow(DIF)
```

```
## [1] 356
```

```r
# Summary of collection methods which differentiate these duplicates. 

DIF$CollectionMethod <- as.factor(DIF$CollectionMethod)
summary(DIF$CollectionMethod)
```

```
## Auto sampler automatically triggered      Auto sampler manually triggered 
##                                   17                                   26 
##                         Not Recorded                        Sediment_Core 
##                                   76                                   11 
##                        Sediment_Grab                           Water_Grab 
##                                   93                                  133
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
##                                               40 
## San Joaquin County Delta Water Quality Coalition 
##                                              111 
##                                 SWAMP Monitoring 
##                                                4 
##                            SWAMP RWB5 Monitoring 
##                                                2 
##                    SWAMP Stream Pollution Trends 
##                                                1
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
##                               Analyte CollectionMethod       Date Datum
##   1:         Benz(a)anthracene, Total     Not Recorded 2014-02-26 NAD83
##   2:         Benz(a)anthracene, Total       Water_Grab 2014-02-26 NAD83
##   3:         OilandGrease, HEM, Total     Not Recorded 2014-02-26 NAD83
##   4:         OilandGrease, HEM, Total       Water_Grab 2014-02-26 NAD83
##   5:     Dibenz(a,h)anthracene, Total     Not Recorded 2014-02-26 NAD83
##  ---                                                                   
## 682:            Ammonia as NH3, Total       Water_Grab 2019-04-18 NAD83
## 683: Ammonia as NH3, Unionized, Total       Water_Grab 2019-04-18 NAD83
## 684: Ammonia as NH3, Unionized, Total    Sediment_Grab 2019-04-18 NAD83
## 685: Ammonia as NH3, Unionized, Total       Water_Grab 2019-05-23 NAD83
## 686: Ammonia as NH3, Unionized, Total    Sediment_Grab 2019-05-23 NAD83
##                            geometry Latitude LocationCode Longitude  MatrixName
##   1: c(-121.49340057373, 38.456501)  38.4565 Not Recorded -121.4934 samplewater
##   2: c(-121.49340057373, 38.456501)  38.4565 Not Recorded -121.4934 samplewater
##   3: c(-121.49340057373, 38.456501)  38.4565 Not Recorded -121.4934 samplewater
##   4: c(-121.49340057373, 38.456501)  38.4565 Not Recorded -121.4934 samplewater
##   5: c(-121.49340057373, 38.456501)  38.4565 Not Recorded -121.4934 samplewater
##  ---                                                                           
## 682:            c(-121.794, 38.307)  38.3070   Midchannel -121.7940 samplewater
## 683:            c(-121.794, 38.307)  38.3070   Midchannel -121.7940 samplewater
## 684:            c(-121.794, 38.307)  38.3070         Bank -121.7940    sediment
## 685:           c(-121.693, 38.3068)  38.3068         Bank -121.6930 samplewater
## 686:           c(-121.693, 38.3068)  38.3068         Bank -121.6930    sediment
##        MDL                                 ParentProject
##   1: 0.028 CA Dept of Transit NPDES Monitoring 2014-2015
##   2: 0.028 CA Dept of Transit NPDES Monitoring 2014-2015
##   3: 1.800 CA Dept of Transit NPDES Monitoring 2014-2015
##   4: 1.800 CA Dept of Transit NPDES Monitoring 2014-2015
##   5: 0.028 CA Dept of Transit NPDES Monitoring 2014-2015
##  ---                                                    
## 682:    NA     Sacramento Valley Water Quality Coalition
## 683:    NA     Sacramento Valley Water Quality Coalition
## 684:    NA     Sacramento Valley Water Quality Coalition
## 685:    NA     Sacramento Valley Water Quality Coalition
## 686:    NA     Sacramento Valley Water Quality Coalition
##                                           Program
##   1: CA Department of Transportation NPDES Permit
##   2: CA Department of Transportation NPDES Permit
##   3: CA Department of Transportation NPDES Permit
##   4: CA Department of Transportation NPDES Permit
##   5: CA Department of Transportation NPDES Permit
##  ---                                             
## 682:           Irrigated Lands Regulatory Program
## 683:           Irrigated Lands Regulatory Program
## 684:           Irrigated Lands Regulatory Program
## 685:           Irrigated Lands Regulatory Program
## 686:           Irrigated Lands Regulatory Program
##                                              Project rb_number regional_board
##   1:   CA Dept of Transit NPDES Monitoring 2013-2014         5 Central Valley
##   2:   CA Dept of Transit NPDES Monitoring 2013-2014         5 Central Valley
##   3:   CA Dept of Transit NPDES Monitoring 2013-2014         5 Central Valley
##   4:   CA Dept of Transit NPDES Monitoring 2013-2014         5 Central Valley
##   5:   CA Dept of Transit NPDES Monitoring 2013-2014         5 Central Valley
##  ---                                                                         
## 682: SVC, Order R5-2014-0030, Dec 1st 2019, WY19, Q3         5 Central Valley
## 683: SVC, Order R5-2014-0030, Dec 1st 2019, WY19, Q3         5 Central Valley
## 684: SVC, Order R5-2014-0030, Dec 1st 2019, WY19, Q3         5 Central Valley
## 685: SVC, Order R5-2014-0030, Dec 1st 2019, WY19, Q3         5 Central Valley
## 686: SVC, Order R5-2014-0030, Dec 1st 2019, WY19, Q3         5 Central Valley
##       Result   RL Source StationCode                          StationName
##   1:   0.028 0.07  CEDEN       3-217             28-ft RVTS Strip Station
##   2:   0.028 0.07  CEDEN       3-217             28-ft RVTS Strip Station
##   3:   2.400 6.50  CEDEN       3-217             28-ft RVTS Strip Station
##   4:   2.400 6.50  CEDEN       3-217             28-ft RVTS Strip Station
##   5:   0.028 0.07  CEDEN       3-217             28-ft RVTS Strip Station
##  ---                                                                     
## 682: -88.000   NA  CEDEN   511ULCABR           Ulatis Creek at Brown Road
## 683: -88.000   NA  CEDEN   511ULCABR           Ulatis Creek at Brown Road
## 684: -88.000   NA  CEDEN   511ULCABR           Ulatis Creek at Brown Road
## 685: -88.000   NA  CEDEN   511XSSLIB Shag Slough at Liberty Island Bridge
## 686: -88.000   NA  CEDEN   511XSSLIB Shag Slough at Liberty Island Bridge
##             Subregion Unit
##   1: Sacramento River ug/L
##   2: Sacramento River ug/L
##   3: Sacramento River mg/L
##   4: Sacramento River mg/L
##   5: Sacramento River ug/L
##  ---                      
## 682:      North Delta mg/L
## 683:      North Delta mg/L
## 684:      North Delta mg/L
## 685:      North Delta mg/L
## 686:      North Delta mg/L
```

```r
# How does number of "not recorded" methods in these duplications compare to those in the entire df?

paste("Collection Method = Not Recorded in...")
```

```
## [1] "Collection Method = Not Recorded in..."
```

```r
tibble(Entire = nrow(CEDEN_ALL_DupChecked[CEDEN_ALL_DupChecked$CollectionMethod== "Not Recorded"]), Dup = nrow(Example[Example$CollectionMethod== "Not Recorded"]))
```

```
## # A tibble: 1 x 2
##   Entire   Dup
##    <int> <int>
## 1    260   153
```

### Fix CEDEN nomenclature

**Split Analyte Column**

Because of formatting differences between the amount of data recorded under "Analyte" in CEDEN compared to the "Chemical_name" in SURF (which will be renamed Analyte), we opted to split the data in CEDEN's analyte column into two columns: 

Analyte (Chemical Name), and Analyte_Type (ie: total or particulate)

Using separate() to split the column and requiring the separation to be a comma and space ", " seems to have worked well, except for one name which appears to be empty

**Convert to lower-case**

SURF chemicals are all lowercase. We can make all letters in the CEDEN data lowercase using tolower() so that they will be compatible.


```r
# Remove subset of data determined to be >50% duplication

CEDEN_ALL_DupChecked <- CEDEN_ALL_DupChecked %>% filter(CollectionMethod != "Not Recorded")

# Split Analyte column

CEDEN_ALL_DupChecked <- CEDEN_ALL_DupChecked %>%
  separate(Analyte, into = c("Analyte", "Analyte_type"), sep = ", " , extra = "merge")
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 19377 rows [54,
## 55, 57, 58, 101, 103, 104, 106, 140, 144, 145, 146, 673, 678, 681, 682, 2657,
## 2658, 2660, 2664, ...].
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
## [3] "2-ethylhexyl-diphenyl phosphate"      
## [4] "2,4,6-tribromophenyl allyl ether"     
## [5] "acenaphthene"                         
## [6] "acenaphthenes"
```

```r
# Looks like requiring the separation from extra to analyte to contain a comma and a space allowed the full names to be retained. Without that, the separation led to an analyte "1" which should have been 1,2-bis(2,4,6- tribromophenoxy)ethane, etc.
```

This simplification of Analyte name would lead to 15,290 more records being considered "duplication" using our current method, mostly (14,776) containing a zero-result. Because these differences in analytes are not retained in the SURF dataset, it makes sense to condense them (remove duplicates) prior to merging the databases.


```r
Check <- distinct(CEDEN_ALL_DupChecked, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(CEDEN_ALL_DupChecked) - nrow(Check)

DIF<- setdiff(CEDEN_ALL_DupChecked, Check)
length(DIF$Result[DIF$Result == "0"])
```

Removal of these simplified duplicates also eliminates the utility of retaining the "Analyte Type" column. For example, a reading of Analyte X with Type = Total Type = Suspended have the exact same result. Removing duplicates would keep the first record (Analyte = X, Type = Total) and remove the second (Analyte X, Type = S). In the dataframe, if you are trying to reason backwards to the meaning of that remaining meaning (Analyte X, Type = Total), you're missing the other half of the story (Type = Sus too). So, to avoid improper interpretation of this dataframe, Analyte Type should be removed. 


```r
CEDEN_ALL_DupChecked <- distinct(CEDEN_ALL_DupChecked, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE) %>%
  select(-Analyte_type)

# 127,874
```

### CEDEN merge result

Using these QA/QC methods, 87794 unique records are available through the CEDEN datasets. 

<br>

<br>


## SURF data {.tabset}

There are 91021 records in the WQ dataset
and 35346 in the SED dataset. 

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
## logical   91021
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
## [1] 15333
```
<br>

<br>

### SURF sediment

#### Data Prep

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
## logical   35346
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
## [1] 4080
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
# tibble(SURF = names(NoDupSED), CEDEN = names(NoDupWQ))

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
## [1] 1598
```

```r
unique(DIF$Result) # All unique "result" values within the duplicates that were removed.
```

```
##  [1] 0.0000 0.0036 0.0160 0.0100 0.0140 0.0250 0.0130 0.0032 0.0270 0.0430
## [11] 0.0024 0.0029 0.1500 0.0470
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
##  1:                      ADH Environmental       tetramethrin
##  2: San Francisco Estuary Institute (SFEI)       tetramethrin
##  3:                      ADH Environmental   fipronil sulfone
##  4: San Francisco Estuary Institute (SFEI)   fipronil sulfone
##  5:                      ADH Environmental           fipronil
##  6: San Francisco Estuary Institute (SFEI)           fipronil
##  7:                      ADH Environmental   fipronil sulfone
##  8: San Francisco Estuary Institute (SFEI)   fipronil sulfone
##  9:                      ADH Environmental         cyfluthrin
## 10: San Francisco Estuary Institute (SFEI)         cyfluthrin
## 11:                      ADH Environmental   fipronil sulfone
## 12: San Francisco Estuary Institute (SFEI)   fipronil sulfone
## 13:                      ADH Environmental lambda cyhalothrin
## 14: San Francisco Estuary Institute (SFEI) lambda cyhalothrin
## 15:                      ADH Environmental         bifenthrin
## 16: San Francisco Estuary Institute (SFEI)         bifenthrin
## 17:                      ADH Environmental         bifenthrin
## 18: San Francisco Estuary Institute (SFEI)         bifenthrin
## 19:                      ADH Environmental         cyfluthrin
## 20: San Francisco Estuary Institute (SFEI)         cyfluthrin
## 21:                      ADH Environmental         cyfluthrin
## 22: San Francisco Estuary Institute (SFEI)         cyfluthrin
## 23:                      ADH Environmental           fipronil
## 24: San Francisco Estuary Institute (SFEI)           fipronil
## 25:                      ADH Environmental           fipronil
## 26: San Francisco Estuary Institute (SFEI)           fipronil
## 27:                      ADH Environmental   fipronil sulfone
## 28: San Francisco Estuary Institute (SFEI)   fipronil sulfone
## 29:                      ADH Environmental         bifenthrin
## 30: San Francisco Estuary Institute (SFEI)         bifenthrin
## 31:                      ADH Environmental         cyfluthrin
## 32: San Francisco Estuary Institute (SFEI)         cyfluthrin
## 33:                      ADH Environmental           fipronil
## 34: San Francisco Estuary Institute (SFEI)           fipronil
## 35:                      ADH Environmental         bifenthrin
## 36: San Francisco Estuary Institute (SFEI)         bifenthrin
##                                     Agency            Analyte
##              CollectionMethod       County Data.source       Date
##  1: Single whole water sample Contra Costa       CEDEN 2013-04-05
##  2:                           Contra Costa       CEDEN 2013-04-05
##  3:       Sample-type unknown Contra Costa       CEDEN 2012-12-01
##  4:                           Contra Costa       CEDEN 2012-12-01
##  5:       Sample-type unknown Contra Costa       CEDEN 2012-11-29
##  6:                           Contra Costa       CEDEN 2012-11-29
##  7:       Sample-type unknown Contra Costa       CEDEN 2012-11-29
##  8:                           Contra Costa       CEDEN 2012-11-29
##  9: Single whole water sample Contra Costa       CEDEN 2012-12-01
## 10:                           Contra Costa       CEDEN 2012-12-01
## 11:       Sample-type unknown Contra Costa       CEDEN 2012-12-22
## 12:                           Contra Costa       CEDEN 2012-12-22
## 13: Single whole water sample Contra Costa       CEDEN 2012-12-01
## 14:                           Contra Costa       CEDEN 2012-12-01
## 15: Single whole water sample Contra Costa       CEDEN 2013-04-05
## 16:                           Contra Costa       CEDEN 2013-04-05
## 17: Single whole water sample Contra Costa       CEDEN 2012-12-22
## 18:                           Contra Costa       CEDEN 2012-12-22
## 19: Single whole water sample Contra Costa       CEDEN 2013-04-05
## 20:                           Contra Costa       CEDEN 2013-04-05
## 21: Single whole water sample Contra Costa       CEDEN 2012-11-29
## 22:                           Contra Costa       CEDEN 2012-11-29
## 23:       Sample-type unknown Contra Costa       CEDEN 2012-12-01
## 24:                           Contra Costa       CEDEN 2012-12-01
## 25:       Sample-type unknown Contra Costa       CEDEN 2012-12-22
## 26:                           Contra Costa       CEDEN 2012-12-22
## 27:       Sample-type unknown Contra Costa       CEDEN 2013-04-05
## 28:                           Contra Costa       CEDEN 2013-04-05
## 29: Single whole water sample Contra Costa       CEDEN 2012-12-01
## 30:                           Contra Costa       CEDEN 2012-12-01
## 31: Single whole water sample Contra Costa       CEDEN 2012-12-22
## 32:                           Contra Costa       CEDEN 2012-12-22
## 33:       Sample-type unknown Contra Costa       CEDEN 2013-04-05
## 34:                           Contra Costa       CEDEN 2013-04-05
## 35: Single whole water sample Contra Costa       CEDEN 2012-11-29
## 36:                           Contra Costa       CEDEN 2012-11-29
##              CollectionMethod       County Data.source       Date
##                   geometry Latitude Longitude    LOQ       MDL Record_id Result
##  1: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0001324   1105972 0.0036
##  2: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0001324    464883 0.0036
##  3: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000   1206612 0.0160
##  4: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000    472857 0.0160
##  5: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000   1206615 0.0100
##  6: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000    478238 0.0100
##  7: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000   1206611 0.0140
##  8: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000    465500 0.0140
##  9: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0002000   1105938 0.0250
## 10: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0002000    470161 0.0250
## 11: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000   1206613 0.0130
## 12: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000    479675 0.0130
## 13: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0002000   1105942 0.0032
## 14: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0002000    470985 0.0032
## 15: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0000662   1105936 0.0270
## 16: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0000662    470154 0.0270
## 17: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0017 0.0003000   1105935 0.0430
## 18: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0017 0.0003000    470960 0.0430
## 19: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0001324   1105940 0.0024
## 20: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0001324    469790 0.0024
## 21: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0002000   1105937 0.0029
## 22: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0002000    469794 0.0029
## 23: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000   1206616 0.0130
## 24: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000    479007 0.0130
## 25: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000   1206617 0.0100
## 26: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000    479721 0.0100
## 27: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000   1206614 0.0140
## 28: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000    465543 0.0140
## 29: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0001000   1105934 0.1500
## 30: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0001000    470522 0.1500
## 31: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0017 0.0007000   1105939 0.0024
## 32: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0017 0.0007000    470532 0.0024
## 33: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000   1206618 0.0100
## 34: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0020 0.0005000    465162 0.0100
## 35: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0001000   1105933 0.0470
## 36: c(-121.7083, 37.99151) 37.99151 -121.7083 0.0015 0.0001000    464882 0.0470
##                   geometry Latitude Longitude    LOQ       MDL Record_id Result
##     Source StationCode                                              StationName
##  1:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
##  2:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
##  3:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
##  4:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
##  5:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
##  6:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
##  7:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
##  8:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
##  9:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 10:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 11:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 12:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 13:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 14:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 15:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 16:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 17:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 18:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 19:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 20:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 21:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 22:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 23:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 24:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 25:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 26:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 27:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 28:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 29:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 30:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 31:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 32:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 33:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 34:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 35:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
## 36:   SURF        07_4 Marsh Creek at Cypress Rd bridge (trib to western Delta)
##     Source StationCode                                              StationName
##     Study_cd
##  1:      523
##  2:      305
##  3:      523
##  4:      305
##  5:      523
##  6:      305
##  7:      523
##  8:      305
##  9:      523
## 10:      305
## 11:      523
## 12:      305
## 13:      523
## 14:      305
## 15:      523
## 16:      305
## 17:      523
## 18:      305
## 19:      523
## 20:      305
## 21:      523
## 22:      305
## 23:      523
## 24:      305
## 25:      523
## 26:      305
## 27:      523
## 28:      305
## 29:      523
## 30:      305
## 31:      523
## 32:      305
## 33:      523
## 34:      305
## 35:      523
## 36:      305
##     Study_cd
##                                                                                                     Study_description
##  1:                         STLS Monitoring WY2013 , SF Bay STLS Monitoring , STLS Monitoring Contra Costa CWP WY2013
##  2: SFEI River Loading Study 2013 , SFEI Stormwater Monitoring Study , 2013 Tributary Loading Study (CEDEN May, 2015)
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
##                                                                                                     Study_description
##             Study_weblink  Subregion Total organic carbon (%) Unit
##  1: http://www.ceden.org/ Confluence                       NA  ppb
##  2: http://www.ceden.org/ Confluence                       NA  ppb
##  3: http://www.ceden.org/ Confluence                       NA  ppb
##  4: http://www.ceden.org/ Confluence                       NA  ppb
##  5: http://www.ceden.org/ Confluence                       NA  ppb
##  6: http://www.ceden.org/ Confluence                       NA  ppb
##  7: http://www.ceden.org/ Confluence                       NA  ppb
##  8: http://www.ceden.org/ Confluence                       NA  ppb
##  9: http://www.ceden.org/ Confluence                       NA  ppb
## 10: http://www.ceden.org/ Confluence                       NA  ppb
## 11: http://www.ceden.org/ Confluence                       NA  ppb
## 12: http://www.ceden.org/ Confluence                       NA  ppb
## 13: http://www.ceden.org/ Confluence                       NA  ppb
## 14: http://www.ceden.org/ Confluence                       NA  ppb
## 15: http://www.ceden.org/ Confluence                       NA  ppb
## 16: http://www.ceden.org/ Confluence                       NA  ppb
## 17: http://www.ceden.org/ Confluence                       NA  ppb
## 18: http://www.ceden.org/ Confluence                       NA  ppb
## 19: http://www.ceden.org/ Confluence                       NA  ppb
## 20: http://www.ceden.org/ Confluence                       NA  ppb
## 21: http://www.ceden.org/ Confluence                       NA  ppb
## 22: http://www.ceden.org/ Confluence                       NA  ppb
## 23: http://www.ceden.org/ Confluence                       NA  ppb
## 24: http://www.ceden.org/ Confluence                       NA  ppb
## 25: http://www.ceden.org/ Confluence                       NA  ppb
## 26: http://www.ceden.org/ Confluence                       NA  ppb
## 27: http://www.ceden.org/ Confluence                       NA  ppb
## 28: http://www.ceden.org/ Confluence                       NA  ppb
## 29: http://www.ceden.org/ Confluence                       NA  ppb
## 30: http://www.ceden.org/ Confluence                       NA  ppb
## 31: http://www.ceden.org/ Confluence                       NA  ppb
## 32: http://www.ceden.org/ Confluence                       NA  ppb
## 33: http://www.ceden.org/ Confluence                       NA  ppb
## 34: http://www.ceden.org/ Confluence                       NA  ppb
## 35: http://www.ceden.org/ Confluence                       NA  ppb
## 36: http://www.ceden.org/ Confluence                       NA  ppb
##             Study_weblink  Subregion Total organic carbon (%) Unit
```


```r
unique(Example$Data.source)
```

```
## [1] "CEDEN"
```

```r
unique(Example$Study_cd)
```

```
## [1] 523 305
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
## [1] 1047
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
## [1] 106890
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
# tibble(SURF = names(SURF_ALL_DupChecked), CEDEN = names(CEDEN_ALL_DupChecked))

# MERGE
CEDENSURF <- rbind(SURF_ALL_DupChecked, CEDEN_ALL_DupChecked)
```

### Check for duplicates {.tabset}

Retaining the 45,064 records of CEDEN data in SURF prior to merge, this method removes 15,290.

This means 2.9774\times 10^{4} records of SURF's CEDEN-sourced data are retained. Some may be unique, and some may be additional duplicates that we are not catching. 


```r
# How many SURF records are sourced from CEDEN?
nrow(filter(CEDENSURF, Data.source == "CEDEN"))
```

```
## [1] 28100
```

```r
# Remove duplicate rows of the dataframe using multiple variables

CEDENSURF_DupChecked <- distinct(CEDENSURF, Date, Analyte, CollectionMethod, StationName, Result, .keep_all= TRUE)

nrow(CEDENSURF)-nrow(CEDENSURF_DupChecked)
```

```
## [1] 0
```
Causes of these records being retained include:

A. No match actually exists in CEDEN. Record SHOULD be retained.

B. CEDEN and SURF have different naming protocols - both Station Name and Station Code differ for the same sites.

C. Latitude and Longitude are rounded differently between the databases.

#### Examples of A

1. There is a measure of endosulfan sulfate at Grizzly Bay in the SURF dataset, but a list of ALL analytes measured at Grizzley Bay in the CEDEN set reveals none including "endosulfan") 


```r
# Subset to preview CEDEN Data in the SURF set

head(filter(CEDENSURF, Data.source == "CEDEN"))
```

```
##                                                                                     Agency
## 1: USGS Pesticide Fate Research Group (PFRG), Organic Chemistry Research Laboratory (OCRL)
## 2: USGS Pesticide Fate Research Group (PFRG), Organic Chemistry Research Laboratory (OCRL)
## 3:                                                    USGS California Water Science Center
## 4:                                                    USGS California Water Science Center
## 5:                                                    USGS California Water Science Center
## 6:                                                    USGS California Water Science Center
##                Analyte      CollectionMethod       County Data.source
## 1:          fenhexamid Filtered water sample  San Joaquin       CEDEN
## 2: chlorantraniliprole Filtered water sample       Solano       CEDEN
## 3:              diuron Filtered water sample   Sacramento       CEDEN
## 4:  piperonyl butoxide Filtered water sample   Sacramento       CEDEN
## 5:             linuron Filtered water sample       Solano       CEDEN
## 6:           malathion Filtered water sample Contra Costa       CEDEN
##          Date Datum                   geometry Latitude LocationCode Longitude
## 1: 2015-08-18  <NA> c(-121.4188919, 38.236111) 38.23611         <NA> -121.4189
## 2: 2015-11-10  <NA> c(-121.7941971, 38.306999) 38.30700         <NA> -121.7942
## 3: 2012-03-27  <NA>    c(-121.82051, 38.06242) 38.06242         <NA> -121.8205
## 4: 2013-04-04  <NA>    c(-121.50134, 38.45602) 38.45602         <NA> -121.5013
## 5: 2011-04-21  <NA>    c(-122.03972, 38.11708) 38.11708         <NA> -122.0397
## 6: 2011-04-18  <NA>    c(-121.92013, 38.04278) 38.04278         <NA> -121.9201
##       LOQ MatrixName       MDL ParentProject Program Project rb_number
## 1: 0.0076       <NA>    0.0076          <NA>    <NA>    <NA>        NA
## 2: 0.0040       <NA>    0.0040          <NA>    <NA>    <NA>        NA
## 3: 0.0032       <NA> -999.0000          <NA>    <NA>    <NA>        NA
## 4: 0.0023       <NA> -999.0000          <NA>    <NA>    <NA>        NA
## 5: 0.0043       <NA> -999.0000          <NA>    <NA>    <NA>        NA
## 6: 0.0037       <NA> -999.0000          <NA>    <NA>    <NA>        NA
##    Record_id regional_board Result RL Source StationCode
## 1:   1808445           <NA> 0.0000 NA   SURF       39_19
## 2:   1802814           <NA> 0.0000 NA   SURF        48_6
## 3:   1810745           <NA> 0.0301 NA   SURF       34_72
## 4:   1809789           <NA> 0.0000 NA   SURF        34_5
## 5:   1805592           <NA> 0.0000 NA   SURF       48_52
## 6:   1803940           <NA> 0.0000 NA   SURF      07_113
##                                                   StationName Study_cd
## 1:           Mokelumne River at New Hope Rd Bridge (in Delta)      364
## 2:                                 Ulatis Creek at Brown Road      364
## 3:  Sacramento River above Point Sacramento. CEDEN: 510SAC0D4      825
## 4:               Sacramento River at Freeport (USGS-11447650)      746
## 5: Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7      825
## 6:                                          Mallard Island-MI      825
##                                                                                                 Study_description
## 1: Delta RMP - Current Use Pesticides , Delta Regional Monitoring Program , 2015 Delta RMP Current Use Pesticides
## 2: Delta RMP - Current Use Pesticides , Delta Regional Monitoring Program , 2015 Delta RMP Current Use Pesticides
## 3: SuisunBayMonitoring _SFCWA_USGS , Suisun Bay Monitoring Project , USGS Suisun Bay Monitoring Project 2011-2012
## 4:    SFCWA FreeportVernalis , State and Federal Contractors Water Agency , SFCWA FreeportVernalis PEST 2012-2013
## 5: SuisunBayMonitoring _SFCWA_USGS , Suisun Bay Monitoring Project , USGS Suisun Bay Monitoring Project 2011-2012
## 6: SuisunBayMonitoring _SFCWA_USGS , Suisun Bay Monitoring Project , USGS Suisun Bay Monitoring Project 2011-2012
##                                                                                                         Study_weblink
## 1: http://www.waterboards.ca.gov/centralvalley/water_issues/delta_water_quality/delta_regional_monitoring/index.shtml
## 2: http://www.waterboards.ca.gov/centralvalley/water_issues/delta_water_quality/delta_regional_monitoring/index.shtml
## 3:                                                                                              http://www.ceden.org/
## 4:                                                                                              http://www.ceden.org/
## 5:                                                                                              http://www.ceden.org/
## 6:                                                                                              http://www.ceden.org/
##           Subregion Total organic carbon (%) Unit
## 1:    Central Delta                       NA  ppb
## 2:      North Delta                       NA  ppb
## 3:       Confluence                       NA  ppb
## 4: Sacramento River                       NA  ppb
## 5:       Suisun Bay                       NA  ppb
## 6:       Confluence                       NA  ppb
```

```r
# Locate similar location records in CEDEN and SURF using queries

# Record Analyte = "endosulfan sulfate", Agency = Applied Marine Sciences, Inc. California, Collection Method = Filtered water sample, Date = 1997-04-22, Station Name: "Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7", StationCode: "48_52"

# Locate similar location records in CEDEN and SURF using flexible queries

A <- CEDENSURF %>% filter(grepl('Grizzly', StationName)) %>%
  filter(grepl('Dolphin', StationName)) %>%
  filter(Source == "SURF")

B <- CEDENSURF %>% filter(grepl('Grizzly', StationName)) %>%
  filter(grepl('Dolphin', StationName)) %>%
  filter(Source == "CEDEN")

# Sort analytes alphabetically, then display chunk around where "endosulfan" should be
C<- sort(unique(B$Analyte))
C[15:30]
```

```
##  [1] "lead"                "manganese"           "mbas"               
##  [4] "mercury"             "nickel"              "nitrate as n"       
##  [7] "nitrite as n"        "nitrogen"            "orthophosphate as p"
## [10] "oxygen"              "ph"                  "phosphorus as p"    
## [13] "salinity"            "secchi depth"        "silicate as si"     
## [16] "silver"
```

2. 
SURF Name: "Toe Drain Nr Babel Slough Nr Freeport Ca"
SURF Code: "57_58"

Could not ID this location in CEDEN data, using grepl() to allow approximate naming. None with "Babel" in StationName


#### Examples of B

It appears that SURF has appended station names from CEDEN with extra info. For example:

SURF: "Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7"
SURF Code: "48_52"

CEDEN: "Grizzly Bay at Dolphin nr. Suisun Slough"
CEDEN Code: "207SNB0D7"

and 

SURF Name: "Sacramento River at Freeport (USGS-11447650)"
SURF Code: "34_5"

CEDEN Name: "Sacramento River at Freeport, CA"
CEDEN Code: 11447650


```r
rbind(A[1,], B[1,])
```

```
##                                  Agency Analyte      CollectionMethod County
## 1: USGS California Water Science Center linuron Filtered water sample Solano
## 2:                                 <NA>  oxygen          Field Method   <NA>
##    Data.source       Date Datum                       geometry Latitude
## 1:       CEDEN 2011-04-21  <NA>        c(-122.03972, 38.11708) 38.11708
## 2:        <NA> 2010-03-17 NAD83 c(-122.03971862793, 38.117081) 38.11708
##    LocationCode Longitude    LOQ  MatrixName  MDL
## 1:         <NA> -122.0397 0.0043        <NA> -999
## 2:    OpenWater -122.0397     NA samplewater   NA
##                         ParentProject                       Program
## 1:                               <NA>                          <NA>
## 2: Suisun Bay Monitoring Project RWB2 Suisun Bay Monitoring Project
##                                  Project rb_number Record_id    regional_board
## 1:                                  <NA>        NA   1805592              <NA>
## 2: RWB2 Suisun Bay Monitoring Study 2010         2        NA San Francisco Bay
##    Result RL Source StationCode
## 1:   0.00 NA   SURF       48_52
## 2:   9.16 NA  CEDEN   207SNB0D7
##                                                   StationName Study_cd
## 1: Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7      825
## 2:                   Grizzly Bay at Dolphin nr. Suisun Slough       NA
##                                                                                                 Study_description
## 1: SuisunBayMonitoring _SFCWA_USGS , Suisun Bay Monitoring Project , USGS Suisun Bay Monitoring Project 2011-2012
## 2:                                                                                                           <NA>
##            Study_weblink  Subregion Total organic carbon (%) Unit
## 1: http://www.ceden.org/ Suisun Bay                       NA  ppb
## 2:                  <NA> Suisun Bay                       NA mg/L
```

#### Example of C

Same station at Grizzly Bay; differences in coordinates listed in SURF (1998 and 2012), compared to CEDEN (2010)


```r
C <- CEDENSURF %>%
  filter(grepl('Grizzly', StationName)) %>%
  filter(grepl('Dolphin', StationName)) %>%
  filter(grepl('2012', Date)) %>%
  filter(Data.source == "CEDEN")

rbind(A[1,c(23, 25, 7:10,12)], C[1,c(23, 25, 7:10,12)], B[1,c(23, 25, 7:10,12)])
```

```
##    Source                                                StationName Datum
## 1:   SURF Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7  <NA>
## 2:   SURF Grizzly Bay at Dolphin nr. Suisun Slough. CEDEN: 207SNB0D7  <NA>
## 3:  CEDEN                   Grizzly Bay at Dolphin nr. Suisun Slough NAD83
##                          geometry Latitude LocationCode    LOQ
## 1:        c(-122.03972, 38.11708) 38.11708         <NA> 0.0043
## 2:        c(-122.03972, 38.11708) 38.11708         <NA> 0.0037
## 3: c(-122.03971862793, 38.117081) 38.11708    OpenWater     NA
```

# Next Steps

### 1. Decide whether to retain composite samples, or else how to remove

### 2. Correcting coordinates

Skyler is correcting differences in projection and coordinate data in both CEDEN and SURF

### 3. Naming conventions (analyte, station name, station code, etc)


### Wayne Q: Differences in duplication by year?{.tabset}

There is definitely a more streamlined way to do this, but for expediency... These tables contain the total count of **Exact** duplicates identified in each dataframe. Recall from above that there were more duplicates found usign the final method, but this will give us an initial picture. 

Because there were no exact duplicates for the SURF df, I have not compared their duplication by year.

#### CEDEN WQ

```r
CEDENMod_WQ$year <- format(as.Date(CEDENMod_WQ$Date, format="%d/%m/%Y"),"%Y")

aggregate(DupCheck~year,FUN=length,data=CEDENMod_WQ[CEDENMod_WQ$DupCheck=="TRUE",])
```

```
##    year DupCheck
## 1  2010       79
## 2  2011       41
## 3  2012       49
## 4  2013       21
## 5  2014      138
## 6  2015       40
## 7  2016        7
## 8  2017       84
## 9  2018       32
## 10 2019        8
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
## 4  2012      777
## 5  2013     1070
## 6  2014     1238
## 7  2015     2007
## 8  2016     2607
## 9  2017     1660
## 10 2018     1214
## 11 2019     1876
```
