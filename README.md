# CEDENSURF-Mod

### Description
Code for integration of SURF and CEDEN datasets, plus requisite QA/QC processes, to create a
limited, combined data set of CEDEN and SURF WQ data that can be used for the Bayesian network analyses we have planned for the USFE project. 

The work is organized into a series of rmd's which have been clearly numbered in their titles. The output from each is used in subsequent analyses. 

Current work is continuing on 05_ToxUnits, awaiting collaborator's production of EC50 tables for each target species.

#### Table of Contents

1. [Issues](#issues)
2. [Usage](#usage)
4. [Spatial Metadata](#spatial-metadata)
5. [Source Information](#source-information)
6. [Required Environment.](#required-environment)

### Issues
For problems or suggestions, please open an [issue](https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/issues) or create a [pull request](https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/pulls).

### Usage
There are many intermediate datasets produced, and two final formats. 

**Final Output**
  - CEDENSURF_Limited_FixedUnits.csv: A limited dataset containing only records from CEDENSURFMod.csv measuring analytes relevant to the USFE BN-RRM conceptual model or Year 1 Report. Each analyte is categorized within a new column "SelectList", and converted so that each analyte is only measured in one unit (for that analyte). Long format.
  - **IN PRODUCTION:** CEDENSURF_ToxUnits_Wide: Transformed and summarized CEDENSURF data prepared for integration with the NETICA model. This limited dataset has had all units converted to molarity, then transformed to a relative toxicity for each species using EC50 values, and summarized by location and date.

Results from this dataset can be accessed usign the URL:

```R
# Example: This will read in the long-format dataset that has unified units within each analyte (though units differ between analytes)

library(data.table)

Limited <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/CEDENSURF_Limited_FixedUnits.csv")
```

**Intermediate Output**
  - SURFMod csv's:  Results of 01_SURF_Mod.rmd. File contains records from the SURF water (SURFMod_water.csv) or sediment dataset (SURFMOD_SED.csv) for the study area and timeframe, with modified columns available. Datum NAD83
  - CEDENSURFMod.csv: File containins water quality and tox data from both SURF and CEDEN. Data has undergone preliminary QA/QC
  - CEDENSURF_Limited_FixedUnits.csv: A limited dataset containing only records from CEDENSURFMod.csv measuring analytes relevant to the USFE BN-RRM conceptual model or Year 1 Report. Each analyte is categorized within a new column "SelectList", and converted so that each analyte is only measured in one unit (for that analyte).
  - Wide datasets: Contains results of all CEDENSURF_Limited_FixedUnits.csv data summarized by date and location into wide format. These were used in designing the first Netica Model, but will need to be reproduced using the converted data resulting from 05_ToxUnits.rmd.

### Spatial Metadata

All data utilized in these markdowns has been tranformed to NAD83 and is projected in [State Plane California III FIPS 0403](https://www.spatialreference.org/ref/esri/102643/).

### Source Information

Data sources are primarily intermediate output from within this repository. External data sources were used in [01_SURF_Mod](https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/blob/main/01_SURF_mod.md), where you can find the code used to access each.

- **SURF Data**: SURF Data was acquired at the DPR SURF database web page. Details in [01_SURF_Mod](https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/blob/main/01_SURF_mod.md)

- **CEDEN-mod Data**: Two modified tables from the [CEDEN-mod Repository](https://github.com/WWU-IETC-R-Collab/CEDEN-mod/tree/main)
  - [CEDENMod_Toxicity](https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_Toxicity.csv)
  - [CEDENMod_WQ](https://github.com/WWU-IETC-R-Collab/CEDEN-mod/blob/main/Data/Output/CEDENMod_WQ.csv)

- **Risk Region**: The same [USFE Risk Regions shapefile](https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/USFE_RiskRegions_9292020.zip) used in CEDEN-mod was used here as well. Its coordinate system is WGS84, and it was transformed to NAD83 prior to use.

### Required Environment

This code was produced and executed within the following environment:

R version 4.0.3 (2020-10-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)
