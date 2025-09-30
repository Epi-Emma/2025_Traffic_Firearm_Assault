# 2025_Traffic_Firearm_Assault

This repository contains code used to assess potential relationships between vehicle traffic volume and the risk of firearm assault injuries using data from the City of Portland, OR. 

--------------
**The following files are included:**

- *tract_cross_sectional.R* - This file runs through the tract-level cross-sectional analysis looking at firearm injury risk by traffic volume within tracts at the same type and time of day, adjusting for temporal variables and clustering standard errors at the tract. 
- *tract_case_crossover.R* - This file runs through the case-only case-crossover analysis in which months with an injury are compared to adjacent months with no injury (assessesing the relationship to pre and post months separately).
- *Hazelwood_street_analysis.R* - This code repeats the analyses that were conducted at the tract level in the previous code files, but using street-level traffic and point-level firearm injury data within the Hazelwood neighborhood of Portland. 


--------------
**Data Availability:**

Firearm assault injury data are publicly available and can be found on the Portland Police Department’s website using the [Portland Shooting Incident Statistics Dashboard](https://www.portland.gov/police/open-data/shooting-incident-statistics). 

The traffic volume estimates were obtained through a Research Agreement with [StreetLight, Inc.](https://www.streetlightdata.com/), and cannot be shared.


___

**If you use this R code, please cite the following publication:**

[INSERT AFTER PUBLICATION]
