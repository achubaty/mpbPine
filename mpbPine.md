---
title: "mpbPine"
author:
  - "Alex M. Chubaty"
  - "Barry J. Cooke"
  - "Eliot J. B. McIntire"
date: "May 25, 2021"
output:
  html_document:
    keep_md: yes
    toc: true
    toc_depth: 3
    toc_float: true
bibliography:
  - bibliography.bib
editor_options:
  chunk_output_type: console
---



# Overview

Download and use proportion pine data, based on provincial forest inventory data and prepared for the updated MPB Risk Assessment [@Bleiker:2019].
These data cover most of the managed boreal forests in Alberta and Saskatchewan.

In the current model, we calculate the number of available jack pine and lodgepole pine trees from the proportion pine for each species, by multiplying by the average stem density of 1125 mature stems per hectare, which is typical for beetle-prone lodgepole pine stands [@Whitehead:2004rz; @Cooke:2017fem].

A more sophisticated estimate of pine stem density for each species is currently under development for future versions of this module.

# Usage


```r
library(SpaDES.core)

setPaths(modulePath = file.path(".."))
getPaths() # shows where the 4 relevant paths are

times <- list(start = 0, end = 10)

parameters <- list(
  #.progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)
modules <- list("mpbPine")
objects <- list()
inputs <- list()
outputs <- list()

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects)

mySimOut <- spades(mySim)
```

# Parameters

Provide a summary of user-visible parameters.


```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|paramName        |paramClass |default    |min |max |paramDesc                                                                                                                                                                                                              |
|:----------------|:----------|:----------|:---|:---|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|lowMemory        |logical    |FALSE      |NA  |NA  |Should high memory-usage steps be skipped? Useful for running on laptops.                                                                                                                                              |
|sppEquivCol      |character  |Boreal     |NA  |NA  |The column in sim$specieEquivalency data.table to use as a naming convention                                                                                                                                           |
|simplifyPines    |logical    |TRUE       |NA  |NA  |A test evaluating whether one of the pines represents > 90% of the abundance. If it does, and this parameter is TRUE, then the two pines will be collapsed into one species, labelled with the dominmant species name. |
|.maxMemory       |numeric    |1e+09      |NA  |NA  |Used to set the 'maxmemory' raster option. See '?rasterOptions'.                                                                                                                                                       |
|.plotInitialTime |numeric    |start(sim) |NA  |NA  |This describes the simulation time at which the first plot event should occur                                                                                                                                          |
|.plotInterval    |numeric    |NA         |NA  |NA  |This describes the simulation time interval between plot events                                                                                                                                                        |
|.saveInitialTime |numeric    |NA         |NA  |NA  |This describes the simulation time at which the first save event should occur                                                                                                                                          |
|.saveInterval    |numeric    |NA         |NA  |NA  |This describes the simulation time interval between save events                                                                                                                                                        |
|.tempdir         |character  |           |NA  |NA  |Temporary (scratch) directory to use for transient files (e.g., GIS intermediates).                                                                                                                                    |
|.useCache        |logical    |FALSE      |NA  |NA  |Should this entire module be run with caching activated?                                                                                                                                                               |

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("mpbPine", "..")` may be sufficient.


```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|objectName     |objectClass     |desc                                                                                                                                                      |sourceURL                                                                                                                                                                                   |
|:--------------|:---------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|mpbGrowthDT    |data.table      |Current MPB attack map (number of red attacked trees).                                                                                                    |NA                                                                                                                                                                                          |
|pineMap        |RasterStack     |Pine percent cover maps (lodgepole and jack pine).                                                                                                        |                                                                                                                                                                                            |
|rasterToMatch  |RasterLayer     |if not supplied, will default to standAgeMap                                                                                                              |NA                                                                                                                                                                                          |
|sppColors      |character       |A named vector of colors to use for plotting. The names must be in sim$speciesEquivalency[[sim$sppEquivCol]], and should also contain a color for 'Mixed' |NA                                                                                                                                                                                          |
|sppEquiv       |data.table      |table of species equivalencies. See LandR::sppEquivalencies_CA.                                                                                           |                                                                                                                                                                                            |
|standAgeMap    |RasterLayer     |stand age map in study area, default is Canada national stand age map                                                                                     |http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif |
|studyArea      |SpatialPolygons |The study area to which all maps will be cropped and reprojected.                                                                                         |NA                                                                                                                                                                                          |
|studyAreaLarge |SpatialPolygons |The larger study area to use for spread parameter estimation.                                                                                             |NA                                                                                                                                                                                          |

## Output data

Description of the module outputs.


```
## defineParameter: '.plotInitialTime' is not of specified type 'numeric'.
```



|objectName |objectClass |desc                                                        |
|:----------|:-----------|:-----------------------------------------------------------|
|pineDT     |data.table  |Proportion cover etc. by species (lodgepole and jack pine). |
|pineMap    |RasterLayer |Percent cover maps by species (lodgepole and jack pine).    |

# Links to other modules

Describe any anticipated linkages to other modules.
# References
