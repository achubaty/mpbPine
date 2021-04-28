defineModule(sim, list(
  name = "mpbPine",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "mpbPine.Rmd"),
  reqdPkgs = list("achubaty/amc@development", "data.table",
                  "PredictiveEcology/LandR@development",
                  "magrittr", "quickPlot", "raster", "reproducible", "sp", "spatialEco"),
  parameters = rbind(
    defineParameter("lowMemory", "logical", FALSE, NA, NA,
                    desc = "Should high memory-usage steps be skipped? Useful for running on laptops."),
    defineParameter("sppEquivCol", "character", "Boreal", NA, NA,
                    desc = "The column in sim$specieEquivalency data.table to use as a naming convention"),
    defineParameter("simplifyPines", "logical", TRUE, NA, NA,
                    desc = paste0("A test evaluating whether one of the pines represents > 90% of the abundance. If it ",
                    "does, and this parameter is TRUE, then the two pines will be collapsed into one species, ",
                    "labelled with the dominmant species name.")),
    defineParameter(".maxMemory", "numeric", 1e+9, NA, NA,
                    "Used to set the 'maxmemory' raster option. See '?rasterOptions'."),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    desc = "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    desc = "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    desc = "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    desc = "This describes the simulation time interval between save events"),
    defineParameter(".tempdir", "character", tempdir(), NA, NA,
                    "Temporary (scratch) directory to use for transient files (e.g., GIS intermediates)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    desc = "Should this entire module be run with caching activated?")
  ),
  inputObjects = bindrows(
    expectsInput("mpbGrowthDT", "data.table",
                 desc = "Current MPB attack map (number of red attacked trees).",
                 sourceURL = NA),
    expectsInput("pineMap", "RasterStack",
                 desc = "Percent cover maps by species (lodgepole and jack pine).",
                 sourceURL = paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                    "canada-forests-attributes_attributs-forests-canada/",
                                    "2001-attributes_attributs-2001/")),
    expectsInput("rasterToMatch", "RasterLayer",
                 desc = "if not supplied, will default to standAgeMap", # TODO: description needed
                 sourceURL = NA),
    expectsInput("sppColors", "character",
                 desc = paste("A named vector of colors to use for plotting.",
                              "The names must be in sim$speciesEquivalency[[sim$sppEquivCol]],",
                              "and should also contain a color for 'Mixed'"),
                 sourceURL = NA),
    expectsInput("sppEquiv", "data.table",
                 desc = "table of species equivalencies. See LandR::sppEquivalencies_CA.",
                 sourceURL = ""),
    expectsInput("standAgeMap", "RasterLayer",
                 desc = "stand age map in study area, default is Canada national stand age map",
                 sourceURL = paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                    "canada-forests-attributes_attributs-forests-canada/",
                                    "2001-attributes_attributs-2001/",
                                    "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif")),
    expectsInput("studyArea", "SpatialPolygons",
                 desc = "The study area to which all maps will be cropped and reprojected.",
                 sourceURL = NA), ## TODO: link to Google Drive
    expectsInput(NA, NA,
                 desc = "Additional documentation for kNN datasets.",
                 sourceURL = "http://tree.nfis.org/cjfr-2013-0401suppl.pdf"),
    expectsInput(NA, NA,
                 desc = "Additional documentation for kNN datasets.",
                 sourceURL = "http://tree.nfis.org/FR_NFI_and_kNN_Mapping_20160628.docx"),
    expectsInput(NA, NA,
                 desc = "Additional documentation for kNN datasets.",
                 sourceURL = "http://tree.nfis.org/NFI_and_kNN_Mapping_20160628.docx")
  ),
  outputObjects = bindrows(
    createsOutput("pineDT", "data.table", "Proportion cover etc. by species (lodgepole and jack pine)."),
    createsOutput("pineMap", "RasterLayer", "Percent cover maps by species (lodgepole and jack pine).")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.mpbPine <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      # do stuff for this event
      sim <- importMap(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "mpbPine", "plot", .last() - 1)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "mpbPine", "save", .last())
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      pineMap <- sum(sim$pineMap)
      Plot(pineMap, title = "Percent Pine")
      Plot(sim$studyArea, addTo = "pineMap", gp = gpar(col = "black", fill = 0), title = "")

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "mpbPine", "plot")
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  if (getOption("LandR.verbose", TRUE) > 0)
    message(currentModule(sim), ": using dataPath '", dPath, "'.")

  #mod$prj <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113",
  #                 "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  mod$prj <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

  ## load study area
  if (!suppliedElsewhere("studyArea")) {
    sim$studyArea <- amc::loadStudyArea(dataPath(sim), "studyArea.kml", mod$prj)
  }

  ## raster to match
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- Cache(
      LandR::prepInputsLCC,
      year = 2005,
      destinationPath = dPath,
      studyArea = sim$studyArea
    )
  }

  ## stand age map
  if (!suppliedElsewhere("standAgeMap", sim)) {
    sim$standAgeMap <- LandR::prepInputsStandAgeMap(
      startTime = 2010,
      ageUrl = na.omit(extractURL("standAgeMap")),
      destinationPath = dPath,
      studyArea = sim$studyArea,
      rasterToMatch = sim$rasterToMatch,
      userTags = c("stable", currentModule(sim)) ## TODO: does this need rasterToMatch? it IS rtm!
    )
    sim$standAgeMap[] <- asInteger(sim$standAgeMap[])
  }

  ## stand volumes
  # if (!suppliedElsewhere("standVolumeMap", sim)) {
  #   standVolMapFilename <- file.path(dPath, "NFI_MODIS250m_kNN_Structure_Volume_Total_v0.tif")
  #   sim$standVolMap <- Cache(prepInputs,
  #                            targetFile = basename(standVolMapFilename),
  #                            archive = asPath(c("kNN-StructureStandVolume.tar",
  #                                               "NFI_MODIS250m_kNN_Structure_Volume_Total_v0.zip")),
  #                            destinationPath = dPath,
  #                            url = na.omit(extractURL("standAgeMap")),
  #                            fun = "raster::raster",
  #                            studyArea = sim$studyAreaLarge,
  #                            rasterToMatch = sim$rasterToMatch,
  #                            method = "bilinear",
  #                            datatype = "INT2U",
  #                            filename2 = paste0(tools::file_path_sans_ext(basename(standVolMapFilename)), "_cropped"),
  #                            overwrite = TRUE,
  #                            userTags = c("stable", currentModule(sim)))
  #   sim$standVolMap[] <- asInteger(sim$standVolMap[])
  # }

  if (!suppliedElsewhere("sppEquiv", sim)) {
    data("sppEquivalencies_CA", package = "LandR", envir = environment())
    sim$sppEquiv <- as.data.table(sppEquivalencies_CA) %>%
      .[KNN %in% c("Pinu_Ban", "Pinu_Con", "Pinu_Con_Lat"), ] ## NOTE!
    ## Pinu_Con shouldn't be relevant, but it *is* in the areas of interest.
    ## Also, need 3 or more species as workaround for RColorBrewer palette (LandR#3)

    ## add default colors for species used in model
    if (!is.null(sim$sppColors))
      stop("If you provide sppColors, you MUST also provide sppEquiv")
    sim$sppColors <- sppColors(sim$sppEquiv, P(sim)$sppEquivCol, palette = "Accent")
  }

  ## percent pine layers
  if (!suppliedElsewhere(sim$pineMap)) {
    message("Checking for kNN-Species data layers...")
    sim$pineMap <- Cache(loadkNNSpeciesLayers,
                         dPath = dPath,
                         rasterToMatch = sim$rasterToMatch,
                         studyArea = sim$studyAreaLarge,
                         sppEquiv = sim$sppEquiv,
                         knnNamesCol = "KNN",
                         sppEquivCol = P(sim)$sppEquivCol,
                         url = na.omit(extractURL("pineMap")),
                         cachePath = cachePath(sim),
                         userTags = c(cacheTags, "speciesLayers"))
  }
  if (!P(sim)$lowMemory) {
    sim$pineMap[] <- sim$pineMap[]
  }

  if (nlayers(sim$pineMap)) {
    pinuTotal <- list()
    pinuTotal[[1]] <- sum(sim$pineMap[[sim$sppEquiv$KNN[1]]][], na.rm = T)
    pinuTotal[[2]] <- sum(sim$pineMap[[sim$sppEquiv$KNN[2]]][], na.rm = T)
    whGreater <- pinuTotal[[1]] < pinuTotal[[2]]
    ratioOfPines <- pinuTotal[[whGreater + 2]]/pinuTotal[[whGreater + 1]]
    if (ratioOfPines < 0.1 && !isFALSE(P(sim)$simplifyPines)) {

      message(sim$sppEquiv$KNN[[whGreater + 1]], " represents ", round((1 - ratioOfPines) * 100, 0),
              "% of the pine abundance; ",
              "collapsing all pine into 1 species (",sim$sppEquiv$KNN[[whGreater + 1]],"). ",
              " To prevent this, set simplifyPines parameter to FALSE")
    }
    sim$pineMap <- sum(sim$pineMap)
    names(sim$pineMap) <- sim$sppEquiv$KNN[[whGreater + 1]]
  }


  return(invisible(sim))
}

## event functions

importMap <- function(sim) {
  ## create data.table version
  pctPine <- if (nlayers(sim$pineMap) > 1) sum(sim$pineMap)[] else sim$pineMap[]
  sim$pineDT <- data.table(ID = 1L:ncell(sim$pineMap), ## TODO: use sppEquivNames
                           PROPPINE = pctPine / 100) # use proportion
  sim$pineDT[, NUMTREES := PROPPINE * 1125 * prod(res(sim$pineMap)) / 100^2]
  ## NOTE: 1125 is mean stems/ha for pine stands, per Whitehead & Russo (2005), Cooke & Carroll (2017)

  setkey(sim$pineDT, ID)
  sim$pineDT[is.na(PROPPINE), ':='(PROPPINE = 0.00, NUMTREES = 0.00)]

  return(invisible(sim))
}
