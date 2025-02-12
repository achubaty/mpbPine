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
  reqdPkgs = list("achubaty/amc@development", "data.table", "jimhester/archive",
                  "PredictiveEcology/LandR@development", "PredictiveEcology/mpbutils (>= 0.1.2)",
                  "magrittr", "quickPlot", "raster",
                  "PredictiveEcology/reproducible@development (>= 1.2.7.9001)", "sp", "spatialEco"),
  parameters = rbind(
    defineParameter("lowMemory", "logical", FALSE, NA, NA,
                    desc = "Should high memory-usage steps be skipped? Useful for running on laptops."),
    defineParameter("pineSpToUse", "character", c("Pinu_con" , "Pinu_ban"), NA, NA,
                    desc = "The 1 or 2 or 3 species to use as possible host for MPB. There are currently",
                    " no differentiation by species"),
    defineParameter("stemsPerHaAvg", "integer", 1125, NA, NA,
                    desc = "The average number of pine stems per ha in the study area. ",
                    "Taken from Whitehead & Russo (2005), Cooke & Carroll (2017)"),
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
    defineParameter(".tempdir", "character", NULL, NA, NA,
                    "Temporary (scratch) directory to use for transient files (e.g., GIS intermediates)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    desc = "Should this entire module be run with caching activated?")
  ),
  inputObjects = bindrows(
    expectsInput("mpbGrowthDT", "data.table",
                 desc = "Current MPB attack map (number of red attacked trees).",
                 sourceURL = NA),
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
                 sourceURL = NA),
    expectsInput("studyAreaLarge", "SpatialPolygons",
                 desc = "The larger study area to use for spread parameter estimation.", ## TODO: not used yet; will use with LandR
                 sourceURL = NA),
    # expectsInput(NA, NA,
    #              desc = "Additional documentation for kNN datasets.",
    #              sourceURL = "http://tree.nfis.org/cjfr-2013-0401suppl.pdf"),
    # expectsInput(NA, NA,
    #              desc = "Additional documentation for kNN datasets.",
    #              sourceURL = "http://tree.nfis.org/FR_NFI_and_kNN_Mapping_20160628.docx"),
    # expectsInput(NA, NA,
    #              desc = "Additional documentation for kNN datasets.",
    #              sourceURL = "http://tree.nfis.org/NFI_and_kNN_Mapping_20160628.docx")
  ),
  outputObjects = bindrows(
    createsOutput("pineDT", "data.table", "Proportion cover etc. by species (lodgepole and jack pine)."),
    createsOutput("pineMap", "SpatRaster", desc = "Pine percent cover maps (lodgepole and jack pine).")
    # createsOutput("pineMap", "RasterLayer", "Percent cover maps by species (lodgepole and jack pine).")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.mpbPine <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      cPath <- cachePath(sim)
      dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)

      # do stuff for this event

      ## percent pine layers
      url_AB <- "https://drive.google.com/file/d/15EzncjIR_dn5v6hruoVbsUQVF706nTEL/"
      opts <- options(reproducible.useTerra = TRUE)
      on.exit(options(opts), add = TRUE)

      dirForExtract <- file.path(inputPath(sim), "Pine_data_AB")

      AB <- prepInputs(url = url_AB,
                       targetFile = "AB_PineVolumes_Lambert.gdb",
                       rasterizeMask = rasterizeMask,
                       layers = "OVERSTOREY_PINE", destinationPath = dirForExtract,
                       rasterToMatch = sim$rasterToMatch,
                       useCache = FALSE,
                       fun = quote(rasterizeMask(targetFile, layer = layers, rasterToMatch = rasterToMatch))
      ) |> Cache()


      # AB <- Cache(prepInputs_ABPine, url = url_AB, rasterToMatch = sim$rasterToMatch,
      #             layers = "OVERSTOREY_PINE", destinationPath = inputPath(sim),
      #             maskWithRTM = TRUE, fun = "sf::st_as_sf")
      AB[] <- AB[] * 10

      url_SK <- "https://drive.google.com/file/d/1gpA9M4nhrnfIIvGQ7jcM9A7Fo-3MYpU1/"
      options(opts) # this next still not working with postProcessTerra (Eliot Feb 28, 2022)
      SK <- Cache(
prepInputs(url = url_SK,
                  targetFile = "SK_INV_JPpct10_Lambert.tif",
                  alsoExtract = "similar",
                  rastTimes10 = rastTimes10,
                  fun = quote(rastTimes10(targetFile)),
                  rasterToMatch = sim$rasterToMatch,
                  maskWithRTM = TRUE,
                  destinationPath = dPath,
                  cachePath = cPath)
)
      # SK[] <- SK[] * 10

      # In terra version 1.7.71, this mosaic returns an error:
      #   Error : [sprc] list elementis a: logical -- the work around is to use `raster::mosaic`

      sim$pineMap <- try(terra::mosaic(AB, SK, fun = "mean", na.rm = TRUE))
      if (is(sim$pineMap, "try-error")) {
        sim$pineMap <- {terra::rast(raster::mosaic(raster::raster(AB), raster::raster(SK),
                                                  fun = "mean", na.rm = TRUE))} |>
          Cache()
      }

      if (P(sim)$lowMemory) {
        sim$pineMap <- writeRaster(sim$pineMap, filename = file.path(inputPath(sim), "pineMap_AB_SK.tif"),
                                   overwrite = TRUE)
      } else {
        sim$pineMap[] <- sim$pineMap[]
      }

      if (nlyr(sim$pineMap) > 1) {
        browser() ## TODO: next lines are broken -- there will likely be more species than just pine
        if (any(grep("layer", names(sim$pineMap) )))
          names(sim$pineMap) <- sim$sppEquiv$KNN
        pinuTotal <- list()
        pinuTotal[[1]] <- sum(sim$pineMap[[sim$sppEquiv$KNN[1]]][], na.rm = TRUE)
        pinuTotal[[2]] <- sum(sim$pineMap[[sim$sppEquiv$KNN[2]]][], na.rm = TRUE)
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

      sim <- importMap(sim)
      numLayersInPM <- nlyr(sim$pineMap)
      pineSpeciesNames <- P(sim)$pineSpToUse
      if (!is.null(sim$sppEquiv)) {
        # check this again because it may not have existed in the .inputObjects
        if (numLayersInPM == 1) {
          colInSppEquiv <- equivalentNameColumn(pineSpeciesNames, sim$sppEquiv)
          sim$sppEquiv[get(colInSppEquiv) %in% pineSpeciesNames, MPB := "Pinu"]
        }
      }
      if (!is.null(sim$speciesLayers)) {
        pinesInSpeciesLayers <- equivalentName(pineSpeciesNames, sim$sppEquiv,
                                            equivalentNameColumn(names(sim$speciesLayers), sim$sppEquiv))
        numLayersInSL <- nlyr(sim$speciesLayers[[pinesInSpeciesLayers]])
        if (numLayersInSL != numLayersInPM) {
          if (numLayersInPM == 1 && numLayersInSL == 2) {
            message("Squashing the sim$speciesLayers pine layers into one species because sim$pineMaps has only one layer")
            whLayerMax <- which.max(apply(sim$speciesLayers[[pinesInSpeciesLayers]][], 2, sum, na.rm = TRUE))
            message("The pine layer will show up as ", pinesInSpeciesLayers[whLayerMax])
            whLayerDrop <- which(names(sim$speciesLayers) %in% pinesInSpeciesLayers[-whLayerMax])
            sim$speciesLayers <- raster::dropLayer(sim$speciesLayers, whLayerDrop)
            sim$speciesLayers[[pinesInSpeciesLayers[whLayerMax]]][] <- sim$pineMap[]
          }
        } else {
          browser() # This is not ready yet
        }
      }
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "mpbPine", "plot", .last() - 1)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "mpbPine", "save", .last())
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      pineMap <- if (terra::nlyr(sim$pineMap) > 1)
        sum(sim$pineMap) else sim$pineMap
      plotPineAndSA <- function(pine, sa, title) {
        aa <- terra::plot(pineMap, main = title)
        aa <- terra::plot(sim$studyArea, add = TRUE)
      }
      Plots(pineMap, title = "Percent Pine", sa = sim$studyArea, fn = plotPineAndSA)
      # Plots(sim$studyArea, addTo = "pineMap", gp = gpar(col = "black", fill = 0), title = "")

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
  cacheTags <- c(currentModule(sim))
  cPath <- cachePath(sim)
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  if (getOption("LandR.verbose", TRUE) > 0)
    message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # mod$targetCRS <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113",
  #                        "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  mod$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

  ## load study area
  if (!suppliedElsewhere("studyArea")) {
    sim$studyArea <- mpbStudyArea(ecoregions = c(112, 120, 122, 124, 126), mod$targetCRS,
                                  cPath, dPath)
  }

  ## raster to match
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- Cache(
      LandR::prepInputsLCC,
      year = 2005, ## TODO: use 2010
      destinationPath = dPath,
      studyArea = sf::as_Spatial(sim$studyArea)
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
      userTags = c("stable", currentModule(sim))
    )
    sim$standAgeMap[] <- asInteger(sim$standAgeMap[])
  }

  if (!(suppliedElsewhere("sppEquiv", sim) || suppliedElsewhere("sppNameVector", sim))) {
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

  return(invisible(sim))
}

## event functions

importMap <- function(sim) {
  ## create data.table version
  pctPine <- if (nlyr(sim$pineMap) > 1) sum(sim$pineMap)[] else sim$pineMap[]
  pctPine <- as.vector(pctPine)
  sim$pineDT <- data.table(ID = 1L:ncell(sim$pineMap), ## TODO: use sppEquivNames
                           PROPPINE = pctPine / 100) # use proportion
  sim$pineDT[, NUMTREES := asInteger(PROPPINE * P(sim)$stemsPerHaAvg * prod(res(sim$pineMap)) / 100^2)]

  setkey(sim$pineDT, ID)
  sim$pineDT[is.na(PROPPINE), ':='(PROPPINE = 0.00, NUMTREES = 0.00)]

  return(invisible(sim))
}

prepInputs_ABPine <- function(url, rasterToMatch, layers, destinationPath, ...) {
  dirForExtract <- file.path(destinationPath, "Pine_data_AB")

  AB <- prepInputs(url = url,
                    destinationPath = dirForExtract,
                    targetFile = "AB_PineVolumes_Lambert.gdb",
                    rasterizeMask = rasterizeMask,
                    layers = layers,
                    rasterToMatch = rasterToMatch,
                    useCache = FALSE,
                    fun = rasterizeMask(targetFile, layer = layers, rasterToMatch = rasterToMatch)
  ) |> Cache()

  if (FALSE) { # old way without prepInputs
    # fileInfo <- preProcess(url = url, archive = NA, destinationPath = destinationPath, ...)
    # out <- archive::archive_extract(fileInfo$targetFilePath, dir = dirForExtract)

    gdbName <- unique(dirname(out))[2]
    origDir <- setwd(dirForExtract)
    on.exit(setwd(origDir))

    layerNames <- sf::st_layers(gdbName)$name
    rtmCRS <- sf::st_crs(rasterToMatch)
    for (y in layerNames[1]) {
      co <- capture.output({
        pineMap <- sf::st_read(gdbName, layer = y)
      })
      pineMap <- st_transform(pineMap, rtmCRS)
      pineMap <- st_cast(pineMap, "MULTIPOLYGON")
      pineMap <- fixErrors(pineMap, testValidity = FALSE, useCache = FALSE)
      rrr <- st_crop(pineMap, rasterToMatch)
      rrr <- st_cast(rrr, "MULTIPOLYGON")
      AB <- terra::rast(fasterize::fasterize(rrr, raster = raster::raster(rasterToMatch), field = "PCT_P"))
      AB <- maskInputs(AB, rasterToMatch = rasterToMatch, studyArea = NULL, maskWithRTM = TRUE)
    }
  }
  return(AB)
}


rasterizeMask <- function(targetFile, rasterToMatch, layer) {
  out <- sf::st_read(targetFile, layer = layer)
  ras <- raster::raster(rasterToMatch)
  AB <- terra::rast(fasterize::fasterize(out, raster = ras, field = "PCT_P"))
  AB <- maskInputs(AB, rasterToMatch = rasterToMatch, studyArea = NULL, maskWithRTM = TRUE)
}

rastTimes10 <- function(ras) {
  ras <- terra::rast(ras)
  ras[] <- ras[] * 10
  ras
}
