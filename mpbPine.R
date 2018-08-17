
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbPine",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "mpbPine.Rmd"),
  reqdPkgs = list("data.table", "magrittr", "quickPlot", "raster", "reproducible", "sp"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("lowMemory", "numeric", FALSE, NA, NA, "Should high memory-usage steps be skipped? Useful for running on laptops.")
  ),
  inputObjects = bind_rows(
    expectsInput("studyArea", "SpatialPolygons", "The study area to which all maps will be cropped and reprojected.", sourceURL = NA),
    expectsInput("mpbGrowthDT", "data.table", "Current MPB attack map (number of red attacked trees)."),
    expectsInput(NA, NA, "kNN species data.", "http://tree.nfis.org/kNN-Species.tar"),
    expectsInput(NA, NA, "kNN stand structure and volume data.", "http://tree.nfis.org/kNN-StructureStandVolume.tar"),
    expectsInput(NA, NA, "Additional documentation for kNN datasets.", "http://tree.nfis.org/cjfr-2013-0401suppl.pdf"),
    expectsInput(NA, NA, "Additional documentation for kNN datasets.", "http://tree.nfis.org/FR_NFI_and_kNN_Mapping_20160628.docx"),
    expectsInput(NA, NA, "Additional documentation for kNN datasets.", "http://tree.nfis.org/NFI_and_kNN_Mapping_20160628.docx")
  ),
  outputObjects = bind_rows(
    createsOutput("pineMap", "RasterLayer", "Current lodgepole and jack pine available for MPB.")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.mpbPine <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      # do stuff for this event
      sim <- sim$mpbPineImportMap(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "mpbPine", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "mpbPine", "save")
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      Plot(sim$pineMap, title = c("Percent Jack Pine", "Percent Lodgepole Pine"))
      Plot(sim$studyArea, addTo = "sim$pineMap") # TODO: check that this correctly adds polygons to each map

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
  # ! ----- EDIT BELOW ----- ! #
  dataDir <- file.path(modulePath(sim), "mpbPine", "data")

  ## percent pine layers
  message("Checking for kNN-Species data layers...")
  destDir <- file.path(dataDir, "kNN-Species")

  # create dir if doesn't already exist (as dir or as symlink)
  if (!dir.exists(destDir)) dir.create(destDir)

  f <- c("NFI_MODIS250m_kNN_Species_Pinu_Ban_v0.zip",
         "NFI_MODIS250m_kNN_Species_Pinu_Con_v0.zip")
  if (!all(file.exists(file.path(destDir, f)))) {
    message("Extracting kNN-Species data layers...")
    untar(file.path(dataDir, "kNN-Species.tar"), files = f, exdir = path.expand(destDir))
  }

  zipFile <- file.path(destDir, f[1])
  destDir2 <- file.path(tools::file_path_sans_ext(zipFile))
  files <- file.path(destDir2,
                     c("NFI_MODIS250m_kNN_Species_Pinu_Ban_v0.tif",
                       "NFI_MODIS250m_kNN_Species_Pinu_Ban_v0.tif.aux.xml",
                       "NFI_MODIS250m_kNN_Species_Pinu_Ban_v0.tif.xml"))
  if (!all(file.exists(files))) {
    message("  Extracting NFI_MODIS250m_kNN_Species_Pinu_Ban_v0...")
    unzip(zipFile, exdir = destDir2)
  }
  if (!all(file.exists(file.path(dataDir, basename(files))))) {
    file.copy(files, file.path(dataDir, basename(files)))
  }

  zipFile <- file.path(destDir, f[2])
  destDir2 <- file.path(tools::file_path_sans_ext(zipFile))
  files <- file.path(destDir2,
                     c("NFI_MODIS250m_kNN_Species_Pinu_Con_v0.tif",
                       "NFI_MODIS250m_kNN_Species_Pinu_Con_v0.tif.aux.xml",
                       "NFI_MODIS250m_kNN_Species_Pinu_Con_v0.tif.xml"))
  if (!all(file.exists(files))) {
    message("  Extracting NFI_MODIS250m_kNN_Species_Pinu_Con_v0...")
    unzip(zipFile, exdir = destDir2)
  }
  if (!all(file.exists(file.path(dataDir, basename(files))))) {
    file.copy(files, file.path(dataDir, basename(files)))
  }

  ## age & total volume layers
  message("Checking for kNN-StructureStandVolume data layers...")
  destDir <- file.path(dataDir, "kNN-StructureStandVolume")
  if (!dir.exists(destDir)) dir.create(destDir)

  f <- c("NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip",
         "NFI_MODIS250m_kNN_Structure_Volume_Total_v0.zip")
  if (!all(file.exists(file.path(destDir, f)))) {
    message("Extracting kNN-StructureStandVolume data layers...")
    untar(file.path(dataDir, "kNN-StructureStandVolume.tar"), files = f, exdir = path.expand(destDir))
  }

  zipFile <- file.path(destDir, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip")
  destDir2 <- file.path(tools::file_path_sans_ext(zipFile))
  unzip(zipFile, exdir = destDir2)
  files <- file.path(destDir2,
                     c("NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif",
                       "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif.aux.xml",
                       "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif.xml"))
  if (!all(file.exists(files))) {
    message("  Extracting NFI_MODIS250m_kNN_Structure_Stand_Age_v0...")
    unzip(zipFile, exdir = destDir2)
  }
  if (!all(file.exists(file.path(dataDir, basename(files))))) {
    file.copy(files, file.path(dataDir, basename(files)))
  }

  zipFile <- file.path(destDir, "NFI_MODIS250m_kNN_Structure_Volume_Total_v0.zip")
  destDir2 <- file.path(tools::file_path_sans_ext(zipFile))
  files <- file.path(destDir2,
                     c("NFI_MODIS250m_kNN_Structure_Volume_Total_v0.tif",
                       "NFI_MODIS250m_kNN_Structure_Volume_Total_v0.tif.aux.xml",
                       "NFI_MODIS250m_kNN_Structure_Volume_Total_v0.tif.xml"))
  if (!all(file.exists(files))) {
    message("  Extracting NFI_MODIS250m_kNN_Structure_Volume_Total_v0...")
    unzip(zipFile, exdir = destDir2)
  }
  if (!all(file.exists(file.path(dataDir, basename(files))))) {
    file.copy(files, file.path(dataDir, basename(files)))
  }

  ## load study area
  if (!('studyArea' %in% sim$.userSuppliedObjNames)) {
    f <- file.path(modulePath(sim), "mpbPine", "data", "studyArea.kml")
    prj <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113",
                 "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    sim$studyArea <- readOGR(f, "studyArea.kml") %>%
      sp::spTransform(., prj)
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

mpbPineImportMap <- function(sim) {
  layerNames <- c("Jack_Pine", "Lodgepole_Pine")

  f <- file.path(modulePath(sim), "mpbPine", "data",
                 c("NFI_MODIS250m_kNN_Species_Pinu_Ban_v0.tif",
                   "NFI_MODIS250m_kNN_Species_Pinu_Con_v0.tif"))
  stopifnot(all(file.exists(f)))

  s <- raster::stack(f) # this map is a percentage; need to use proportion (see below)
  names(s) <- layerNames
  sim$pineMap <- Cache(amc::cropReproj, x = s, studyArea = sim$studyArea,
                       layerNames = layerNames, inRAM = TRUE)

  ## create data.table version
  jpDT <- data.table(ID = 1L:ncell(sim$pineMap[["Jack_Pine"]]),
                     PROPPINE = sim$pineMap[["Jack_Pine"]][] / 100) # use proportion
  jpDT <- jpDT[PROPPINE > 0]
  jpDT <- jpDT[, SPECIES := "jack"]

  lpDT <- data.table(ID = 1L:ncell(sim$pineMap[["Lodgepole_Pine"]]),
                     PROPPINE = sim$pineMap[["Lodgepole_Pine"]][] / 100) # use proportion
  lpDT <- lpDT[PROPPINE > 0]
  lpDT <- lpDT[, SPECIES := "lodgepole"]

  sim$pineDT <- merge(lpDT, jpDT, all = TRUE)
  setkey(sim$pineDT, ID)

  return(invisible(sim))
}
