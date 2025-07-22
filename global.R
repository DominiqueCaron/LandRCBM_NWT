###
###
# This script runs LandR and CBM in NWT.
###
###

# Get the minimal amount of packages
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project")){
  Require::Install(c("SpaDES.project", "SpaDES.core", "reproducible"), repos = repos, dependencies = TRUE)
}

out <- SpaDES.project::setupProject(
  paths = list(projectPath = getwd(),
               inputPath = "../LandRCBM/inputs",
               outputPath = "outputs/LandRCBM_NWTsmall",
               cachePath = "cache"),
  options = options(spades.moduleCodeChecks = FALSE,
                    spades.recoveryMode = FALSE),
  times = list(start = 1985, end = 2015),
  modules = c(
    "PredictiveEcology/Biomass_speciesFactorial@development",
    "PredictiveEcology/Biomass_borealDataPrep@development",
    "PredictiveEcology/Biomass_speciesParameters@development",
    "PredictiveEcology/CBM_defaults@development",
    "PredictiveEcology/Biomass_regeneration@development",
    "PredictiveEcology/Biomass_yieldTables@main",
    "PredictiveEcology/Biomass_core@development",
    "PredictiveEcology/CBM_dataPrep@development",
    "DominiqueCaron/LandRCBM_split3pools@optimize",
    "PredictiveEcology/CBM_core@development",
    file.path("PredictiveEcology/scfm@development/modules",
              c("scfmDataPrep",
                "scfmIgnition", "scfmEscape", "scfmSpread",
                "scfmDiagnostics"))
  ),
  packages = c("googledrive", 'RCurl', 'XML', "stars", "httr2"),
  useGit = T,
  # Study area is RIA
  studyArea = {
    sa <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1sScLiFW6eaFa1knVyT7ZasBEN9niE7ua/view?usp=drive_link",
                                   destinationPath = "inputs",
                                   overwrite = TRUE)
    sa <- sf::st_crop(sa, c(xmin = -1200000, xmax = -1000000, ymin = 7500000, ymax = 7700000)) |> 
      reproducible::Cache() 
    sa <- sf::st_union(sa) |> 
      reproducible::Cache() |> sf::st_as_sf()
    sa
  },
  studyArea_biomassParam = studyArea,
  rasterToMatch = {
    sa <- terra::vect(studyArea)
    targetCRS <- terra::crs(sa)
    rtm <- terra::rast(sa, res = c(250, 250))
    terra::crs(rtm) <- targetCRS
    rtm[] <- 1
    rtm <- terra::mask(rtm, sa)
    rtm
  },
  masterRaster = {
    masterRaster = rasterToMatch
    masterRaster
  },
  sppEquiv = {
    speciesInStudy <- LandR::speciesInStudyArea(studyArea,
                                                dPath = "inputs")
    species <- LandR::equivalentName(speciesInStudy$speciesList, df = LandR::sppEquivalencies_CA, "LandR")
    sppEquiv <- LandR::sppEquivalencies_CA[LandR %in% species]
    sppEquiv <- sppEquiv[KNN != "" & LANDIS_traits != ""] #avoid a bug with shore pine
  },
  disturbanceMeta = data.table(
    eventID = 1,
    wholeStand = 1,
    disturbance_type_id = 1,
    distName = "Wildfire",
    objectName = "rstCurrentBurn",
    delay = 1
  ),
  outputs = as.data.frame(expand.grid(
    objectName = c("cbmPools", "NPP"),
    saveTime   = sort(c(times$start, times$start + c(1:(times$end - times$start))))
  )),
  params = list(
    .globals = list(
      .plots = c("png"),
      .plotInterval = 10,
      sppEquivCol = 'LandR',
      .studyAreaName = "NWT"
    ),
    CBM_core = list(
      skipCohortGroupHandling = TRUE,
      skipPrepareCBMvars = TRUE
    ),
    Biomass_borealDataPrep = list(
      .studyAreaName = "RIA",
      subsetDataBiomassModel = 50
    ),
    Biomass_speciesFactorial = list(
      .plots = NULL, #"pdf",
      runExperiment = TRUE,
      factorialSize = "medium"
    ),
    Biomass_speciesParameters = list(
      .plots = "png",
      standAgesForFitting = c(0, 125),
      .useCache = c(".inputObjects", "init"),
      speciesFittingApproach = "focal"
    ),
    Biomass_yieldTables = list(
      moduleNameAndBranch = "PredictiveEcology/Biomass_core@development",
      maxAge = 200,
      .plots = "png",
      .useCache = "generateData"
    ),
    scfmDataPrep = list(targetN = 4000,
                        flammabilityThreshold = 0.05,
                        .useParallelFireRegimePolys = FALSE,
                        fireEpoch = c(1971, 2020),
                        fireRegimePolysType = "FRU"
    )
  )
)

out$loadOrder <- unlist(out$modules)

initOut <- SpaDES.core::simInit2(out)
simOut <- SpaDES.core::spades(initOut)

