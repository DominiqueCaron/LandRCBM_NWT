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
               inputPath = "inputs",
               outputPath = "outputs/withSCFM",
               cachePath = "cache"),
  options = options(
    repos = c(repos = repos),
    Require.cloneFrom = Sys.getenv("R_LIBS_USER"),
    spades.moduleCodeChecks = FALSE,
    spades.recoveryMode = FALSE,
    reproducible.useMemoise = TRUE),
  times = list(start = 1990, end = 2490),
  modules = c(
    "PredictiveEcology/Biomass_borealDataPrep@development",
    "PredictiveEcology/Biomass_speciesFactorial@development",
    "PredictiveEcology/Biomass_speciesParameters@development",
    "PredictiveEcology/CBM_defaults@development",
    "PredictiveEcology/Biomass_regeneration@development",
    "PredictiveEcology/Biomass_yieldTables@main",
    "PredictiveEcology/Biomass_core@development",
    "PredictiveEcology/CBM_dataPrep@development",
    "PredictiveEcology/LandRCBM_split3pools@main",
    "PredictiveEcology/CBM_core@development",
    file.path("PredictiveEcology/scfm@development/modules",
              c("scfmDataPrep",
                "scfmIgnition", "scfmEscape", "scfmSpread",
                "scfmDiagnostics"))
  ),
  packages = c("googledrive", 'RCurl', 'XML', "stars", "httr2"),
  # Study area is RIA
  studyArea = {
    # northwest territories boundaries
    nwt <- reproducible::prepInputs(url = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000a21a_e.zip",
                                    destinationPath = "inputs")
    nwt <- nwt[nwt$PRENAME == "Northwest Territories",]
    # ecozone: Taiga plains
    boreal <- reproducible::prepInputs(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                                       destinationPath = "inputs", projectTo = nwt)
    boreal <- boreal[boreal$ECOZONE == 4, ]
    sa <- reproducible::postProcessTo(boreal, cropTo = nwt, maskTo = nwt) |> 
      reproducible::Cache() |> 
      sf::st_union() |>
      sf::st_as_sf() |> sf::st_buffer(-125)
    sa
  },
  studyArea_biomassParam = studyArea,
  rasterToMatch = {
    sa <- terra::vect(studyArea)
    rtm <- terra::rast(sa, res = c(250, 250))
    terra::crs(rtm) <- terra::crs(sa)
    sa$id <- 1
    rtm <- terra::rasterize(sa, rtm, field = "id", touches = FALSE)
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
  disturbanceMeta = data.table(eventID = 1,
                               disturbance_type_id = 1,
                               wholeStand = 1,
                               name = "Wildfire",
                               sourceValue = 1,
                               sourceDelay = 0,
                               sourceObjectName = "rstCurrentBurn"),
  outputs = as.data.frame(expand.grid(
    objectName = c("cbmPools", "NPP"),
    saveTime = seq(from = times$start, to = times$end, by = 10)
  )),
  params = list(
    .globals = list(
      .plots = c("png"),
      .plotInterval = 10,
      sppEquivCol = 'LandR',
      .studyAreaName = "NWT",
      dataYear = 2011
    ),
    CBM_core = list(
      skipPrepareCBMvars = TRUE
    ),
    Biomass_borealDataPrep = list(
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
                        dataYear = 2020,
                        .useParallelFireRegimePolys = FALSE,
                        fireEpoch = c(1971, 2020)
    )
  )
)

out$loadOrder <- unlist(out$modules)

initOut <- SpaDES.core::simInit2(out)
simOut <- SpaDES.core::spades(initOut)

