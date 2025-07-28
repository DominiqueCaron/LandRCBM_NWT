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
               outputPath = "outputs",
               cachePath = "cache"),
  options = options(spades.moduleCodeChecks = FALSE,
                    spades.recoveryMode = FALSE,
                    reproducible.useMemoise = TRUE),
  times = list(start = 1990, end = 2024),
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
    "PredictiveEcology/CBM_core@development"
  ),
  packages = c("googledrive", 'RCurl', 'XML', "stars", "httr2"),
  # Study area is RIA
  studyArea = {
    # northwest territories boundaries
    nwt <- reproducible::prepInputs(url = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000a21a_e.zip",
                                    destinationPath = "inputs")
    nwt <- nwt[nwt$PRENAME == "Northwest Territories",]
    # ecozone: Taiga + boreal
    boreal <- reproducible::prepInputs(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                                       destinationPath = "inputs", projectTo = nwt)
    boreal <- boreal[boreal$ECOZONE %in% c(4, 5, 6, 9, 11, 12), ]
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
  disturbanceSource = "NTEMS",
  sppEquiv = {
    speciesInStudy <- LandR::speciesInStudyArea(studyArea,
                                                dPath = "inputs")
    species <- LandR::equivalentName(speciesInStudy$speciesList, df = LandR::sppEquivalencies_CA, "LandR")
    sppEquiv <- LandR::sppEquivalencies_CA[LandR %in% species]
    sppEquiv <- sppEquiv[KNN != "" & LANDIS_traits != ""] #avoid a bug with shore pine
  },
  outputs = as.data.frame(expand.grid(
    objectName = c("cbmPools", "NPP"),
    saveTime   = sort(c(times$start, times$start + c(1:(times$end - times$start))))
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
    )
  )
)

out$loadOrder <- unlist(out$modules)

initOut <- SpaDES.core::simInit2(out)
reproducible::clearCache(fun = "doEvent.Biomass_core::init")
Y
reproducible::clearCache(fun = "doEvent.Biomass_regeneration::init")
Y
reproducible::clearCache(fun = "doEvent.Biomass_speciesParameters::init")
Y
simOut <- SpaDES.core::spades(initOut)

