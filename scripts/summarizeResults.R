# Summarize results:
# stocks: AGB BGB, Dead wood, litter, SOM
# Primary production: NPP, NEP (NPP-respiration), 
# Ecosystem exchanges: NEEc, NEEco2, NEEghg, balance
# Area burnt
stocks <- data.table()
productivity <- data.table()
exchanges <- data.table()
burnedArea <- data.table()
nPix <- length(unique(simOut$cbm_vars$key$pixelIndex))
for (year in start(simOut):end(simOut)){
  # get stocks
  cbmPools <- readRDS(paste0("outputs/cbmPools_year", year, ".rds"))
  # per pixel
  currentStock <- data.table(
    year = year,
    AGB = cbmPools[, sum(N * (Merch + Foliage + Other))]/nPix,
    BGB = cbmPools[, sum(N * (CoarseRoots + FineRoots))]/nPix,
    litter = cbmPools[, sum(N * (AboveGroundVeryFastSoil + AboveGroundFastSoil + AboveGroundSlowSoil))]/nPix,
    SOM = cbmPools[, sum(N * (BelowGroundSlowSoil + BelowGroundVeryFastSoil))]/nPix,
    DW = cbmPools[, sum(N * (MediumSoil + StemSnag + BranchSnag + BelowGroundFastSoil))]/nPix
  )
  stocks <- rbind(stocks, currentStock)
  
  # get productivity
  currentProductivity <- data.table(
    year = year,
    NPP = sum(simOut$NPP[simYear == year, NPP * N])/nPix 
  )
  productivity <- rbind(productivity,
                        currentProductivity)
  
  # get exchanges
  currentExchanges <- data.table(
    year = year,
    NEEc = cbmPools[, sum(N * (CO2 + CH4 + CO))]/nPix,
    NEEco2 = cbmPools[, sum(N * CO2)]/nPix
  )
  currentExchanges$balance <- (currentProductivity$NPP - currentExchanges$NEEc)
  exchanges <- rbind(exchanges, currentExchanges)
  
  # burned area
  current_year <- year
  currentBurnedArea <- data.table(
    year = year,
    Npix = sum(unique(simOut$disturbanceEvents[eventID == 1 & year == current_year, pixelIndex]))
  )
  burnedArea <- rbind(burnedArea, currentBurnedArea)
}

pixelCarbon <- function(Cpools, key){
  # summarise carbon per group
  summary_Cpools <- data.table(
    AGB_C = rowSums(Cpools[,.(Merch, Foliage, Other)]),
    BGB_C = rowSums(Cpools[,.(CoarseRoots, FineRoots)]),
    DW_C = rowSums(Cpools[,.(StemSnag, MediumSoil, BranchSnag, BelowGroundFastSoil)]),
    Litter_C = rowSums(Cpools[,.(AboveGroundVeryFastSoil, AboveGroundFastSoil, AboveGroundSlowSoil)]),
    SOM_C = rowSums(Cpools[,.(BelowGroundVeryFastSoil, BelowGroundSlowSoil)])
  )
  summary_Cpools[, Total_C := AGB_C + BGB_C + DW_C + Litter_C + SOM_C]
  # expand data.table to get 1 row per cohort
  summary_Cpools <- summary_Cpools[match(key$row_idx, Cpools$row_idx),]
  summary_Cpools$pixelIndex <- key$pixelIndex
  summary_Cpools <- summary_Cpools[,.(AGB_C = sum(AGB_C), BGB_C = sum(BGB_C), DW_C = sum(DW_C), Litter_C = sum(Litter_C), SOM_C = sum(SOM_C), Total_C = sum(Total_C)), by = "pixelIndex"]
  return(summary_Cpools)
}

summarizeSimulation <- function(CBMOutPath, years, managementForestDT){
  fullPath <- file.path(CBMOutPath, "spadesCBMdb", "data")
  carbonComponents <- c("AGB_C", "BGB_C", "DW_C", "Litter_C", "SOM_C", "Total_C")
  carbonComponentsLabs <- c("Aboveground biomass", "Belowground biomass", "Dead wood", "Litter", "Soil organic matter", "Total carbon")
  Cdynamic_dt <- data.table()
  for (year in years) {
    key_current <- qs2::qd_read(file.path(fullPath, paste0(year, "_key.qs2")))
    carbonPools_current <- qs2::qd_read(file.path(
      fullPath,
      paste0(year, "_pools.qs2")
    ))
    pixelCarbonPools <- pixelCarbon(
      Cpools = carbonPools_current,
      key = key_current
    )
    pixelCarbonPools <- merge(pixelCarbonPools, managementForestDT)
    dt_long <- melt(
      pixelCarbonPools,
      id.vars = c("pixelIndex", "OBJECTID"),
      measure.vars = carbonComponents,
      variable.name = "component",
      value.name = "value"
    )
    stats_dt <- dt_long[, .(mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      nPixels = .N),
      by = .(management = OBJECTID, component)
    ]
    stats_dt[, year := year]
    setcolorder(
      stats_dt,
      c("year", "management", "component", "mean", "median", "sd", "nPixels")
    )
    Cdynamic_dt <- rbind(Cdynamic_dt, stats_dt, use.names = TRUE, fill = TRUE)
  }
  Cdynamic_dt$component <- factor(Cdynamic_dt$component, levels = carbonComponents, carbonComponentsLabs)
  return(Cdynamic_dt)
}

pixelFlux <- function(emissions, key){
  setDT(emissions)
  # summarise carbon per group
  emissions[, CO2 := DisturbanceBioCO2Emission + DecayDOMCO2Emission + DisturbanceDOMCO2Emission]
  emissions[, CH4 := DisturbanceBioCH4Emission + DisturbanceDOMCH4Emission]
  emissions[, CO  := DisturbanceBioCOEmission + DisturbanceDOMCOEmission]
  emissions[, Emissions := CO2 + CH4 + CO]

  # expand data.table to get 1 row per cohort
  emissions <- emissions[match(key$row_idx, emissions$row_idx),]
  emissions$pixelIndex <- key$pixelIndex
  summary_emissions <- emissions[, .(pixelIndex, CO2, CH4, CO, Emissions)]
  return(summary_emissions)
}

summarizeFluxes <- function(CBMOutPath, years, managementForestDT){
  fullPath <- file.path(CBMOutPath, "spadesCBMdb", "data")
  Cdynamic_dt <- data.table()
  for (year in years) {
    key_current <- qs2::qd_read(file.path(fullPath, paste0(year, "_key.qs2")))
    carbonFlux_current <- qs2::qd_read(file.path(
      fullPath,
      paste0(year, "_flux.qs2")
    ))
    pixelCarbonFlux <- pixelFlux(
      emissions = carbonFlux_current,
      key = key_current
    )
    pixelCarbonFlux <- merge(pixelCarbonFlux, managementForestDT)

    dt_long <- melt(
      pixelCarbonFlux,
      id.vars = c("pixelIndex", "OBJECTID"),
      measure.vars = c("CO2", "CH4", "CO", "Emissions"),
      variable.name = "component",
      value.name = "value"
    )
    stats_dt <- dt_long[, .(mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      nPixels = .N),
      by = .(management = OBJECTID, component)
    ]
    stats_dt[, year := year]
    setcolorder(
      stats_dt,
      c("year", "management", "component", "mean", "median", "sd", "nPixels")
    )
    Cdynamic_dt <- rbind(Cdynamic_dt, stats_dt, use.names = TRUE, fill = TRUE)
  }
  return(Cdynamic_dt)
}
