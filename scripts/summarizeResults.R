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
  summary_Cpools <- summary_Cpools[,.(AGB_C = sum(AGB_C), BGB_C = sum(BGB_C), DW_C = sum(DW_C), Litter_C = sum(Litter_C), SOM_C = sum(SOM_C)), by = "pixelIndex"]
  return(summary_Cpools)
}

summarizeSimulation <- function(CBMOutPath, years, managementForestDT){
  carbonComponents <- c("AGB_C", "BGB_C", "DW_C", "Litter_C", "SOM_C")
  carbonComponentsLabs <- c("Aboveground biomass", "Belowground biomass", "Dead wood", "Litter", "Soil organic matter")
  Cdynamic_dt <- data.table()
  for (year in years){
    quantiles <- c(0.05, 0.25, 0.5, 0.75, 0.95)
    key_current <- qs2::qd_read(file.path(CBMOutPath, paste0(year,"_key.qs2")))
    carbonPools_current <- qs2::qd_read(file.path(CBMOutPath, paste0(year,"_pools.qs2")))
    pixelCarbonPools <- pixelCarbon(Cpools = carbonPools_current, key = key_current)
    pixelCarbonPools <- merge(pixelCarbonPools, managementForestDT)
    for (management in c(1,2)){
      dt <- pixelCarbonPools[pixelCarbonPools$OBJECTID == management,]
      AGB_quantiles <- quantile(dt$AGB_C, probs = quantiles)
      BGB_quantiles <- quantile(dt$BGB_C, probs = quantiles)
      DW_quantiles <- quantile(dt$DW_C, probs = quantiles)
      Litter_quantiles <- quantile(dt$Litter_C, probs = quantiles)
      SOM_quantiles <- quantile(dt$SOM_C, probs = quantiles)
      current_dt <- data.table(
        year = year,
        management = management,
        component = carbonComponents,
        q0_05 = c(AGB_quantiles[1], BGB_quantiles[1], DW_quantiles[1], Litter_quantiles[1], SOM_quantiles[1]),
        q0_25 = c(AGB_quantiles[2], BGB_quantiles[2], DW_quantiles[2], Litter_quantiles[2], SOM_quantiles[2]),
        q0_50 = c(AGB_quantiles[3], BGB_quantiles[3], DW_quantiles[3], Litter_quantiles[3], SOM_quantiles[3]),
        q0_75 = c(AGB_quantiles[4], BGB_quantiles[4], DW_quantiles[4], Litter_quantiles[4], SOM_quantiles[4]),
        q0_95 = c(AGB_quantiles[5], BGB_quantiles[5], DW_quantiles[5], Litter_quantiles[5], SOM_quantiles[5])
      )
      Cdynamic_dt <- rbind(
        Cdynamic_dt,
        current_dt
        )
    }

  }
  Cdynamic_dt$component <- factor(Cdynamic_dt$component, levels = carbonComponents, carbonComponentsLabs)
  return(Cdynamic_dt)
}
