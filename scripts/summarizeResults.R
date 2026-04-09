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

