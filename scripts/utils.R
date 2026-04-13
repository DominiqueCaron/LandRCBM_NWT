plotYieldCurves <- function(yieldTables, id){
  # estract the yieldCurves for the id selected
  yieldTables <- yieldTables[yieldTables$yieldTableIndex == id,]

  # prepare species-specific colors and name
  yieldTables$speciesName <- as.factor(LandR::sppEquivalencies_CA$EN_generic_full[match(
    yieldTables$speciesCode,
    LandR::sppEquivalencies_CA$LandR
  )])
  spCols <- LandR::sppEquivalencies_CA$colorHex[match(
    unique(yieldTables$speciesCode),
    LandR::sppEquivalencies_CA$LandR
  )]
  names(spCols) <- unique(yieldTables$speciesName)
  # make sure lodgepole pine gets the right color (avoid confusion with shore pine)
  if("Lodgepole pine" %in% names(spCols)) {
    spCols["Lodgepole pine"] <- "#02AD24"
  }

ggplot(yieldTables, aes(x = age, y = merch, colour = speciesName)) +
  geom_line(size = 0.9) +
  labs(y = "Merchantable biomass (tC/ha)", x = "Stand age (years)", colour = NULL) +
  scale_colour_manual(values = spCols) +
  scale_x_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0)) +
  biplot_theme +
  theme(
    legend.position = c(0.01, 0.99),
    legend.justification = c(0, 1),
    plot.margin = margin(10, 10, 10, 10),
    legend.key.height = unit(0.8, "lines"),
    legend.background = element_blank()
  )

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

getCarbonChange <- function(poolSummary, CBMOutPath, managementForestDT){
  # get the total carbon after the spinup (year 0)
  spinupCarbon <- pixelCarbon(
    Cpools = qs2::qd_read(file.path(CBMOutPath, "spadesCBMdb", "data", "0_pools.qs2")),
    key = qs2::qd_read(file.path(CBMOutPath, "spadesCBMdb", "data", "0_key.qs2"))
  )
  spinupCarbon <- merge(spinupCarbon, managementForestDT)
  spinupCarbon <- melt(
    spinupCarbon,
    id.vars = c("pixelIndex", "OBJECTID"),
    measure.vars = "Total_C",
    variable.name = "component",
    value.name = "value"
  )
  spinupCarbon <- spinupCarbon[, .(mean = mean(value, na.rm = TRUE)),
    by = .(management = OBJECTID, component)
  ] |> na.omit()

  out <- c()
  for (iyear in (unique(poolSummary$year))){
    if (iyear != min(poolSummary$year)){
      
      Cpre <- poolSummary[year == iyear - 1 & component == "Total carbon", ]$mean
      Cpost <- poolSummary[year == iyear & component == "Total carbon", ]$mean
      Cchange <- Cpost - Cpre

      out <- rbind(out,
        data.frame(year = rep(iyear,2),
        management = poolSummary[year == iyear & component == "Total carbon", ]$management, 
        ChangeC = Cchange)
      )
    }
  }
  return(setDT(out))
}
