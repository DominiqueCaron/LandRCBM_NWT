# Code to summarize results from simulation 2:
# Figure 5 
# (a) shows the map of cumulative fire.
# (b) shows species dynamics.
# (c) shows the carbon dynamics.
# (d) shows the emission dynamics.

library(qs2)
library(terra)
library(tidyterra)
library(data.table)
library(ggplot2)
library(patchwork)
source("scripts/themes.R")
source("scripts/utils.R")

outputPath <- "~/repos/LandRCBM_NWT/outputs/SCFM/"
spadesCBMpath <- file.path(outputPath, "spadesCBMdb", "data")
yearStart <- 2020
yearEnd <- 2520

# template raster of the simulated pixels
pixelCarbonEnd <- pixelCarbon(
  Cpools = qs2::qd_read(file.path(spadesCBMpath, paste0(yearEnd, "_pools.qs2"))),
  key = qs2::qd_read(file.path(spadesCBMpath, paste0(yearEnd, "_key.qs2")))
)
rasterToMatch <- rast(file.path(outputPath, paste0("rasterToMatch_year", yearEnd, ".tif")))
set.values(rasterToMatch, 1:ncell(rasterToMatch), NA)
set.values(rasterToMatch, pixelCarbonEnd$pixelIndex, 1L)
Npixels <- sum(values(rasterToMatch), na.rm = T)
pixelArea <- prod(res(rasterToMatch))

managedForest <- vect("inputs/MFUF_26july2016/MFUF_26july2016.shp") |>
  project(crs(rasterToMatch))
managedForest <- rasterize(managedForest, rasterToMatch, field = "OBJECTID")
managedForest <- data.frame(
  pixelIndex = 1:ncell(managedForest),
  managedForest = managedForest[]
)

# Figure a: map of cumulative fire
fires <- readRDS(file.path(outputPath, paste0("disturbanceEvents_year", yearEnd, ".rds")))
fires <- fires[, .(Nfires = sum(eventID)), by = pixelIndex]

firesRast <- rast(rasterToMatch, vals = 0L) * rasterToMatch
set.values(firesRast, fires$pixelIndex, fires$Nfires)
set.names(firesRast, "Nfirest")

fig5a <- ggplot() +
  geom_spatraster(aes(fill = Nfirest), data = firesRast) +
  scale_fill_distiller("Number of fires", palette = "Reds", direction = 1, na.value = "transparent") +
  coord_sf()+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", linewidth = 0.7, fill = NA)
  )

# Figure b: species dynamics
speciesDynamics <- readRDS(file.path(outputPath, paste0("summaryBySpecies_year", yearEnd, ".rds")))
sppEquiv <- LandR::sppEquivalencies_CA[, c("LandR", "EN_generic_full", "colorHex")]
spNames <- sppEquiv$EN_generic_full[match(speciesDynamics$speciesCode, sppEquiv$LandR)]
spCol <- sppEquiv$colorHex[match(speciesDynamics$speciesCode, sppEquiv$LandR)]
spCol[speciesDynamics$speciesCode == "Pinu_con"] <-  "#02AD24"
speciesDynamics$col <- factor(x = spNames, levels = unique(spNames), labels = unique(spNames))

# convert above ground biomass into tonnes/ha
speciesDynamics$AGB <- speciesDynamics$BiomassBySpecies / Npixels # per pixels
speciesDynamics$AGB <- speciesDynamics$AGB * 10000 # per ha
speciesDynamics$AGB <- speciesDynamics$AGB / 10^6 # tonnes/ha
speciesDynamics$AGB <- speciesDynamics$AGB * 0.5 # tonnesC/ha

fig5b <- ggplot() +
  geom_line(aes(x = year, y = AGB, color = col), data = speciesDynamics) +
  scale_color_manual(values = unique(spCol)) +
  labs(x = "Year", y = "Aboveground biomass (tC/ha)", color = "Species") + 
  biplot_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Figure c: carbon dynamics
poolSummary <- summarizeSimulation(
  outputPath,
  years = seq(from = yearStart, to = yearEnd, by = 10),
  managedForest
) |>
  na.omit()
# only keep total C
poolSummary <- poolSummary[component == "Total carbon",]
poolSummary$management <- ifelse(
  poolSummary$management == 1,
  "Managed forest",
  "Unmanaged forest"
)

# For the plot, we only need mean
poolSummary2 <- poolSummary[, .(year, management, component, mean)]

# create dead organic matter (sum of Dead wood, Litter, Soil organic matter)
dead_parts <- c("Dead wood", "Litter", "Soil organic matter")
dead_dt <- poolSummary2[
  component %in% dead_parts,
  .(mean = sum(mean)),
  by = .(year, management)
]
dead_dt[, component := "Dead organic matter"]

# keep other components (drop the three detailed dead pools) and add dead organic matter
poolSummary2 <- rbindlist(
  list(poolSummary2[!component %in% dead_parts], dead_dt),
  use.names = TRUE,
  fill = TRUE
)
poolSummary2 <- poolSummary2[,
  component := factor(
    component,
    levels = c(
      "Aboveground biomass",
      "Belowground biomass",
      "Dead organic matter",
      "Total carbon"
    ),
    labels = c(
      "Aboveground\nbiomass",
      "Belowground\nbiomass",
      "Dead organic\nmatter",
      "Total carbon"
    )
  )
]

fig5c <- ggplot(data = poolSummary2) +
  geom_line(
    aes(
    x = year,
    y = mean,
    color = management,
    linetype = management,
    group = management
  ),
    linewidth = line_width
  ) +
  color_scale +
  linetype_scale +
  scale_x_continuous(name = "Year", limits = c(2000, 2550), breaks = c(2000, 2100, 2200, 2300, 2400, 2500), expand = c(0.01, 0.01)) +
  facet_wrap(~component, nrow = 1, scales = "free_y") +
  labs(y = "Carbon (tC/ha)", x = "Year") +
  lims(y = c(0, NA)) +
  biplot_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.1)))

# Panel d: summarizing emissions over years
fluxSummary <- summarizeFluxes(outputPath, years = seq(from = yearStart, to = yearEnd, by = 10), managedForest) |>
  na.omit()
fluxSummary$management <- ifelse(
  fluxSummary$management == 1,
  "Managed forest",
  "Unmanaged forest"
)


# For the plot, we only need mean and total emissions
fluxSummary2 <- fluxSummary[component == "Emissions", .(year, management, mean)]
# We also want to see the net ecosystem change
carbonChange <- getCarbonChange(poolSummary, outputPath, managedForest)

fluxSummary2 <- rbind(
  fluxSummary2[,.(year, management, component = "Emissions", value = mean)],
  carbonChange[,.(year, management, component = "Net biome\nproduction", value = ChangeC)]
)

fig5di <- ggplot(data = fluxSummary2[component == "Emissions"]) +
  geom_line(
    aes(
      x = year,
      y = value,
      colour = management,
      linetype = management,
      group = management
    ),
    linewidth = line_width
  ) +
  color_scale +
  linetype_scale +
  facet_wrap(~component, nrow = 1, scales = "free_y") +
  scale_x_continuous(name = "Year", limits = c(2000, 2550), breaks = c(2000, 2100, 2200, 2300, 2400, 2500), expand = c(0.01, 0.01)) +
  labs(y = "Carbon (tC/ha)", x = "Year") +
  lims(y = c(0, NA)) +
  biplot_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(linetype = "none")

fig5dii <- ggplot(data = fluxSummary2[component == "Net biome\nproduction"]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(
    aes(
      x = year,
      y = value,
      colour = management,
      linetype = management,
      group = management
    ),
    linewidth = line_width
  ) +
  color_scale +
  linetype_scale +
  facet_wrap(~component, nrow = 1, scales = "free_y") +
  scale_x_continuous(name = "Year", limits = c(2000, 2550), breaks = c(2000, 2100, 2200, 2300, 2400, 2500), expand = c(0.01, 0.01)) +
  labs(y = "Carbon (tC/ha)", x = "Year") +
  biplot_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(linetype = "none")

fig5d <- (fig5di + ggtitle("(d) Carbon exchange") | fig5dii)  + plot_layout(axis_titles = "collect") 

# Combine panels and collect legends
final_plot <- {
  top <- (fig5a + ggtitle("(a) Wildfires") + theme(legend.position = "bottom")) +
    (fig5b + ggtitle("(b) Species dynamics") + theme(legend.position = "bottom")) +
    plot_layout(ncol = 2, guides = "keep")
  bottom <- ((fig5c + ggtitle("(c) Total carbon")) + fig5d) +
    plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")
  top / bottom + plot_layout(heights = c(1,1))
}

ggsave(plot = final_plot, "pubFigures/figure5.png", width = 12, height = 8)
ggsave(plot = final_plot, "pubFigures/figure5.pdf", width = 12, height = 8)
