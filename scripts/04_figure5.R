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

# Shared aesthetics and theme
base_size <- 12
line_width <- 0.9
point_size <- 1.6
base_theme <- theme_classic(base_size = base_size) +
  theme(
    legend.key.width = unit(2, "lines"),
    axis.title = element_text(size = 10),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "gray97", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    plot.margin = margin(6, 6, 6, 6)
  )

managedForestColor <- "darkred"
unmanagedForestColor <- "#1b9e77"

fill_scale <- scale_fill_manual(
  values = c(
    "Managed forest" = managedForestColor,
    "Unmanaged forest" = unmanagedForestColor
  ),
  guide = "none"
)
color_scale <- scale_color_manual(
  values = c(
    "Managed forest" = managedForestColor,
    "Unmanaged forest" = unmanagedForestColor
  ),
  name = "Management",
  guide = "none"
)
linetype_scale <- scale_linetype_manual(
  values = c("Managed forest" = "solid", "Unmanaged forest" = "dashed"),
  guide = "none"
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
  base_theme

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

fig5c <- ggplot(data = poolSummary) +
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
  labs(y = "Carbon (tC/ha)", x = "Year") +
  lims(y = c(0, NA)) +
  base_theme +
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

fig5d <- ggplot(data = fluxSummary2) +
  geom_line(
    aes(
      x = year,
      y = mean,
      colour = management,
      linetype = management,
      group = management
    ),
    linewidth = line_width
  ) +
  geom_point(aes(x = year, y = mean, colour = management), size = point_size) +
  color_scale +
  linetype_scale +
  labs(y = "Emissions (tC/ha)", x = "Year") +
  lims(y = c(0, NA)) +
  base_theme +
  guides(linetype = "none")

# Combine panels and collect legends
final_plot <- {
  top <- (fig5a + ggtitle("(a) Wildfires") + theme(legend.position = "bottom")) +
    (fig5b + ggtitle("(b) Species dynamics") + theme(legend.position = "bottom")) +
    plot_layout(ncol = 2, guides = "keep")
  bottom <- ((fig5c + ggtitle("(c) Total carbon")) + (fig5d + ggtitle("(d) Emissions"))) +
    plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")
  top / bottom + plot_layout(heights = c(1,1))
}



