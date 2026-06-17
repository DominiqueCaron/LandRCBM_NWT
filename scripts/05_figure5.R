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
library(ggpubr)
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
fig5a <- annotate_figure(fig5a, fig.lab = "(a) Wildfires")

# Appendix figure: histogram of cumulative burned
dt_appendixS2_2 <- data.frame(
  Nfires = values(firesRast, na.rm = TRUE)
)
appendixS2_2 <- ggplot(dt_appendixS2_2) + 
  geom_histogram(aes(x = Nfirest), binwidth = 1) +
  labs(y = "Number of pixels", x = "Cumulative burn") +
  biplot_theme
ggsave(plot = appendixS2_2, "appFigures/appendixS2_2.png", width = 12, height = 8)

# Appendix figure: number of fires per year
dt_appendixS2_1 <- readRDS(file.path(outputPath, paste0("disturbanceEvents_year", yearEnd, ".rds")))
dt_appendixS2_1 <- merge(dt_appendixS2_1, managedForest)
dt_appendixS2_1 <- dt_appendixS2_1[, .(area = .N), by = c("year", "eventID", "OBJECTID")] |>
  na.omit()
# convert number of cells to ha
dt_appendixS2_1$area <- dt_appendixS2_1$area * pixelArea / 10^6 / 10000
dt_appendixS2_1$distType <- as.factor(ifelse(
  dt_appendixS2_1$eventID == 1,
  "Wildfire",
  "Harvesting"
))
dt_appendixS2_1$management <- as.factor(ifelse(
  dt_appendixS2_1$OBJECTID == 1,
  "Managed forest",
  "Unmanaged forest"
))
setorder(dt_appendixS2_1, management)
appendixS2_1 <- ggplot(
  dt_appendixS2_1,
  aes(
    x = year - yearStart,
    y = area,
    fill = management,
    color = management,
    group = management
  )
) +
  geom_col(width = 1, position = position_identity(), alpha = 0.5) +
  color_scale +
  fill_scale +
  scale_y_continuous(name = "Area burned (Mha)", breaks = c(0.0, 0.2, 0.4, 0.6), limits = c(0,0.6)) +
  scale_x_continuous(name = "Year", limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), expand = c(0.01, 0.01)) +
  labs(color = "Management", title = NULL) +
  biplot_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(plot = appendixS2_1, "appFigures/appendixS2_1.png", width = 12, height = 8)

# Figure b: species dynamics
speciesDynamics <- readRDS(file.path(outputPath, paste0("summaryBySpecies_year", yearEnd, ".rds")))

# Remove Pice_eng_gla and Popu_bal as it is veru very low
speciesDynamics <- speciesDynamics[!(speciesCode %in% c("Pice_eng_gla", "Popu_bal")), ]

sppEquiv <- LandR::sppEquivalencies_CA[, c("LandR", "EN_generic_full", "colorHex")]
spNames <- sppEquiv$EN_generic_full[match(speciesDynamics$speciesCode, sppEquiv$LandR)]
spCol <- sppEquiv$colorHex[match(speciesDynamics$speciesCode, sppEquiv$LandR)]
spCol[speciesDynamics$speciesCode == "Pinu_con"] <-  "#02AD24"
speciesDynamics$col <- factor(x = spNames, levels = unique(spNames), labels = unique(spNames))

# convert above ground biomass into tonnes/ha
speciesDynamics$AGB <- speciesDynamics$BiomassBySpecies / Npixels # per pixels
speciesDynamics$AGB <- speciesDynamics$AGB * 10000 # per ha
speciesDynamics$AGB <- speciesDynamics$AGB / 10^6 # tonnes/ha

fig5b <- ggplot() +
  geom_line(aes(x = year - yearStart, y = AGB, color = col), data = speciesDynamics, linewidth = line_width) +
  scale_color_manual(values = unique(spCol)) +
  labs(x = "Year", y = "Aboveground biomass (t/ha)", color = "Species") + 
  biplot_theme +
  theme(
    legend.position = "right",
    legend.key.height = unit(1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 12)
  )
fig5b <- annotate_figure(fig5b, fig.lab = c("(b) Species dynamics"))

# Figure c: carbon dynamics
poolSummary <- summarizeSimulation(
  outputPath,
  years = seq(from = yearStart, to = yearEnd, by = 10),
  managedForest
) |>
  na.omit()
# only keep total C
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

setorder(poolSummary2, management)
fig5c <- ggplot(data = poolSummary2) +
  geom_col(
    aes(
      x = year-2020,
      y = mean,
      color = management,
      fill = management,
      group = management
    ),
    alpha = 0.5,
    width = 10,
    position = position_identity(),
    linewidth = 0.2
  ) +
  color_scale +
  fill_scale +
  scale_x_continuous(name = "Year", limits = c(-5, 505), breaks = c(0, 100, 200, 300, 400, 500)) +
  facet_wrap(~component, nrow = 1, scales = "free_y") +
  labs(y = "Carbon (tC/ha)", x = "Year") +
  lims(y = c(0, NA)) +
  biplot_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(t = 12, r = 5)
  )
plot_legend <- get_legend(fig5c)
fig5c <- annotate_figure(
  fig5c + theme(legend.position = "none"),
  fig.lab = "(c) Carbon stocks",
  bottom = text_grob("Year", hjust = 0.5),
  left = text_grob("Carbon (tC/ha)", rot = 90, vjust = 0.5)
)

# Panel d: summarizing emissions over years
fluxSummary <- summarizeFluxes(outputPath, years = seq(from = yearStart+10, to = yearEnd, by = 10), managedForest) |>
  na.omit()
fluxSummary$management <- ifelse(
  fluxSummary$management == 1,
  "Managed forest",
  "Unmanaged forest"
)


# For the plot, we only need mean and total emissions
fluxSummary2 <- fluxSummary[component %in% c("NPP", "NBP", "Emissions"), .(year, management, component, mean)]
fluxSummary2$component <- factor(fluxSummary2$component, levels = c("NPP", "Emissions", "NBP"), labels = c("Net primary\nproductivity", "Emissions", "Net biome\nproductivity"))

setorder(fluxSummary2, management)
fig5di <- ggplot(data = fluxSummary2[component != "Net biome\nproductivity"]) +
  geom_col(
    aes(
      x = year - yearStart,
      y = mean,
      colour = management,
      fill = management,
      group = management
    ),
    alpha = 0.5,
    position = position_identity(),
    width = 10
  ) +
  color_scale +
  fill_scale + 
  facet_wrap(~component, nrow = 1, axes = "all") +
  scale_x_continuous(name = "Year", limits = c(0, 505), breaks = c(0, 100, 200, 300, 400, 500)) +
  labs(y = "Carbon (tC/ha)", x = "Year") +
  lims(y = c(0, NA)) +
  biplot_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(t = 12)
  )

fig5dii <- ggplot(data = fluxSummary2[component == "Net biome\nproductivity"]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(
    aes(
      x = year - yearStart,
      y = mean,
      colour = management,
      fill = management,
      group = management
    ),
    alpha = 0.5,
    position = position_identity(),
    width = 10,
    linewidth = 0.2
  ) +
  color_scale +
  fill_scale +
  facet_wrap(~component, nrow = 1, scales = "free_y") +
  scale_x_continuous(name = "Year", limits = c(0, 505), breaks = c(0, 100, 200, 300, 400, 500)) +
  labs(y = "Carbon (tC/ha)", x = "Year") +
  biplot_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(t = 12)
  )

fig5d <- ggarrange(fig5di, fig5dii, widths = c(2, 1))
fig5d <- annotate_figure(
  fig5d,
  fig.lab = "(d) Carbon exchange",
  bottom = text_grob("Year", hjust = 0.5),
  left = text_grob("Carbon (tC/ha)", rot = 90, vjust = 0.5)
) + theme(plot.margin = margin(l = 5))

bottom_plots <- ggarrange(fig5c, fig5d) + theme(plot.margin = margin(l = 10, r = 10))

# Combine panels and collect legends
top_plots <- ggarrange(fig5a, fig5b)
final_plot <- ggarrange(top_plots, bottom_plots, nrow = 2) |>
  annotate_figure(
    bottom = plot_legend
  ) + theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(plot = final_plot, "pubFigures/figure5.png", width = 12, height = 8)
ggsave(plot = final_plot, "pubFigures/figure5.pdf", width = 12, height = 8)
