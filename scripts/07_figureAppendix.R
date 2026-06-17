# Code to create figures for Appendix 1:
# 1 (a) Map of stand ages + (b) Histogram of stand ages in managed vs unmanaged forest
# 2 (a) Map of biomass + (b) Histogram of biomass in managed vs unmanaged forest
# 3 (a) Map of leading species + (b) Histogram of leading species in managed vs unmanaged forest


library(terra)
library(tidyterra)
library(qs2)
library(data.table)
library(ggplot2)
library(ggpubr)
source("scripts/themes.R")
source("scripts/utils.R")

outputPath <- "~/repos/LandRCBM_NWT/outputs/SCFM/"
spadesCBMpath <- file.path(outputPath, "spadesCBMdb", "data")
yearEnd <- 2520
pixelArea <- 240 * 240 / 10000 # pixels are 240mx240m -> ha per pixel

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


# 1 stand ages
# map
standAgeMap <- rast(file.path(outputPath, "standAgeMap.tif"))
standAgeMap <- standAgeMap * rasterToMatch

appendixS1_1a <- ggplot() +
  geom_spatraster(data = standAgeMap) +
  scale_fill_binned(
    "Stand age",
    palette = "viridis",
    limits = c(0, NA),
    breaks = c(0, 25, 50, 75, 100),
    na.value = "transparent"
  ) +
  map_theme

# histogram
standAges <- cbind(managedForest, age = as.vector(standAgeMap[])) |> na.omit()

standAges$management <- as.factor(ifelse(
  standAges$OBJECTID == 1,
  "Managed forest",
  "Unmanaged forest"
))

setDT(standAges)
standAges[,
          age_bin := cut(
            age,
            breaks = seq(0, ceiling(max(age, na.rm = TRUE) / 10) * 10, by = 10),
            right = FALSE
          )
]
standAges <- standAges[,.(area = .N * pixelArea / 10 ^ 6), by = c("management", "age_bin")]

appendixS1_1b <- ggplot(standAges, aes(x = age_bin, y = area ,fill = management)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9, colour = NA) +
  scale_fill_manual(values = c(
      "Managed forest" = managedForestColor,
      "Unmanaged forest" = unmanagedForestColor)
      ) +  
  scale_y_continuous() +
  labs(x = "Stand age (years)", y = "Area (Mha)") +
  biplot_theme +
  theme(
    legend.position = c(0.8, 0.9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 20, r = 10, l = 10, b = 10)
  )
appendixS1_1 <- ggarrange(appendixS1_1a, appendixS1_1b, labels = c("(a)", "(b)")) + 
  theme(plot.background = element_rect(fill = "white"))

# 2 biomass
# map
biomassMap <- rast(file.path(outputPath, "biomassMap.tif"))
biomassMap <- biomassMap * rasterToMatch * 0.01

appendixS1_2a <- ggplot() +
  geom_spatraster(data = biomassMap) +
  scale_fill_binned(
    "Above ground\nbiomass (t/ha)",
    palette = "viridis",
    limits = c(0, NA),
    breaks = c(0, 25, 50, 75, 100),
    na.value = "transparent"
  ) +
  map_theme

# histogram
biomass <- cbind(managedForest, biomass = as.vector(biomassMap[])) |> na.omit()
biomass$management <- as.factor(ifelse(
  biomass$OBJECTID == 1,
  "Managed forest",
  "Unmanaged forest"
))

setDT(biomass)
biomass[,
        biomass_bin := cut(
            biomass,
            breaks = seq(0, ceiling(max(biomass, na.rm = TRUE) / 10) * 10, by = 10),
            right = FALSE
          )
]
biomass <- biomass[,.(area = .N * pixelArea / 10 ^ 6), by = c("management", "biomass_bin")]

appendixS1_2b <- ggplot(biomass, aes(x = biomass_bin, y = area ,fill = management)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9, colour = NA) +
  scale_fill_manual(values = c(
    "Managed forest" = managedForestColor,
    "Unmanaged forest" = unmanagedForestColor)
  ) +  
  scale_y_continuous() +
  labs(x = "Above ground biomass (t/ha)", y = "Area (Mha)") +
  biplot_theme +
  theme(
    legend.position = c(0.8, 0.9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 20, r = 10, l = 10, b = 10)
  )
appendixS1_2 <- ggarrange(appendixS1_2a, appendixS1_2b, labels = c("(a)", "(b)")) + 
  theme(plot.background = element_rect(fill = "white"))

# 3 leading species
# map
leadingSpecies_rast <- rast(file.path(outputPath, "leadingSpecies.tif"))

appendixS1_3a <- ggplot() +
  geom_spatraster(data = leadingSpecies_rast) +
  map_theme +
  theme(legend.title = element_blank())

# histogram
leadingSpecies <- cbind(managedForest, leadingSpecies = as.vector(leadingSpecies_rast[])) |> na.omit()
leadingSpecies$management <- as.factor(ifelse(
  leadingSpecies$OBJECTID == 1,
  "Managed forest",
  "Unmanaged forest"
))

setDT(leadingSpecies)
leadingSpecies <- leadingSpecies[ , .(area = .N * pixelArea / 10 ^ 6), by = c("leadingSpecies", "management")]
lvls <- cats(leadingSpecies_rast)[[1]]
leadingSpecies$leadingSpecies <- factor(leadingSpecies$leadingSpecies, levels = lvls$id, labels = lvls$values)

appendixS1_3b <- ggplot(leadingSpecies, aes(x = leadingSpecies, y = area , fill = management)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9, colour = NA) +
  scale_fill_manual(values = c(
    "Managed forest" = managedForestColor,
    "Unmanaged forest" = unmanagedForestColor)
  ) +  
  scale_y_continuous() +
  labs(x = "Leading species", y = "Area (Mha)") +
  biplot_theme +
  theme(
    legend.position = c(0.8, 0.9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 20, r = 10, l = 10, b = 10)
  )
appendixS1_3 <- ggarrange(appendixS1_3a, appendixS1_3b, labels = c("(a)", "(b)")) + 
  theme(plot.background = element_rect(fill = "white"))

ggsave(appendixS1_1, filename = "appFigures/appendixS1_1.png", width = 10, height = 5)
ggsave(appendixS1_2, filename = "appFigures/appendixS1_2.png", width = 10, height = 5)
ggsave(appendixS1_3, filename = "appFigures/appendixS1_3.png", width = 10, height = 5)
