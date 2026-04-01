# Code to create figure 4
# Results of simulations 1
# Panel a: Stand age at the start of the simulation
# Panel b: area burned and area harvested in the managed vs unmanaged forest
# Panel c: carbon stock in aboveground biomass, belowground biomass, and DOM in the managed vs unmanaged forest
# Panel d: Emissions in the managed vs unmanaged forest

library(qs2)
library(data.table)
library(terra)
library(ggplot2)
library(patchwork)

# Some setup
outputPath <- "~/../Downloads/historicalDisturbances/"
managedForestColor <- "darkred"
unmanagedForestColor <- "#1b9e77"
pixelArea <- 240 * 240 / 10000 # pixels are 240mx240m -> ha per pixel

rasterToMatch <- rast(file.path(outputPath, "rasterToMatch_year2024.tif"))
disturbanceEvents <- readRDS(file.path(
  outputPath,
  "disturbanceEvents_year2024.rds"
))
managedForest <- vect("~/../Downloads/MFUF_26july2016/MFUF_26july2016.shp") |>
  project(crs(rasterToMatch))
managedForest <- rasterize(managedForest, rasterToMatch, field = "OBJECTID")
managedForest <- data.frame(
  pixelIndex = 1:ncell(managedForest),
  managedForest = managedForest[]
)

# Panel a: stand ages of pixel in and outside the managed forest
standAges <- rast(file.path(outputPath, "standAgeMap_year2000.tif"))
pixelGroupMap <- rast(file.path(outputPath, "pixelGroupMap_year2000.tif"))

# remove non-treed pixels
pixelGroupMap[!is.na(pixelGroupMap[])] <- 1
standAges <- standAges * pixelGroupMap
standAges <- cbind(managedForest, age = as.vector(standAges[])) |> na.omit()

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

# Shared aesthetics and theme
base_size <- 12
line_width <- 0.9
point_size <- 1.6
base_theme <- theme_classic(base_size = base_size) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.height = unit(0.6, "lines"),
    legend.key.width = unit(2, "lines"),
    axis.title = element_text(size = 10),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "gray97", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    plot.margin = margin(6, 6, 6, 6)
  )

# Only show a single color legend (Managed vs Unmanaged).
# Hide fill and linetype legends; use color legend with simple color boxes.
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

fig4a <- ggplot(standAges, aes(x = age_bin, fill = management)) +
  geom_bar(position = position_dodge(width = 0.9), width = 0.9, colour = NA) +
  fill_scale +
  labs(x = "Stand age (years)", y = "Number of pixels") +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Panel b area burned and area harvested in the managed vs unmanaged forest
fig4b_dt <- merge(disturbanceEvents, managedForest)
fig4b_dt <- fig4b_dt[, .(area = .N), by = c("year", "eventID", "OBJECTID")] |>
  na.omit()
# convert number of cells to ha
fig4b_dt$area <- fig4b_dt$area * pixelArea
fig4b_dt$distType <- as.factor(ifelse(
  fig4b_dt$eventID == 1,
  "Wildfire",
  "Harvesting"
))
fig4b_dt$management <- as.factor(ifelse(
  fig4b_dt$OBJECTID == 1,
  "Managed forest",
  "Unmanaged forest"
))

fig4b <- ggplot(
  fig4b_dt,
  aes(
    x = year,
    y = area,
    color = management,
    linetype = management,
    group = management
  )
) +
  geom_line(linewidth = line_width) +
  geom_point(size = point_size) +
  color_scale +
  linetype_scale +
  scale_y_continuous(name = "Area burned (ha)", labels = scales::label_comma()) +
  scale_x_continuous(name = "Year", expand = c(0.01, 0.01)) +
  labs(color = "Management", title = NULL) +
  base_theme +
  guides(linetype = "none")

# Panel c: carbon stock in aboveground biomass, belowground biomass, and DOM in the managed vs unmanaged forest
poolSummary <- summarizeSimulation(
  outputPath,
  years = 2000:2024,
  managedForest
) |>
  na.omit()
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
    )
  )
]

fig4c <- ggplot(data = poolSummary2) +
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
  facet_wrap(~component, nrow = 1, scales = "free_y") +
  labs(y = "Carbon (tC/ha)", x = "Year") +
  lims(y = c(0, NA)) +
  base_theme +
  guides(color = guide_legend(override.aes = list(size = 1.1)))

# Panel d: summarizing emissions over years
fluxSummary <- summarizeFluxes(outputPath, years = 2000:2024, managedForest) |>
  na.omit()
fluxSummary$management <- ifelse(
  fluxSummary$management == 1,
  "Managed forest",
  "Unmanaged forest"
)

# For the plot, we only need mean and total emissions
fluxSummary2 <- fluxSummary[component == "Emissions", .(year, management, mean)]

fig4d <- ggplot(data = fluxSummary2) +
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
final_plot <- (fig4a + ggtitle("(a) Stand age")) + 
  (fig4b + ggtitle("(b) Area burned")) + 
  (fig4c + ggtitle("(c) Carbon stock")) + 
  (fig4d + ggtitle("(d) Emissions")) +
  plot_layout(nrow = 2, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

final_plot
ggsave(plot = final_plot, "pubFigures/figure4.png", width = 12, height = 8)
