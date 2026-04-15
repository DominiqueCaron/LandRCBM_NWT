# Code to create figure 4
# Results of simulations 1
# Panel a: Stand age at the start of the simulation
# Panel b: area burned and area harvested in the managed vs unmanaged forest
# Panel c: carbon stock in aboveground biomass, belowground biomass, and DOM in the managed vs unmanaged forest
# Panel d: Emissions in the managed vs unmanaged forest

Require::Require(c("qs2", "data.table", "terra", "ggplot2", "ggpubr"))
source("scripts/themes.R")
source("scripts/utils.R")

# Some setup
outputPath <- "~/../Downloads/historicalDisturbances/"
pixelArea <- 240 * 240 / 10000 # pixels are 240mx240m -> ha per pixel

# PixelIndex within the managed forest
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
standAges <- standAges[,.(area = .N * pixelArea / 10 ^ 6), by = c("management", "age_bin")]

fig4a <- ggplot(standAges, aes(x = age_bin, y = area ,fill = management)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9, colour = NA) +
  fill_scale +
  scale_y_continuous() +
  labs(x = "Stand age (years)", y = "Area (Mha)") +
  biplot_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
fig4a <- annotate_figure(fig4a,
  fig.lab = "(a) Stand Ages"
)

# Panel b area burned and area harvested in the managed vs unmanaged forest
fig4b_dt <- merge(disturbanceEvents, managedForest)
fig4b_dt <- fig4b_dt[, .(area = .N), by = c("year", "eventID", "OBJECTID")] |>
  na.omit()
# convert number of cells to ha
fig4b_dt$area <- fig4b_dt$area * pixelArea / 10^6
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
  scale_y_continuous(name = "Area burned (Mha)") +
  scale_x_continuous(name = "Year", limits = c(2000, 2024), breaks = c(2000, 2005, 2010, 2015, 2020), expand = c(0.01, 0.01)) +
  labs(color = "Management", title = NULL) +
  biplot_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
fig4b <- annotate_figure(fig4b, fig.lab = "(b) Area burned")

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
                               ),
                               labels = c(
                                 "Aboveground\nbiomass",
                                 "Belowground\nbiomass",
                                 "Dead organic\nmatter",
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
  scale_x_continuous(name = "Year", limits = c(2000, 2024), breaks = c(2000, 2005, 2010, 2015, 2020), expand = c(0.01, 0.01)) +
  facet_wrap(~component, nrow = 1, scales = "free_y") +
  labs(y = "Carbon (tC/ha)", x = "Year") +
  lims(y = c(0, NA)) +
  biplot_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

plot_legend <- get_legend(fig4c)
fig4c <- annotate_figure(
  fig4c + theme(legend.position = "none"),
  fig.lab = "(c) Carbon stocks",
  bottom = text_grob("Year", hjust = 0.5),
  left = text_grob("Carbon (tC/ha)", rot = 90, vjust = 0.5)
)

# Panel d: summarizing emissions over years
fluxSummary <- summarizeFluxes(outputPath, years = 2000:2024, managedForest) |>
  na.omit()
fluxSummary$management <- ifelse(
  fluxSummary$management == 1,
  "Managed forest",
  "Unmanaged forest"
)

# For the plot, we only need mean and total emissions
fluxSummary2 <- fluxSummary[component %in% c("NPP", "NBP", "Emissions"), .(year, management, component, mean)]
fluxSummary2$component <- factor(fluxSummary2$component, levels = c("NPP", "Emissions", "NBP"), labels = c("Net primary\nproductivity", "Emissions", "Net biome\nproductivity"))

fig4di <- ggplot(data = fluxSummary2[component != "Net biome\nproductivity"]) +
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
  color_scale +
  linetype_scale +
  facet_wrap(~component, nrow = 1, axes = "all") +
  scale_x_continuous(name = "Year", limits = c(2000, 2024), breaks = c(2000, 2005, 2010, 2015, 2020), expand = c(0.01, 0.01)) +
  labs(y = "Carbon (tC/ha)", x = "Year") +
  lims(y = c(0, NA)) +
  biplot_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(r = 0, t = 15, b = 15, l = 15)
  )

fig4dii <- ggplot(data = fluxSummary2[component == "Net biome\nproductivity"]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
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
  color_scale +
  linetype_scale +
  facet_wrap(~component, nrow = 1, scales = "free_y") +
  scale_x_continuous(name = "Year", limits = c(2000, 2024), breaks = c(2000, 2005, 2010, 2015, 2020), expand = c(0.01, 0.01)) +
  labs(y = "Carbon (tC/ha)", x = "Year") +
  biplot_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(l = 0, t = 15, b = 15, r = 15)
  )

fig4d <- ggarrange(fig4di, fig4dii, widths = c(2, 1))
fig4d <- annotate_figure(
  fig4d,
  fig.lab = "(d) Carbon exchange",
  bottom = text_grob("Year", hjust = 0.5),
  left = text_grob("Carbon (tC/ha)", rot = 90, vjust = 0.5)
)

bottom_plots <- ggarrange(fig4c, fig4d)

# Combine panels and collect legends
top_plots <- ggarrange(fig4a, fig4b)
final_plot <- ggarrange(top_plots, bottom_plots, nrow = 2) |>
  annotate_figure(
    bottom = plot_legend
  ) + theme(plot.background = element_rect(fill = "white", colour = "white"))

ggsave(plot = final_plot, "pubFigures/figure4.png", width = 12, height = 8)
ggsave(plot = final_plot, "pubFigures/figure4.pdf", width = 12, height = 8)
