# Code to create figure 4
# Results of simulations 1
# Panel a: area burned and area harvested in the managed vs unmanaged forest
# Panel b: carbon stock in aboveground biomass, belowground biomass, and DOM in the managed vs unmanaged forest
# Panel c: Emissions in the managed vs unmanaged forest

library(qs2)
library(data.table)
library(terra)
library(ggplot2)
library(patchwork)

outputPath <- "~/../Downloads/LandRCBM_NWT_historical/"
rasterToMatch <- rast(file.path(outputPath, "rasterToMatch.tif"))
disturbanceEvents <- qs_read(file.path(outputPath, "disturbanceEvents.qs"))
managedForest <- vect("~/../Downloads/MFUF_26july2016/MFUF_26july2016.shp") |>
  project(crs(rasterToMatch))
managedForest <- rasterize(managedForest, rasterToMatch, field = "OBJECTID")
managedForest <- data.frame(
  pixelIndex = 1:ncell(managedForest),
  managedForest = managedForest[]
)

yieldTableId <- qs_read(file.path(outputPath, "yieldTablesId.qs"))
yieldTableMap <- terra::rast(file.path(outputPath, "rasterToMatch.tif"))

# Figure 4 a area burned and area harvested in the managed vs unmanaged forest
fig4a_dt <- merge(disturbanceEvents, managedForest)
fig4a_dt <- fig4a_dt[,.(area = .N), by = c("year", "eventID", "OBJECTID")] |> na.omit()
# convert number of cells to ha
fig4a_dt$area <- fig4a_dt$area * 240^2 / 10000
fig4a_dt$distType <- as.factor(ifelse(fig4a_dt$eventID == 1, "Wildfire", "Harvesting"))
fig4a_dt$management <- as.factor(ifelse(fig4a_dt$OBJECTID == 1, "Managed forest", "Unmanaged forest"))

fig4a <- ggplot(fig4a_dt, aes(x = year, y = area, color = management, linetype = management, group = management)) +
  geom_line(size = 1.1) +
  geom_point(size = 1.6) +
  facet_grid(. ~ distType, scales = "free_y") +
  scale_color_manual(values = c("Managed forest" = "darkred", "Unmanaged forest" = "#1b9e77")) +
  scale_linetype_manual(values = c("Managed forest" = "solid", "Unmanaged forest" = "dashed")) +
  scale_y_continuous(name = "Area (ha)", labels = scales::label_comma()) +
  scale_x_continuous(name = "Year", expand = c(0.01, 0.01)) +
  labs(color = NULL, title = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.height = unit(0.6, "lines"),
    legend.key.width = unit(2, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    strip.background = element_rect(fill = "gray97", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(color = "black"),
    plot.margin = margin(6, 6, 6, 6)
  ) +
  guides(linetype = "none")

# Panel b: carbon stock in aboveground biomass, belowground biomass, and DOM in the managed vs unmanaged forest

# Get a datatable with the aboveground biomass, belowground biomass, dead wood litter and som (mean, CV, total, 50% interquantile, and 90% interquantile)
poolSummary <- summarizeSimulation("~/../Downloads/LandRCBM_NWT_historical/data", years = 2000:2024, managedForest)
ggplot(data = Cdynamic_dt) +
  geom_ribbon(aes(x = year, ymin = q0_05, ymax = q0_95, fill = "grey80")) +
  geom_ribbon(aes(x = year, ymin = q0_25, ymax = q0_75, fill = "grey50")) +
  geom_line(aes(x = year, y = q0_50, linetype = "1"), linewidth = 0.7) +
  facet_wrap(~component, nrow = 1,scales = "free_y") +
  labs(y = "Carbon in pixel (t/ha)", x = "Year") +
  lims(y = c(0, NA)) +
  scale_fill_manual(values = c("grey50", "grey80"), labels = c("50% of pixels", "90% of pixels"), name = NULL) +
  scale_linetype_manual(values = 1, labels = "Median pixel", name = NULL) +
  theme_bw() +
  guides(fill = guide_legend(order = 1, reverse = TRUE)) +
  theme(legend.spacing = unit(-10, 'pt'),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = c(0.92, 0.25),
        legend.text = element_text(size = 8),
        strip.background = element_rect(fill = "white")
        )
