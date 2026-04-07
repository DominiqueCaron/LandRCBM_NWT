library(qs2)
library(terra)
library(tidyterra)
library(data.table)
library(ggplot2)
library(patchwork)

outputPath <- "~/repos/LandRCBM_NWT/outputs/SCFM/"
spadesCBMpath <- file.path(outputPath, "spadesCBMdb", "data")

# Panel a: Total carbon start at the simulation
yearStart <- 2020
yearEnd <- 2520

pixelCarbonStart <- pixelCarbon(
  Cpools = qs2::qd_read(file.path(spadesCBMpath, paste0(yearStart, "_pools.qs2"))),
  key = qs2::qd_read(file.path(spadesCBMpath, paste0(yearStart, "_key.qs2")))
)

carbonRastStart <- rast(file.path(outputPath, paste0("rasterToMatch_year", yearEnd, ".tif")))
set.values(carbonRastStart, 1:ncell(carbonRastStart), NA)
set.values(carbonRastStart, pixelCarbonStart$pixelIndex, pixelCarbonStart$Total_C)
set.names(carbonRastStart, "carbon")

carbonRastStart[carbonRastStart > 200] <- 200

p6a <- ggplot() +
  geom_spatraster(aes(fill = carbon), data = carbonRastStart) +
  scale_fill_distiller("Carbon (tC/ha)", palette = "YlGn", direction = 1, na.value = "transparent", limit = c(0, NA),
                       breaks = c(0, 50, 100, 150, 200), labels = c("0", "50", "100", "150", "> 200")) +
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

# Panel b: Total carbon end at the simulation
pixelCarbonEnd <- pixelCarbon(
  Cpools = qs2::qd_read(file.path(spadesCBMpath, paste0(yearEnd, "_pools.qs2"))),
  key = qs2::qd_read(file.path(spadesCBMpath, paste0(yearEnd, "_key.qs2")))
)

carbonRastEnd <- rast(file.path(outputPath, paste0("rasterToMatch_year", yearEnd, ".tif")))
set.values(carbonRastEnd, 1:ncell(carbonRastEnd), NA)
set.values(carbonRastEnd, pixelCarbonEnd$pixelIndex, pixelCarbonEnd$Total_C)
set.names(carbonRastEnd, "carbon")

carbonRastEnd[carbonRastEnd > 200] <- 200

p6b <- ggplot() +
  geom_spatraster(aes(fill = carbon), data = carbonRastEnd) +
  scale_fill_distiller("Carbon (tC/ha)", palette = "YlGn", direction = 1, na.value = "transparent", limit = c(0, NA),
                       breaks = c(0, 50, 100, 150, 200), labels = c("0", "50", "100", "150", "> 200")) +
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

# Panel c: Difference
carbonRastDiff <- carbonRastEnd - carbonRastStart

carbonRastDiff[carbonRastDiff < -100] <- -100
carbonRastDiff[carbonRastDiff > 100] <- 100

p6c <- ggplot() +
  geom_spatraster(aes(fill = carbon), data = carbonRastDiff) +
  scale_fill_distiller(expression(Delta * " carbon (tC/ha)"), palette = "RdYlBu", direction = 1, na.value = "transparent", labels = c("< -100", "-50", "0", "50", "> 100"), breaks = c(-100, -50, 0, 50, 100)) +
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

left <- (p6a + ggtitle(paste("(a)", yearStart))) +
  (p6b + ggtitle(paste("(b)", yearEnd))) +
  plot_layout(ncol = 2, guides = "collect")

left <- left &
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.box.just = "center"
  )

right <- p6c + ggtitle("(c) Difference")
right <- right &
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.box.just = "center"
  )

# keep both legends and give left (two panels) twice the width of right

p6b <- p6b + theme(legend.position = "none")

final_plot <- (left | right) +
  plot_layout(widths = c(2, 1), heights = 1, guides = "keep")
ggsave("pubFigures/figure6.png", plot = final_plot)
