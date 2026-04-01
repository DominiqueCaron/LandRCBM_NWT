# Code to create figure 3: yield tables for 6 pixels in the NWT.
library(terra)
library(ggplot2)
library(patchwork)

outputPath <- "~/../Downloads/historicalDisturbances/"
yieldTableDT <- readRDS(file.path(outputPath, "yieldTablesCumulative_year2024.rds"))
yieldTableId <- readRDS(file.path(outputPath, "yieldTablesId_year2024.rds"))
yieldTableMap <- rast(file.path(outputPath, "rasterToMatch_year2024.tif"))

yieldTableMap[yieldTableId$pixelIndex] <- yieldTableId$yieldTableIndex

id_to_plot <- c(2425, 1138, 1565, 2043, 2991, 860)
id <- sample(yieldTableId$yieldTableIndex, 1)
plotYieldCurves(yieldTableDT, id)

fig3a <- plotYieldCurves(yieldTableDT, id_to_plot[1])
fig3b <- plotYieldCurves(yieldTableDT, id_to_plot[2])
fig3c <- plotYieldCurves(yieldTableDT, id_to_plot[3])
fig3d <- plotYieldCurves(yieldTableDT, id_to_plot[4])
fig3e <- plotYieldCurves(yieldTableDT, id_to_plot[5])
fig3f <- plotYieldCurves(yieldTableDT, id_to_plot[6])

fig3a + fig3b + fig3c + fig3d + fig3e + fig3f + plot_layout(ncol = 3, nrow = 2, axes = "collect")

ggsave("pubFigures/figure3.png", width = 12)
