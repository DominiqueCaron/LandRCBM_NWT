# Characterizing study area
Require::Require(c("reproducible", "terra", "sf", "ggplot2", "tidyterra", "cowplot", "ggspatial"))

# Base layer
# read vector data using rnaturalearth
provinces <- prepInputs(url = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000b21a_e.zip",
                        destinationPath = "inputs")

land <- prepInputs(
  url = "https://naturalearth.s3.amazonaws.com/50m_physical/ne_50m_land.zip",
  destinationPath = "inputs",
  projectTo = provinces
)

# studya area:
studyArea = {
  # northwest territories boundaries
  nwt <- prepInputs(url = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000a21a_e.zip",
                    destinationPath = "inputs",
                    projectTo = provinces)
  nwt <- nwt[nwt$PRENAME == "Northwest Territories",]
  # ecozone: Taiga plains
  taigaPlains <- prepInputs(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                            destinationPath = "inputs", projectTo = nwt)
  taigaPlains <- taigaPlains[taigaPlains$ECOZONE == 4, ]
  sa <- postProcessTo(taigaPlains, cropTo = nwt, maskTo = nwt) |> 
    reproducible::Cache() |> 
    st_union() |>
    st_as_sf() |> st_buffer(-125)
  sa
}
rm(nwt, sa, taigaPlains)


p <- ggplot() +
  
  # first layer is the land mass outline
  # as a grey border (fancy)
  geom_sf(data = land,
          color = "grey50",
          fill = "#dfdfdf",
          size = 0.5) +
  geom_sf(data = provinces,
          color = "white",
          fill = "#dfdfdf",
          size = 0.2) +
  geom_sf(data = studyArea,
          color = "grey30",
          fill = "blue",
          alpha = 0.8) +
  
  # crop the whole thing to size
  coord_sf(xlim = c(3658201, 8000000),
           ylim = c(658873, 4500000)) +
  theme(
    plot.background = element_rect(fill = "lightblue", color = NA),
    panel.background = element_rect(fill = "lightblue", color = NA),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(0,0,-0.1,-0.1,"cm"), # <- set to negative to remove white border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", linewidth = 0.7, fill = NA)
  )

### main plot

# Add lancover:
LandCover <- prepInputs(
  url = "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_2022.zip",
  cropTo = st_buffer(studyArea, 1000),
  destinationPath = "inputs",
  overwrite = TRUE
) |> Cache()
names(LandCover) <- "cover"
rcl <- data.frame(is = c(0, 20, 31, 32, 33, 40, 50, 80, 81, 100, 210, 220, 230),
                  becomes = c(1, 2, 1, 1, 1, 1, 1, 3, 3, 1, 4, 5, 6))
LandCover <- classify(LandCover, rcl = rcl)
cls <- data.frame(id=c(1:6), 
                  cover=c("Non-forested land", 
                          "Water", 
                          "Wetland", 
                          "Coniferous forest", 
                          "Broadleaf forest", 
                          "Mixedwood forest")
                  )
cls <- cls[c(4, 5, 6, 3, 2, 1),]
levels(LandCover) <- cls

## Add management areas:
managementArea <- prepInputs(
  "~/../Downloads/MFUF_26july2016/MFUF_26july2016.shp",
  maskTo = studyArea,
  cropTo = studyArea,
  fun = sf::st_read,
  overwrite = TRUE
) |> Cache()
# rcl_MA <- data.frame(is = c(0, 11, 12, 13, 20, 31, 32, 33, 40, 50, 100),
#                      becomes = c(NaN, 1, 1, NaN, NaN, NaN, NaN, NaN ,NaN, NaN, NaN))
# managementArea  <- classify(managementArea, rcl = rcl_MA)
# cls_MA <- data.frame(id=c(1), 
#                      management =c("Managed Forest"))
# levels(managementArea) <- cls_MA
# 
# managementArea_poly <- as.polygons(managementArea) |> st_as_sf()
managementArea_poly <- managementArea[managementArea$ManagedFor == "Managed Forest", ]

ggplot() +
  geom_spatraster(data = LandCover, aes(fill = cover)) +
  scale_fill_manual(values = c("Non-forested land" = "#d2cdc0", "Water"= "#5475a8", "Wetland" = "#64b3d5", "Coniferous forest" = "#38814e", "Broadleaf forest" = "#85c77e", "Mixedwood forest" = "#d4e7b0"), na.translate = FALSE) +
  geom_sf(data = managementArea_poly, aes(color = ManagedFor), fill = "darkred", alpha = 0.25, linewidth = 0.5) +
  scale_color_manual(values = c("Managed Forest" = "darkred")) +
  geom_sf(data = studyArea,
          color = "grey10",
          fill = "transparent",
          linewidth = 1) +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black")
        )

ggdraw(
  ggplot() +
    geom_spatraster(data = LandCover, aes(fill = cover)) +
    scale_fill_manual(
      values = c(
        "Non-forested land" = "#d2cdc0",
        "Water" = "#5475a8",
        "Wetland" = "#64b3d5",
        "Coniferous forest" = "#38814e",
        "Broadleaf forest" = "#85c77e",
        "Mixedwood forest" = "#d4e7b0"
      ),
      na.translate = FALSE,
      name = "Land Cover"
    ) +
    geom_sf(
      data = managementArea_poly,
      aes(color = ManagedFor),
      fill = "darkred",
      alpha = 0.2,
      linewidth = 0.5
    ) +
    scale_color_manual(values = c("Managed Forest" = "darkred"), name = NULL) +
    geom_sf(
      data = studyArea,
      color = "grey10",
      fill = "transparent",
      linewidth = 0.75
    ) +
    coord_sf(expand = F) +
    annotation_scale(width_hint = 0.2, text_cex = 1) + 
    guides(
      fill = guide_legend(order = 1),
      color = guide_legend(order = 2)
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, colour = "black")
    )
) +
  draw_plot({
    p
  },
  x = 0.42,
  y = 0.8,
  width = 0.20,
  height = 0.19)
ggsave("pubFigures/figure1.png", width = 2244, height = 1683, units = "px", dpi = 300)
