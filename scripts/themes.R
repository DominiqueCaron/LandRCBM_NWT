# Common variables
base_size <- 12
line_width <- 0.9
point_size <- 1.6
managedForestColor <- "darkred"
unmanagedForestColor <- "#1b9e77"

# Custom theme for biplots
biplot_theme <- theme_classic(base_size = base_size) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key.height = unit(0.6, "lines"),
    legend.key.width = unit(2, "lines"),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(fill = "gray97", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    plot.margin = margin(6, 6, 6, 6)
  )

# Custom theme for maps
map_theme <- theme(
  legend.title = element_text(size = 11),
  legend.text = element_text(size = 10),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", linewidth = 0.7, fill = NA),
  panel.background = element_blank()
)

# Fill scale
fill_scale <- scale_fill_manual(
  values = c(
    "Managed forest" = managedForestColor,
    "Unmanaged forest" = unmanagedForestColor
  ),
  guide = "none"
)

# Color scale
color_scale <- scale_color_manual(
  values = c(
    "Managed forest" = managedForestColor,
    "Unmanaged forest" = unmanagedForestColor
  ),
  name = "Management",
  guide = "none"
)

# Linetype scale
linetype_scale <- scale_linetype_manual(
  values = c("Managed forest" = "solid", "Unmanaged forest" = "dashed"),
  guide = "none"
)
