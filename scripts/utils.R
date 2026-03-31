plotYieldCurves <- function(yieldTables, id){
  # estract the yieldCurves for the id selected
  yieldTables <- yieldTables[yieldTables$yieldTableIndex == id,]

  # prepare species-specific colors and name
  yieldTables$speciesName <- as.factor(LandR::sppEquivalencies_CA$EN_generic_full[match(
    yieldTables$speciesCode,
    LandR::sppEquivalencies_CA$LandR
  )])
  spCols <- LandR::sppEquivalencies_CA$colorHex[match(
    unique(yieldTables$speciesCode),
    LandR::sppEquivalencies_CA$LandR
  )]
  names(spCols) <- unique(yieldTables$speciesName)
  # make sure lodgepole pine gets the right color (avoid confusion with shore pine)
  if("Lodgepole pine" %in% names(spCols)) {
    spCols["Lodgepole pine"] <- "#02AD24"
  }

ggplot(yieldTables, aes(x = age, y = merch, colour = speciesName)) +
  geom_line(size = 1) +
  labs(y = "Merchantable biomass (tC/ha)", x = "Stand age (years)", colour = NULL) +
  scale_colour_manual(values = spCols) +
  scale_x_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.position = c(0.01, 0.99),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "black", fill = NA)
  )



}
