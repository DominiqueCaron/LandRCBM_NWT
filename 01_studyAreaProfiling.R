# Characterizing study area
Require::Require(c("reproducible", "terra", "sf", "LandR", "units", "dplyr"))

# polygon of the study area: intersection of NWT and 
studyArea = {
  # northwest territories boundaries
  nwt <- prepInputs(url = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000a21a_e.zip",
                                  destinationPath = "inputs")
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

# Total area (1m2 = 0.0001 ha)
studyAreaHa <- st_area(studyArea) |> set_units("ha")

## NTEMS LandCover:
LandCover <- prepInputs(
  url = "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_2022.zip",
  maskTo = studyArea,
  cropTo = studyArea,
  destinationPath = "inputs",
  overwrite = TRUE
) |> Cache()
# Count frequency of each value (excluding NAs)
freq_table <- freq(LandCover)

# Get proportions
freq_table$proportion <- freq_table$count / sum(freq_table$count)

# Management area
managementArea <- prepInputs(
 "~/../Downloads/Canada_MFv2020/Canada_MFv2020.tif",
  maskTo = studyArea,
  cropTo = studyArea,
  destinationPath = "inputs",
 fun = terra::rast,
  overwrite = TRUE
) |> Cache()
# Count frequency of each value (excluding NAs)
freq_table <- freq(managementArea)

# Get proportions
freq_table$proportion <- freq_table$count / sum(freq_table$count)

# get area in ha for each management  
area <- expanse(managementArea, unit = "ha", byValue = TRUE)

# Disturbances (get the mean per year x type; sample every 5 years between 1985 and 2024):
disturbances <- c()
for (year in c(1985:2024)){
  disturbances_year <- prepInputs(
    url = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canlad_including_insect_defoliation/v1/Disturbances_Time_Series/canlad_annual_", year, "_v1.tif"),
    maskTo = studyArea,
    cropTo = studyArea,
    destinationPath = "inputs",
    fun = terra::rast,
    overwrite = TRUE
  )
  disturbances_year[disturbances_year == 0] <- NA
  cell_area <- prod(res(disturbances_year)) * 10^-4
  area_year <- freq(disturbances_year)
  disturbances <- rbind(disturbances,
                        data.frame(year = year,
                                   disturbanceID = area_year$value,
                                   area = area_year$count)
                        )
}

disturbance_summary <- disturbances %>%
  group_by(disturbanceID) %>%
  summarise(
    mean_area = mean(area, na.rm = TRUE),
    sd_area   = sd(area, na.rm = TRUE),
    min = min(area, na.rm = TRUE),
    max = max(area, na.rm = TRUE),
    .groups = "drop"
  )
