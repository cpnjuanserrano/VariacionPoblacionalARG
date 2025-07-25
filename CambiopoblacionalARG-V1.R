# ───────────────────────
# CARGA DE LIBRERÍAS
# ───────────────────────
libs <- c("tidyverse", "terra", "giscoR", "sf", "viridis")
installed <- libs %in% rownames(installed.packages())
if (any(!installed)) install.packages(libs[!installed])
invisible(lapply(libs, library, character.only = TRUE))

# ───────────────────────
# CARGA DATASETS
# ───────────────────────

urls <- c(
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E1990_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E1990_GLOBE_R2023A_4326_30ss_V1_0.zip",
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.zip"
)

options(timeout = 300)

for (url in urls) {
  download.file(
    url = url,
    path = getwd(),
    destfile = basename(url)
  )
}

file.exists(basename(urls))
lapply(
  basename(urls),
  unzip
)



# ───────────────────────
# CARGAR RASTERS DE POBLACIÓN
# ───────────────────────
file_names <- list.files(pattern = "\\.tif$", full.names = TRUE)
if (length(file_names) < 2) stop("Se requieren al menos dos archivos .tif")
pop_rasters <- lapply(file_names, terra::rast)

# ───────────────────────
# CARGAR ARGENTINA + MALVINAS
# ───────────────────────
arg <- gisco_get_countries(country = "AR", resolution = "1")
malvinas <- gisco_get_countries(country = "FK", resolution = "1")
country_sf <- rbind(arg, malvinas) |> st_union()
country_sf <- st_transform(country_sf, crs = terra::crs(pop_rasters[[1]]))
country_vect <- terra::vect(country_sf)

# ───────────────────────
# RECORTAR LOS RASTERS A LA REGIÓN
# ───────────────────────
pop_cropped <- lapply(
  pop_rasters,
  function(x) terra::crop(x, country_vect, snap = "in", mask = TRUE)
)

# ───────────────────────
# DEFINIR CRS LAMBERT ARGENTINO
# ───────────────────────
crs_lambert <- "+proj=aea +lat_1=-52 +lat_2=-20 +lat_0=-36 +lon_0=-60 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# ───────────────────────
# CÁLCULO Y REPROYECCIÓN DEL CAMBIO
# ───────────────────────
pop_change <- (pop_cropped[[2]] - pop_cropped[[1]]) |> terra::project(crs_lambert)

# ───────────────────────
# CLASIFICACIÓN DEL CAMBIO
# ───────────────────────
get_categories <- function(x){
  terra::ifel(
    x == 0, 0,
    terra::ifel(x > 0, 1, -1)
  )
}
pop_change_cats <- get_categories(pop_change)

# ───────────────────────
# CONVERTIR A PUNTOS
# ───────────────────────
sp_points <- terra::as.points(pop_change_cats, na.rm = TRUE)
pop_points <- data.frame(terra::crds(sp_points), change_cat = as.vector(sp_points[[1]]))
colnames(pop_points)[3] <- "change_cat"

# ───────────────────────
# COLORES CONTRASTANTES
# ───────────────────────
cols <- c(
  "-1" = "#C9A66B",  # verde seco (declive)
  "0"  = "#F2F2F2",  # gris claro (sin cambio)
  "1"  = "#4C6B33"   # verde oliva oscuro (crecimiento)
)
labels <- c("-1" = "Declive", "0" = "Sin cambio", "1" = "Crecimiento")

# ───────────────────────
# VISUALIZACIÓN FINAL
# ───────────────────────
gg <- ggplot() +
  geom_point(
      data = pop_points,
      aes(x = x, y = y, color = as.factor(change_cat)),
      shape = 16, size = 0.1, alpha = 0.8
  ) +
  geom_sf(data = country_sf, fill = NA, color = NA) +  # sin borde gris
  scale_color_manual(
    name = "Cambio poblacional",
    values = cols,
    labels = labels
  ) +
  coord_sf(crs = crs_lambert) +
  labs(
    title = "Cambio de Población Argentina + Malvinas (1990–2020)",
    caption = "Fuente: Global Human Settlement Layer"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "#2C3E50"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, color = "grey50", hjust = .5)
  )

# ───────────────────────
# EXPORTAR LA IMAGEN
# ───────────────────────
ggsave("mapa_poblacion_arg_malvinas.png", plot = gg, width = 12, height = 14, dpi = 900,)
