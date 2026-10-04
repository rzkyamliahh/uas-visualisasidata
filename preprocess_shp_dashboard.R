# Jalankan script ini SEKALI di komputer lokal.
# Tujuannya membuat SHP versi ringan untuk shinyapps.io.

library(sf)

input <- "data/shp_nasional.shp"
output <- "data/shp_nasional_web.shp"

cat("Membaca SHP nasional...\n")
shp <- st_read(input, quiet = TRUE)

cat("Memperbaiki geometri...\n")
shp <- st_make_valid(shp)

cat("Transformasi dan simplifikasi...\n")
shp_web <- shp %>%
  st_transform(3857) %>%
  st_simplify(dTolerance = 750, preserveTopology = TRUE) %>%
  st_transform(4326)

cat("Menyimpan SHP ringan...\n")
st_write(
  shp_web,
  output,
  delete_dsn = TRUE,
  quiet = TRUE
)

cat("SELESAI:\n")
cat(normalizePath(output), "\n")
cat("Jumlah fitur:", nrow(shp_web), "\n")
