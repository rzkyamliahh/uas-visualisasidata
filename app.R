# Nama : Rezky Amaliah
# NIM : 222313344
# Kelas : 3SD1
# Dashboard Menyoroti NTT - Ketimpangan Kesejahteraan Nasional
# Topik: geospasial nasional, multivariat, dan hierarki

library(shiny)
library(bslib)
library(bsicons)
library(sf)
library(leaflet)
library(dplyr)
library(tidyr)
library(plotly)
library(classInt)
library(FactoMineR)
library(factoextra)
library(RColorBrewer)
library(DT)
library(readxl)
library(shinycssloaders)

# ---------------------------------------------------------------------
# Fungsi bantu
# ---------------------------------------------------------------------

norm_kab_ntt <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("\\s+", " ", x)
  x <- gsub("[.,'-]", "", x)
  x <- gsub("^KABUPATEN ", "", x)
  x <- gsub("^KAB\\. ", "", x)
  gsub("\\s+", "", x)
}

norm_nama <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("\\s+", " ", x)
  x <- gsub("[.,'-]", "", x)
  x <- gsub("TOBA SAMOSIR", "TOBA", x)
  x <- gsub("LABUHAN BATU SELATAN", "LABUHANBATUSELATAN", x)
  x <- gsub("LABUHAN BATU UTARA", "LABUHANBATUUTARA", x)
  x <- gsub("LABUHAN BATU", "LABUHANBATU", x)
  x <- gsub("PEMATANG SIANTAR", "PEMATANGSIANTAR", x)
  x <- gsub("SAWAH LUNTO", "SAWAHLUNTO", x)
  x <- gsub("BATANG HARI", "BATANGHARI", x)
  x <- gsub("BANYU ASIN", "BANYUASIN", x)
  x <- gsub("^KABUPATEN ", "", x)
  x <- gsub("^KAB\\. ", "", x)
  x <- gsub("^KOTA ", "", x)
  gsub("\\s+", "", x)
}

sumber_plotly <- function(p) {
  p %>%
    layout(
      annotations = list(
        text = SUMBER,
        x = 1, y = -0.12,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        xanchor = "right",
        font = list(size = 11, color = "#7A746B")
      )
    ) %>%
    config(responsive = TRUE, displayModeBar = FALSE)
}

kelas_pal <- function(x, metode = "quantile", n = 5) {
  xx <- x[is.finite(x)]

  if (length(xx) == 0) {
    return(colorBin(
      "viridis", domain = c(0, 1), bins = c(0, 1),
      na.color = "#E4DED3"
    ))
  }

  brks <- tryCatch(
    classIntervals(xx, n = n, style = metode)$brks,
    error = function(e) {
      quantile(
        xx,
        probs = seq(0, 1, length.out = n + 1),
        na.rm = TRUE
      )
    }
  )

  brks <- unique(brks)
  if (length(brks) < 2) {
    brks <- c(min(xx, na.rm = TRUE), max(xx, na.rm = TRUE) + 1e-8)
  }

  colorBin("viridis", domain = x, bins = brks, na.color = "#E4DED3")
}

# ---------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------

SUMBER <- "Sumber: bps.go.id"
HUB <- "Nusa Tenggara Timur"

nas <- read_excel("data/nasional_kabkota.xlsx") %>%
  mutate(
    nmprov = as.character(nmprov),
    nmkab = as.character(nmkab),
    kunci = norm_nama(paste(nmprov, nmkab))
  )

shp_raw <- st_read("data/shp_nasional_web.shp", quiet = TRUE)

prov_col <- names(shp_raw)[tolower(names(shp_raw)) == "wadmpr"]
if (length(prov_col) == 0) {
  prov_col <- names(shp_raw)[tolower(names(shp_raw)) %in% c("nmprov", "nmpov", "wadmpr")]
}

if (length(prov_col) == 0) {
  stop("Kolom provinsi pada SHP tidak ditemukan. Cek nama kolom shapefile.")
}

if (!"nmkab" %in% names(shp_raw)) {
  stop("Kolom 'nmkab' tidak ditemukan pada SHP nasional.")
}

shp_nas <- shp_raw %>%
  st_transform(4326) %>%
  transmute(
    nmkab_shp = as.character(nmkab),
    nmprov_shp = as.character(.data[[prov_col[1]]]),
    geometry = geometry,
    kunci = norm_nama(paste(nmprov_shp, nmkab_shp))
  )

tak_cocok <- setdiff(nas$kunci, shp_nas$kunci)
if (length(tak_cocok) > 0) {
  message(
    sprintf(
      "PERINGATAN: %d nama kab/kota nasional tidak cocok ke SHP:",
      length(tak_cocok)
    )
  )
  message(paste(tak_cocok, collapse = "\n"))
} else {
  message("OK: seluruh nama kab/kota nasional cocok dengan SHP.")
}

peta_nas_data <- shp_nas %>%
  left_join(nas, by = "kunci")

peta_nas_leaflet <- peta_nas_data %>%
  st_transform(3857) %>%
  st_simplify(dTolerance = 750, preserveTopology = TRUE) %>%
  st_transform(4326)

peta_nas_titik <- suppressWarnings(st_point_on_surface(peta_nas_leaflet))

batas_ntt <- peta_nas_leaflet %>%
  filter(nmprov == HUB) %>%
  st_union()

prov_ringkas <- nas %>%
  group_by(nmprov) %>%
  summarise(
    ipm = mean(ipm, na.rm = TRUE),
    kemiskinan_persen = mean(kemiskinan_persen, na.rm = TRUE),
    .groups = "drop"
  )

kpi_nas_ipm <- round(mean(nas$ipm, na.rm = TRUE), 1)
kpi_nas_miskin <- round(mean(nas$kemiskinan_persen, na.rm = TRUE), 1)
kpi_ntt_ipm <- round(prov_ringkas$ipm[prov_ringkas$nmprov == HUB], 1)
kpi_ntt_miskin <- round(prov_ringkas$kemiskinan_persen[prov_ringkas$nmprov == HUB], 1)
kpi_ntt_rank_ipm <- 5
kpi_ntt_rank_miskin <- 5
kpi_n_prov <- nrow(prov_ringkas)

kab <- read_excel("data/ntt_kabkota_panel.xlsx") %>%
  mutate(
    nmkab = as.character(nmkab),
    tahun = as.numeric(tahun),
    kunci_kab = norm_kab_ntt(nmkab)
  )

GUGUS_PULAU <- tibble::tribble(
  ~nmkab, ~gugus_pulau,
  "Kota Kupang", "Pulau Timor",
  "Kabupaten Kupang", "Pulau Timor",
  "Kabupaten Timor Tengah Selatan", "Pulau Timor",
  "Kabupaten Timor Tengah Utara", "Pulau Timor",
  "Kabupaten Belu", "Pulau Timor",
  "Kabupaten Malaka", "Pulau Timor",
  "Kabupaten Ende", "Pulau Flores",
  "Kabupaten Sikka", "Pulau Flores",
  "Kabupaten Flores Timur", "Pulau Flores",
  "Kabupaten Nagekeo", "Pulau Flores",
  "Kabupaten Ngada", "Pulau Flores",
  "Kabupaten Manggarai", "Pulau Flores",
  "Kabupaten Manggarai Barat", "Pulau Flores",
  "Kabupaten Manggarai Timur", "Pulau Flores",
  "Kabupaten Sumba Barat", "Pulau Sumba",
  "Kabupaten Sumba Barat Daya", "Pulau Sumba",
  "Kabupaten Sumba Tengah", "Pulau Sumba",
  "Kabupaten Sumba Timur", "Pulau Sumba",
  "Kabupaten Alor", "Kepulauan Alor-Lembata",
  "Kabupaten Lembata", "Kepulauan Alor-Lembata",
  "Kabupaten Rote Ndao", "Kepulauan Rote-Sabu",
  "Kabupaten Sabu Raijua", "Kepulauan Rote-Sabu"
) %>%
  mutate(kunci_kab = norm_kab_ntt(nmkab)) %>%
  select(kunci_kab, gugus_pulau) %>%
  distinct(kunci_kab, .keep_all = TRUE)

cek_gugus <- setdiff(unique(kab$kunci_kab), GUGUS_PULAU$kunci_kab)
if (length(cek_gugus) > 0) {
  message(
    "PERINGATAN: kab/kota NTT belum ada gugus pulau: ",
    paste(cek_gugus, collapse = ", ")
  )
}

# ---------------------------------------------------------------------
# Variabel dan tema
# ---------------------------------------------------------------------

var_label <- c(
  ipm = "IPM",
  kemiskinan_persen = "Penduduk miskin (%)",
  tpt = "TPT (%)",
  rls = "Rata-rata lama sekolah (thn)",
  hls = "Harapan lama sekolah (thn)",
  pengeluaran_perkapita = "Pengeluaran per kapita (ribu Rp/thn)",
  akses_sanitasi = "Akses sanitasi layak (%)",
  akses_air_minum = "Akses air minum layak (%)"
)

var_label_short <- c(
  ipm = "IPM",
  kemiskinan_persen = "Miskin (%)",
  tpt = "TPT (%)",
  rls = "RLS (thn)",
  hls = "HLS (thn)",
  pengeluaran_perkapita = "Pengeluaran/kapita",
  akses_sanitasi = "Sanitasi layak (%)",
  akses_air_minum = "Air minum layak (%)"
)

var_numerik <- names(var_label)
var_arah <- c(
  ipm = 1,
  kemiskinan_persen = -1,
  tpt = -1,
  rls = 1,
  hls = 1,
  pengeluaran_perkapita = 1,
  akses_sanitasi = 1,
  akses_air_minum = 1
)

tahun_all <- sort(unique(kab$tahun))
tahun_max <- max(tahun_all, na.rm = TRUE)

kab_terendah <- kab %>%
  filter(tahun == tahun_max) %>%
  arrange(ipm) %>%
  slice(1) %>%
  pull(nmkab)

kab_tertinggi <- kab %>%
  filter(tahun == tahun_max) %>%
  arrange(desc(ipm)) %>%
  slice(1) %>%
  pull(nmkab)

WARNA <- list(
  teal = "#0E7C66",
  teal_dk = "#0A5A4B",
  amber = "#E8A33D",
  coral = "#E2583E",
  cream = "#FFFBF2",
  ink = "#2B2621"
)

tema <- bs_theme(
  version = 5,
  bg = WARNA$cream,
  fg = WARNA$ink,
  primary = WARNA$teal,
  secondary = WARNA$amber,
  success = WARNA$teal,
  info = "#3D7A8A",
  warning = WARNA$amber,
  danger = WARNA$coral,
  base_font = font_google("Plus Jakarta Sans"),
  heading_font = font_google("Fraunces"),
  "border-radius" = "0.9rem"
) %>%
  bs_add_rules(sprintf(
    ".navbar-brand { font-family: 'Fraunces', serif; font-weight: 600; letter-spacing: .2px; }
     .nav-link { font-weight: 500; }
     .bslib-value-box { border: none; box-shadow: 0 2px 10px rgba(43,38,33,.08); }
     .card, .bslib-card { border: none; box-shadow: 0 2px 10px rgba(43,38,33,.06); border-radius: 1rem; }
     .btn-primary { background: %s; border-color: %s; }
     .form-check-input:checked { background-color: %s; border-color: %s; }
     hr { border-top: 1px solid rgba(43,38,33,.1); }",
    WARNA$teal, WARNA$teal, WARNA$teal, WARNA$teal
  ))

kpi_box <- function(title, value, subtitle, icon, warna = WARNA$teal) {
  value_box(
    title = title,
    value = value,
    showcase = bs_icon(icon, size = "2rem"),
    theme = value_box_theme(bg = warna, fg = "white"),
    p(subtitle, style = "opacity:.9; font-size:.85rem;")
  )
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------

ui <- tagList(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      html, body { width: 100%; overflow-x: hidden; }
      .container-fluid { width: 100%; }
      .leaflet-container { width: 100% !important; }
      .leaflet-control-layers {
        max-width: min(280px, 78vw);
        font-size: 0.88rem;
      }
      .leaflet-control-zoom {
        margin-top: 8px !important;
        margin-left: 8px !important;
      }
      .plotly, .html-widget { max-width: 100% !important; }
      .card, .bslib-card { max-width: 100%; }
      @media (max-width: 768px) {
      #peta_nas {
        height: 55vh !important;
        min-height: 380px;
      }
        .navbar-brand {
          font-size: 0.95rem;
          line-height: 1.15;
          white-space: normal;
          max-width: 72vw;
        }
        .navbar-toggler { margin-right: 0.35rem; }
        .container-fluid { padding-left: 0.7rem !important; padding-right: 0.7rem !important; }
        .bslib-sidebar-layout {
          --bslib-sidebar-width: 100% !important;
        }
        .bslib-sidebar-layout > .sidebar {
          width: 100% !important;
          max-width: 100% !important;
        }
        .bslib-sidebar-layout > .main {
          min-width: 0 !important;
        }
        .bslib-grid {
          grid-template-columns: 1fr !important;
        }
        .value-box { min-height: 110px; }
        .leaflet-container { min-height: 430px !important; }
        .leaflet-control-layers {
          max-width: 72vw;
          font-size: 0.82rem;
        }
        .leaflet-control-attribution { font-size: 8px; }
        .plot-container, .js-plotly-plot, .plotly {
          width: 100% !important;
        }
        .nav-pills {
          overflow-x: auto;
          flex-wrap: nowrap !important;
          -webkit-overflow-scrolling: touch;
        }
        .nav-pills .nav-link { white-space: nowrap; }
        .dt-container { overflow-x: auto; }
      }
      @media (min-width: 769px) {
        .bslib-sidebar-layout > .sidebar { max-width: 320px; }
      }
    "))
  ),
  page_navbar(
  title = tagList(
    bs_icon("globe-asia-australia"),
    " Menyoroti NTT · Ketimpangan Kesejahteraan Nasional "
  ),
  theme = tema,
  fillable = FALSE,
  navbar_options = navbar_options(bg = WARNA$teal_dk),

  nav_panel(
    title = "Peta Nasional",
    icon = bs_icon("map"),
    div(
      class = "container-fluid py-3",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        kpi_box("Rata-rata IPM Nasional", kpi_nas_ipm,
                "rata-rata kab/kota se-Indonesia", "bar-chart-line", WARNA$teal),
        kpi_box("Rata-rata Kemiskinan", paste0(kpi_nas_miskin, "%"),
                "rata-rata kab/kota se-Indonesia", "percent", WARNA$coral),
        kpi_box("IPM NTT", kpi_ntt_ipm,
                sprintf("peringkat %d dari %d provinsi (terendah)",
                        kpi_ntt_rank_ipm, kpi_n_prov), "geo-alt", WARNA$amber),
        kpi_box("Kemiskinan NTT", paste0(kpi_ntt_miskin, "%"),
                sprintf("peringkat %d termiskin se-Indonesia", kpi_ntt_rank_miskin),
                "exclamation-triangle", WARNA$teal_dk)
      ),
      layout_sidebar(
        sidebar = sidebar(
          width = 300,
          selectInput(
            "var_peta", "Indikator:",
            choices = c("IPM" = "ipm", "Penduduk miskin (%)" = "kemiskinan_persen")
          ),
          selectInput(
            "metode_kab", "Klasifikasi kelas:",
            choices = c(
              "Kuantil" = "quantile",
              "Jenks (natural breaks)" = "jenks",
              "Interval sama" = "equal"
            )
          ),
          checkboxInput(
            "simbol_kab",
            "Tambah simbol proporsional (jml penduduk miskin)",
            TRUE
          ),
          checkboxInput("sorot_ntt", "Sorot batas Provinsi NTT", TRUE),
          hr(),
          helpText(
            "Koroplet menampilkan indikator rasio/indeks. Simbol proporsional " ,
            "menampilkan jumlah penduduk miskin absolut. NTT diberi batas tebal " ,
            "sebagai wilayah fokus analisis lanjutan."
          )
        ),
        withSpinner(
          leafletOutput("peta_nas", height = "620px"),
          color = WARNA$teal,
          type = 6
        ),
        helpText(SUMBER)
      )
    )
  ),

  nav_panel(
    title = "Multivariat",
    icon = bs_icon("diagram-3"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        checkboxGroupInput(
          "tahun_mv", "Tahun:",
          choices = tahun_all,
          selected = tahun_all,
          inline = TRUE
        ),
        selectizeInput(
          "var_mv", "Variabel (minimal 8):",
          choices = setNames(var_numerik, var_label),
          multiple = TRUE,
          selected = var_numerik
        ),
        sliderInput("k_klaster", "Jumlah klaster:", 2, 6, 3, 1),
        hr(),
        helpText(
          "Pilih satu atau beberapa tahun untuk memperbanyak observasi. " ,
          "Gunakan box-select pada PCA untuk brushing & linking dengan " ,
          "parallel coordinates."
        )
      ),
      navset_card_pill(
        nav_panel(
          "PCA + Parallel Coordinates",
          layout_columns(
            col_widths = c(6, 6),
            withSpinner(plotlyOutput("pca_scatter", height = "500px"),
                        color = WARNA$teal, type = 6),
            withSpinner(plotlyOutput("parcoord", height = "500px"),
                        color = WARNA$teal, type = 6)
          ),
          helpText(SUMBER)
        ),
        nav_panel(
          "Heatmap Terklaster",
          withSpinner(plotOutput("heatmap_cluster", height = "620px"),
                      color = WARNA$teal, type = 6),
          helpText(SUMBER)
        ),
        nav_panel(
          "Arah Variabel (Loadings)",
          withSpinner(plotOutput("pca_var", height = "530px"),
                      color = WARNA$teal, type = 6),
          helpText(SUMBER)
        ),
        nav_panel(
          "Kelompok & Pencilan",
          h5(bs_icon("collection"), " Rata-rata variabel per klaster"),
          DTOutput("tbl_klaster"),
          br(),
          h5(bs_icon("exclamation-diamond"),
             " Observasi pencilan (Mahalanobis, α = 0,025)"),
          DTOutput("tbl_pencilan"),
          helpText(SUMBER)
        )
      )
    )
  ),

  nav_panel(
    title = "Struktur Wilayah",
    icon = bs_icon("diagram-2"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        radioButtons(
          "jenis_hier", "Representasi:",
          choices = c("Treemap" = "treemap", "Sunburst" = "sunburst")
        ),
        hr(),
        helpText(
          "Hierarki: NTT → Gugus Pulau → Kab/Kota. Ukuran = jumlah penduduk; " ,
          "warna = persentase kemiskinan. Gugus pulau merupakan pengelompokan " ,
          "geografis non-BPS."
        )
      ),
      withSpinner(plotlyOutput("hier_plot", height = "640px"),
                  color = WARNA$teal, type = 6),
      helpText(SUMBER)
    )
  ),

  nav_panel(
    title = "Fokus Wilayah",
    icon = bs_icon("binoculars"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput(
          "kab_fokus", "Wilayah fokus:",
          choices = sort(unique(kab$nmkab)),
          selected = kab_terendah
        ),
        selectInput(
          "kab_banding", "Pembanding:",
          choices = sort(unique(kab$nmkab)),
          selected = kab_tertinggi
        ),
        hr(),
        helpText(
          "Wilayah fokus secara default adalah kab/kota ber-IPM terendah " ,
          "pada tahun terbaru."
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        withSpinner(plotlyOutput("fokus_bar", height = "440px"),
                    color = WARNA$teal, type = 6),
        withSpinner(plotlyOutput("fokus_tren", height = "440px"),
                    color = WARNA$teal, type = 6)
      ),
      card(
        card_header(bs_icon("lightbulb"),
                    " Rekomendasi indikatif berdasarkan 3 indikator terlemah"),
        uiOutput("fokus_rekom")
      ),
      helpText(SUMBER)
    )
  ),

  )
)

# ---------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------

server <- function(input, output, session) {

  # Peta nasional ------------------------------------------------------

  output$peta_nas <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        preferCanvas = TRUE,
        zoomControl = TRUE
      )
    ) %>%
      addProviderTiles(
        providers$OpenStreetMap,
        group = "OpenStreetMap"
      ) %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Citra Satelit"
      ) %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "Citra Satelit"
        ),
        overlayGroups = c(
          "Koroplet",
          "Simbol proporsional",
          "Batas NTT (fokus)"
        ),
        options = layersControlOptions(
          collapsed = TRUE,
          position = "topright"
        )
      ) %>%
      addControl(SUMBER, position = "bottomleft", layerId = "sumber_ctl")
  })

  update_koroplet <- function() {
    req(input$var_peta)
    d <- peta_nas_leaflet
    v <- d[[input$var_peta]]
    lab_v <- if (input$var_peta == "ipm") "IPM" else "Penduduk miskin (%)"
    pal <- kelas_pal(v, input$metode_kab)
    lab <- sprintf("<b>%s</b><br>%s: %s<br><i>%s</i>", d$nmkab, lab_v, round(v, 2), SUMBER)

    leafletProxy("peta_nas") %>%
      clearGroup("Koroplet") %>%
      removeControl("legend_peta") %>%
      addPolygons(
        data = d, fillColor = pal(v), fillOpacity = 0.82,
        color = "white", weight = 0.35, smoothFactor = 0.5,
        label = lapply(lab, HTML), group = "Koroplet",
        highlightOptions = highlightOptions(weight = 2, color = WARNA$ink, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = v, title = lab_v,
                position = "bottomright", layerId = "legend_peta")
  }

  observeEvent(
    list(input$var_peta, input$metode_kab),
    update_koroplet(),
    ignoreInit = FALSE
  )

  observeEvent(input$simbol_kab, {
    proxy <- leafletProxy("peta_nas") %>% clearGroup("Simbol proporsional")
    if (isTRUE(input$simbol_kab)) {
      cen <- peta_nas_titik
      radius <- ifelse(is.na(cen$jml_miskin_ribu), 3,
                       pmin(sqrt(pmax(cen$jml_miskin_ribu, 0)) * 0.5, 18))
      proxy %>% addCircleMarkers(
        data = cen, radius = radius, color = WARNA$coral, weight = 1,
        fillColor = WARNA$coral, fillOpacity = 0.45,
        label = lapply(sprintf("<b>%s</b><br>Penduduk miskin: %s ribu jiwa",
                               cen$nmkab, round(cen$jml_miskin_ribu, 2)), HTML),
        group = "Simbol proporsional"
      )
    }
  }, ignoreInit = FALSE)

  observeEvent(input$sorot_ntt, {
    proxy <- leafletProxy("peta_nas") %>% clearGroup("Batas NTT (fokus)")
    if (isTRUE(input$sorot_ntt) && length(batas_ntt) > 0) {
      proxy %>% addPolylines(data = batas_ntt, color = WARNA$ink,
                             weight = 3, opacity = 1,
                             group = "Batas NTT (fokus)")
    }
  }, ignoreInit = FALSE)

  # Multivariat --------------------------------------------------------

  mv_data <- reactive({
    validate(
      need(length(input$var_mv) >= 8, "Pilih minimal 8 variabel."),
      need(length(input$tahun_mv) >= 1, "Pilih minimal 1 tahun.")
    )

    d <- kab %>%
      filter(tahun %in% input$tahun_mv) %>%
      select(nmkab, tahun, all_of(input$var_mv)) %>%
      drop_na()

    d$id <- paste(d$nmkab, d$tahun, sep = " | ")

    validate(
      need(
        nrow(d) >= 8,
        sprintf(
          "Data yang tersedia hanya %d observasi. Minimal 8 observasi diperlukan untuk menjalankan PCA dan klaster.",
          nrow(d)
        )
      )
    )

    d
  })

  pca_fit <- reactive({
    d <- mv_data()
    X <- as.data.frame(d[, input$var_mv]) %>%
      mutate(across(everything(), as.numeric))

    keep <- vapply(
      X,
      function(x) {
        x <- x[is.finite(x)]
        length(x) > 1 && sd(x) > 0
      },
      logical(1)
    )

    validate(
      need(sum(keep) >= 2,
           "Minimal 2 variabel dengan variasi data diperlukan untuk PCA.")
    )

    X <- X[, keep, drop = FALSE]

    PCA(
      X,
      scale.unit = TRUE,
      graph = FALSE,
      ncp = min(4, ncol(X), nrow(X) - 1)
    )
  })

  skor <- reactive({
    d <- mv_data()
    p <- pca_fit()

    sc <- as.data.frame(p$ind$coord[, 1:2])
    names(sc) <- c("PC1", "PC2")

    k <- min(input$k_klaster, nrow(sc) - 1)
    validate(need(k >= 2, "Minimal 2 klaster diperlukan."))

    set.seed(2026)
    km <- kmeans(sc, centers = k, nstart = 20)
    sc$klaster <- factor(km$cluster)

    md <- mahalanobis(
      sc[, c("PC1", "PC2")],
      colMeans(sc[, c("PC1", "PC2")]),
      cov(sc[, c("PC1", "PC2")])
    )
    sc$pencilan <- md > qchisq(0.975, df = 2)

    cbind(d[, c("id", "nmkab", "tahun")], sc)
  })

  output$pca_scatter <- renderPlotly({
    s <- skor()
    ev <- round(pca_fit()$eig[1:2, 2], 1)

    plot_ly(
      s,
      x = ~PC1,
      y = ~PC2,
      color = ~klaster,
      colors = "Dark2",
      symbol = ~ifelse(pencilan, "Pencilan", "Normal"),
      symbols = c("circle", "diamond"),
      key = ~id,
      source = "pca",
      type = "scatter",
      mode = "markers",
      text = ~paste0(nmkab, " (", tahun, ")"),
      hoverinfo = "text",
      marker = list(size = 9, opacity = 0.85)
    ) %>%
      layout(
        dragmode = "select",
        title = list(
          text = "PCA — pilih wilayah untuk melihat keterkaitannya",
          font = list(size = 13)
        ),
        xaxis = list(title = paste0("PC1 (", ev[1], "%)")),
        yaxis = list(title = paste0("PC2 (", ev[2], "%)"))
      ) %>%
      sumber_plotly() %>%
      event_register("plotly_selected")
  })

  sel_id <- reactive({
    ev <- event_data("plotly_selected", source = "pca")
    if (is.null(ev) || length(ev) == 0) NULL else ev$key
  })

  output$parcoord <- renderPlotly({
    d <- mv_data()
    ids <- sel_id()

    flag <- if (is.null(ids)) {
      rep(1, nrow(d))
    } else {
      as.numeric(d$id %in% ids)
    }

    o <- order(flag, decreasing = TRUE)
    d <- d[o, , drop = FALSE]
    flag <- flag[o]

    cs <- if (is.null(ids)) {
      list(list(0, WARNA$teal), list(1, WARNA$teal))
    } else {
      list(list(0, "#D9D2C5"), list(1, WARNA$coral))
    }

    dims <- lapply(input$var_mv, function(v) {
      nilai <- as.numeric(d[[v]])
      list(
        label = var_label_short[[v]],
        values = nilai,
        range = range(nilai, na.rm = TRUE),
        tickformat = ".1f"
      )
    })

    plot_ly(
      type = "parcoords",
      line = list(
        color = flag,
        colorscale = cs,
        cmin = 0,
        cmax = 1,
        showscale = FALSE
      ),
      dimensions = dims,
      labelfont = list(size = 10, color = WARNA$ink),
      tickfont = list(size = 9, color = "#6F685F"),
      labelside = "top",
      labelangle = -20
    ) %>%
      layout(
        title = list(
          text = "Parallel Coordinates — oranye = terseleksi",
          font = list(size = 13)
        ),
        margin = list(l = 50, r = 30, t = 90, b = 40)
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE) %>%
      sumber_plotly()
  })

  output$heatmap_cluster <- renderPlot({
    d <- mv_data()
    mat <- scale(as.matrix(d[, input$var_mv]))
    rownames(mat) <- d$id
    colnames(mat) <- unname(var_label[input$var_mv])

    heatmap(
      mat,
      scale = "none",
      col = colorRampPalette(rev(brewer.pal(11, "RdBu")))(60),
      cexRow = 0.55,
      cexCol = 0.8,
      margins = c(12, 8)
    )
  })

  output$pca_var <- renderPlot({
    fviz_pca_var(
      pca_fit(),
      repel = TRUE,
      col.var = "contrib",
      gradient.cols = c(WARNA$teal, WARNA$coral)
    )
  })

  output$tbl_klaster <- renderDT({
    mv_data() %>%
      left_join(skor()[, c("id", "klaster")], by = "id") %>%
      group_by(klaster) %>%
      summarise(
        n = n(),
        across(all_of(input$var_mv), ~ round(mean(.x, na.rm = TRUE), 2)),
        .groups = "drop"
      ) %>%
      datatable(
        options = list(scrollX = TRUE, dom = "t"),
        rownames = FALSE
      )
  })

  output$tbl_pencilan <- renderDT({
    skor() %>%
      filter(pencilan) %>%
      select(nmkab, tahun, klaster, PC1, PC2) %>%
      mutate(across(c(PC1, PC2), ~ round(.x, 2))) %>%
      datatable(
        options = list(scrollX = TRUE, dom = "tp"),
        rownames = FALSE
      )
  })

  # Hierarki -----------------------------------------------------------

  hier_df <- reactive({
    kk <- kab %>%
      filter(tahun == tahun_max) %>%
      distinct(kunci_kab, .keep_all = TRUE) %>%
      select(-any_of(c("gugus_pulau", "gugus_pulau.x", "gugus_pulau.y"))) %>%
      left_join(GUGUS_PULAU, by = "kunci_kab") %>%
      mutate(gugus_pulau = coalesce(gugus_pulau, "Lainnya"))

    missing_gugus <- kk %>%
      filter(gugus_pulau == "Lainnya") %>%
      distinct(nmkab) %>%
      pull(nmkab)

    validate(
      need(
        length(missing_gugus) == 0,
        paste0(
          "Kab/kota berikut belum memiliki gugus pulau: ",
          paste(missing_gugus, collapse = ", ")
        )
      )
    )

    root <- data.frame(
      ids = HUB,
      labels = HUB,
      parents = "",
      values = sum(kk$populasi, na.rm = TRUE),
      kemiskinan_persen = weighted.mean(
        kk$kemiskinan_persen, kk$populasi, na.rm = TRUE
      )
    )

    l2 <- kk %>%
      group_by(gugus_pulau) %>%
      summarise(
        values = sum(populasi, na.rm = TRUE),
        kemiskinan_persen = weighted.mean(
          kemiskinan_persen, populasi, na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      transmute(
        ids = paste(HUB, gugus_pulau, sep = " / "),
        labels = gugus_pulau,
        parents = HUB,
        values,
        kemiskinan_persen
      )

    l3 <- kk %>%
      transmute(
        ids = paste(HUB, gugus_pulau, nmkab, sep = " / "),
        labels = nmkab,
        parents = paste(HUB, gugus_pulau, sep = " / "),
        values = populasi,
        kemiskinan_persen
      )

    bind_rows(root, l2, l3)
  })

  output$hier_plot <- renderPlotly({
    h <- hier_df()

    mk <- list(
      colors = h$kemiskinan_persen,
      colorscale = "YlOrRd",
      showscale = TRUE,
      colorbar = list(title = "Kemiskinan<br>(%)")
    )

    p <- plot_ly(
      ids = h$ids,
      labels = h$labels,
      parents = h$parents,
      values = h$values,
      type = input$jenis_hier,
      branchvalues = "total",
      marker = mk,
      customdata = round(h$kemiskinan_persen, 1),
      hovertemplate = paste0(
        "<b>%{label}</b><br>Penduduk: %{value:,.0f}",
        "<br>Kemiskinan: %{customdata}%<extra></extra>"
      )
    )

    if (input$jenis_hier == "treemap") {
      p <- p %>% style(pathbar = list(visible = TRUE))
    }

    sumber_plotly(p)
  })

  # Fokus wilayah ------------------------------------------------------

  fokus_long <- reactive({
    kab %>%
      filter(tahun == tahun_max) %>%
      mutate(across(all_of(var_numerik), as.numeric)) %>%
      mutate(across(all_of(var_numerik), ~ as.numeric(scale(.x)))) %>%
      pivot_longer(
        all_of(var_numerik),
        names_to = "var",
        values_to = "z"
      ) %>%
      mutate(
        z = z * unname(var_arah[var]),
        label = unname(var_label[var])
      )
  })

  output$fokus_bar <- renderPlotly({
    l <- fokus_long() %>%
      filter(nmkab %in% c(input$kab_fokus, input$kab_banding))

    plot_ly(
      l,
      x = ~z,
      y = ~label,
      color = ~nmkab,
      colors = c(WARNA$coral, WARNA$teal),
      type = "bar",
      orientation = "h",
      hovertemplate = "%{y}: %{x:.2f}<extra></extra>"
    ) %>%
      layout(
        barmode = "group",
        title = list(
          text = paste0(
            "Skor relatif ", tahun_max,
            " (0 = rata-rata NTT; kanan = lebih baik)"
          ),
          font = list(size = 13)
        ),
        xaxis = list(title = "z-score (arah sudah diseragamkan)"),
        yaxis = list(title = "")
      ) %>%
      sumber_plotly()
  })

  output$fokus_tren <- renderPlotly({
    rata <- kab %>%
      group_by(tahun) %>%
      summarise(ipm = mean(ipm, na.rm = TRUE), .groups = "drop")

    l <- kab %>%
      filter(nmkab %in% c(input$kab_fokus, input$kab_banding))

    plot_ly() %>%
      add_trace(
        data = l,
        x = ~tahun,
        y = ~ipm,
        color = ~nmkab,
        colors = c(WARNA$coral, WARNA$teal),
        type = "scatter",
        mode = "lines+markers"
      ) %>%
      add_trace(
        data = rata,
        x = ~tahun,
        y = ~ipm,
        name = "Rata-rata kab/kota",
        type = "scatter",
        mode = "lines",
        line = list(dash = "dash", color = "#8A8378")
      ) %>%
      layout(
        title = list(text = "Tren IPM", font = list(size = 13)),
        xaxis = list(title = "Tahun", dtick = 1),
        yaxis = list(title = "IPM")
      ) %>%
      sumber_plotly()
  })

  rekom <- c(
    rls = "Perkuat akses dan retensi sekolah melalui afirmasi pendidikan, transportasi/asrama, dan pencegahan putus sekolah.",
    hls = "Perluas layanan PAUD dan pendidikan menengah serta program penjangkauan agar harapan lama sekolah meningkat.",
    kemiskinan_persen = "Perkuat perlindungan sosial tepat sasaran dan program pemberdayaan ekonomi rumah tangga miskin.",
    tpt = "Perluas lapangan kerja lokal, pelatihan vokasi sesuai kebutuhan sektor unggulan, dan penempatan kerja terampil.",
    pengeluaran_perkapita = "Tingkatkan pendapatan melalui nilai tambah komoditas unggulan lokal, penguatan UMKM, dan akses pasar.",
    akses_sanitasi = "Percepat sanitasi layak pada kabupaten/kota dengan cakupan rendah.",
    akses_air_minum = "Perluas jaringan dan sumber air minum layak pada kabupaten/kota dengan cakupan rendah."
  )

  output$fokus_rekom <- renderUI({
    w <- fokus_long() %>%
      filter(nmkab == input$kab_fokus, var != "ipm") %>%
      arrange(z) %>%
      slice_head(n = 3)

    if (nrow(w) == 0) {
      return(tags$p("Data indikator wilayah fokus belum tersedia."))
    }

    tags$ol(
      lapply(
        seq_len(nrow(w)),
        function(i) {
          tags$li(
            tags$b(w$label[i]),
            sprintf(" (z = %.2f): ", w$z[i]),
            rekom[[w$var[i]]]
          )
        }
      )
    )
  })
}

shinyApp(ui = ui, server = server)
