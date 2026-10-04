# Dashboard Visualisasi Ketimpangan Kesejahteraan Indonesia

Dashboard interaktif berbasis **R Shiny** untuk memvisualisasikan ketimpangan kesejahteraan antarwilayah di Indonesia dengan fokus khusus pada **Provinsi Nusa Tenggara Timur (NTT)**.

Dashboard ini dikembangkan sebagai bagian dari tugas **UAS Visualisasi Data dan Informasi 2026** dengan memanfaatkan data dari **Badan Pusat Statistik (BPS)**.

## 🌐 Live Dashboard

Dashboard dapat diakses secara online melalui:

https://rzkyamliahh.shinyapps.io/dashboard-ntt/

---

## 📌 Tujuan

Dashboard ini bertujuan untuk menyajikan informasi mengenai kondisi dan ketimpangan kesejahteraan antarwilayah secara interaktif melalui beberapa pendekatan visualisasi.

Fokus analisis mencakup:

- Perbandingan indikator kesejahteraan kabupaten/kota di Indonesia.
- Analisis karakteristik multidimensi kabupaten/kota di Nusa Tenggara Timur.
- Visualisasi struktur wilayah NTT berdasarkan gugus pulau.
- Identifikasi kelompok wilayah dengan karakteristik yang relatif serupa maupun berbeda.

---

## 📊 Topik Visualisasi

Dashboard mencakup tiga jenis visualisasi utama.

### 1. Geospatial Visualization

Visualisasi spasial digunakan untuk melihat persebaran indikator kesejahteraan pada tingkat kabupaten/kota di Indonesia.

Fitur yang tersedia:

- Peta koroplet.
- Simbol proporsional.
- Highlight wilayah Nusa Tenggara Timur.
- Tooltip informasi wilayah.
- Zoom dan pan.
- Pilihan base map:
  - OpenStreetMap
  - Citra Satelit
- Layer control untuk mengatur tampilan peta.

---

### 2. Multivariate Visualization

Analisis multivariat difokuskan pada kabupaten/kota di Provinsi Nusa Tenggara Timur.

Visualisasi yang digunakan meliputi:

- **Principal Component Analysis (PCA)**
- **Parallel Coordinates**
- **Clustered Heatmap**
- Analisis loading PCA.
- K-Means clustering.
- Identifikasi observasi yang memiliki karakteristik berbeda menggunakan pendekatan jarak Mahalanobis.
- Tabel informasi kabupaten/kota.

Visualisasi digunakan untuk membantu melihat hubungan antarindikator serta pola kemiripan dan perbedaan karakteristik antarwilayah.

---

### 3. Hierarchical Visualization

Visualisasi hierarki menggambarkan struktur wilayah Nusa Tenggara Timur dari tingkat:

**Provinsi → Gugus Pulau → Kabupaten/Kota**

Representasi yang digunakan:

- Treemap.
- Sunburst.

Ukuran dan warna digunakan untuk merepresentasikan indikator yang dipilih sehingga pengguna dapat mengeksplorasi struktur wilayah secara interaktif.

---

## 🗺️ Fokus Wilayah

Provinsi **Nusa Tenggara Timur (NTT)** menjadi fokus utama analisis karena dashboard tidak hanya menampilkan kondisi wilayah secara nasional, tetapi juga menyediakan eksplorasi lebih mendalam terhadap kabupaten/kota di NTT.

Analisis pada wilayah NTT mencakup:

- Perbandingan indikator antar kabupaten/kota.
- Perubahan indikator berdasarkan tahun.
- Pengelompokan wilayah.
- Struktur wilayah berdasarkan gugus pulau.
- Identifikasi wilayah dengan karakteristik yang berbeda.

---

## 🗃️ Data

Dashboard menggunakan data statistik dan data spasial yang disiapkan dalam folder `data/`.

File utama yang digunakan:

```text
data/
├── nasional_kabkota.xlsx
├── ntt_kabkota_panel.xlsx
├── shp_nasional_web.shp
├── shp_nasional_web.shx
├── shp_nasional_web.dbf
├── shp_nasional_web.prj
└── ...