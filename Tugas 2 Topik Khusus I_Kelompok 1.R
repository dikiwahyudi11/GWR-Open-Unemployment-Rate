# Data untuk membuat peta
library(sf)
# https://geosai.my.id/download-shp-kabupaten-kota-indonesia/
maps <- st_read("D:/Materi Kuliah UI/Topik Khusus I - Analisis Data Spasial/Tugas Topik Khusus I - Analisis Data Spasial/Sumatera_Utara_ADMIN_BPS.shp")
maps$Kabupaten <- gsub("Kota ", "", maps$Kabupaten)
maps <- maps[order(maps$Kabupaten), ]
class(maps)
library(ggplot2)
ggplot(maps) + geom_sf()

# Data Tingkat Pengangguran Terbuka
library(readxl)
df <- read_excel("D:/Materi Kuliah UI/Topik Khusus I - Analisis Data Spasial/Tugas Topik Khusus I - Analisis Data Spasial/data_sumut.xlsx")
df <- data.frame(df)
colnames(df)[1] <- "kab_kota"
df <- df[-1, ] # Menghilangkan Sumatera Utara
df <- df[order(df$kab_kota), ]
df[, -1] <- lapply(df[, -1], as.numeric)
View(df)

# Preprocessing
# Periksa nama kabupaten/kota
maps <- maps[!maps$Kabupaten=="Danau Toba", ]
df$kab_kota==maps$Kabupaten
df[df$kab_kota=="Labuhan Batu", ]$kab_kota <- "Labuhanbatu"
df[df$kab_kota=="Labuanbatu Utara", ]$kab_kota <- "Labuhanbatu Utara"
maps[maps$Kabupaten=="Labuhan Batu", ]$Kabupaten <- "Labuhanbatu"
maps[maps$Kabupaten=="Labuhan Batu Selatan", ]$Kabupaten <- "Labuhanbatu Selatan"
maps[maps$Kabupaten=="Labuhan Batu Utara", ]$Kabupaten <- "Labuhanbatu Utara"
maps[maps$Kabupaten=="Tanjung Balai", ]$Kabupaten <- "Tanjungbalai"
maps[maps$Kabupaten=="Pematang Siantar", ]$Kabupaten <- "Pematangsiantar"
maps[maps$Kabupaten=="Toba Samosir", ]$Kabupaten <- "Toba"
df <- df[order(df$kab_kota), ]
maps <- maps[order(maps$Kabupaten), ]
colnames(maps)[5] <- "kab_kota"
df$kab_kota==maps$kab_kota

# Merge data
library(dplyr)
df_merge_sp <- inner_join(maps, df, by = "kab_kota")
df_merge_sp <- as(df_merge_sp, "Spatial")

# Cari centroid koordinat 
centroid <- st_centroid(maps)
coords <- st_coordinates(centroid)
# Cek centroid pada peta
ggplot(maps) + geom_sf(aes(fill = df$kab_kota), show.legend = FALSE) + 
  geom_sf(data = centroid, col = "black") 
# Save koordinat
library(sp)
df_sp <- SpatialPointsDataFrame(coords, df)

# Label plot
label <- c()
i <- 1
for(lab in df$kab_kota){
  if (length(strsplit(lab, " ")[[1]]) > 1 || nchar(lab)>4){
    # Extract first three-letter string
    first_three <- gsub("\\b(\\w{3})\\w*\\b", "\\1", lab)
    first_three <- gsub("\\s", "", first_three)
    label[i] <- first_three
  } else{
    label[i] <- df$kab_kota[i]
  }
  i <- i + 1
}
label
labels <- data.frame(label = label, long = coords[, 1], lat = coords[, 2])

# Statistika Deskriptif
summary(df)
df[df$TPT_2022==min(df$TPT_2022), ]
df[df$TPT_2022==max(df$TPT_2022), ]

# Tingkat Pengangguran Terbuka (TPT) Tahun 2022
library(ggspatial)
df[order(df$TPT_2022), ]
p1 <- ggplot(maps) + geom_sf(aes(fill = df$TPT_2022)) +
  geom_text(data = labels, aes(x = long, y = lat, label = label), size = 4, color = "black") + 
  scale_fill_gradient(name = "TPT", low = "lightblue", high = "darkblue") + 
  ggtitle("Tingkat Pengangguran Terbuka (TPT) \ndi Provinsi Sumatera Utara Tahun 2022") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))
# https://bookdown.org/brianwood1/QDASS/simple-static-maps.html
p1

# Persentase Penduduk Miskin (PPM) Tahun 2022
p2 <- ggplot(maps) + geom_sf(aes(fill = df$PPM_2022)) + 
  scale_fill_gradient(name = "PPM", low = "lightgreen", high = "darkgreen") + 
  ggtitle("Persentase Penduduk Miskin (PPM) \ndi Provinsi Sumatera Utara Tahun 2022") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))
p2

# Dependency Ratio (DR) Tahun 2022
p3 <- ggplot(maps) + geom_sf(aes(fill = df$DR_2022)) + 
  scale_fill_gradient(name = "DR", low = "yellow", high = "red") + 
  ggtitle("Dependency Ratio (DR) \ndi Provinsi Sumatera Utara Tahun 2022") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))
p3

# Gini Rasio (GR) Tahun 2022
library(viridis)
p4 <- ggplot(maps) + geom_sf(aes(fill = df$GR_2022)) + 
  scale_fill_viridis_c(option = "cividis",  name = "GR", direction = -1) + 
  ggtitle("Gini Rasio (GR) \ndi Provinsi Sumatera Utara Tahun 2022") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))
p4

# Laju Pertumbuhan Penduduk (LPP) Tahun 2022
p5 <- ggplot(maps) + geom_sf(aes(fill = df$LPP_2022)) + 
  scale_fill_viridis_c(option = "rocket",  name = "DR", direction = -1) + 
  ggtitle("Laju Pertumbuhan Penduduk (LPP) \ndi Provinsi Sumatera Utara Tahun 2022") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))
p5

# Plot Transformasi Variabel
df$log_TPT <- log(df$TPT_2022)
density_data1 <- data.frame(x = df$TPT_2022, group = "Sebelum")
density_data2 <- data.frame(x = log(df$TPT_2022), group = "Sesudah")
# Combine the datasets
density_data <- rbind(density_data1, density_data2)
# Create the plot
ggplot(density_data, aes(x = x, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Perbandingan Densitas Sebelum dan Sesudah \nTransformasi Logaritma Natural",
       x = "Values", y = "Density") +
  theme_minimal()

# Regresi Linier
reg <- lm(log(TPT_2022) ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, data = df)
summary(reg)
AIC(reg)

# Uji Asumsi
# Uji Multikolinearitas
library(car)
vif(reg)
cor(df[, -1])

# Uji Normalitas
qqnorm(reg$residuals, ylab = "Residuals", xlab = "Normal Scores")
qqline(reg$residuals)
shapiro.test(reg$residuals)
ks.test(reg$residuals, "pnorm")

# Uji Durbin Watson (Autocorrelation)
library(lmtest)
dwtest(log(TPT_2022) ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, data = df)

# Uji Heteroskedastisitas
bptest(reg)
spreadLevelPlot(reg)
par(mfrow = c(2, 2)) 
plot(reg)
par(mfrow = c(1, 1)) 
plot(reg, 1)

# Geographically Weighted Regression (GWR)
# https://crd230.github.io/gwr.html
library(spgwr)

# Estimated Optimal Bandwidth
# Gaussian Fixed Bandwidth
gwr_b1 <- gwr.sel(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                  data = df, coords = coords, adapt = FALSE)
gwr_b1
# Fit Model
gwr_fit1 <- gwr(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                data = df, coords = coords, bandwidth = gwr_b1, se.fit = TRUE, hatmatrix = TRUE)
gwr_fit1

# Gaussian Adaptive Bandwidth
gwr_b2 <- gwr.sel(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                  data = df, coords = coords, adapt = TRUE)
gwr_b2
# Fit Model
gwr_fit2 <- gwr(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                 data = df, coords = coords, bandwidth = gwr_b2, se.fit = TRUE, hatmatrix = TRUE)
gwr_fit2

# Bisquare Fixed Bandwidth
gwr_b3 <- gwr.sel(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                  data = df, coords = coords, adapt = FALSE, gweight = gwr.bisquare)
gwr_b3
# Fit Model
gwr_fit3 <- gwr(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                data = df, coords = coords, bandwidth = gwr_b3, se.fit = TRUE, hatmatrix = TRUE)
gwr_fit3

# Bisquare Adaptive Bandwidth
gwr_b4 <- gwr.sel(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                  data = df, coords = coords, adapt = TRUE, gweight = gwr.bisquare)
gwr_b4
# Fit Model
gwr_fit4 <- gwr(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                data = df, coords = coords, bandwidth = gwr_b4, se.fit = TRUE, hatmatrix = TRUE)
gwr_fit4

# Tricube Fixed Bandwidth
gwr_b5 <- gwr.sel(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                  data = df, coords = coords, adapt = FALSE, gweight = gwr.tricube)
gwr_b5
# Fit Model
gwr_fit5 <- gwr(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                data = df, coords = coords, bandwidth = gwr_b5, se.fit = TRUE, hatmatrix = TRUE)
gwr_fit5

# Tricube Adaptive Bandwidth
gwr_b6 <- gwr.sel(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                  data = df, coords = coords, adapt = TRUE, gweight = gwr.tricube)
gwr_b6
# Fit Model
gwr_fit6 <- gwr(log_TPT ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                data = df, coords = coords, bandwidth = gwr_b6, se.fit = TRUE, hatmatrix = TRUE)
gwr_fit6

# Model Terbaik
# AIC
AIC(reg)
gwr_fit1$results$AICh
gwr_fit2$results$AICh
gwr_fit3$results$AICh
gwr_fit4$results$AICh
gwr_fit5$results$AICh
gwr_fit6$results$AICh

# Untuk model pilihan:
gwr_fit <- gwr_fit6
gwr_fit

# Statistical Test
dfree <- gwr_fit$results$edf

df$PPM_2022_t <- gwr_fit$SDF$PPM_2022/gwr_fit$SDF$PPM_2022_se
df$PPM_2022_t.pval <- 2*pt(-abs(df$PPM_2022_t), dfree)
df$PPM_2022_t.signif <- ifelse(df$PPM_2022_t.pval<0.05, "Signifikan", "Tidak Signifikan")

df$DR_2022_t <- gwr_fit$SDF$DR_2022/gwr_fit$SDF$DR_2022_se
df$DR_2022_t.pval <- 2*pt(-abs(df$DR_2022_t), dfree)
df$DR_2022_t.signif <- ifelse(df$DR_2022_t.pval<0.05, "Signifikan", "Tidak Signifikan")

df$GR_2022_t <- gwr_fit$SDF$GR_2022/gwr_fit$SDF$GR_2022_se
df$GR_2022_t.pval <- 2*pt(-abs(df$GR_2022_t), dfree)
df$GR_2022_t.signif <- ifelse(df$GR_2022_t.pval<0.05, "Signifikan", "Tidak Signifikan")

df$LPP_2022_t <- gwr_fit$SDF$LPP_2022/gwr_fit$SDF$LPP_2022_se
df$LPP_2022_t.pval <- 2*pt(-abs(df$LPP_2022_t), dfree)
df$LPP_2022_t.signif <- ifelse(df$LPP_2022_t.pval<0.05, "Signifikan", "Tidak Signifikan")

# Kelompok variabel signifikan
x1 <- df[df$PPM_2022_t.signif=="Signifikan", ]$kab_kota
length(x1)
x1
x2 <- df[df$DR_2022_t.signif=="Signifikan", ]$kab_kota
length(x2)
x2
x3 <- df[df$GR_2022_t.signif=="Signifikan", ]$kab_kota
length(x3)
x3
x4 <- df[df$LPP_2022_t.signif=="Signifikan", ]$kab_kota
length(x4)
x4
df_sig <- data.frame(kab_kota = df$kab_kota)
rownames(df_sig) <- df_sig$kab_kota
df_sig$x1 <- ifelse(df_sig$kab_kota %in% x1, "x1", "0")
df_sig$x2 <- ifelse(df_sig$kab_kota %in% x2, "x2", "0")
df_sig$x3 <- ifelse(df_sig$kab_kota %in% x3, "x3", "0")
df_sig$x4 <- ifelse(df_sig$kab_kota %in% x4, "x4", "0")
df_sig <- df_sig[, -1]
head(df_sig)
tabulasi <- apply(df_sig, 1, function(x) paste(x, collapse = "_"))
df_sig$tabulasi <- tabulasi
table(df_sig$tabulasi)
list_nama <- split(rownames(df_sig), df_sig$tabulasi)
length(list_nama)
nama <- c(1, 4, 3, 6, 2, 5, 8, 7, 9)
names(nama) <- names(list_nama)
library(plyr)
df_sig$kelompok <- plyr::mapvalues(df_sig$tabulasi, from = names(nama), to = nama)
View(df_sig)
# Visualisasi
View(maps)
ggplot(maps) + geom_sf(aes(fill = df_sig$kelompok)) +
  geom_text(data = labels, aes(x = long, y = lat, label = label), size = 4, color = "black") + 
  ggtitle("Kelompok Kabupaten/Kota Berdasarkan \nVariabel yang Signifikan") +
  xlab("Longitude") + ylab("Latitude") + labs(fill = "Kelompok") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Visualisasi P-value Koefisien
breaks <- c(0, 0.01, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1)
# PPM
ppm_class <- cut(df$PPM_2022_t.pval, breaks)
unique(ppm_class)
ppm_class
k11 <- ggplot(maps) + geom_sf(aes(fill = df$PPM_2022_t.pval)) + 
  scale_fill_gradient(name = "p-value", low = "white", high = "blue", 
                      breaks = breaks, labels = breaks) + 
  ggtitle("P-value Koefisien Persentase Penduduk Miskin (PPM)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# DR
k12 <- ggplot(maps) + geom_sf(aes(fill = df$DR_2022_t.pval)) + 
  scale_fill_gradient(name = "p-value", low = "white", high = "blue", 
                      breaks = breaks, labels = breaks) +
  ggtitle("P-value Koefisien Dependency Ratio (DR)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# GR
k13 <- ggplot(maps) + geom_sf(aes(fill = df$GR_2022_t.pval)) + 
  scale_fill_gradient(name = "p-value", low = "white", high = "blue", 
                      breaks = breaks, labels = breaks) +
  ggtitle("P-value Koefisien Gini Rasio (GR)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# LPP 
k14 <- ggplot(maps) + geom_sf(aes(fill = df$LPP_2022_t.pval)) + 
  scale_fill_gradient(name = "p-value", low = "white", high = "blue", 
                      breaks = breaks, labels = breaks) +
  ggtitle("P-value Koefisien Laju Pertambahan Penduduk (LPP)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# Visualisasi Koefisien
# PPM
k21 <- ggplot(maps) + geom_sf(aes(fill = gwr_fit$SDF$PPM_2022)) + 
  scale_fill_gradient2(name = "Koefisien", low = "red", mid = "white", high = "blue") + 
  ggtitle("Koefisien Persentase Penduduk Miskin (PPM)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# DR
k22 <- ggplot(maps) + geom_sf(aes(fill = gwr_fit$SDF$DR_2022)) + 
  scale_fill_gradient2(name = "Koefisien", low = "red", mid = "white", high = "blue") + 
  ggtitle("Koefisien Dependency Ratio (DR)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# GR
k23 <- ggplot(maps) + geom_sf(aes(fill = gwr_fit$SDF$GR_2022)) + 
  scale_fill_gradient2(name = "Koefisien", low = "red", mid = "white", high = "blue") + 
  ggtitle("Koefisien Gini Rasio (GR)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# LPP 
k24 <- ggplot(maps) + geom_sf(aes(fill = gwr_fit$SDF$LPP_2022)) + 
  scale_fill_gradient2(name = "Koefisien", low = "red", mid = "white", high = "blue") + 
  ggtitle("Koefisien Laju Pertambahan Penduduk (LPP)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# Gabungan Visualisasi
library(ggpubr)
ggarrange(k11, k21, ncol = 2)
ggarrange(k12, k22, ncol = 2)
ggarrange(k13, k23, ncol = 2)
ggarrange(k14, k24, ncol = 2)

# Visualisasi Residual
# Regresi Linier
r1 <- ggplot(maps) + geom_sf(aes(fill = reg$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white", 
                       high = "blue", midpoint = 0) + 
  ggtitle("Residual Model Regresi Linier") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# GWR
r2 <- ggplot(maps) + geom_sf(aes(fill = gwr_fit$SDF$gwr.e)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white", 
                       high = "blue", midpoint = 0) + 
  ggtitle("Residual Model Geographically Weighted Regression") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
ggarrange(r1, r2, ncol = 2)

# Save Hasil
df$longitude <- coords[, 1]
df$latitude <- coords[, 2]
df$est_PPM <- gwr_fit$SDF$PPM_2022
df$est_PPM_se <- gwr_fit$SDF$PPM_2022_se
df$est_DR <- gwr_fit$SDF$DR_2022
df$est_DR_se <- gwr_fit$SDF$DR_2022_se
df$est_GR <- gwr_fit$SDF$GR_2022
df$est_GR_se <- gwr_fit$SDF$GR_2022_se
df$est_LPP <- gwr_fit$SDF$LPP_2022
df$est_LPP_se <- gwr_fit$SDF$LPP_2022_se
library("writexl")
tabel_data <- df[, c("kab_kota", "TPT_2022", "PPM_2022", "DR_2022", "GR_2022", "LPP_2022", 
                     "longitude", "latitude")]
write_xlsx(tabel_data, "D:/Materi Kuliah UI/Topik Khusus I - Analisis Data Spasial/Tugas Topik Khusus I - Analisis Data Spasial/tabel_data2.xlsx")
tabel_hasil <- df[, c("kab_kota", "longitude", "latitude", 
                      "est_PPM", "est_PPM_se", "PPM_2022_t", "PPM_2022_t.pval", "PPM_2022_t.signif",
                      "est_DR", "est_DR_se", "DR_2022_t", "DR_2022_t.pval", "DR_2022_t.signif",
                      "est_GR", "est_GR_se", "GR_2022_t", "GR_2022_t.pval", "GR_2022_t.signif",
                      "est_LPP", "est_LPP_se", "LPP_2022_t", "LPP_2022_t.pval", "LPP_2022_t.signif")]
write_xlsx(tabel_hasil, "D:/Materi Kuliah UI/Topik Khusus I - Analisis Data Spasial/Tugas Topik Khusus I - Analisis Data Spasial/tabel_hasil2.xlsx")

# GWR dengan GWmodel
library(GWmodel)
bw.gwr.tri <- bw.gwr(log(TPT_2022) ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                     data = df_sp, approach = "CV", kernel = "tricube", 
                     adaptive = TRUE)
gwr.tricube <- gwr.basic(log(TPT_2022) ~ PPM_2022 + DR_2022 + GR_2022 + LPP_2022, 
                         data = df_sp, bw = bw.gwr.tri, 
                         kernel = "tricube", adaptive = TRUE)
gwr.tricube
res_sig <- gwr.t.adjust(gwr.tricube)
View(res_sig$SDF)
res_sig$SDF
