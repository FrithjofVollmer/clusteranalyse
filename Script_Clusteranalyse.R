################################################################################
# 
# Datenclusterung mit k-Means, k-Medoids und PCA in R
#
#
# geeignet für nicht-normalverteilte Datensätze (z.B. zu musikalischer Performanz)
# Stand: 08.03.2025, Version 1.0
# zusammengestellt von Frithjof Vollmer
#
#
# Das folgende kommentierte Skript gibt die Befehlskette für eine zweifache
# Clusteranalyse von performanzbezogenen Daten wieder, wie sie vom Autor 
# zur Analyse von Messdaten zu 57 Aufnahmen von Beethovens Violinkonzert 
# op. 61 (1912–1956) verwendet wurde.
#
#
# Zentrale Pakete und Funktionen:
#
# "kmeans"      (für k-Means-Clusteranalyse)
# "cluster"     (Standardpaket für robustere Clustermethoden)
# "pam"         (Funktion aus "cluster" für k-Medoids-Analyse)
# "prcomp"      (für Hauptkomponentenanalyse, PCA)
#
#
# Ausgaben:     (werden im Arbeitsverzeichnis abgespeichert)
#
# "_Analyse"    Visualisierung der Clusterzentren 
# "_ANOVA"      Analysis of Variance (Prüfung auf signifikante Unterschiede der 
#               Clustervektoren) 
# "_Distanzen"  PDF mit Scatter Plot (Punktdiagramm) der Datenpunkte pro Cluster 
#               und deren Entfernung von den Clusterzentren 
# "_sort_kM"    CSV mit Zuordnung der Datenpunkte nach k-Means-Analyse, 
#               gestaffelt nach Entfernung vom Clusterzentrum
# "_sort_kMed"  ebd., CSV für k-Medoids-Analyse
# "_CVect"      CSV mit Vektoren der Clusterzentren 
# "_mitCluster" ursprünglicher Datensatz als CSV, pro Datenpunkt ergänzt um 
#               Clusterzugehörigkeit
# "_PCA"         Hauptkomponentenanalysen zur Interpretation der Cluster
#               (Visualisierung 2PC und 3PC als 2D- und 3D-Scatterplot)
#
################################################################################

# Schnellzugriff: Anzahl der Cluster für k-Means und k-Medoids setzen

k <- 4 # 4 Cluster 

# Wenn Sie mit der Clusterzahl unsicher sind, wiederholen Sie die Analyse 
# mehrfach mit verschiedenen Clusterzahlen, bis sich das Ergebnis sinnvoll 
# interpretieren lässt.   

# Pfad zum Arbeitsverzeichnis setzen (Dateipfad, Pfade in den Klammern sind
# entsprechend anzupassen. Ordner kann in der R-Konsole einfach zwischen die ""  
# gezogen werden, es erscheint der Pfad). 

setwd("~/Desktop/Clusteranalysen B61/cantabile final/Tests")

# Laden des Datensatzes in die Variablen "file_path" (um den Dateinamen für spätere
# Ausgaben zu verwenden) und "data" (Quelldatensatz): Den unten gegebenen Pfad  
# "cantabile_data.csv" durch den eigenen CSV-Datensatz im Arbeitsverzeichnis ersetzen
# (geht auch mit anderen Dateiformaten wie etwa TXT, dann über "read.table"). In 
# diesen Einstellungen enthalten die Werte nach kontinentaleuropäischem Standard 
# Kommata (",") als Dezimaltrennzeichen und sind durch Semikolons (";") voneinander 
# getrennt. Wird zur Datensatzerstellung z.B. mit einer englischsprachigen Version 
# von Excel gearbeitet, werden die Dateien evtl. mit Punkten (".") als 
# Dezimaltrennzeichen und Kommata als Separatoren (",") ausgegeben. Die Werte 
# für die Argumente "sep" und "dec" sind dann entsprechend anzupassen. Wenn es 
# im Datensatz keine Kopfzeile für Spaltennamen gibt: "header = FALSE". "row.names"
# gibt die Zahl der Spalten mit Zeilennamen (Labels pro Datenpunkt) an, die für 
# die folgenden Analysen nicht berücksichtigt werden.

file_path <- "cantabile_data.csv"

# Funktion, um den Dateinamen für spätere Ausgaben zu verwenden (z.B. 
# "[Datensatzname]_Analysis.pdf")
set_file_name <- function(file_path) {
  # Extrahiere den Basisnamen der Datei ohne Pfad und Erweiterung
  file_name <- tools::file_path_sans_ext(basename(file_path))
  # Gebe den Dateinamen zurück
  return(file_name)
}
output_name <- set_file_name(file_path)

# Einlesen des Quelldatensatzes
data <- read.csv(file_path,  header = TRUE, 
                 row.names = 1)

# Fehlermeldung, falls der Datensatz nicht-numerische  Spalten enthält (k-Means 
# arbeitet nur mit numerischen Daten)
if (!all(sapply(data, is.numeric))) {
  stop("Der Datensatz enthält nicht-numerische Spalten. Stellen Sie sicher, dass 
       alle Variablen numerisch sind.")
}

# Warnung, falls leere Werte enthalten sind, und Anpassung
if (any(is.na(data))) {
  warning("Der Ausgangsdatensatz enthielt fehlende Werte. Diese wurden vor der 
          Analyse entfernt. Bitte prüfen Sie die ausgegebene CSV-Datei auf 
          relevante, aber fehlende Zeilen.")
  data <- na.omit(data)  # Entferne Zeilen mit fehlenden Werten
}

# Warnung, falls evtl. zum falschen Datensatz gegriffen wurde
if (nrow(data) < 2) {
  stop("Der Datensatz enthält zu wenige Zeilen für eine Clusteranalyse. 
       Es werden mindestens 2 Datenpunkte benötigt.")
}

# Vorschau des geladenen Datensatzes
head(data)

# Bei unterschiedlichen Skalen im Datensatz sollte dieser vor der Analyse 
# normalisiert bzw. standardisiert werden (Herstellung von Vergleichbarkeit, 
# etwa bei BPM vs. Sekunden). In den folgenden Einstellungen wird das automatisch
# über eine z-Standardisierung vorgenommen, die den Mittelpunkt auf Null und die
# Standardverteilung auf 1 setzt; dabei werden Verteilungsmuster beibehalten.
# Wenn ihr Datensatz bereits standardisiert ist oder Sie zu der Einschätzung kommen, 
# dass ihr Datensatz eine andere Normalisierung benötigt, sollten Sie die folgende 
# Eingabe durch "data_numeric <- data.matrix(data)" ersetzen. 
data_numeric <- scale(data.matrix(data))
rownames(data_numeric) <- rownames(data)

# Alle Abbildungen (Plots) der folgenden Analysen in einer gesammelten PDF im 
# Arbeitsverzeichnis speichern
pdf(file = paste0(output_name, "_Analyse.pdf"))

# Farbpalette definieren (light yellow bis dark red)
color_palette <- colorRampPalette(c("lightyellow", "darkred"))


# Es folgen die k-Means und k-Medoids-Analysen: k-Means ist schneller und 
# funktioniert auch gut mit großen Datensätzen; k-Medoids ist robuster gegenüber
# Ausreißern sowie unsymmetrischen Datenverteilungen und stabiler bei kleinen
# Datensätzen. Im oben genannten Beispiel der Beethoven-Tondokumente wurde sich  
# deshalb letztlich für die k-Medoids-Analyseergebnisse entschieden.


### k-Means-Clusteranalyse #####################################################

# Durchführung der k-Means-Analyse
set.seed(123)  # Seed für Reproduzierbarkeit
kmeans_result <- kmeans(data_numeric, centers = k)

# Clusterzuweisungen und Zentren
data$Cluster_kMeans <- kmeans_result$cluster
kmeans_centers <- kmeans_result$centers

# Visualisierung der Clusterzentren
barplot(t(kmeans_centers), beside = TRUE, main = "k-Means Clusterzentren", 
        xlab = "Cluster", ylab = "Gestaltungsvariablen (z-standardisiert)", col = 
          color_palette(ncol(kmeans_centers)))


### k-Medoids-Clusteranalyse ##################################################

# Berechnung der Distanzmatrix
library(cluster)
euclidean_distance <- dist(data_numeric, method = "euclidean")

# Durchführung der k-Medoids-Analyse
set.seed(456)  # Seed für Reproduzierbarkeit
kmedoids_result <- pam(euclidean_distance, k = k)

# Clusterzuweisungen und Medoids
data$Cluster_kMedoids <- kmedoids_result$clustering
kmedoids_centers <- data_numeric[kmedoids_result$medoids, ]

# Visualisierung der Medoid-Zentren
barplot(t(kmedoids_centers), beside = TRUE, main = "k-Medoids Clusterzentren", 
        xlab = "Medoid", ylab = "Gestaltungsvariablen (z-standardisiert)", col = 
          color_palette(ncol(kmeans_centers)))


### Legenden als separate Plots ###############################################

# Legende für k-Means
plot.new()
legend("center", legend = colnames(kmeans_centers), fill = color_palette(ncol(kmeans_centers)), 
       title = "Variablen (k-Means)", cex = 1.2, bty = "n")

# Legende für k-Medoids
plot.new()
legend("center", legend = colnames(kmedoids_centers), fill = color_palette(ncol(kmedoids_centers)), 
       title = "Variablen (k-Medoids)", cex = 1.2, bty = "n")


### Abstand der Clusterzentren #################################################

# Berechnung der paarweisen Abstände zwischen Clusterzentren
kmeans_distances_matrix <- as.matrix(dist(kmeans_centers))
kmedoids_distances_matrix <- as.matrix(dist(kmedoids_centers))

# Speicherung der Abstände als CSV
write.csv2(kmeans_distances_matrix, file = paste0(output_name, "_kM_Centr.csv"))
write.csv2(kmedoids_distances_matrix, file = paste0(output_name, "_kMed_Centr.csv"))


### Ausgabe der Ergebnisse ####################################################

dev.off()  # Schließen der PDF-Datei

# Ergänzung der Clusterzuweisungen im Datensatz
write.csv2(data, file = paste0(output_name, "_mitCluster.csv"))

# Ausgabe der Clusterzentren
kmeans_centers_df <- as.data.frame(kmeans_centers)
kmeans_centers_df$Method <- "k-Means"
kmedoids_centers_df <- as.data.frame(kmedoids_centers)
kmedoids_centers_df$Method <- "k-Medoids"
combined_centers <- rbind(kmeans_centers_df, kmedoids_centers_df)
write.csv2(combined_centers, file = paste0(output_name, "_CVect.csv"))

# Sortierte Tabellen der Datenpunkte nach Entfernung zu den Clusterzentren
kmeans_distances <- apply(data_numeric, 1, function(row) {
  cluster <- kmeans_result$cluster[rownames(data_numeric) == rownames(row)]
  sum((row - kmeans_result$centers[cluster, ])^2)
})

data$Distance_kMeans <- kmeans_distances
kmedoids_distances <- apply(data_numeric, 1, function(row) {
  cluster <- kmedoids_result$clustering[rownames(data_numeric) == rownames(row)]
  sum((row - kmedoids_centers[cluster, ])^2)
})

data$Distance_kMedoids <- kmedoids_distances

sorted_kmeans <- data[order(data$Cluster_kMeans, data$Distance_kMeans), ]
sorted_kmedoids <- data[order(data$Cluster_kMedoids, data$Distance_kMedoids), ]


### Zusätzliche Visualisierungen ##############################################

# Dynamische Anpassung der Scatterplots
adjust_plot_parameters <- function(n_rows) {
  plot_width <- max(11, n_rows * 0.2)  # Breite an Anzahl der Zeilen anpassen
  font_size <- max(6, 12 - n_rows * 0.05)  # Schriftgröße (min. 6)
  bottom_margin <- max(5, n_rows * 0.15)  # Unterer Rand für Labels
  list(width = plot_width, font_size = font_size / 10, bottom_margin = bottom_margin)
}


### Tabelle: Zuordnung von Zeilennamen zu Clustern und Sortierung nach Entfernung ###

# Erstellen eines DataFrames für die Tabelle
cluster_table <- data.frame(
  Zeilenname = rownames(data),
  Cluster_kMeans = data$Cluster_kMeans,
  Distance_kMeans = data$Distance_kMeans,
  Cluster_kMedoids = data$Cluster_kMedoids,
  Distance_kMedoids = data$Distance_kMedoids
)

# Sortieren der Tabelle nach Entfernung zu den Clusterzentren
# Für k-Means
cluster_table_sorted_kMeans <- cluster_table[order(cluster_table$Cluster_kMeans, 
                                                   cluster_table$Distance_kMeans), ]

# Für k-Medoids
cluster_table_sorted_kMedoids <- cluster_table[order(cluster_table$Cluster_kMedoids, 
                                                     cluster_table$Distance_kMedoids), ]

# Ausgabe der sortierten Tabellen in separaten CSV-Dateien
write.csv2(cluster_table_sorted_kMeans, file = paste0(output_name, "_sort_kM.csv"), row.names = FALSE)
write.csv2(cluster_table_sorted_kMedoids, file = paste0(output_name, "_sort_kMed.csv"), 
           row.names = FALSE)

# Ausgabe der erfolgreichen Speicherung
print("Die Tabellen mit Clusterzuweisungen und Entfernungen wurden erfolgreich gespeichert:")
print(cat("1. k-Means:", output_name, "_sort_kM.csv"))
print(cat("2. k-Medoids:", output_name, "_sort_kMed.csv"))


# Scatterplot für k-Means mit Zeilennamen auf der x-Achse
plot_kMeans <- function(cluster_table, params) {
  par(mar = c(params$bottom_margin, 4, 4, 2))  # Anpassung der Ränder
  plot(1:nrow(cluster_table), cluster_table$Distance_kMeans, 
       col = cluster_table$Cluster_kMeans, 
       pch = 19, 
       main = "Entfernungen zu k-Means-Clusterzentren",
       xlab = "", 
       ylab = "Distanz zum Clusterzentrum",
       xaxt = "n")  # Keine automatische Achsenbeschriftung
  axis(1, at = 1:nrow(cluster_table), labels = cluster_table$Zeilenname, las = 2, cex.axis = params$font_size)  # X-Achse mit Zeilennamen
}

# Scatterplot für k-Medoids mit Zeilennamen auf der x-Achse
plot_kMedoids <- function(cluster_table, params) {
  par(mar = c(params$bottom_margin, 4, 4, 2))  # Anpassung der Ränder
  plot(1:nrow(cluster_table), cluster_table$Distance_kMedoids, 
       col = cluster_table$Cluster_kMedoids, 
       pch = 19, 
       main = "Entfernungen zu k-Medoids-Clusterzentren",
       xlab = "", 
       ylab = "Distanz zum Clusterzentrum",
       xaxt = "n")  # Keine automatische Achsenbeschriftung
  axis(1, at = 1:nrow(cluster_table), labels = cluster_table$Zeilenname, las = 2, cex.axis = params$font_size)  # X-Achse mit Zeilennamen
}

# Berechnung der k-Means-Distanzen
data$Distance_kMeans <- sapply(1:nrow(data_numeric), function(i) {
  cluster <- kmeans_result$cluster[i]
  sum((data_numeric[i, ] - kmeans_centers[cluster, ])^2)
})

# Berechnung der k-Medoids-Distanzen
data$Distance_kMedoids <- sapply(1:nrow(data_numeric), function(i) {
  cluster <- kmedoids_result$clustering[i]
  sum((data_numeric[i, ] - kmedoids_centers[cluster, ])^2)
})

# Sortierte Tabellen erstellen
cluster_table_sorted_kMeans <- data.frame(
  Zeilenname = rownames(data),
  Cluster_kMeans = data$Cluster_kMeans,
  Distance_kMeans = data$Distance_kMeans
)[order(data$Cluster_kMeans, data$Distance_kMeans), ]

cluster_table_sorted_kMedoids <- data.frame(
  Zeilenname = rownames(data),
  Cluster_kMedoids = data$Cluster_kMedoids,
  Distance_kMedoids = data$Distance_kMedoids
)[order(data$Cluster_kMedoids, data$Distance_kMedoids), ]

params <- adjust_plot_parameters(nrow(cluster_table))

write.csv2(cluster_table_sorted_kMeans, file = paste0(output_name, "_sort_kM.csv"), row.names = TRUE)
write.csv2(cluster_table_sorted_kMedoids, file = paste0(output_name, "_sort_kMed.csv"), row.names = TRUE)

pdf(paste0(output_name, "_Distanzen.pdf"), width = params$width, height = 8.5)

# Scatterplots erzeugen
plot_kMeans(cluster_table_sorted_kMeans, params)
plot_kMedoids(cluster_table_sorted_kMedoids, params)

dev.off()  # PDF schließen

# Ergänzung der Clusterzuweisungen im Datensatz
write.csv2(data, file = paste0(output_name, "_mitCluster.csv"))

print("Die angepassten Abbildungen zu den Clusterzuordnungen und Distanzen wurden erfolgreich gespeichert.")


### ein paar Fehlermeldungen ###################################################

# Funktion zur Überprüfung der Clusterzuweisungen in den CSV-Dateien
check_csv_cluster_assignments <- function(csv_path, expected_clusters, method_name) {
  # Einlesen der CSV-Datei
  data <- read.csv2(csv_path, header = TRUE, sep = ";", dec = ",")
  
  # 1. Überprüfung auf NA-Werte in den Clusterzuweisungen
  if (any(is.na(data$Cluster_kMeans)) || any(is.na(data$Cluster_kMedoids))) {
    warning(paste("Die Datei", csv_path, "enthält NA-Werte in den Clusterzuweisungen für", method_name, 
                  ". Bitte prüfen Sie die Datenintegrität."))
  }
  
  # 2. Überprüfung der Anzahl der gefundenen Cluster
  k_means_clusters <- unique(data$Cluster_kMeans)
  k_medoids_clusters <- unique(data$Cluster_kMedoids)
  
  if (length(k_means_clusters) != expected_clusters) {
    warning(paste("Die Anzahl der gefundenen Cluster in der Datei", csv_path, 
                  "für k-Means entspricht nicht der erwarteten Anzahl (erwartet:", expected_clusters, 
                  ", gefunden:", length(k_means_clusters), ")."))
  }
  
  if (length(k_medoids_clusters) != expected_clusters) {
    warning(paste("Die Anzahl der gefundenen Cluster in der Datei", csv_path, 
                  "für k-Medoids entspricht nicht der erwarteten Anzahl (erwartet:", expected_clusters, 
                  ", gefunden:", length(k_medoids_clusters), ")."))
  }
  
  # 3. Überprüfung auf leere Cluster
  k_means_counts <- table(data$Cluster_kMeans)
  k_medoids_counts <- table(data$Cluster_kMedoids)
  
  if (any(k_means_counts == 0)) {
    warning(paste("Es gibt leere Cluster in der Datei", csv_path, "für k-Means."))
  }
  
  if (any(k_medoids_counts == 0)) {
    warning(paste("Es gibt leere Cluster in der Datei", csv_path, "für k-Medoids."))
  }
  
  # 4. Ausgabe einer Bestätigung, wenn keine Probleme gefunden wurden
  if (!any(is.na(data$Cluster_kMeans)) &&
      !any(is.na(data$Cluster_kMedoids)) &&
      length(k_means_clusters) == expected_clusters &&
      length(k_medoids_clusters) == expected_clusters &&
      all(k_means_counts > 0) &&
      all(k_medoids_counts > 0)) {
    message(paste("Die Clusterzuweisungen in der Datei", csv_path, "sind korrekt und vollständig für", method_name, "."))
  }
}

# Beispielaufruf für die überprüften CSV-Dateien
check_csv_cluster_assignments(
  csv_path = paste0(output_name, "_mitCluster.csv"), 
  expected_clusters = k,  # Anzahl der erwarteten Cluster
  method_name = "k-Means und k-Medoids"
)


### Ergänzung ANOVA ###########################################################

# ANOVA für Cluster basierend auf k-Means und k-Medoids
anova_results <- data.frame(Variable = character(), 
                            Cluster_Method = character(), 
                            Df = integer(), 
                            SumSq = numeric(), 
                            MeanSq = numeric(), 
                            FValue = numeric(), 
                            Pr_F = numeric(), 
                            stringsAsFactors = FALSE)

# ANOVA für k-Means
for (i in 1:ncol(data_numeric)) {
  anova_result <- summary(aov(data_numeric[, i] ~ factor(data$Cluster_kMeans)))
  df <- anova_result[[1]]$Df[1]
  sum_sq <- anova_result[[1]]$`Sum Sq`[1]
  mean_sq <- anova_result[[1]]$`Mean Sq`[1]
  f_value <- anova_result[[1]]$`F value`[1]
  p_value <- anova_result[[1]]$`Pr(>F)`[1]
  
  anova_results <- rbind(anova_results, 
                         data.frame(Variable = colnames(data_numeric)[i],
                                    Cluster_Method = "k-Means",
                                    Df = df, 
                                    SumSq = sum_sq, 
                                    MeanSq = mean_sq, 
                                    FValue = f_value, 
                                    Pr_F = p_value))
}

# ANOVA für k-Medoids
for (i in 1:ncol(data_numeric)) {
  anova_result <- summary(aov(data_numeric[, i] ~ factor(data$Cluster_kMedoids)))
  df <- anova_result[[1]]$Df[1]
  sum_sq <- anova_result[[1]]$`Sum Sq`[1]
  mean_sq <- anova_result[[1]]$`Mean Sq`[1]
  f_value <- anova_result[[1]]$`F value`[1]
  p_value <- anova_result[[1]]$`Pr(>F)`[1]
  
  anova_results <- rbind(anova_results, 
                         data.frame(Variable = colnames(data_numeric)[i],
                                    Cluster_Method = "k-Medoids",
                                    Df = df, 
                                    SumSq = sum_sq, 
                                    MeanSq = mean_sq, 
                                    FValue = f_value, 
                                    Pr_F = p_value))
}

# Ausgabe der ANOVA-Ergebnisse als CSV
write.csv2(anova_results, file = paste0(output_name, "_ANOVA.csv"), row.names = FALSE)

# Funktion zur Überprüfung der p-Werte der ANOVA
check_anova_p_values <- function(anova_csv_path) {
  # Einlesen der ANOVA-Ergebnisse aus der CSV-Datei
  anova_results <- read.csv2(anova_csv_path, header = TRUE, sep = ";", dec = ",")
  
  # Überprüfung auf p-Werte über 0,05
  high_p_values <- anova_results[anova_results$Pr_F > 0.05, ]
  
  if (nrow(high_p_values) > 0) {
    warning(paste("Die ANOVA-Ergebnisse zeigen, dass einige Variablen keinen signifikanten Unterschied zwischen den Clustern aufweisen (p > 0.05):",
                  paste(high_p_values$Variable, collapse = ", "), 
                  ". Bitte überprüfen Sie die betroffenen Variablen."))
  } else {
    message("Alle ANOVA-Ergebnisse zeigen signifikante Unterschiede zwischen den Clustern (p <= 0.05).")
  }
}

# Ausgabe der ANOVA-Ergebnisse
print(cat(output_name, ": ANOVA Ergebnisse für Cluster (k-Means und k-Medoids):"))
print(anova_results)

# ANOVA-Zusammenfassung
check_anova_p_values(paste0(output_name, "_ANOVA.csv"))


### Hauptkomponentenanalysen (PCA-Plots) zur Interpretation der Clusterlagen ###

#ggfs. Plotpakete installieren
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("scatterplot3d")
library(scatterplot3d)
if (!requireNamespace("scatterplot3d", quietly = TRUE)) install.packages("scatterplot3d")
library(scatterplot3d) 

pdf(paste(output_name, "_PCA.pdf"))

# kMeans-PCA
pca_result <- prcomp(data_numeric, center = TRUE, scale. = TRUE) #PCA durchführen
pca_data <- as.data.frame(pca_result$x)  # Hauptkomponentendaten extrahieren
pca_data$Cluster <- as.factor(kmeans_result$cluster)  # Clusterzuweisung hinzufügen

# Clusterzentren für k-Means in PCA-Raum transformieren
centers_pca <- as.data.frame(predict(pca_result, kmeans_result$centers))

# kMedoids-PCA
pca_result_kMedoids <- prcomp(data_numeric, center = TRUE, scale. = TRUE) #PCA durchführen
pca_data_kMedoids <- as.data.frame(pca_result_kMedoids$x)  # Hauptkomponentendaten extrahieren
pca_data_kMedoids$Cluster <- as.factor(kmedoids_result$clustering)  # Clusterzuweisung hinzufügen

medoid_indices <- kmedoids_result$id.med  # Indizes der Medoiden in den Daten
medoids_numeric <- data_numeric[medoid_indices, , drop = FALSE]  # Richtige Werte extrahieren

# Clusterzentren für k-Medoids in PCA-Raum transformieren
centers_pca_kMedoids <- as.data.frame(predict(pca_result_kMedoids, medoids_numeric))

# Farbpalette basierend auf der Anzahl der Cluster
library(RColorBrewer)
num_clusters <- length(unique(pca_data$Cluster))
cluster_colors <- brewer.pal(min(num_clusters, 9), "Set1")  # Max. 9 Farben (erweiterbar mit Viridis)


library(ggplot2) #PCA mit 2 PCs plotten
  
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_point(data = centers_pca, aes(x = PC1, y = PC2, color = as.factor(1:num_clusters)), 
             shape = 8, size = 6) +  # Gleiche Farben für Clusterzentren
  theme_minimal() +
  labs(title = "PCA-Plot mit k-Means-Clustern", x = "PC1", y = "PC2") +
  scale_color_manual(values = cluster_colors)  # Dynamische Farben

ggplot(pca_data_kMedoids, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_point(data = centers_pca_kMedoids, aes(x = PC1, y = PC2, color = as.factor(1:num_clusters)), 
             shape = 8, size = 6) +  # Gleiche Farben für Clusterzentren
  theme_minimal() +
  labs(title = "PCA-Plot mit k-Medoids-Clustern", x = "PC1", y = "PC2") +
  scale_color_manual(values = cluster_colors)  # Dynamische Farben


library(scatterplot3d) #PCA mit 3 PCs plotten

# PCA mit 3 PCs plotten (k-Means)
colors <- cluster_colors[as.numeric(pca_data$Cluster)]  # gleiche Farben wie oben

s3d <- scatterplot3d(pca_data$PC1, pca_data$PC2, pca_data$PC3,
                     color = colors, pch = 19,
                     xlab = "PC1", ylab = "PC2", zlab = "PC3",
                     main =  "3D-PCA-Plot mit k-Means-Clustern")

# Clusterzentren als größere Punkte in 3D-Plot (k-Means)
s3d$points3d(centers_pca$PC1, centers_pca$PC2, centers_pca$PC3, 
             col = cluster_colors, pch = 8, cex = 2)

# PCA mit 3 PCs plotten (k-Medoids)
colors <- cluster_colors[as.numeric(pca_data_kMedoids$Cluster)]  # gleiche Farben wie oben

s3d_kMedoids <- scatterplot3d(pca_data_kMedoids$PC1, pca_data_kMedoids$PC2, pca_data_kMedoids$PC3,
                              color = colors, pch = 19,
                              xlab = "PC1", ylab = "PC2", zlab = "PC3",
                              main =  "3D-PCA-Plot mit k-Medoids-Clustern")

# Clusterzentren als größere Punkte in 3D-Plot (k-Medoids)
s3d_kMedoids$points3d(centers_pca_kMedoids$PC1, centers_pca_kMedoids$PC2, centers_pca_kMedoids$PC3, 
                      col = cluster_colors, pch = 8, cex = 2)


# PCA-Screeplots und -Biplots (k-Means und k-Medoids)
screeplot(pca_result, main = "PCA-Screeplot cantabile", type = "lines")
biplot(pca_result_kMedoids, main = "Biplot der ersten zwei Hauptkomponenten cantabile")

dev.off()

pca_result <- prcomp(data_numeric, scale. = TRUE)
pca_result$rotation[, 1:2]  # Zeigt die Ladungen der ersten beiden PCs

