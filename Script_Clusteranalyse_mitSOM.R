################################################################################
# 
# Zweischritt-Datenclusterung mit SOM und k-Means / k-Medoids in R
#
#
# geeignet für nicht-normalverteilte Datensätze (z.B. zu musikalischer Performanz)
# Stand: 22.01.2025, Version 1.0
# zusammengetragen von Frithjof Vollmer
#
#
# Das folgende kommentierte Skript gibt die Befehlskette für eine Zweischritt-
# Clusteranalyse von performanzbezogenen Daten wieder, wie sie vom Autor zur 
# Analyse von Messdaten zu 57 Aufnahmen Beethovens Violinkonzert op. 61 
# (1912-1956) erwogen wurde. 
#
#
# Zentrale Pakete und Funktionen:
#
# "kohonen"     (neuronales Netzwerk, selbstorganisierende Karte zur Reduktion
#               der Dimensionen bei vielen Variablen, Vorsortierung)
# "kmeans"      (für k-Means-Clusteranalyse)
# "cluster"     (Standardpaket für robustere Clustermethoden)
# "pam"         (Funktion aus "cluster" für k-Medoids-Analyse)
#
#
# Ausgaben:     (werden im Arbeitsverzeichnis abgespeichert)
#
# "_Analyse"    PDF mit Abbildungen zur SOM: Lernfortschritt, Gewichtungsvektoren, 
#               Datenpunktverteilung, U-Matrix, Visualisierungen der Zentren
#               und Verteilungen der k-Means- und k-Medoids-Analysen auf der SOM
# "_ANOVA"      Analysis of Variance (Prüfung auf signifikante Unterschiede der 
#               Clustervektoren) 
# "_Distanzen"  PDF mit Scatter Plot (Punktdiagramm) der Datenpunkte pro Cluster 
#               und deren Entfernung von den Clusterzentren 
# "_sort_kM"    CSV mit Zuordnung der Datenpunkte nach k-Means-Analyse, 
#               gestaffelt nach Entfernung vom Clusterzentrum
# "_sort_kMed"  ebd., CSV für k-Medoids-Analyse
# "_CVect"      CSV mit Vektoren der Clusterzentren 
# "_mitCluster" ursprünglicher Datensatz als CSV, pro Datenpunkt ergänzt um 
#               Neuronen- und Clusterzugehörigkeit
#
################################################################################

# Schnellzugriff: Anzahl der Cluster für k-Means und k-Medoids setzen

k <- k_medoids <- 3 # 3 Cluster 

# Pfad zum Arbeitsverzeichnis setzen (Dateipfad, Pfade in den Klammern sind
# entsprechend anzupassen. Ordner kann in der R-Konsole einfach zwischen die ""  
# gezogen werden, es erscheint der Pfad). 

setwd("~/Desktop/Clusteranalysen B61/Tests")

# Laden des Datensatzes in die Variablen "file_path" (um den Dateinamen für spätere
# Ausgaben zu verwenden) und "data" (Quelldatensatz): Den unten gegebenen Pfad  
# "cantabile_data.csv" durch den eigenen CSV-Datensatz im Arbeitsverzeichnis ersetzen
# (geht auch mit anderen Dateiformaten wie etwa TXT, dann über "read.table"). In 
# diesen Einstellungen enthalten die Werte nach internationalem Standard 
# Punkte (".") als Dezimaltrennzeichen und sind durch Kommata (",") voneinander 
# getrennt. Wird zur Datensatzerstellung z.B. mit einer deutschsprachigen Version 
# von Excel gearbeitet, werden die Dateien evtl. mit Kommata (",") als 
# Dezimaltrennzeichen und Semikolons als Separatoren (";") ausgegeben. Die Werte 
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
data <- read.csv(file_path,  header = TRUE, sep = ",", dec = ".", 
                 row.names = 1)

# Fehlermeldung, falls der Datensatz nicht-numerische  Spalten enthält (SOM 
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

# Alle Abbildungen (Plots) der folgenden Analysen in einer gesammelten PDF im 
# Arbeitsverzeichnis speichern
pdf(file = paste0(output_name, "_Analyse.pdf"))


### Trainieren des Kohonen-Netzwerks (Vorsortierung der Datenpunkte) ###########

# Installieren und Laden des Pakets "kohonen", falls noch nicht vorhanden
if (!requireNamespace("kohonen", quietly = TRUE)) install.packages("kohonen")
library(kohonen)

# Definition des SOM-Grids (3x3)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "rectangular")


# Funktion zur Anpassung der Grid-Größe an die Größe des Datensatzes
adjust_som_grid_dynamic <- function(data, xdim, ydim, min_ratio = 5, max_ratio = 20) {
  n <- nrow(data)  # Anzahl der Datenpunkte
  original_neurons <- xdim * ydim  # Ursprüngliche Anzahl der Neuronen
  ratio <- n / original_neurons  # Aktuelles Verhältnis
  
  # Empfehlungen für Neuronenanzahl basierend auf min_ratio und max_ratio
  min_neurons <- ceiling(n / max_ratio)
  max_neurons <- floor(n / min_ratio)
  
  # Überprüfung auf zu niedriges Verhältnis
  if (original_neurons < min_neurons) {
    # Anpassen der Neuronenanzahl nach oben
    recommended_neurons <- min_neurons
    new_dim <- ceiling(sqrt(recommended_neurons))
    xdim <- new_dim
    ydim <- new_dim
    warning(paste(
      "Das Verhältnis von Datenpunkten zu Neuronen war zu hoch.",
      "Die ursprüngliche Anzahl der Neuronen betrug", original_neurons, 
      "(", xdim, "x", ydim, "Grid).",
      "Es wurde auf", xdim * ydim, "Neuronen angepasst (", new_dim, "x", new_dim, "Grid),",
      "um ein Verhältnis von maximal", max_ratio, "Datenpunkten pro Neuron zu gewährleisten."
    ))
  }
  
  # Überprüfung auf zu hohes Verhältnis
  if (original_neurons > max_neurons) {
    # Anpassen der Neuronenanzahl nach unten
    recommended_neurons <- max_neurons
    new_dim <- floor(sqrt(recommended_neurons))
    xdim <- new_dim
    ydim <- new_dim
    warning(paste(
      "Das Verhältnis von Datenpunkten zu Neuronen war zu niedrig.",
      "Die ursprüngliche Anzahl der Neuronen betrug", original_neurons, 
      "(", xdim, "x", ydim, "Grid).",
      "Es wurde auf", xdim * ydim, "Neuronen angepasst (", new_dim, "x", new_dim, "Grid),",
      "um ein Verhältnis von mindestens", min_ratio, "Datenpunkten pro Neuron zu gewährleisten."
    ))
  }
  
  return(list(xdim = xdim, ydim = ydim))
}

# Anwendung der Grid-Anpassungsfunktion, falls nötig  
grid_dims <- adjust_som_grid_dynamic(data = data_numeric, xdim = 5, ydim = 5, 
                                     min_ratio = 5, max_ratio = 20)
xdim <- grid_dims$xdim
ydim <- grid_dims$ydim

som_grid <- somgrid(xdim = xdim, ydim = ydim, topo = "rectangular")


# Training der SOM (unsupervised)
som_model <- som(data_numeric, 
                 grid = som_grid, 
                 rlen = 5000,          # Anzahl der Iterationen
                 alpha = c(2, 0.01))   # Anfangs- und Endlernrate

# Plot zur Veränderungen während des Trainings (Ausgabe PDF im Arbeitsverzeichnis)
plot(som_model, type = "changes", main = "Lernfortschritt SOM")

# Diese SOM wurde mit einer sehr hohen Anfangslernrate ("alpha = c(2, ...)") erstellt, 
# um die Hochdimensionalität des Beethoven-Datensatzes zu verarbeiten, was aber auch 
# zu höheren Fehlerraten führen kann. Schätzen Sie in der Ihnen angezeigten Kurve 
# zum Lernfortschritt ein, ob er ungefähr umgekehrt logarithmisch verläuft (anfangs
# stark abnehmend, dann auslaufend). Wenn nicht, experimentieren Sie ein wenig mit
# den Lernraten und der Iterationsanzahl ("rlen").

# Die folgenden Visalisierungen ("plot") zeigen die Ergebnisse des Trainings:

# Gewichtungsvektoren der Neuronen anzeigen
plot(som_model, type = "codes", main = paste("SOM-Neuronen", output_name, "(Gewichtungsvektoren)"))

# Visualisierung der Datenpunktverteilung auf den SOM-Neuronen ("Hitmap")
plot(som_model, type = "count", main = paste("Datenpunktverteilung auf den SOM-Neuronen", output_name))

# Visualisierung der U-Matrix (Distanzen zwischen den Gewichtsvektoren benachbarter 
# Neuronen: zur Bestimmung geeigneter Clustergrößen für k-Means und k-Medoids)
plot(som_model, type = "dist.neighbours", main = paste("U-Matrix SOM", output_name,
     "(Distanzen zwischen benachbarten Neuronen)"))

# Schätzen Sie die Zahl geeigneter Cluster anhand dieser U-Matrix: helle Farben 
# deuten auf eng beieinanderliegende Neuronen, dunkle auf Clustergrenzen. Gibt es 
# bspw. drei große helle Bereiche in der SOM, deutet dies auf drei Cluster. Wenn 
# Sie mit der Clusterzahl unsicher sind, wiederholen Sie die folgenden Schritte
# mehrfach mit verschiedenen Clusterzahlen, bis sich das Ergebnis sinnvoll 
# interpretieren lässt.   


### k-Means-Clusteranalyse #####################################################

# Es folgen die k-Means und k-Medoids-Analysen: k-Means ist schneller und 
# funktioniert auch gut mit großen Datensätzen; k-Medoids ist robuster gegenüber
# Ausreißern sowie unsymmetrischen Datenverteilungen und stabiler bei kleinen
# Datensätzen. Im oben genannten Beispiel der Beethoven-Tondokumente wurde sich  
# deshalb letztlich für die k-Medoids-Analyseergebnisse entschieden. 

# Extrahieren der Codebook-Vektoren
codebook_vectors <- som_model$codes[[1]]

# Fehlermeldung, falls Clusterzahl zu groß
if (k > nrow(codebook_vectors)) {
  stop("Die Anzahl der Cluster (k) darf nicht größer sein als die Anzahl der Neuronen.")
}

# Seed (Startwert für den Zufallsgenerator) setzen, damit Analysen reporduzierbar
# werden (beliebige Zahl einsetzen)
set.seed(123)  

# k-Means-Clustering der Codebook-Vektoren
kmeans_result <- kmeans(codebook_vectors, centers = k)

# Clusterzuweisungen der Neuronen
neuron_clusters <- kmeans_result$cluster

# Berechnung und Ausgabe der paarweisen Distanzen zwischen den Clusterzentren
kmeans_distances <- dist(kmeans_result$centers)
print(as.matrix(kmeans_distances))

# Anzeige der Vektoren eines jeden Clusterzentrums
print("Vektoren der k-Means-Clusterzentren:")
print(kmeans_result$centers)

# Visualisierung der Vektoren als Diagramm mit gruppierten Balken
# Erstellen der benutzerdefinierten Farbpalette von light yellow bis dark red
color_palette <- colorRampPalette(c("lightyellow", "darkred"))(ncol(kmeans_result$centers))
# Visualisierung der k-Means Clusterzentren 
barplot(t(kmeans_result$centers), beside = TRUE, 
        col = color_palette,  # Farbschema von light yellow bis dark red
        main = "Visualisierung der k-Means Clusterzentren", 
        xlab = "Cluster", ylab = "Gestaltungsvariablen (z-standardisiert)", 
        las = 2)  # Balkenbezeichner vertikal anzeigen


# Die folgenden Zeilen dienen der Herstellung einer hübschen Visualisierung der 
# Clusterzuweisungen. Die Zahl der Datenpunkte in den Neuronen ist durch kleine 
# angezeigt; transparente Textfelder geben die Neuronennummer an (von 1 = links
# unten bis 25 = rechts oben).

# Farbpalette in Gelb-Orange-Rot-Abstufung definieren
heat_colors <- colorRampPalette(c("lightyellow", "orange", "red"))(k)
neuron_colors <- heat_colors[neuron_clusters]  # Farben basierend auf Clusterzuweisungen

# SOM-Plot mit Verteilungen und Clusterfarben
plot(som_model, type = "mapping", main = "Verteilungen mit k-Means-Clustern", 
     bgcol = neuron_colors)

# Hinzufügen der Datenpunkte zur Karte
points(som_model$grid$pts, pch = 21, bg = neuron_colors, col = "black")

# Positionen der Neuronen
neuron_positions <- som_model$grid$pts  # Positionen der Neuronen

# Hinzufügen der Neuronennummern mit einem "transparenten" Textfeld
for (i in 1:nrow(neuron_positions)) {
  neuron_x <- neuron_positions[i, 1]
  neuron_y <- neuron_positions[i, 2] - 0.2  # Text an unteren Rand des Neurons setzen
  
  # Hintergrundrechteck für Textfeld
  rect(neuron_x - 0.3, neuron_y - 0.1, neuron_x + 0.3, neuron_y + 0.1, 
       col = rgb(1, 1, 1, 0.7), border = NA)  # Rechteck mit Transparenz
  
  # Neuronennummer einfügen
  text(neuron_x, neuron_y, labels = i, col = "black", cex = 0.8)
}

# Legende hinzufügen
legend("topright", legend = paste("Cluster", 1:k), fill = heat_colors, cex = 0.8)


### k-Medoids-Analyse (mit euklidischer Distanz) ###############################

# (Euklidisch deshalb, da vor SOM-Training standardisiert wurde; Gower-Matrix 
# würde die Distanzen hier also durch weitere Normalisierung verzerren.)

# Erinnerung: Clusterzahl (k) wird von oben übernommen (k-Means)

# Installieren und Laden des Pakets "cluster" (für k-Medoids)
if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
library(cluster)

# Berechnung der euklidischen Distanzmatrix für Codebook-Vektoren
euclidean_distance <- dist(codebook_vectors, method = "euclidean")

# Fehlermeldungen bei ungültiger Distanzmatric  
if (any(is.na(euclidean_distance)) || any(is.infinite(euclidean_distance))) {
  stop("Die Distanzmatrix enthält ungültige Werte. Überprüfen Sie die Eingabedaten.")
}

# Durchführung k-Medoids-Clustering mit euklidischer Distanz
set.seed(456)  # Seed setzen für Reproduzierbarkeit
kmedoids_clusters <- pam(euclidean_distance, k = k_medoids)

# Clusterzuweisungen der Neuronen (k-Medoids)
neuron_clusters_euclidean <- kmedoids_clusters$clustering

# Berechnung und Ausgabe der paarweisen Distanzen zwischen den Clusterzentren
kmedoids_centers_euclidean <- codebook_vectors[kmedoids_clusters$id.med, ]
kmedoids_distances_euclidean <- dist(kmedoids_centers_euclidean)
print(as.matrix(kmedoids_distances_euclidean))

# Anzeige der Vektoren eines jeden Clusterzentrums (Medoids) der k-Medoids-Analyse
print("Vektoren der k-Medoids-Clusterzentren (Euklidische Distanz):")
print(codebook_vectors[kmedoids_clusters$id.med, ])  # Medoids als Clusterzentren

# Visualisierung der Vektoren als Diagramm mit gruppierten Balken
barplot(t(codebook_vectors[kmedoids_clusters$id.med, ]), beside = TRUE, 
        col = color_palette,  # Farbschema von light yellow bis dark red
        main = "Visualisierung der k-Medoids Clusterzentren", 
        xlab = "Neuron mit Medoid", ylab = "Gestaltungsvariablen (z-standardisiert)", 
        las = 2)  # Balkenbezeichner vertikal anzeigen


# Visualisierungsabschnitt: Farbpalette in Gelb-Orange-Rot-Abstufung definieren
heatmap_colors <- colorRampPalette(c("lightyellow", "orange", "red"))(k_medoids)
neuron_colors_medoid <- heatmap_colors[neuron_clusters_euclidean]  # Farben basierend auf Clusterzuweisungen

# SOM-Plot mit Verteilungen und Clusterfarben
plot(som_model, type = "mapping", main = "Verteilungen mit k-Medoids-Clustern", 
     bgcol = neuron_colors_medoid)

# Hinzufügen der Datenpunkte zur Karte
points(som_model$grid$pts, pch = 21, bg = neuron_colors_medoid, col = "black")

# Positionen der Neuronen
neuron_positions_medoids <- som_model$grid$pts  # Positionen der Neuronen

# Hinzufügen der Neuronennummern mit einem "transparenten" Textfeld
for (i in 1:nrow(neuron_positions_medoids)) {
  neuron_x <- neuron_positions_medoids[i, 1]
  neuron_y <- neuron_positions_medoids[i, 2] - 0.2  # Text an unteren Rand des Neurons setzen
  
  # Hintergrundrechteck für Textfeld
  rect(neuron_x - 0.3, neuron_y - 0.1, neuron_x + 0.3, neuron_y + 0.1, 
       col = rgb(1, 1, 1, 0.7), border = NA)  # Rechteck mit Transparenz
  
  # Neuronennummer einfügen
  text(neuron_x, neuron_y, labels = i, col = "black", cex = 0.8)
}

# Legende hinzufügen
legend("topright", legend = paste("Cluster", 1:k_medoids), fill = heatmap_colors, cex = 0.8)

### Ergänzung der Analyseergebnisse im ursprünglichen Datensatz ################

# 1. Neuronenzuordnung
neuron_assignments <- som_model$unit.classif  # Neuronenzuordnung für jeden Datenpunkt

# 2. k-Means-Clusterzuordnung und Entfernungen
cluster_assignments_kmeans <- neuron_clusters[neuron_assignments]  # Cluster basierend auf k-Means
distances_to_kmeans <- sapply(1:nrow(data), function(i) {
  cluster_center <- kmeans_result$centers[cluster_assignments_kmeans[i], ]  # Clusterzentrum des Datenpunkts
  sqrt(sum((data_numeric[i, ] - cluster_center)^2))  # Euklidische Distanz
})

# 3. # k-Medoids-Clusterzuordnung und Entfernungen
cluster_assignments_kmedoids <- neuron_clusters_euclidean[neuron_assignments]  # Cluster basierend auf k-Medoids
distances_to_kmedoids <- sapply(1:nrow(data), function(i) {
  medoid <- codebook_vectors[kmedoids_clusters$id.med[cluster_assignments_kmedoids[i]], ]  # Medoid des Datenpunkts
  sqrt(sum((data_numeric[i, ] - medoid)^2))  # Euklidische Distanz
})

# 4. Erweiterung des Datensatzes "data" um die Zuordnungen und Entfernungen
data$Neuron <- neuron_assignments
data$Cluster_kMeans <- cluster_assignments_kmeans
data$Distance_kMeans <- distances_to_kmeans
data$Cluster_kMedoids <- cluster_assignments_kmedoids
data$Distance_kMedoids <- distances_to_kmedoids

# Vorschau des erweiterten Datensatzes
head(data)

# Ausgabe des aktualisierten Datensatzes als CSV. Wenn in internationalen Standard
# (Punkt als Dezimalzeichen, Komma als Separator) gewünscht: "csv" statt "csv2"
write.csv2(data, file = paste0(output_name, "_mitCluster.csv"))


### Gesonderte Ausgabe der Vektoren der Clusterzentren als CSV (falls Bedarf) ##

# k-Means Clusterzentren als Data Frame
kmeans_centers <- as.data.frame(kmeans_result$centers)
kmeans_centers$Method <- "k-Means"  # Hinzufügen der Spalte "Method" mit "k-Means"

# k-Medoids Clusterzentren als Data Frame
kmedoids_centers <- as.data.frame(codebook_vectors[kmedoids_clusters$id.med, ])
kmedoids_centers$Method <- "k-Medoids"  # Hinzufügen der Spalte "Method" mit "k-Medoids"

# Kombinieren der beiden Clusterzentren
combined_centers <- rbind(kmeans_centers, kmedoids_centers)

# Speichern der kombinierten Clusterzentren in einer CSV-Datei
write.csv2(combined_centers, file = paste0(output_name, "_CVect.csv"), row.names = TRUE)

# Ausgabe der erfolgreichen Speicherung
print(cat("Die kombinierten Clusterzentren wurden als CSV-Datei gespeichert: ", output_name,"_CVect.csv"))


### Ausgabe der Visualisierungen als PDF #######################################

# laufende PDF-Aufzeichnung mit Analyseplots (SOM, k-Means, k-Medoids) beenden 
dev.off()

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


###

# zusätzliche PDF (neues Seitenverhältnis) mit Scatter Plots zur Clusterzuteilung  
# der Datenpunkte, Staffelung nach Entfernungen zum Clusterzentrum.
# 1. Dynamische Anpassung der Schriftgröße und Plot-Breite
adjust_plot_parameters <- function(n_rows) {
  plot_width <- max(11, n_rows * 0.2)  # Breite an Anzahl der Zeilen anpassen
  font_size <- max(6, 12 - n_rows * 0.05)  # Schriftgröße (min. 6)
  bottom_margin <- max(5, n_rows * 0.15)  # Unterer Rand für Labels
  list(width = plot_width, font_size = font_size / 10, bottom_margin = bottom_margin)
}

# 2. k-Means: Scatterplot mit dynamisch angepasster x-Achse
plot_kMeans <- function(cluster_table, params) {
  par(mar = c(params$bottom_margin, 2, 4, 2))  # Dynamischer Rand
  plot(cluster_table$Distance_kMeans, 
       col = cluster_table$Cluster_kMeans, 
       pch = 19, 
       main = "Entfernungen zu k-Means-Clusterzentren",
       xlab = "",  # Keine Achsenbeschriftung
       ylab = "",  # Keine Achsenbeschriftung
       xaxt = "n", 
       yaxt = "n")
  axis(1, at = 1:nrow(cluster_table), labels = cluster_table$Zeilenname, las = 2, cex.axis = params$font_size)
  legend("top", legend = unique(cluster_table$Cluster_kMeans), 
         col = unique(cluster_table$Cluster_kMeans), 
         pch = 19, title = "Cluster", horiz = TRUE, bty = "n")
}

# 3. k-Medoids: Scatterplot mit dynamisch angepasster x-Achse
plot_kMedoids <- function(cluster_table, params) {
  par(mar = c(params$bottom_margin, 2, 4, 2))  # Dynamischer Rand
  plot(cluster_table$Distance_kMedoids, 
       col = cluster_table$Cluster_kMedoids, 
       pch = 19, 
       main = "Entfernungen zu k-Medoids-Clusterzentren",
       xlab = "",  # Keine Achsenbeschriftung
       ylab = "",  # Keine Achsenbeschriftung
       xaxt = "n", 
       yaxt = "n")
  axis(1, at = 1:nrow(cluster_table), labels = cluster_table$Zeilenname, las = 2, cex.axis = params$font_size)
  legend("top", legend = unique(cluster_table$Cluster_kMedoids), 
         col = unique(cluster_table$Cluster_kMedoids), 
         pch = 19, title = "Cluster", horiz = TRUE, bty = "n")
}

# Sortierte Tabelle für die Visualisierung
cluster_table_sorted_kMeans <- cluster_table[order(cluster_table$Cluster_kMeans, cluster_table$Distance_kMeans), ]
cluster_table_sorted_kMedoids <- cluster_table[order(cluster_table$Cluster_kMedoids, cluster_table$Distance_kMedoids), ]

# Dynamische Parameter basierend auf der Anzahl der Zeilen
params <- adjust_plot_parameters(nrow(cluster_table))

# PDF speichern mit dynamischer Breite
pdf(paste0(output_name,"_Distanzen.pdf"), width = params$width, height = 8.5)

# k-Means Plot
plot_kMeans(cluster_table_sorted_kMeans, params)

# k-Medoids Plot
plot_kMedoids(cluster_table_sorted_kMedoids, params)

# PDF schließen
dev.off()

# Hinweis auf gespeicherte Abbildungen
print(cat("Die angepassten Abbildungen zu den Clusterzuordnungen und Distanzen wurden 
      als PDF gespeichert:", output_name,"_Distanzen.pdf"))


### Ergänzung ANOVA (Analysis of Variance): prüft die Clustervektoren auf signifikante
# Unterschiede. Wird separat als CSV ausgegeben. ############################### 

# ANOVA für Cluster basierend auf den Neuronenzuordnungen
anova_results <- data.frame(Variable = character(), 
                            Cluster_Method = character(), 
                            Df = integer(), 
                            SumSq = numeric(), 
                            MeanSq = numeric(), 
                            FValue = numeric(), 
                            Pr_F = numeric(), 
                            stringsAsFactors = FALSE)

# ANOVA für jede Variable unter Verwendung der Neuronenzuordnungen
for (i in 1:ncol(data_numeric)) {
  # ANOVA für jede Variable (Spalte) unter Verwendung der Neuronenzuordnungen
  anova_result <- summary(aov(data_numeric[, i] ~ factor(som_model$unit.classif)))
  
  # Extrahieren der ANOVA-Ergebnisse
  df <- anova_result[[1]]$Df[1]  # Degrees of Freedom
  sum_sq <- anova_result[[1]]$`Sum Sq`[1]  # Sum of Squares
  mean_sq <- anova_result[[1]]$`Mean Sq`[1]  # Mean Square
  f_value <- anova_result[[1]]$`F value`[1]  # F-Value
  p_value <- anova_result[[1]]$`Pr(>F)`[1]  # p-value
  
  # Ergebnisse in der Tabelle speichern
  anova_results <- rbind(anova_results, 
                         data.frame(Variable = colnames(data_numeric)[i],
                                    Cluster_Method = "SOM",
                                    Df = df, 
                                    SumSq = sum_sq, 
                                    MeanSq = mean_sq, 
                                    FValue = f_value, 
                                    Pr_F = p_value))
}

# Ausgabe der ANOVA-Ergebnisse als CSV
write.csv2(anova_results, file = paste0(output_name, "_ANOVA.csv"), row.names = FALSE)

# Ausgabe der ANOVA-Ergebnisse
print(cat(output_name, ": ANOVA Ergebnisse für Cluster (basierend auf Neuronenzuordnungen):"))
print(anova_results)


### Einige abschließende Prüfungen und Warnhinweise: 

# Funktion zur Überprüfung der Zuordnung und Konsistenz der Clusterergebnisse
check_cluster_mapping <- function(data, neuron_clusters, cluster_assignments, cluster_method) {
  # 1. Überprüfung, ob jedem Neuron ein Cluster zugewiesen wurde
  if (any(is.na(neuron_clusters))) {
    warning(paste("Die Neuronenzuordnungen enthalten NA-Werte für", cluster_method, ".",
                  "Stellen Sie sicher, dass alle Neuronen einem Cluster zugeordnet sind."))
  }
  
  # 2. Überprüfung, ob jedem Datenpunkt ein gültiges Cluster zugewiesen wurde
  if (any(is.na(cluster_assignments))) {
    warning(paste("Die Datenpunkt-Clusterzuweisungen enthalten NA-Werte für", cluster_method, ".",
                  "Bitte überprüfen Sie die Zuordnung der Cluster zu den Datenpunkten."))
  }
  
  # 3. Überprüfung, ob die Clusterzuweisungen die erwartete Anzahl an Clustern enthalten
  unique_clusters <- unique(cluster_assignments)
  if (length(unique_clusters) < length(unique(neuron_clusters))) {
    warning(paste("Die Anzahl der gefundenen Cluster für", cluster_method,
                  "ist geringer als erwartet. Überprüfen Sie die Clusterparameter."))
  }
  
  # 4. Überprüfung auf leere Cluster
  cluster_counts <- table(cluster_assignments)
  if (any(cluster_counts == 0)) {
    warning(paste("Es gibt leere Cluster für", cluster_method, ".",
                  "Bitte prüfen Sie die Clusteranzahl und Datenverteilung."))
  }
  
  # 5. Konsistenzprüfung: Stimmt die Zuordnung von Datenpunkten zu Neuronen mit den Clustern überein?
  invalid_mapping <- sum(cluster_assignments != neuron_clusters[data$Neuron])
  if (invalid_mapping > 0) {
    warning(paste("Die Zuordnung der Datenpunkte zu Clustern ist für", cluster_method,
                  "nicht konsistent mit den Neuronenzuordnungen.",
                  invalid_mapping, "Datenpunkte sind betroffen."))
  }
  
  # 6. Ausgabe einer Bestätigung, wenn keine Probleme gefunden wurden
  if (!any(is.na(neuron_clusters)) &&
      !any(is.na(cluster_assignments)) &&
      length(unique_clusters) >= length(unique(neuron_clusters)) &&
      all(cluster_counts > 0) &&
      invalid_mapping == 0) {
    message(paste("Die Clusterzuweisungen für", cluster_method, "sind korrekt und konsistent."))
  }
}
# Beispielaufruf für k-Medoids-Clusteranalyse
check_cluster_mapping(
  data = data,
  neuron_clusters = neuron_clusters_euclidean,
  cluster_assignments = data$Cluster_kMedoids,
  cluster_method = "k-Medoids"
)

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
  expected_clusters = 3,  # Anzahl der erwarteten Cluster
  method_name = "k-Means und k-Medoids"
)

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

# ANOVA-Zusammenfassung
check_anova_p_values(paste0(output_name, "_ANOVA.csv"))