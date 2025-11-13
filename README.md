<b>Data Clustering via k-Means, k-Medoids, and PCA in R</b>  
applicable to non-normally distributed datasets (e.g., in musical performance research)  
Version 1.0 (08 March 2025) 
compiled by Frithjof Vollmer  

This script reproduces the command sequence for a dual cluster analysis of performance-related data, as 
employed by the author in the analysis of measurement data from 57 recordings of Beethoven’s Violin Concerto, 
Op. 61 (1912–1956). Explanatory comments in German.

Core packages and functions:

- kmeans – for k-means cluster analysis
- cluster – standard package for more robust clustering methods
- pam – function from cluster for k-medoids analysis
- prcomp – for principal component analysis (PCA)

Output files (saved to the working directory):
- _Analyse – visualization of cluster centres
- _ANOVA – analysis of variance testing for significant differences among cluster vectors
- _Distanzen – PDF with scatter plot of data points per cluster and their distances from the cluster centres
- _sort_kM – CSV file assigning data points to clusters from the k-means analysis, ordered by distance from the cluster centre
- _sort_kMed – same as above, for the k-medoids analysis
- _CVect – CSV file containing the cluster centre vectors
- _mitCluster – original dataset as CSV, augmented with cluster membership for each data point
- _PCA – principal component analyses for cluster interpretation (visualizations in 2D and 3D scatter plots for 2PC and 3PC, respectively)
