#!/usr/bin/env Rscript

petri_plot <- function(data) {
  petri_data <- data[paste0("petri", 1:4), ]
  plot(
    c(), c(),
    xlim = c(1, ncol(data)),
    ylim = c(0, max(petri_data, na.rm = TRUE)), xaxt='n', xlab="Group")
  axis(side = 1, at = 1:4, labels = c("No Mask 1", "Surgical", "Cotton", "No Mask 2"))
  legend(x = "bottomright", legend = colnames(petri_data), col=1:ncol(petri_data), pch=19)
  for (idx in 1:ncol(petri_data)) {
    points(1:ncol(petri_data), petri_data[, idx], col=idx, pch=19)
    lines(1:ncol(petri_data), petri_data[, idx], col=idx)
  }
}

mask_plot <- function(data) {
  mask_data <- data[c("SO", "CO", "SI", "CI"), ]
  pheatmap::pheatmap(
    mat = mask_data,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    color = viridis::viridis(20))
}

data <- read.csv("viral_load.csv", header=TRUE, row.names=1)
petri_plot(data)
mask_plot(data)
