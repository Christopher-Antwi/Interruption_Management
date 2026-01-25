library(tidyverse)
library(janitor)
library(kableExtra)
library(lubridate)
library(skimr)
library(hms)
library(knitr)
library(Rgraphviz)
library(readxl)
library(writexl)
library(data.table)
library(zoo)
library(distill)
library(scales)

LiW_Feeders = read_excel("data/processed/LiW_Feeders_Final.xlsx")



# Must Create Subsets Because Volmode and RefAct Are binary
liw_ids = c(1:5,7)
refact_cols = paste0("RefAct PD", liw_ids)
volmode_cols = paste0("VolMode PD", liw_ids)
shutdown_cols = c(refact_cols, volmode_cols)

running_index = rowSums(select(LiW_Feeders, all_of(shutdown_cols))) == 0


vars <- c(
  paste0("Massflow PD ", liw_ids),
  paste0("Screw RPM PD", liw_ids),
  paste0("Feed Factor PD", liw_ids),
  paste0("Estimated weight IBC PS", liw_ids),
  paste0("Net Weight PD", liw_ids)
)

X = as.matrix(LiW_Running[, vars])
X_s = scale(X, center = TRUE, scale = TRUE)

sv = svd(X_s)

sv_vec = apply(X, 2, sd, na.rm = TRUE)
sv_vec

keep_cols = sv_vec > 0 & !is.na(sv_vec)
X_clean = X[, keep_cols]
LiW_PCA_Active_Wip =  LiW_Feeders %>%
  filter(if_all(starts_with("RefAct"), ~.x ==0))
LiW_PCA_Active_Wip =  LiW_Feeders %>%
  filter(if_all(starts_with("VolMode"), ~.x ==0))

LiW_PCA_Active = LiW_PCA_Active_Wip %>%
  select(
    starts_with("Feed"),
    starts_with("Screw"),
    starts_with("Massflow"),
    starts_with("%"),
    starts_with("Estimated"),
    starts_with("Net")
  )

write_xlsx(LiW_PCA_Active, "LiW_PCA_Active.xlsx")
scaled_pca_liw = scale(LiW_PCA_Active)


pca_fit = prcomp(
  scaled_pca_liw,
  center = TRUE,
  scale. = FALSE
)
pca_sum <- summary(pca_fit)

pca_tbl <- data.frame(
  PC = paste0("PC", seq_along(pca_sum$importance["Proportion of Variance", ])),
  `Std. Deviation` = round(pca_sum$importance["Standard deviation", ], 3),
  `Proportion of Variance` = round(pca_sum$importance["Proportion of Variance", ], 4),
  `Cumulative Proportion` = round(pca_sum$importance["Cumulative Proportion", ], 4)
)

knitr::kable(
  pca_tbl,
  caption = "PCA Variance Explained Summary",
  align = "c"
)

scores <- as.data.frame(pca_fit$x)

liw_pca_summary = summary(pca_fit)

variance_exp = pca_fit$sdev^2/sum(pca_fit$sdev^2)

ggplot(
  data.frame(
    PC = seq_along(variance_exp),
    Variance = variance_exp
  ),
  aes(PC, Variance)
) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "LiW Feeders Variance Explained by PCA",
    y = " Proportion of Variance"
  )

cat("\n\n## Variance Explained\n\n")

plot(pca_fit, type = "lines")

# —- PCA Loading PNGs (exact filenames)

png_files <- c(
  
  "LiW PC1 Loadings.png",
  
  "LiW PC2 Loadings.png",
  
  "LiW PC3 Loadings.png",
  
  "LiW PC4 Loadings.png",
  
  "LiW PC5 Loadings.png",
  
  "LiW PC6 Loadings.png",
  
  "LiW PC7 Loadings.png",
  
  "LiW PC8 Loadings.png",
  
  "LiW PC9 Loadings.png",
  
  "LiW PC10 Loadings.png"
  
)

cat("\n\n## PCA Loading Plots\n\n")

for (f in png_files) {
  
  pc <- gsub(".*PC([0-9]+).*", "PC\\1", f)
  
  cat("###", pc, " Loadings\n\n", sep = "")
  
  knitr::include_graphics(f)
  
  cat("\n\n")
  
}
loadings = pca_fit$rotation
head(loadings[, 1:3])


LiW_PC1_Loadings = sort(loadings[,"PC1"], decreasing = TRUE)
LiW_PC1_Plot = barplot(
  LiW_PC1_Loadings,
  las = 2,
  cex.names = 0.6,
  main = "PC1 Loadings",
  ylab = "Loading Value"
)

LiW_PC2_Loadings = sort(loadings[,"PC2"], decreasing = TRUE)
LiW_PC2_Plot = barplot(
  LiW_PC2_Loadings,
  las = 2,
  cex.names = 0.6,
  main = "PC2 Loadings",
  ylab = "Loading Value"
)


LiW_PC3_Loadings = sort(loadings[,"PC3"], decreasing = TRUE)
LiW_PC3_Plot = barplot(
  LiW_PC3_Loadings,
  las = 2,
  cex.names = 0.6,
  main = "PC3 Loadings",
  ylab = "Loading Value"
)

LiW_PC4_Loadings = sort(loadings[,"PC4"], decreasing = TRUE)
LiW_PC4_Plot = barplot(
  LiW_PC4_Loadings,
  las = 2,
  cex.names = 0.6,
  main = "PC4 Loadings",
  ylab = "Loading Value"
)

LiW_PC5_Loadings = sort(loadings[,"PC5"], decreasing = TRUE)
LiW_PC5_Plot = barplot(
  LiW_PC5_Loadings,
  las = 2,
  cex.names = 0.6,
  main = "PC5 Loadings",
  ylab = "Loading Value"
)

LiW_PC6_Loadings = sort(loadings[,"PC6"], decreasing = TRUE)
LiW_PC6_Plot = barplot(
  LiW_PC6_Loadings,
  las = 2,
  cex.names = 0.6,
  main = "PC6 Loadings",
  ylab = "Loading Value"
)

LiW_PC7_Loadings = sort(loadings[,"PC7"], decreasing = TRUE)
LiW_PC7_Plot = barplot(
  LiW_PC7_Loadings,
  las = 2,
  cex.names = 0.6,
  main = "PC7 Loadings",
  ylab = "Loading Value"
)

LiW_PC8_Loadings = sort(loadings[,"PC8"], decreasing = TRUE)
LiW_PC8_Plot = barplot(
  LiW_PC8_Loadings,
  las = 2,
  cex.names = 0.6,
  main = "PC8 Loadings",
  ylab = "Loading Value"
)

LiW_PC9_Loadings = sort(loadings[,"PC9"], decreasing = TRUE)
LiW_PC9_Plot = barplot(
  LiW_PC9_Loadings,
  las = 2,
  cex.names = 0.6,
  main = "PC9 Loadings",
  ylab = "Loading Value"
)

LiW_PC10_Loadings = sort(loadings[,"PC10"], decreasing = TRUE)
LiW_PC10_Plot = barplot(
  LiW_PC10_Loadings,
  las = 2,
  cex.names = 0.6,
  main = "PC10 Loadings",
  ylab = "Loading Value"
)


## PCA Scores Plot
library(ggplot2)

scores <- as.data.frame(pca_fit$x)

PCA_Scores_Plot = ggplot(scores, aes(PC1, PC2)) +
  geom_point(size = 2, alpha = 0.7, color = "navy") +
  theme_minimal() +
  labs(
    title = "PCA Scores Plot",
    subtitle = "PC1 vs PC2",
    x = "PC1",
    y = "PC2"
  )
ggsave(
  filename = "PCA_Scores_Plot.png",
  plot = PCA_Scores_Plot,
  width = 8,
  height = 6,
  dpi = 300
)


# OLD 3D PCA PLOT LIW
library(plotly)
library(readxl)

LiW_PCA_Active = read_xlsx("LiW_PCA_Active.xlsx")
scaled_pca_liw = scale(LiW_PCA_Active)
pca_fit = prcomp(
  scaled_pca_liw,
  center = TRUE,
  scale. = FALSE
)

group_col = NULL
ve = (pca_fit$sdev^2)/sum(pca_fit$sdev^2)
pc1_lab = paste0("PC1 (", scales::percent(ve[1], accuracy = 0.1), ")")
pc2_lab = paste0("PC2 (", scales::percent(ve[2], accuracy = 0.1), ")")
pc3_lab = paste0("PC3 (", scales::percent(ve[3], accuracy = 0.1), ")")

scores <- as.data.frame(pca_fit$x)
loadings3 = pca_fit$rotation %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  as_tibble()

arrow_scale = 6

scores3 = scores %>% select(PC1, PC2, PC3, everything())
loads3  = loadings3 %>% transmute(variable,
                                  PC1 = PC1 * arrow_scale,
                                  PC2 = PC2 * arrow_scale,
                                  PC3 = PC3 * arrow_scale)

plt <- plot_ly()

# Points
if (!is.null(group_col)) {
  plt <- plt %>%
    add_markers(
      data = scores3,
      x = ~PC1, y = ~PC2, z = ~PC3,
      color = ~.data[[group_col]],
      marker = list(size = 3, opacity = 0.55)
    )
} else {
  plt <- plt %>%
    add_markers(
      data = scores3,
      x = ~PC1, y = ~PC2, z = ~PC3,
      marker = list(size = 3, opacity = 0.55)
    )
}

# Loading vectors (arrows as line segments from origin)
plt = plt %>%
  add_segments(
    data = loads3,
    x = 0, y = 0, z = 0,
    xend = ~PC1, yend = ~PC2, zend = ~PC3,
    line = list(width = 6),
    inherit = FALSE,
    showlegend = FALSE
  ) %>%
  add_text(
    data = loads3,
    x = ~PC1, y = ~PC2, z = ~PC3,
    text = ~variable,
    textposition = "top center",
    inherit = FALSE,
    showlegend = FALSE
  ) %>%
  layout(
    scene = list(
      xaxis = list(title = pc1_lab),
      yaxis = list(title = pc2_lab),
      zaxis = list(title = pc3_lab)
    ),
    title = "3D PCA Scores + Loadings"
  )


# ============================================================
# PCA: scaled loadings + 3 separate 3D plots (scores, loadings, biplot)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(plotly)
  library(readxl)
})


LiW_PCA_Active = read_xlsx("LiW_PCA_Active.xlsx")
scaled_pca_liw = scale(LiW_PCA_Active)
pca_fit = prcomp(
  scaled_pca_liw,
  center = TRUE,
  scale. = FALSE
)
# ---- USER INPUTS ----
# df: your numeric data frame/matrix used for PCA (rows = observations, cols = variables)
# Example: df <- your_data %>% select(where(is.numeric))
# group_col: optional column in your ORIGINAL data to color points (e.g., "Batch", "Tier", "Cluster")
# Set to NULL if you don't want grouping
# <<--- set this
group_col <- NULL   # <<--- e.g. "Batch" or NULL

# Optional: if grouping column lives in a different object, set it here:
# meta <- your_data %>% select(Batch)
meta <- NULL        # <<--- keep NULL unless you want group_col from some metadata frame




# ---- SCORES & LOADINGS (PC1-3) ----
scores3 <- as.data.frame(pca_fit$x[, 1:3, drop = FALSE]) %>%
  rownames_to_column("obs_id")

loads3 <- as.data.frame(pca_fit$rotation[, 1:3, drop = FALSE]) %>%
  rownames_to_column("variable")

colnames(scores3)[2:4] <- c("PC1", "PC2", "PC3")
colnames(loads3)[2:4]  <- c("PC1", "PC2", "PC3")

# ---- OPTIONAL GROUPING FOR SCORES ----
if (!is.null(group_col)) {
  if (!is.null(meta) && group_col %in% names(meta)) {
    scores3[[group_col]] <- meta[[group_col]]
  } else if (group_col %in% names(scaled_pca_liw)) {
    # If group column is inside df (usually not, but just in case)
    scores3[[group_col]] <- scaled_pca_liw[[group_col]]
  } else {
    warning("group_col not found in meta or df. Proceeding with no grouping.")
    group_col <- NULL
  }
}

# ---- SCALE LOADINGS TO MATCH SCORE SPACE ----
# Auto-scale so arrows are visible but not overwhelming:
# scale_factor maps unit-loadings into the score coordinate scale
score_max <- max(abs(unlist(scores3[, c("PC1","PC2","PC3")])), na.rm = TRUE)
load_max  <- max(abs(unlist(loads3[,  c("PC1","PC2","PC3")])),  na.rm = TRUE)

# You can tune arrow_size (0.3–0.8 is typical). Smaller = less spaghetti.
arrow_size <- 0.55
scale_factor <- arrow_size * (score_max / load_max)

loads3s <- loads3 %>%
  mutate(PC1s = PC1 * scale_factor,
         PC2s = PC2 * scale_factor,
         PC3s = PC3 * scale_factor)

# ---- HELPERS TO DRAW 3D ARROWS (as line segments from origin) ----
make_arrow_segments <- function(df_load_scaled) {
  # returns vectors with NA separators so plotly draws separate line segments
  x <- c(rbind(0, df_load_scaled$PC1s, NA))
  y <- c(rbind(0, df_load_scaled$PC2s, NA))
  z <- c(rbind(0, df_load_scaled$PC3s, NA))
  list(x = as.numeric(x), y = as.numeric(y), z = as.numeric(z))
}

seg <- make_arrow_segments(loads3s)

# ---- 1) SCORES-ONLY (3D) ----
p_scores_3d <- plot_ly(
  data = scores3,
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, opacity = 0.8)
)

if (!is.null(group_col)) {
  p_scores_3d <- plot_ly(
    data = scores3,
    x = ~PC1, y = ~PC2, z = ~PC3,
    type = "scatter3d",
    mode = "markers",
    color = scores3[[group_col]],
    marker = list(size = 3, opacity = 0.85)
  )
}

p_scores_3d <- p_scores_3d %>%
  layout(
    title = "PCA Scores (PC1–PC3)",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )

p_scores_3d

# ---- 2) LOADINGS-ONLY (3D, SCALED) ----
p_loadings_3d <- plot_ly() %>%
  add_trace(
    x = seg$x, y = seg$y, z = seg$z,
    type = "scatter3d",
    mode = "lines",
    line = list(width = 6),
    name = "Loadings (scaled)"
  ) %>%
  add_trace(
    data = loads3s,
    x = ~PC1s, y = ~PC2s, z = ~PC3s,
    type = "scatter3d",
    mode = "text",
    text = ~variable,
    textposition = "top center",
    name = "Variable labels"
  ) %>%
  layout(
    title = paste0("PCA Loadings (scaled) | scale_factor = ", signif(scale_factor, 3)),
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )

p_loadings_3d

# ---- 3) BIPLOT (3D: SCORES + SCALED LOADINGS) ----
p_biplot_3d <- plot_ly() %>%
  add_trace(
    data = scores3,
    x = ~PC1, y = ~PC2, z = ~PC3,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 3, opacity = 0.75),
    name = "Scores"
  ) %>%
  add_trace(
    x = seg$x, y = seg$y, z = seg$z,
    type = "scatter3d",
    mode = "lines",
    line = list(width = 6),
    name = "Loadings (scaled)"
  ) %>%
  add_trace(
    data = loads3s,
    x = ~PC1s, y = ~PC2s, z = ~PC3s,
    type = "scatter3d",
    mode = "text",
    text = ~variable,
    textposition = "top center",
    name = "Variable labels"
  ) %>%
  layout(
    title = "3D PCA Biplot (Scores + Scaled Loadings)",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )

# If grouped, recolor scores layer (rebuild biplot with color)
if (!is.null(group_col)) {
  p_biplot_3d <- plot_ly() %>%
    add_trace(
      data = scores3,
      x = ~PC1, y = ~PC2, z = ~PC3,
      type = "scatter3d",
      mode = "markers",
      color = scores3[[group_col]],
      marker = list(size = 3, opacity = 0.8),
      name = "Scores"
    ) %>%
    add_trace(
      x = seg$x, y = seg$y, z = seg$z,
      type = "scatter3d",
      mode = "lines",
      line = list(width = 6),
      name = "Loadings (scaled)"
    ) %>%
    add_trace(
      data = loads3s,
      x = ~PC1s, y = ~PC2s, z = ~PC3s,
      type = "scatter3d",
      mode = "text",
      text = ~variable,
      textposition = "top center",
      name = "Variable labels"
    ) %>%
    layout(
      title = "3D PCA Biplot (Scores + Scaled Loadings)",
      scene = list(
        xaxis = list(title = "PC1"),
        yaxis = list(title = "PC2"),
        zaxis = list(title = "PC3")
      )
    )
}

p_biplot_3d


# uncomment if you want files written to working directory
out_dir <- "docs"   # change if your output dir is different

htmlwidgets::saveWidget(p_scores_3d,   file.path(out_dir, "pca_scores_3d.html"),   selfcontained = TRUE)
htmlwidgets::saveWidget(p_loadings_3d, file.path(out_dir, "pca_loadings_3d.html"), selfcontained = TRUE)
htmlwidgets::saveWidget(p_biplot_3d,   file.path(out_dir, "pca_biplot_3d.html"),   selfcontained = TRUE)

