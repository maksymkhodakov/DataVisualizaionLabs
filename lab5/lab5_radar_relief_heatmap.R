# ==========================================================
# Лабораторна робота №5
# Тема: Радіальні діаграми (полігони), рельєфні діаграми та теплові карти
# Мета: Опанувати radar / relief (2D+3D) / heatmap та коректне оформлення.
# ==========================================================

# -------------------------------
# 0) Підготовка середовища
# -------------------------------
packages_required <- c("ggplot2", "dplyr", "tidyr", "scales", "reshape2", "fmsb", "viridisLite")

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, dependencies = TRUE)
    }
  }
}
install_if_missing(packages_required)

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(reshape2)
library(fmsb)
library(viridisLite)

# Папка для результатів
out_dir <- "plots_lab5"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Уніфікована тема для ggplot2
theme_ua <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Допоміжна функція: зберегти base R графік у PNG та PDF
save_both_base <- function(name_base,
                           w_px = 1600, h_px = 1000, res = 170,
                           w_in = 10, h_in = 6.5,
                           expr_plot) {
  # PNG
  png(file.path(out_dir, paste0(name_base, ".png")),
      width = w_px, height = h_px, res = res, bg = "white")
  expr_plot()
  dev.off()
  
  # PDF
  pdf(file.path(out_dir, paste0(name_base, ".pdf")),
      width = w_in, height = h_in)
  expr_plot()
  dev.off()
}

# Допоміжна функція: зберегти ggplot2 графік у PNG та PDF
save_both_gg <- function(plot_obj, name_base, w = 10, h = 7, dpi = 300) {
  ggsave(file.path(out_dir, paste0(name_base, ".png")),
         plot = plot_obj, width = w, height = h, dpi = dpi, bg = "white")
  ggsave(file.path(out_dir, paste0(name_base, ".pdf")),
         plot = plot_obj, width = w, height = h, dpi = dpi, bg = "white")
}

# -------------------------------
# 1) RADAR (радіальні діаграми)
# -------------------------------
# Дані: профілі двох моделей по 6 метриках (0..100)
df_models <- data.frame(
  Accuracy  = c(85, 78),
  Recall    = c(80, 82),
  Precision = c(88, 75),
  F1        = c(84, 78),
  AUC       = c(90, 81),
  Kappa     = c(72, 68)
)
rownames(df_models) <- c("Model_A", "Model_B")

# Фіксуємо порядок осей radar (важливо для відтворюваності!)
axis_order <- c("Accuracy", "Recall", "Precision", "F1", "AUC", "Kappa")
df_models <- df_models[, axis_order, drop = FALSE]

# 1.1 Radar зі шкалою 0–100 (БЕЗ нормалізації)
# fmsb вимагає:
# 1-й рядок = max значення по кожній осі, 2-й = min значення
# Щоб шкала була СПІЛЬНА і зрозуміла — фіксуємо max=100, min=0
df_radar_0_100 <- rbind(
  setNames(rep(100, length(axis_order)), axis_order),
  setNames(rep(0,   length(axis_order)), axis_order),
  df_models
)

save_both_base(
  name_base = "fig_lab5_01_radar_scale_0_100",
  w_in = 9.5, h_in = 7,
  expr_plot = function() {
    op <- par(no.readonly = TRUE)
    par(mar = c(2, 2, 3, 2))
    
    radarchart(
      df_radar_0_100,
      axistype = 1,
      seg = 5,
      caxislabels = c("0", "25", "50", "75", "100"),
      pcol  = c("#1b9e77", "#d95f02"),
      pfcol = alpha(c("#1b9e77", "#d95f02"), 0.25),
      plwd  = 2,
      title = "Радар: профілі метрик (шкала 0–100)"
    )
    legend("topright",
           legend = rownames(df_models),
           col = c("#1b9e77", "#d95f02"),
           lwd = 2, bty = "n")
    
    par(op)
  }
)

# 1.2 Radar з нормалізацією до 0–1 (min-max по кожній ознаці)
# Нормалізація робиться КОЛОНКАМИ (по ознаці), щоб порівнювати профілі коректно.
norm01 <- function(x) (x - min(x)) / (max(x) - min(x) + 1e-9)
df_models_01 <- as.data.frame(lapply(df_models, norm01))
rownames(df_models_01) <- rownames(df_models)

# Для шкали 0–1 фіксуємо max=1, min=0 (щоб було очевидно, що інший масштаб!)
df_radar_0_1 <- rbind(
  setNames(rep(1, length(axis_order)), axis_order),
  setNames(rep(0, length(axis_order)), axis_order),
  df_models_01
)

save_both_base(
  name_base = "fig_lab5_02_radar_scale_0_1_norm01",
  w_in = 9.5, h_in = 7,
  expr_plot = function() {
    op <- par(no.readonly = TRUE)
    par(mar = c(2, 2, 3, 2))
    
    radarchart(
      df_radar_0_1,
      axistype = 1,
      seg = 4,
      caxislabels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
      pcol  = c("#1b9e77", "#d95f02"),
      pfcol = alpha(c("#1b9e77", "#d95f02"), 0.25),
      plwd  = 2,
      title = "Радар: нормалізація метрик (шкала 0–1)"
    )
    legend("topright",
           legend = rownames(df_models_01),
           col = c("#1b9e77", "#d95f02"),
           lwd = 2, bty = "n")
    
    par(op)
  }
)

# -------------------------------
# 2) РЕЛЬЄФ: 2D (contour) ПОРУЧ З 3D (persp)
# -------------------------------
z <- volcano
x <- 1:nrow(z)
y <- 1:ncol(z)

# Додати альтернативний 2D-варіант поруч з 3D
# Робимо один рисунок з двома панелями: зліва contour, справа persp
save_both_base(
  name_base = "fig_lab5_03_relief_2D_contour_and_3D_surface",
  w_in = 12, h_in = 6,
  expr_plot = function() {
    op <- par(no.readonly = TRUE)
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1) + 0.1)
    
    # 2D ізолінії
    contour(x, y, z,
            main = "2D: Ізолінії (volcano)",
            xlab = "X", ylab = "Y")
    
    # 3D поверхня
    persp(x, y, z,
          theta = 135, phi = 25,
          col = "lightblue", shade = 0.5,
          ticktype = "detailed",
          xlab = "X", ylab = "Y", zlab = "Z",
          main = "3D: Поверхня (volcano)")
    
    par(op)
  }
)

# Окремо — заповнені контури (дуже інформативно)
save_both_base(
  name_base = "fig_lab5_04_volcano_filled_contour",
  w_in = 9.5, h_in = 7,
  expr_plot = function() {
    op <- par(no.readonly = TRUE)
    par(mar = c(4, 4, 3, 2) + 0.1)
    filled.contour(
      x, y, z,
      color.palette = terrain.colors,
      plot.title = title(main = "Заповнені контури (volcano)", xlab = "X", ylab = "Y"),
      key.title = title(main = "Висота (Z)")
    )
    par(op)
  }
)

# Варіант у ggplot2 (гарно для звіту)
df_volc <- melt(volcano)
colnames(df_volc) <- c("X", "Y", "Z")

p_contour_filled <- ggplot(df_volc, aes(X, Y, z = Z)) +
  geom_contour_filled() +
  coord_fixed() +
  labs(
    title = "ggplot2: заповнені контури рельєфу (volcano)",
    x = "X", y = "Y",
    fill = "Висота (Z)"
  ) +
  theme_ua

save_both_gg(p_contour_filled, "fig_lab5_05_volcano_ggplot_contour_filled", w = 10, h = 7)

# -------------------------------
# 3) HEATMAP: кореляції mtcars (ggplot2)
# -------------------------------
data(mtcars)
cor_m <- cor(mtcars)

# Перетворення матриці у «довгий» формат
mcor <- melt(cor_m)
colnames(mcor) <- c("Var1", "Var2", "r")

# Легенда має пояснювати смисл шкали:
# r — коефіцієнт кореляції Пірсона, безрозмірна величина, діапазон [-1; 1]
legend_title <- "r (кореляція Пірсона)\nбезрозмірна, [-1; 1]"

# 3.1 Heatmap без чисел (швидкий огляд)
p_heatmap <- ggplot(mcor, aes(Var1, Var2, fill = r)) +
  geom_tile(color = "white", linewidth = 0.25) +
  scale_fill_gradient2(
    low = "#313695", mid = "white", high = "#a50026",
    midpoint = 0, limits = c(-1, 1),
    name = legend_title
  ) +
  labs(
    title = "Теплова карта кореляцій змінних набору mtcars",
    x = "", y = ""
  ) +
  theme_ua +
  theme(axis.text.y = element_text(size = 10))

save_both_gg(p_heatmap, "fig_lab5_06_heatmap_mtcars_ggplot", w = 10, h = 8)

# 3.2 Індивідуальне завдання №9: heatmap з підписами коефіцієнтів
p_heatmap_labeled <- ggplot(mcor, aes(Var1, Var2, fill = r)) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  scale_fill_gradient2(
    low = "#313695", mid = "white", high = "#a50026",
    midpoint = 0, limits = c(-1, 1),
    name = legend_title
  ) +
  labs(
    title = "Індивідуальне завдання №9: heatmap кореляцій mtcars з підписами",
    x = "", y = ""
  ) +
  theme_ua +
  theme(axis.text.y = element_text(size = 10))

save_both_gg(p_heatmap_labeled, "fig_lab5_07_heatmap_mtcars_labeled", w = 10.5, h = 8.5)

# -------------------------------
# 4) pheatmap
# -------------------------------
# 4.1 pheatmap з кластеруванням
if (!requireNamespace("pheatmap", quietly = TRUE)) {
  message("Пакет 'pheatmap' не встановлено")
} else {
  library(pheatmap)
  
  # PNG
  png(file.path(out_dir, "fig_lab5_08_pheatmap_clustered.png"),
      width = 1700, height = 1400, res = 170, bg = "white")
  pheatmap(
    cor_m,
    color = colorRampPalette(c("#313695", "white", "#a50026"))(100),
    display_numbers = TRUE, number_format = "%.2f",
    main = "pheatmap: кореляції mtcars (кластерування рядків/стовпців)",
    clustering_distance_rows = "euclidean",
    clustering_distance_cols = "euclidean",
    clustering_method = "complete"
  )
  dev.off()
  
  # PDF
  pdf(file.path(out_dir, "fig_lab5_08_pheatmap_clustered.pdf"),
      width = 11, height = 9)
  pheatmap(
    cor_m,
    color = colorRampPalette(c("#313695", "white", "#a50026"))(100),
    display_numbers = TRUE, number_format = "%.2f",
    main = "pheatmap: кореляції mtcars (кластерування рядків/стовпців)",
    clustering_distance_rows = "euclidean",
    clustering_distance_cols = "euclidean",
    clustering_method = "complete"
  )
  dev.off()
}

# -------------------------------
# 5) Підсумок: список збережених файлів
# -------------------------------
cat("\nГотово! Графіки збережено у папці:", out_dir, "\n")
print(list.files(out_dir))
