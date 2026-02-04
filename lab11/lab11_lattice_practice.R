############################################################
# ЛАБОРАТОРНА РОБОТА №11
# Практичні приклади використання пакету lattice
############################################################

# ----------------------------------------------------------
# 1. Підготовка середовища
# ----------------------------------------------------------

if (!requireNamespace("lattice", quietly = TRUE)) {
  install.packages("lattice")
}
library(lattice)

# Папка для результатів
out_dir <- "plots_lab11"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Функція збереження lattice-графіків
save_lattice_plot <- function(name, plot_obj,
                              png_w = 1600, png_h = 1000, res = 170,
                              pdf_w = 10, pdf_h = 6.5) {

  png(file.path(out_dir, paste0(name, ".png")),
      width = png_w, height = png_h, res = res, bg = "white")
  print(plot_obj)
  dev.off()

  pdf(file.path(out_dir, paste0(name, ".pdf")),
      width = pdf_w, height = pdf_h)
  print(plot_obj)
  dev.off()
}

# ----------------------------------------------------------
# 2. Дані
# ----------------------------------------------------------
data(iris)
data(volcano)

# ----------------------------------------------------------
# 3. Scatterplot з групуванням
# ----------------------------------------------------------

p1 <- xyplot(
  Sepal.Length ~ Petal.Length,
  data = iris,
  groups = Species,
  auto.key = list(space = "right", points = TRUE),
  pch = 16,
  col = c("darkgreen", "blue", "red"),
  main = "Sepal.Length ~ Petal.Length з групуванням (Species)",
  xlab = "Довжина пелюстки (см)",
  ylab = "Довжина чашолистка (см)"
)

save_lattice_plot("fig_01_xyplot_groups", p1)

# ----------------------------------------------------------
# 4. Scatterplot з трендовою лінією (panel function)
# ----------------------------------------------------------

p2 <- xyplot(
  Sepal.Length ~ Petal.Length | Species,
  data = iris,
  layout = c(3, 1),
  main = "Залежність Sepal.Length ~ Petal.Length з регресією",
  xlab = "Довжина пелюстки (см)",
  ylab = "Довжина чашолистка (см)",
  panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, col = "red", lwd = 2)
    panel.grid(h = -1, v = -1)
  }
)

save_lattice_plot("fig_02_xyplot_regression", p2)

# ----------------------------------------------------------
# 5. Boxplot
# ----------------------------------------------------------

p3 <- bwplot(
  Species ~ Sepal.Length,
  data = iris,
  fill = "lightblue",
  col = "black",
  main = "Boxplot: Sepal.Length за видами",
  xlab = "Довжина чашолистка (см)",
  ylab = "Вид"
)

save_lattice_plot("fig_03_bwplot", p3)

# ----------------------------------------------------------
# 6. Densityplot
# ----------------------------------------------------------

p4 <- densityplot(
  ~ Sepal.Length,
  data = iris,
  groups = Species,
  auto.key = TRUE,
  plot.points = FALSE,
  lwd = 2,
  main = "Оцінка густини Sepal.Length",
  xlab = "Довжина чашолистка (см)"
)

save_lattice_plot("fig_04_densityplot", p4)

# ----------------------------------------------------------
# 7. Histogram з панелями
# ----------------------------------------------------------

p5 <- histogram(
  ~ Sepal.Width | Species,
  data = iris,
  breaks = 10,
  col = "coral",
  main = "Гістограми Sepal.Width за видами",
  xlab = "Ширина чашолистка (см)"
)

save_lattice_plot("fig_05_histogram", p5)

# ----------------------------------------------------------
# 8. Stripplot
# ----------------------------------------------------------

p6 <- stripplot(
  Species ~ Sepal.Width,
  data = iris,
  jitter = TRUE,
  pch = 19,
  col = "purple",
  main = "Stripplot: Sepal.Width за видами",
  xlab = "Ширина чашолистка (см)"
)

save_lattice_plot("fig_06_stripplot", p6)

# ----------------------------------------------------------
# 9. Dotplot
# ----------------------------------------------------------

p7 <- dotplot(
  Species ~ Sepal.Length,
  data = iris,
  jitter = TRUE,
  pch = 16,
  col = "darkblue",
  main = "Dotplot: Sepal.Length за видами",
  xlab = "Довжина чашолистка (см)"
)

save_lattice_plot("fig_07_dotplot", p7)

# ----------------------------------------------------------
# 10. Heatmap (levelplot)
# ----------------------------------------------------------

p8 <- levelplot(
  volcano,
  col.regions = terrain.colors(100),
  main = "Heatmap рельєфу (volcano)",
  xlab = "X",
  ylab = "Y"
)

save_lattice_plot("fig_08_levelplot_volcano", p8)

# ----------------------------------------------------------
# 11. Contourplot
# ----------------------------------------------------------

p9 <- contourplot(
  volcano,
  region = TRUE,
  cuts = 15,
  col.regions = heat.colors(15),
  main = "Contourplot рельєфу (volcano)",
  xlab = "X",
  ylab = "Y"
)

save_lattice_plot("fig_09_contourplot_volcano", p9)

# ----------------------------------------------------------
# 12. ІНДИВІДУАЛЬНЕ ЗАВДАННЯ №9
# Heatmap матриці даних
# ----------------------------------------------------------

# Створення матриці
mat <- matrix(
  rnorm(100),
  nrow = 10,
  ncol = 10
)

p10 <- levelplot(
  mat,
  col.regions = colorRampPalette(c("blue", "white", "red"))(100),
  main = "Індивідуальне завдання №9: Heatmap матриці даних",
  xlab = "Стовпці",
  ylab = "Рядки"
)

save_lattice_plot("fig_10_task9_heatmap_matrix", p10)

# ----------------------------------------------------------
# Завершення
# ----------------------------------------------------------
cat("\nУсі графіки збережено у папці:", out_dir, "\n")
print(list.files(out_dir))
