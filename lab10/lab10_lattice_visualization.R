############################################################
# ЛАБОРАТОРНА РОБОТА №10
# Візуалізація даних з використанням пакету lattice у R
############################################################

# ----------------------------------------------------------
# 1. Підготовка середовища
# ----------------------------------------------------------

# Встановлення та підключення lattice
if (!requireNamespace("lattice", quietly = TRUE)) {
  install.packages("lattice")
}
library(lattice)

# Папка для збереження графіків
out_dir <- "plots_lab10"
if (!dir.exists(out_dir)) dir.create(out_dir)

# ----------------------------------------------------------
# Допоміжна функція для збереження lattice-графіків
# ----------------------------------------------------------
save_lattice_plot <- function(filename, plot_expr,
                              png_w = 1600, png_h = 1000, png_res = 170,
                              pdf_w = 10, pdf_h = 6.5) {

  png(file.path(out_dir, paste0(filename, ".png")),
      width = png_w, height = png_h, res = png_res, bg = "white")
  print(plot_expr)
  dev.off()

  pdf(file.path(out_dir, paste0(filename, ".pdf")),
      width = pdf_w, height = pdf_h)
  print(plot_expr)
  dev.off()
}

# ----------------------------------------------------------
# 2. Дані
# ----------------------------------------------------------
data(iris)
data(volcano)

# ----------------------------------------------------------
# 3. xyplot(): розсіювання + групування
# ----------------------------------------------------------

p1 <- xyplot(
  Petal.Length ~ Sepal.Length,
  data = iris,
  groups = Species,
  auto.key = list(space = "right", points = TRUE),
  pch = 16,
  col = c("darkgreen", "blue", "red"),
  main = "Petal.Length vs Sepal.Length (групування за Species)",
  xlab = "Довжина чашолистка (см)",
  ylab = "Довжина пелюстки (см)"
)

save_lattice_plot("fig_01_xyplot_grouped", p1)

# ----------------------------------------------------------
# 4. xyplot(): фасетування (панелі)
# ----------------------------------------------------------

p2 <- xyplot(
  Sepal.Length ~ Petal.Length | Species,
  data = iris,
  layout = c(3, 1),
  pch = 19,
  col = "steelblue",
  main = "Панельні графіки Sepal.Length ~ Petal.Length за видами",
  xlab = "Довжина пелюстки (см)",
  ylab = "Довжина чашолистка (см)"
)

save_lattice_plot("fig_02_xyplot_facets", p2)

# ----------------------------------------------------------
# 5. bwplot(): boxplot
# ----------------------------------------------------------

p3 <- bwplot(
  Sepal.Length ~ Species,
  data = iris,
  fill = "lightblue",
  col = "black",
  main = "Розподіл Sepal.Length за видами (bwplot)",
  xlab = "Вид",
  ylab = "Довжина чашолистка (см)"
)

save_lattice_plot("fig_03_bwplot", p3)

# ----------------------------------------------------------
# 6. densityplot(): густина розподілу
# ----------------------------------------------------------

p4 <- densityplot(
  ~ Sepal.Length,
  data = iris,
  groups = Species,
  plot.points = FALSE,
  auto.key = TRUE,
  lwd = 2,
  main = "Оцінка густини Sepal.Length за видами",
  xlab = "Довжина чашолистка (см)"
)

save_lattice_plot("fig_04_densityplot", p4)

# ----------------------------------------------------------
# 7. histogram(): гістограми з панелями
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
# 8. dotplot(): точкові порівняння
# ----------------------------------------------------------

p6 <- dotplot(
  Species ~ Sepal.Length,
  data = iris,
  jitter = TRUE,
  col = "darkblue",
  pch = 16,
  main = "Dotplot: Sepal.Length за видами",
  xlab = "Довжина чашолистка (см)"
)

save_lattice_plot("fig_06_dotplot", p6)

# ----------------------------------------------------------
# 9. stripplot(): смуги з шумом
# ----------------------------------------------------------

p7 <- stripplot(
  Species ~ Sepal.Width,
  data = iris,
  jitter = TRUE,
  col = "purple",
  pch = 19,
  main = "Stripplot: Sepal.Width за видами",
  xlab = "Ширина чашолистка (см)"
)

save_lattice_plot("fig_07_stripplot", p7)

# ----------------------------------------------------------
# 10. levelplot(): 2D heatmap
# ----------------------------------------------------------

p8 <- levelplot(
  volcano,
  col.regions = terrain.colors(100),
  main = "Levelplot: теплове представлення рельєфу (volcano)",
  xlab = "X",
  ylab = "Y"
)

save_lattice_plot("fig_08_levelplot", p8)

# ----------------------------------------------------------
# 11. contourplot(): контурні лінії
# ----------------------------------------------------------

p9 <- contourplot(
  volcano,
  cuts = 15,
  region = TRUE,
  col.regions = terrain.colors(15),
  main = "Contourplot: рельєф місцевості (volcano)",
  xlab = "X",
  ylab = "Y"
)

save_lattice_plot("fig_09_contourplot", p9)

# ----------------------------------------------------------
# 12. ІНДИВІДУАЛЬНЕ ЗАВДАННЯ №9
# Contourplot з використанням сітки (grid)
# ----------------------------------------------------------

# Створення штучної сітки
x <- seq(-pi, pi, length = 50)
y <- seq(-pi, pi, length = 50)
grid <- expand.grid(x = x, y = y)

# Функція рельєфу
grid$z <- with(grid, sin(x) * cos(y))

p10 <- contourplot(
  z ~ x * y,
  data = grid,
  cuts = 20,
  region = TRUE,
  col.regions = heat.colors(20),
  main = "Індивідуальне завдання №9: контурна діаграма z = sin(x)·cos(y)",
  xlab = "x",
  ylab = "y"
)

save_lattice_plot("fig_10_task9_contourplot", p10)

# ----------------------------------------------------------
# Завершення
# ----------------------------------------------------------
cat("\nУсі графіки збережені в папці:", out_dir, "\n")
print(list.files(out_dir))
