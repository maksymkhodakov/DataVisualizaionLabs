# ==========================================================
# Лабораторна робота №7
# Тема: Основи lattice: xyplot(), bwplot(), histogram()
# Мета: Засвоїти Trellis Graphics (lattice) для багатопанельних графіків.
# ==========================================================

# -------------------------------
# 0) Підготовка середовища
# -------------------------------
if (!requireNamespace("lattice", quietly = TRUE)) {
  install.packages("lattice", dependencies = TRUE)
}
library(lattice)

# Вбудовані датасети
data(mtcars)
data(iris)
data(airquality)

# Папка для збереження графіків
out_dir <- "plots_lab7"
if (!dir.exists(out_dir)) dir.create(out_dir)

# -------------------------------
# 1) Функції для збереження lattice-графіків у PNG + PDF
# -------------------------------
save_lattice_png_pdf <- function(plot_obj, file_base,
                                 png_w = 1600, png_h = 1000, png_res = 170,
                                 pdf_w = 10, pdf_h = 6.5) {
  # PNG
  png(filename = file.path(out_dir, paste0(file_base, ".png")),
      width = png_w, height = png_h, res = png_res, bg = "white")
  print(plot_obj)
  dev.off()

  # PDF
  pdf(file = file.path(out_dir, paste0(file_base, ".pdf")),
      width = pdf_w, height = pdf_h)
  print(plot_obj)
  dev.off()
}

# -------------------------------
# 2) xyplot(): базовий графік (mpg ~ wt)
# -------------------------------
p1 <- xyplot(
  mpg ~ wt,
  data = mtcars,
  main = "Залежність витрати палива від ваги автомобіля",
  xlab = "Вага (1000 lbs)",
  ylab = "Миль на галон (mpg)",
  pch = 19,
  col = "blue"
)

save_lattice_png_pdf(p1, "fig_lab7_01_xyplot_basic")

# -------------------------------
# 3) xyplot(): умовне розбиття за кількістю циліндрів
# -------------------------------
p2 <- xyplot(
  mpg ~ wt | factor(cyl),
  data = mtcars,
  main = "Витрата палива vs Вага за кількістю циліндрів",
  xlab = "Вага (1000 lbs)",
  ylab = "Миль на галон (mpg)",
  pch = 19,
  col = "darkgreen",
  layout = c(3, 1)  # 3 панелі в один ряд
)

save_lattice_png_pdf(p2, "fig_lab7_02_xyplot_by_cyl")

# -------------------------------
# 4) xyplot(): групування за типом трансмісії (am)
# -------------------------------
# am: 0=автомат, 1=механіка — зробимо читабельні підписи
mtcars2 <- within(mtcars, {
  am_f <- factor(am, levels = c(0, 1), labels = c("Автоматична", "Механічна"))
})

p3 <- xyplot(
  mpg ~ wt,
  data = mtcars2,
  groups = am_f,
  main = "Витрата палива за типом трансмісії",
  xlab = "Вага (1000 lbs)",
  ylab = "Миль на галон (mpg)",
  auto.key = list(
    text = levels(mtcars2$am_f),
    points = TRUE,
    space = "right"
  ),
  pch = c(16, 17),
  col = c("red", "blue")
)

save_lattice_png_pdf(p3, "fig_lab7_03_xyplot_groups_am")

# -------------------------------
# 5) xyplot(): лінійний графік (штучний часовий ряд)
# -------------------------------
set.seed(42)
time_data <- data.frame(
  time = 1:100,
  value = cumsum(rnorm(100)),
  category = rep(c("A", "B"), each = 50)
)

p4 <- xyplot(
  value ~ time | category,
  data = time_data,
  type = "l",
  main = "Часові ряди за категоріями",
  xlab = "Час",
  ylab = "Значення",
  col = "purple",
  lwd = 2
)

save_lattice_png_pdf(p4, "fig_lab7_04_xyplot_time_series")

# -------------------------------
# 6) bwplot(): розподіл mpg за cyl
# -------------------------------
p5 <- bwplot(
  mpg ~ factor(cyl),
  data = mtcars,
  main = "Розподіл витрати палива за кількістю циліндрів",
  xlab = "Кількість циліндрів",
  ylab = "Миль на галон (mpg)",
  col = "orange",
  fill = "lightblue"
)

save_lattice_png_pdf(p5, "fig_lab7_05_bwplot_mpg_by_cyl")

# -------------------------------
# 7) bwplot(): горизонтальний з умовним розбиттям за трансмісією
# -------------------------------
p6 <- bwplot(
  factor(cyl) ~ mpg | factor(am, levels = c(0, 1), labels = c("Автомат", "Механіка")),
  data = mtcars,
  main = "Витрата палива за типом трансмісії",
  xlab = "Миль на галон (mpg)",
  ylab = "Кількість циліндрів",
  horizontal = TRUE,
  panel = function(x, y, ...) {
    panel.bwplot(x, y, ...)
    panel.grid(h = -1, v = 0)
  }
)

save_lattice_png_pdf(p6, "fig_lab7_06_bwplot_horizontal_by_am")

# -------------------------------
# 8) bwplot(): iris (Sepal.Length ~ Species) з налаштуваннями
# -------------------------------
p7 <- bwplot(
  Sepal.Length ~ Species,
  data = iris,
  main = "Розподіл довжини чашолистків за видами",
  xlab = "Вид (Species)",
  ylab = "Довжина чашолистка (см)",
  fill = c("lightgreen", "lightblue", "pink"),
  par.settings = list(
    box.rectangle = list(col = "black", lwd = 1.5),
    box.umbrella  = list(col = "black", lwd = 1.5),
    plot.symbol   = list(col = "red", pch = 19, cex = 0.8)
  )
)

save_lattice_png_pdf(p7, "fig_lab7_07_bwplot_iris_species")

# -------------------------------
# 9) histogram(): mpg (percent, breaks=10)
# -------------------------------
p8 <- histogram(
  ~ mpg,
  data = mtcars,
  main = "Розподіл витрати палива (mpg)",
  xlab = "Миль на галон (mpg)",
  ylab = "Відсоток спостережень (%)",
  type = "percent",
  col = "steelblue",
  breaks = 10
)

save_lattice_png_pdf(p8, "fig_lab7_08_histogram_mpg_percent")

# -------------------------------
# 10) histogram(): mpg | cyl (density, breaks=15)
# -------------------------------
p9 <- histogram(
  ~ mpg | factor(cyl),
  data = mtcars,
  main = "Розподіл витрати палива за кількістю циліндрів",
  xlab = "Миль на галон (mpg)",
  ylab = "Щільність",
  type = "density",
  col = "coral",
  layout = c(1, 3),
  breaks = 15
)

save_lattice_png_pdf(p9, "fig_lab7_09_histogram_mpg_by_cyl")

# -------------------------------
# 11) histogram(): iris Sepal.Length | Species + крива щільності
# -------------------------------
p10 <- histogram(
  ~ Sepal.Length | Species,
  data = iris,
  main = "Розподіл довжини чашолистків (Sepal.Length)",
  xlab = "Довжина чашолистка (см)",
  ylab = "Щільність",
  type = "density",
  col = "lightgreen",
  panel = function(x, ...) {
    panel.histogram(x, ...)
    panel.densityplot(x, col = "darkblue", lwd = 2, ...)
  }
)

save_lattice_png_pdf(p10, "fig_lab7_10_histogram_iris_density")

# -------------------------------
# 12) Додатково: тема (par.settings) + регресія у панелі
# -------------------------------
my_theme <- list(
  background = list(col = "white"),
  strip.background = list(col = c("lightblue", "lightgreen", "pink")),
  strip.border = list(col = "black", lwd = 2)
)

p11 <- xyplot(
  mpg ~ wt | factor(cyl),
  data = mtcars,
  par.settings = my_theme,
  main = "Кастомна тема + розбиття за циліндрами",
  xlab = "Вага (1000 lbs)",
  ylab = "mpg",
  panel = function(x, y, ...) {
    panel.xyplot(x, y, pch = 19, col = "darkgreen", ...)
    panel.lmline(x, y, col = "red", lwd = 2)
    panel.grid(h = -1, v = -1, col = "lightgray")
  }
)

save_lattice_png_pdf(p11, "fig_lab7_11_xyplot_theme_regression")

# ==========================================================
# 13) ІНДИВІДУАЛЬНЕ ЗАВДАННЯ №9
# histogram() для hp:
# - mtcars$hp
# - type = "percent"
# - breaks = 15
# - col = "coral"
# - заголовок і підписи осей українською
# ==========================================================
p_task9 <- histogram(
  ~ hp,
  data = mtcars,
  main = "Розподіл потужності автомобілів (hp)",
  xlab = "Потужність (кінські сили, hp)",
  ylab = "Відсоток спостережень (%)",
  type = "percent",
  breaks = 15,
  col = "coral"
)

save_lattice_png_pdf(p_task9, "fig_lab7_task9_histogram_hp_percent_breaks15")

cat("\nГотово! Усі графіки збережено у папці:", out_dir, "\n")
print(list.files(out_dir))
