############################################################
# Лабораторна робота: Багатовимірні візуалізації в R
# Тема: Тривимірні діаграми, матриці розсіювання, корелограми,
#       мозаїчні діаграми, карти дерева (Treemap)
#
# Мета:
# - Ознайомитися з основними видами багатовимірних графіків у R
# - Навчитися будувати, налаштовувати та інтерпретувати:
#   1) 3D діаграми
#   2) scatterplot matrix
#   3) корелограми
#   4) мозаїчні діаграми
#   5) treemap
#
# Індивідуальне завдання №9:
# - Змінити стиль мозаїчної діаграми: порівняти заповнення (fill/col)
#   та контури (color/border) => зробити читабельніше.
############################################################

# ----------------------------------------------------------
# 0) Підготовка середовища та пакетів
# ----------------------------------------------------------

packages <- c(
  "GGally",        # ggpairs
  "corrplot",      # корелограми
  "treemap",       # treemap
  "scatterplot3d"  # 3D scatter
)

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, dependencies = TRUE)
    }
  }
}

install_if_missing(packages)

library(GGally)
library(corrplot)
library(treemap)
library(scatterplot3d)

# Вбудовані дані
data(mtcars)
data(iris)
data(Titanic)

# Папка для результатів
out_dir <- "plots_labX_multivariate"
if (!dir.exists(out_dir)) dir.create(out_dir)

# ----------------------------------------------------------
# Допоміжна функція: збереження base R графіків у PNG + PDF
# ----------------------------------------------------------
save_base_png_pdf <- function(file_base, expr_plot,
                              png_w = 1600, png_h = 1000, png_res = 170,
                              pdf_w = 10, pdf_h = 6.5) {
  
  png(file.path(out_dir, paste0(file_base, ".png")),
      width = png_w, height = png_h, res = png_res, bg = "white")
  expr_plot()
  dev.off()
  
  pdf(file.path(out_dir, paste0(file_base, ".pdf")),
      width = pdf_w, height = pdf_h)
  expr_plot()
  dev.off()
}

# ----------------------------------------------------------
# 1) Тривимірні діаграми (3D)
# ----------------------------------------------------------

# 1.1 3D scatter: wt, disp, mpg (mtcars)
save_base_png_pdf(
  "fig_01_3D_scatterplot3d_mtcars",
  expr_plot = function() {
    scatterplot3d(
      mtcars$wt, mtcars$disp, mtcars$mpg,
      color = "red",
      pch = 19,
      main = "3D-діаграма: mpg залежно від wt та disp (mtcars)",
      xlab = "Вага авто (wt, 1000 lbs)",
      ylab = "Об’єм двигуна (disp)",
      zlab = "Витрата палива (mpg)"
    )
  },
  pdf_w = 11, pdf_h = 7
)

# 1.2 3D поверхня: persp() на volcano (base R)
z <- volcano
x <- 1:nrow(z)
y <- 1:ncol(z)

save_base_png_pdf(
  "fig_02_3D_persp_volcano",
  expr_plot = function() {
    op <- par(no.readonly = TRUE)
    par(mar = c(4, 4, 3, 2) + 0.1)
    persp(
      x, y, z,
      theta = 135, phi = 30,
      col = "lightblue",
      shade = 0.5,
      ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Висота (Z)",
      main = "3D-поверхня рельєфу (volcano) — persp()"
    )
    par(op)
  },
  pdf_w = 11, pdf_h = 7
)

# ----------------------------------------------------------
# 2) Матриці діаграм розсіювання (Scatterplot matrix)
# ----------------------------------------------------------

# 2.1 pairs() (base R)
save_base_png_pdf(
  "fig_03_pairs_iris",
  expr_plot = function() {
    pairs(
      iris[, 1:4],
      main = "Матриця діаграм розсіювання (iris) — pairs()",
      col = as.numeric(iris$Species) + 1,
      pch = 19
    )
  },
  pdf_w = 11, pdf_h = 7
)

# 2.2 GGally::ggpairs (покращений вигляд)
p_ggpairs <- GGally::ggpairs(
  iris,
  aes(color = Species),
  title = "Матриця розсіювання iris (GGally::ggpairs)"
)

# Збереження ggpairs (PNG + PDF)
ggplot2::ggsave(
  filename = file.path(out_dir, "fig_04_ggpairs_iris.png"),
  plot = p_ggpairs, width = 11, height = 8, dpi = 300, bg = "white"
)
ggplot2::ggsave(
  filename = file.path(out_dir, "fig_04_ggpairs_iris.pdf"),
  plot = p_ggpairs, width = 11, height = 8, bg = "white"
)

# ----------------------------------------------------------
# 3) Корелограми
# ----------------------------------------------------------

cor_matrix <- cor(iris[, 1:4])

# 3.1 corrplot: ellipse
save_base_png_pdf(
  "fig_05_corrplot_ellipse",
  expr_plot = function() {
    corrplot(
      cor_matrix,
      method = "ellipse",
      type = "upper",
      title = "Корелограма (ellipse), r ∈ [-1; 1]",
      mar = c(0, 0, 2, 0)
    )
  },
  pdf_w = 9, pdf_h = 7
)

# 3.2 corrplot: number
save_base_png_pdf(
  "fig_06_corrplot_numbers",
  expr_plot = function() {
    corrplot(
      cor_matrix,
      method = "number",
      type = "upper",
      title = "Корелограма (числові значення), r ∈ [-1; 1]",
      mar = c(0, 0, 2, 0)
    )
  },
  pdf_w = 9, pdf_h = 7
)

# ----------------------------------------------------------
# 4) Мозаїчні діаграми (Titanic) — base R mosaicplot()
# ----------------------------------------------------------

# Titanic — багатовимірна таблиця частот. Перетворимо в contingency table
tab_titanic <- xtabs(Freq ~ Sex + Class + Survived, data = as.data.frame(Titanic))

# 4.1 Базова мозаїчна діаграма
save_base_png_pdf(
  "fig_07_mosaic_basic_titanic",
  expr_plot = function() {
    mosaicplot(
      tab_titanic,
      main = "Мозаїчна діаграма Titanic (базова) — mosaicplot()",
      color = TRUE,   # автоматичні кольори
      las = 1
    )
  },
  pdf_w = 11, pdf_h = 7
)

# ----------------------------------------------------------
# 5) Індивідуальне завдання №9
# Змінити стиль мозаїчної діаграми: fill та color/border
# ----------------------------------------------------------

# Власні кольори (fill): No = червоний, Yes = зелений
# Контури (border): чорний — для підвищення читабельності
cols_fill <- c("firebrick3", "seagreen4")

save_base_png_pdf(
  "fig_08_mosaic_styled_fill_border_task9",
  expr_plot = function() {
    mosaicplot(
      tab_titanic,
      main = "Titanic: мозаїка (покращений стиль: fill + border) — Завд. №9",
      color = cols_fill,  # fill
      border = "black",   # color/border контурів
      las = 1,
      cex.axis = 0.9
    )
    mtext("Fill: Survived (Ні/Так), Border: чорні контури для читабельності",
          side = 1, line = 2, cex = 0.9)
  },
  pdf_w = 11, pdf_h = 7
)

# ----------------------------------------------------------
# 6) Карта дерева (Treemap)
# ----------------------------------------------------------

# Вбудований набір даних пакету treemap
data("GNI2014", package = "treemap")

save_base_png_pdf(
  "fig_09_treemap_GNI2014",
  expr_plot = function() {
    treemap(
      GNI2014,
      index = c("continent", "country"),
      vSize = "population",
      vColor = "GNI",
      type = "value",
      title = "Treemap: населення (площа) та ВНД (колір) — GNI2014"
    )
  },
  pdf_w = 11, pdf_h = 7
)

# ----------------------------------------------------------
# 7) Підсумок
# ----------------------------------------------------------
cat("\nГотово! Усі графіки збережено у папці:", out_dir, "\n")
print(list.files(out_dir))
