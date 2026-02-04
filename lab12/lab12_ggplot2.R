############################################################
# ЛАБОРАТОРНА РОБОТА №12
# Візуалізація даних з використанням пакету ggplot2 у R
############################################################

# ----------------------------------------------------------
# 1) Підготовка середовища
# ----------------------------------------------------------

# Встановлення ggplot2
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Датасети
data(iris)
data(faithfuld)

# Папка для збереження графіків
out_dir <- "plots_lab12"
if (!dir.exists(out_dir)) dir.create(out_dir)

# ----------------------------------------------------------
# 2) Допоміжна функція збереження ggplot у PNG + PDF
# ----------------------------------------------------------
save_plot_png_pdf <- function(filename_no_ext, plot_obj,
                              w = 9, h = 6, dpi = 300) {
  ggsave(
    filename = file.path(out_dir, paste0(filename_no_ext, ".png")),
    plot = plot_obj,
    width = w, height = h, dpi = dpi, bg = "white"
  )
  ggsave(
    filename = file.path(out_dir, paste0(filename_no_ext, ".pdf")),
    plot = plot_obj,
    width = w, height = h, bg = "white"
  )
}

# ----------------------------------------------------------
# 3) Рис. 1 — Scatterplot з групуванням (geom_point)
# ----------------------------------------------------------
p1 <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 3, alpha = 0.85) +
  labs(
    title = "Діаграма розсіювання: Petal.Length від Sepal.Length",
    x = "Довжина чашолистка (см)",
    y = "Довжина пелюстки (см)",
    color = "Вид"
  ) +
  theme_minimal(base_size = 13)

save_plot_png_pdf("fig_01_scatter", p1)

# ----------------------------------------------------------
# 4) Рис. 2 — Boxplot (geom_boxplot)
# ----------------------------------------------------------
p2 <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(width = 0.7, outlier_alpha = 0.6) +
  labs(
    title = "Boxplot: розподіл Sepal.Length за видами",
    x = "Вид",
    y = "Довжина чашолистка (см)",
    fill = "Вид"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

save_plot_png_pdf("fig_02_boxplot", p2)

# ----------------------------------------------------------
# 5) Рис. 3 — Histogram (geom_histogram)
# ----------------------------------------------------------
p3 <- ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black", alpha = 0.9) +
  labs(
    title = "Гістограма: розподіл Sepal.Width",
    x = "Ширина чашолистка (см)",
    y = "Кількість спостережень"
  ) +
  theme_minimal(base_size = 13)

save_plot_png_pdf("fig_03_histogram", p3)

# ----------------------------------------------------------
# 6) Рис. 4 — Scatterplot + Trend (geom_smooth(method="lm"))
# ----------------------------------------------------------
p4 <- ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, color = Species)) +
  geom_point(size = 2.6, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
  labs(
    title = "Залежність Sepal.Length від Petal.Length з лінією тренду (lm)",
    x = "Довжина пелюстки (см)",
    y = "Довжина чашолистка (см)",
    color = "Вид"
  ) +
  theme_minimal(base_size = 13)

save_plot_png_pdf("fig_04_scatter_trend", p4)

# ----------------------------------------------------------
# 7) Рис. 5 — Faceting (facet_wrap)
# ----------------------------------------------------------
p5 <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(color = "steelblue", size = 2.7, alpha = 0.85) +
  facet_wrap(~ Species, nrow = 1) +
  labs(
    title = "Фасетні діаграми розсіювання за видами",
    x = "Довжина чашолистка (см)",
    y = "Довжина пелюстки (см)"
  ) +
  theme_minimal(base_size = 13)

save_plot_png_pdf("fig_05_facet", p5, w = 11, h = 4.5)

# ----------------------------------------------------------
# 8) Рис. 6 — Heatmap (geom_tile) для faithfuld
# ----------------------------------------------------------
p6 <- ggplot(faithfuld, aes(x = waiting, y = eruptions, fill = density)) +
  geom_tile() +
  labs(
    title = "Теплова карта: щільність (faithfuld)",
    x = "Очікування (хв)",
    y = "Тривалість виверження (хв)",
    fill = "Щільність"
  ) +
  theme_minimal(base_size = 13)

save_plot_png_pdf("fig_06_heatmap", p6)

# ----------------------------------------------------------
# 9) Індивідуальне завдання №9 — Barplot (geom_col) + підписи
# ----------------------------------------------------------
# Створимо невеликий data.frame (категорія + значення)
task9_df <- data.frame(
  Категорія = c("A", "B", "C", "D", "E"),
  Значення  = c(12, 19, 7, 15, 10)
)

p7 <- ggplot(task9_df, aes(x = Категорія, y = Значення, fill = Категорія)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = Значення), vjust = -0.4, size = 5) +
  labs(
    title = "Індивідуальне завдання №9: стовпчиковий графік (geom_col)",
    x = "Категорія",
    y = "Значення"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  expand_limits(y = max(task9_df$Значення) * 1.15)

save_plot_png_pdf("fig_07_barplot_task9", p7)

# ----------------------------------------------------------
# 10) Підсумок
# ----------------------------------------------------------
cat("\nПапка з результатами:", out_dir, "\n")
print(list.files(out_dir))
