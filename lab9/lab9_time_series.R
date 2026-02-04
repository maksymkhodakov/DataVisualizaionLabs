############################################################
# ЛАБОРАТОРНА РОБОТА №9
# ВІЗУАЛІЗАЦІЯ ЧАСОВИХ РЯДІВ У R
#
# Мета:
# - Ознайомитися з методами візуалізації часових рядів у R
# - Побудувати графіки: базовий ряд, ggplot-стиль, сезонність,
#   полярна сезонність, monthplot (субсерії), ACF/PACF, heatmap,
#   мультисерійний графік, а також індивідуальне завдання №9
# - Зберегти графіки у PNG та PDF
############################################################

# ----------------------------------------------------------
# 0) Підготовка середовища
# ----------------------------------------------------------

# Мінімально потрібні пакети
pkgs_min <- c("ggplot2", "forecast")
for (p in pkgs_min) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, dependencies = TRUE)
}
library(ggplot2)
library(forecast)

pkgs_opt <- c("tsibble", "fpp3")
for (p in pkgs_opt) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("Опційний пакет НЕ встановлено: ", p, " (це не критично).")
  }
}

# Директорія для результатів
out_dir <- "plots_lab9"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Допоміжні функції збереження
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

save_gg_png_pdf <- function(file_base, p, w = 10, h = 6.5, dpi = 300) {
  ggsave(file.path(out_dir, paste0(file_base, ".png")), plot = p, width = w, height = h, dpi = dpi, bg = "white")
  ggsave(file.path(out_dir, paste0(file_base, ".pdf")), plot = p, width = w, height = h, bg = "white")
}

# ----------------------------------------------------------
# 1) Дані
# ----------------------------------------------------------
data("AirPassengers")  # ts: 1949–1960, monthly
ap <- AirPassengers

# ----------------------------------------------------------
# 2) Базовий графік часового ряду (plot.ts / plot)
# ----------------------------------------------------------
save_base_png_pdf(
  "fig_01_plot_ts_basic",
  expr_plot = function() {
    plot(ap,
         main = "Пасажиропотік (1949–1960): базовий графік",
         xlab = "Рік",
         ylab = "Кількість пасажирів (тис.)",
         col = "blue",
         lwd = 2)
    grid()
  }
)

# ----------------------------------------------------------
# 3) Графік у стилі ggplot2 (autoplot з forecast)
# ----------------------------------------------------------
p2 <- autoplot(ap) +
  ggtitle("AirPassengers — графік у стилі ggplot2") +
  xlab("Рік") + ylab("Кількість пасажирів (тис.)") +
  theme_minimal(base_size = 13)

save_gg_png_pdf("fig_02_autoplot_ggplot", p2)

# ----------------------------------------------------------
# 4) Сезонний графік (ggseasonplot)
# ----------------------------------------------------------
p3 <- ggseasonplot(ap, year.labels = TRUE, year.labels.left = TRUE) +
  ggtitle("Сезонність пасажиропотоку (ggseasonplot)") +
  xlab("Місяць") + ylab("Кількість пасажирів (тис.)") +
  theme_minimal(base_size = 12)

save_gg_png_pdf("fig_03_seasonplot", p3, w = 11, h = 7)

# ----------------------------------------------------------
# 5) Полярна сезонна діаграма (ggseasonplot, polar = TRUE)
# ----------------------------------------------------------
p4 <- ggseasonplot(ap, polar = TRUE) +
  ggtitle("Полярна сезонна діаграма пасажиропотоку") +
  theme_minimal(base_size = 12)

save_gg_png_pdf("fig_04_seasonplot_polar", p4, w = 8, h = 8)

# ----------------------------------------------------------
# 6) Субсерії за місяцями (monthplot) — base R
# ----------------------------------------------------------
save_base_png_pdf(
  "fig_05_monthplot",
  expr_plot = function() {
    monthplot(ap,
              main = "Monthplot: субсерії за місяцями (AirPassengers)",
              ylab = "Кількість пасажирів (тис.)",
              xlab = "Місяць",
              col = "darkgreen")
    grid()
  },
  pdf_w = 11, pdf_h = 7
)

# ----------------------------------------------------------
# 7) ACF та PACF (автокореляція та часткова автокореляція)
# ----------------------------------------------------------
save_base_png_pdf(
  "fig_06_acf",
  expr_plot = function() {
    acf(ap, main = "ACF: автокореляція AirPassengers")
  },
  pdf_w = 10, pdf_h = 6.5
)

save_base_png_pdf(
  "fig_07_pacf",
  expr_plot = function() {
    pacf(ap, main = "PACF: часткова автокореляція AirPassengers")
  },
  pdf_w = 10, pdf_h = 6.5
)

# ----------------------------------------------------------
# 8) Heatmap «рік–місяць» через ggplot2
# ----------------------------------------------------------

# Часова шкала
t <- time(ap)

# Коректне виділення року та місяця
year  <- floor(t)
month <- round(12 * (t - year) + 1)  # 1..12

ap_hm <- data.frame(
  Year  = factor(year),
  Month = factor(
    month,
    levels = 1:12,
    labels = c("Січ","Лют","Бер","Кві","Тра","Чер",
               "Лип","Сер","Вер","Жов","Лис","Гру")
  ),
  Value = as.numeric(ap)
)

# Побудова heatmap
p8 <- ggplot(ap_hm, aes(x = Month, y = Year, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#deebf7",
    high = "#08519c",
    name = "Пасажири\n(тис.)"
  ) +
  labs(
    title = "Heatmap пасажиропотоку: Рік × Місяць",
    x = "Місяць",
    y = "Рік"
  ) +
  theme_minimal(base_size = 12)

# Збереження
save_gg_png_pdf("fig_08_heatmap_year_month", p8, w = 10, h = 7)

# ----------------------------------------------------------
# 9) Мультисерійний графік: оригінал та лог-трансформація (ts.plot)
# ----------------------------------------------------------
save_base_png_pdf(
  "fig_09_multiseries_tsplot",
  expr_plot = function() {
    ts.plot(ap, log(ap),
            col = c("blue", "red"),
            lwd = 2,
            main = "Мультисерійний графік: Оригінал vs Лог-трансформація",
            xlab = "Рік",
            ylab = "Значення")
    legend("topleft",
           legend = c("Оригінал", "Лог-трансформація"),
           col = c("blue", "red"),
           lty = 1,
           lwd = 2,
           bty = "n")
    grid()
  },
  pdf_w = 11, pdf_h = 7
)

# ----------------------------------------------------------
# 10) Індивідуальне завдання №9: порівняння періодів 1949–1954 та 1955–1960
# ----------------------------------------------------------
ap_49_54 <- window(ap, start = c(1949, 1), end = c(1954, 12))
ap_55_60 <- window(ap, start = c(1955, 1), end = c(1960, 12))

# 10.1 графік 1949–1954
save_base_png_pdf(
  "fig_10_task9_period_1949_1954",
  expr_plot = function() {
    plot(ap_49_54,
         main = "Індивідуальне завдання №9: період 1949–1954",
         xlab = "Рік",
         ylab = "Пасажири (тис.)",
         col = "darkblue",
         lwd = 2)
    grid()
  }
)

# 10.2 графік 1955–1960
save_base_png_pdf(
  "fig_11_task9_period_1955_1960",
  expr_plot = function() {
    plot(ap_55_60,
         main = "Індивідуальне завдання №9: період 1955–1960",
         xlab = "Рік",
         ylab = "Пасажири (тис.)",
         col = "darkred",
         lwd = 2)
    grid()
  }
)

# ----------------------------------------------------------
# 11) Підсумок
# ----------------------------------------------------------
cat("\nГотово! Графіки збережено у папці:", out_dir, "\n")
print(list.files(out_dir))
