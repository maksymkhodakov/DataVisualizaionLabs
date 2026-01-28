# ==========================================================
# Лабораторна робота №2
# Тема: Побудова та форматування 2D-діаграм у R (base graphics)
#
# Вимоги:
#  - Відтворити приклади 4.1–4.8
#  - Виконати індивідуальне завдання №9 (підписи точок)
#  - Виконати завдання №16 (експорт PNG, PDF, SVG + порівняння якості/розміру)
#  - Експорт графіків з назвами виду fig_lab2_taskXX.*
# ==========================================================

# -------------------------------
# 0) Підготовка середовища
# -------------------------------

# Папка для збереження графіків
out_dir <- "plots_lab2"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Фіксуємо seed для відтворюваності (де це доречно)
set.seed(2)

# Допоміжна функція для акуратного відновлення par()
with_par <- function(expr) {
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar), add = TRUE)
  force(expr)
}

# ==========================================================
# 4.1 Дані й базовий лінійний графік
# ==========================================================

dose  <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
df    <- data.frame(Dose = dose, DrugA = drugA, DrugB = drugB)

# Рис. 1 — базовий графік (на екран)
with_par({
  plot(dose, drugA, type = "b",
       main = "Рис. 1 — Базовий графік: препарат A",
       xlab = "Доза",
       ylab = "Ефект")
})

# ==========================================================
# 4.2 Зміна символів, ліній, розмірів + експорт (PNG/PDF)
# ==========================================================

# Збережемо оформлений графік у файли
png(file.path(out_dir, "fig_lab2_task02_style_A.png"),
    width = 1400, height = 900, res = 160)
with_par({
  par(lty = 2, pch = 17, lwd = 2, cex = 1.2,
      cex.main = 1.15, cex.lab = 1.0, cex.axis = 0.95,
      mar = c(5, 5, 4, 2) + 0.1)
  
  plot(dose, drugA, type = "b", col = "red",
       main = "Рис. 2 — Препарат A: пунктир, трикутники, збільшені точки",
       xlab = "Доза",
       ylab = "Ефект")
  grid(col = "gray85", lty = 2)
})
dev.off()

pdf(file.path(out_dir, "fig_lab2_task02_style_A.pdf"),
    width = 9, height = 6)
with_par({
  par(lty = 2, pch = 17, lwd = 2, cex = 1.1,
      cex.main = 1.1, cex.lab = 1.0, cex.axis = 0.95,
      mar = c(5, 5, 4, 2) + 0.1)
  
  plot(dose, drugA, type = "b", col = "red",
       main = "Рис. 2 — Препарат A: пунктир, трикутники, збільшені точки",
       xlab = "Доза",
       ylab = "Ефект")
  grid(col = "gray85", lty = 2)
})
dev.off()

# ==========================================================
# 4.3 Кольори: палети і сірі відтінки (pie) + експорт
# ==========================================================

png(file.path(out_dir, "fig_lab2_task03_palettes.png"),
    width = 1600, height = 700, res = 170)
with_par({
  n <- 10
  par(mfrow = c(1, 3), mar = c(2, 2, 4, 2) + 0.1)
  pie(rep(1, n), col = rainbow(n), main = "Рис. 3а — rainbow()")
  pie(rep(1, n), col = gray(0:n/n), main = "Рис. 3б — gray()")
  pie(rep(1, n), col = heat.colors(n), main = "Рис. 3в — heat.colors()")
})
dev.off()

pdf(file.path(out_dir, "fig_lab2_task03_palettes.pdf"),
    width = 12, height = 4.8)
with_par({
  n <- 10
  par(mfrow = c(1, 3), mar = c(2, 2, 4, 2) + 0.1)
  pie(rep(1, n), col = rainbow(n), main = "Рис. 3а — rainbow()")
  pie(rep(1, n), col = gray(0:n/n), main = "Рис. 3б — gray()")
  pie(rep(1, n), col = heat.colors(n), main = "Рис. 3в — heat.colors()")
})
dev.off()

# Повертаємо режим 1 графік на сторінку
par(mfrow = c(1, 1))

# ==========================================================
# 4.4 Шрифти/поля/розміри пристрою + експорт
# ==========================================================

png(file.path(out_dir, "fig_lab2_task04_fonts_margins.png"),
    width = 1400, height = 900, res = 160)
with_par({
  par(cex.main = 1.15, cex.lab = 1.0, cex.axis = 0.95,
      font.lab = 2,
      mar = c(5, 5, 4, 2) + 0.1)
  
  plot(dose, drugA, type = "b", col = "red",
       main = "Рис. 4 — Оформлення: шрифти, підписи, поля",
       xlab = "Доза",
       ylab = "Ефект")
  grid(col = "gray85", lty = 2)
})
dev.off()

pdf(file.path(out_dir, "fig_lab2_task04_fonts_margins.pdf"),
    width = 9, height = 6)
with_par({
  par(cex.main = 1.1, cex.lab = 1.0, cex.axis = 0.95,
      font.lab = 2,
      mar = c(5, 5, 4, 2) + 0.1)
  
  plot(dose, drugA, type = "b", col = "red",
       main = "Рис. 4 — Оформлення: шрифти, підписи, поля",
       xlab = "Доза",
       ylab = "Ефект")
  grid(col = "gray85", lty = 2)
})
dev.off()

# ==========================================================
# 4.5 Керування осями, друга вісь, заголовки + експорт
# ==========================================================

png(file.path(out_dir, "fig_lab2_task05_two_axes.png"),
    width = 1500, height = 900, res = 170)
with_par({
  x <- 1:10
  y <- x
  z <- 10 / x
  
  par(mar = c(5, 5, 4, 9) + 0.1)
  
  plot(x, y, type = "b", pch = 21, col = "red", bg = "white",
       yaxt = "n", lty = 3, ann = FALSE)
  
  lines(x, z, type = "b", pch = 22, col = "blue", bg = "white", lty = 2)
  
  axis(2, at = pretty(y), labels = pretty(y), col.axis = "red", las = 1)
  mtext("Y = X", side = 2, line = 3, col = "red")
  
  axis(4, at = pretty(z), labels = round(pretty(z), 2),
       col.axis = "blue", las = 1, cex.axis = 0.9, tck = -0.01)
  mtext("Y = 10/X", side = 4, line = 3, col = "blue")
  
  title(main = "Рис. 5 — Дві залежності з двома осями",
        xlab = "X",
        ylab = "")
  grid(col = "gray88", lty = 2)
  
  legend("topright", inset = 0.02, bty = "n",
         legend = c("y = x", "y = 10/x"),
         lty = c(3, 2), pch = c(21, 22),
         col = c("red", "blue"))
})
dev.off()

pdf(file.path(out_dir, "fig_lab2_task05_two_axes.pdf"),
    width = 10, height = 6)
with_par({
  x <- 1:10
  y <- x
  z <- 10 / x
  
  par(mar = c(5, 5, 4, 9) + 0.1)
  
  plot(x, y, type = "b", pch = 21, col = "red", bg = "white",
       yaxt = "n", lty = 3, ann = FALSE)
  
  lines(x, z, type = "b", pch = 22, col = "blue", bg = "white", lty = 2)
  
  axis(2, at = pretty(y), labels = pretty(y), col.axis = "red", las = 1)
  mtext("Y = X", side = 2, line = 3, col = "red")
  
  axis(4, at = pretty(z), labels = round(pretty(z), 2),
       col.axis = "blue", las = 1, cex.axis = 0.9, tck = -0.01)
  mtext("Y = 10/X", side = 4, line = 3, col = "blue")
  
  title(main = "Рис. 5 — Дві залежності з двома осями",
        xlab = "X",
        ylab = "")
  grid(col = "gray88", lty = 2)
  
  legend("topright", inset = 0.02, bty = "n",
         legend = c("y = x", "y = 10/x"),
         lty = c(3, 2), pch = c(21, 22),
         col = c("red", "blue"))
})
dev.off()

# ==========================================================
# 4.6 Сітка/орієнтири + легенда: порівняння A та B + експорт
# ==========================================================

has_hmisc <- requireNamespace("Hmisc", quietly = TRUE)

png(file.path(out_dir, "fig_lab2_task06_comparison_AB.png"),
    width = 1500, height = 900, res = 170)
with_par({
  par(lwd = 2, cex = 1.15, font.lab = 2,
      mar = c(5, 5, 4, 2) + 0.1)
  
  plot(dose, drugA, type = "b", pch = 15, lty = 1, col = "red",
       ylim = c(0, 60),
       main = "Рис. 6 — Порівняння препаратів A та B",
       xlab = "Доза",
       ylab = "Ефект")
  
  lines(dose, drugB, type = "b", pch = 17, lty = 2, col = "blue")
  
  # Орієнтири (сітка)
  abline(v = seq(20, 60, 10), lty = 2, col = "gray80")
  abline(h = seq(0, 60, 10),  lty = 2, col = "gray80")
  
  # Дрібні поділки (якщо доступно)
  if (has_hmisc) {
    Hmisc::minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)
  }
  
  legend("topleft", inset = 0.03, title = "Препарат",
         legend = c("A", "B"),
         lty = c(1, 2), pch = c(15, 17),
         col = c("red", "blue"), bty = "n")
})
dev.off()

pdf(file.path(out_dir, "fig_lab2_task06_comparison_AB.pdf"),
    width = 9.5, height = 6)
with_par({
  par(lwd = 2, cex = 1.1, font.lab = 2,
      mar = c(5, 5, 4, 2) + 0.1)
  
  plot(dose, drugA, type = "b", pch = 15, lty = 1, col = "red",
       ylim = c(0, 60),
       main = "Рис. 6 — Порівняння препаратів A та B",
       xlab = "Доза",
       ylab = "Ефект")
  
  lines(dose, drugB, type = "b", pch = 17, lty = 2, col = "blue")
  
  abline(v = seq(20, 60, 10), lty = 2, col = "gray80")
  abline(h = seq(0, 60, 10),  lty = 2, col = "gray80")
  
  if (has_hmisc) {
    Hmisc::minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)
  }
  
  legend("topleft", inset = 0.03, title = "Препарат",
         legend = c("A", "B"),
         lty = c(1, 2), pch = c(15, 17),
         col = c("red", "blue"), bty = "n")
})
dev.off()

# ==========================================================
# 4.7 Підписування точок/анотації (приклад) + експорт
# ==========================================================

png(file.path(out_dir, "fig_lab2_task07_mtcars_example.png"),
    width = 1600, height = 1000, res = 170)
with_par({
  par(mar = c(5, 5, 4, 2) + 0.1)
  
  plot(mtcars$wt, mtcars$mpg, pch = 18, col = "blue",
       main = "Рис. 7 — mtcars: MPG vs маса (приклад з text())",
       xlab = "Маса (1000 lbs)",
       ylab = "MPG")
  
  grid(col = "gray88", lty = 2)
  
  text(mtcars$wt, mtcars$mpg,
       labels = rownames(mtcars),
       cex = 0.65, pos = 4, col = "red")
})
dev.off()

pdf(file.path(out_dir, "fig_lab2_task07_mtcars_example.pdf"),
    width = 10, height = 7)
with_par({
  par(mar = c(5, 5, 4, 2) + 0.1)
  
  plot(mtcars$wt, mtcars$mpg, pch = 18, col = "blue",
       main = "Рис. 7 — mtcars: MPG vs маса (приклад з text())",
       xlab = "Маса (1000 lbs)",
       ylab = "MPG")
  
  grid(col = "gray88", lty = 2)
  
  text(mtcars$wt, mtcars$mpg,
       labels = rownames(mtcars),
       cex = 0.65, pos = 4, col = "red")
})
dev.off()

# ==========================================================
# ІНДИВІДУАЛЬНЕ ЗАВДАННЯ №9
# Підписи точок назвами моделей (mtcars) + експорт PNG/PDF
# ==========================================================

# Підхід: легка сітка + невеликий шрифт підписів, pos=4 (праворуч).
# Це забезпечує читабельність і відповідає вимозі "без критичних накладань".
png(file.path(out_dir, "fig_lab2_task09_mtcars_labels.png"),
    width = 1800, height = 1100, res = 180)
with_par({
  par(mar = c(5, 5, 4, 2) + 0.1)
  
  plot(mtcars$wt, mtcars$mpg,
       pch = 19, col = "darkblue",
       main = "Рис. 8 — Завдання №9: mtcars з підписами моделей",
       xlab = "Маса авто (1000 lbs)",
       ylab = "Витрати палива (MPG)")
  
  abline(h = pretty(mtcars$mpg), v = pretty(mtcars$wt), col = "gray85", lty = 2)
  
  text(mtcars$wt, mtcars$mpg,
       labels = rownames(mtcars),
       pos = 4, cex = 0.70, col = "red")
})
dev.off()

pdf(file.path(out_dir, "fig_lab2_task09_mtcars_labels.pdf"),
    width = 10.5, height = 7.2)
with_par({
  par(mar = c(5, 5, 4, 2) + 0.1)
  
  plot(mtcars$wt, mtcars$mpg,
       pch = 19, col = "darkblue",
       main = "Рис. 8 — Завдання №9: mtcars з підписами моделей",
       xlab = "Маса авто (1000 lbs)",
       ylab = "Витрати палива (MPG)")
  
  abline(h = pretty(mtcars$mpg), v = pretty(mtcars$wt), col = "gray85", lty = 2)
  
  text(mtcars$wt, mtcars$mpg,
       labels = rownames(mtcars),
       pos = 4, cex = 0.70, col = "red")
})
dev.off()

# ==========================================================
# ЗАВДАННЯ №16
# PNG + PDF + SVG (через svglite) + перевірка файлів
# ==========================================================

make_plot <- function() {
  plot(iris$Sepal.Length, iris$Sepal.Width,
       pch = 19, cex = 0.8, col = "steelblue",
       main = "Iris: Sepal.Length vs Sepal.Width",
       xlab = "Sepal Length", ylab = "Sepal Width")
  abline(lm(Sepal.Width ~ Sepal.Length, data = iris), lwd = 2, lty = 2)
  legend("topright", bty = "n", pch = 19, col = "steelblue",
         legend = "points + linear fit")
  grid(col = "gray88", lty = 2)
}

out_dir <- "plots_lab2"
if (!dir.exists(out_dir)) dir.create(out_dir)

f_png <- file.path(out_dir, "fig_lab2_task16_iris.png")
f_pdf <- file.path(out_dir, "fig_lab2_task16_iris.pdf")
f_svg <- file.path(out_dir, "fig_lab2_task16_iris.svg")

# PNG
png(f_png, width = 1600, height = 1000, res = 150)
make_plot()
dev.off()

# PDF
pdf(f_pdf, width = 8, height = 5)
make_plot()
dev.off()

# SVG (краще через svglite)
if (!requireNamespace("svglite", quietly = TRUE)) {
  install.packages("svglite")
}
svglite::svglite(f_svg, width = 8, height = 5)
make_plot()
dev.off()

# Перевірка
cat("\nФайли створені?\n")
cat("PNG:", file.exists(f_png), "\n")
cat("PDF:", file.exists(f_pdf), "\n")
cat("SVG:", file.exists(f_svg), "\n")

# Розмір файлів (КБ)
sizes <- data.frame(
  file = c(basename(f_png), basename(f_pdf), basename(f_svg)),
  size_kb = round(c(file.info(f_png)$size,
                    file.info(f_pdf)$size,
                    file.info(f_svg)$size) / 1024, 2)
)
print(sizes)

# Показати, що є в папці
cat("\nВміст папки", out_dir, ":\n")
print(list.files(out_dir))
