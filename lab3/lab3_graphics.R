# ==========================================================
# Лабораторна робота №3
# Тема: Анотації математичних символів, об’єднання діаграм та стовпчикові діаграми в R
# Вимоги:
#  1) plotmath: expression() у підписах/осях
#  2) Об’єднання діаграм: par(mfrow) та layout()
#  3) barplot(): сортування, підписи, виділення, лінії, rect()-підкладка
#  4) Збереження PNG/PDF (публікаційна якість)
#  5) Індивідуальне завдання №9: підкладка «смуг» rect()
# ==========================================================

# -------------------------------
# 0) Підготовка папки
# -------------------------------
out_dir <- "plots_lab3"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Допоміжний “контекст” для пар(): після блоку автоматично повертає налаштування
with_par <- function(expr) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  force(expr)
}

# ==========================================================
# 1) Завдання 4.1: Математичні анотації (plotmath) — синус
# ==========================================================
make_sine_plot <- function() {
  x <- seq(-pi, pi, length.out = 400)
  y <- sin(x)
  
  with_par({
    par(lwd = 2, cex = 1.1, font.lab = 2, mar = c(5, 5, 4, 2) + 0.1)
    plot(x, y, type = "l", col = "blue", ann = FALSE, xaxt = "n")
    title(
      main = expression("Графік "*italic(sin)*" з анотаціями"),
      xlab = expression("Фазовий кут "*phi),
      ylab = expression(italic(sin)(phi))
    )
    axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
         labels = expression(-pi, -pi/2, 0, pi/2, pi))
    abline(h = seq(-1, 1, 0.5), v = seq(-pi, pi, by = pi/4),
           lty = 2, col = "gray70")
  })
}

# Експорт (PNG/PDF)
png(file.path(out_dir, "fig_lab3_task01_plotmath_sine.png"),
    width = 1400, height = 900, res = 170, bg = "white")
make_sine_plot()
dev.off()

pdf(file.path(out_dir, "fig_lab3_task01_plotmath_sine.pdf"),
    width = 9, height = 6, bg = "white")
make_sine_plot()
dev.off()

# ==========================================================
# 2) Завдання 4.2: Комбінування через par(mfrow)
# ==========================================================
make_mfrow_panel <- function() {
  with_par({
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1) + 0.1, cex = 0.95)
    
    plot(mtcars$wt, mtcars$mpg,
         main = "MPG vs WT",
         xlab = "WT", ylab = "MPG", pch = 19, col = "darkblue")
    grid(col = "gray85", lty = 2)
    
    plot(mtcars$wt, mtcars$disp,
         main = "DISP vs WT",
         xlab = "WT", ylab = "DISP", pch = 19, col = "darkgreen")
    grid(col = "gray85", lty = 2)
    
    hist(mtcars$wt, main = "Histogram: WT", xlab = "WT", col = "gray80", border = "white")
    grid(col = "gray85", lty = 2)
    
    boxplot(mtcars$wt, horizontal = TRUE, main = "Boxplot: WT", xlab = "WT", col = "gray90")
    grid(col = "gray85", lty = 2)
  })
}

png(file.path(out_dir, "fig_lab3_task02_mfrow_2x2.png"),
    width = 1600, height = 1100, res = 170, bg = "white")
make_mfrow_panel()
dev.off()

pdf(file.path(out_dir, "fig_lab3_task02_mfrow_2x2.pdf"),
    width = 10, height = 7, bg = "white")
make_mfrow_panel()
dev.off()

# ==========================================================
# 3) Завдання 4.3: Гнучка композиція через layout()
# ==========================================================
make_layout_panel <- function() {
  with_par({
    layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE),
           widths = c(3, 1), heights = c(1, 2))
    
    par(mar = c(4, 4, 3, 1) + 0.1, cex = 0.95)
    
    hist(mtcars$wt, main = "WT (верхній рядок)", xlab = "WT",
         col = "gray80", border = "white")
    grid(col = "gray85", lty = 2)
    
    hist(mtcars$mpg, main = "MPG (ліва нижня панель)", xlab = "MPG",
         col = "gray80", border = "white")
    grid(col = "gray85", lty = 2)
    
    hist(mtcars$disp, main = "DISP (права нижня панель)", xlab = "DISP",
         col = "gray80", border = "white")
    grid(col = "gray85", lty = 2)
  })
}

png(file.path(out_dir, "fig_lab3_task03_layout.png"),
    width = 1600, height = 1100, res = 170, bg = "white")
make_layout_panel()
dev.off()

pdf(file.path(out_dir, "fig_lab3_task03_layout.pdf"),
    width = 10, height = 7, bg = "white")
make_layout_panel()
dev.off()

# ==========================================================
# 4) Завдання 4.4 + Індивідуальне №9:
#    Горизонтальний barplot() + підкладка «смуг» через rect()
# ==========================================================

# Дані
ipsos <- data.frame(
  Country = c("Brazil","Germany","Poland","South Africa","Canada","Great Britain",
              "Italy","South Korea","China","Indonesia","Russia","Turkey",
              "France","Hungary","India","USA"),
  Percent = c(84,27,51,83,46,25,50,18,9,93,56,91,19,29,56,70)
)

# Сортування за зростанням
ipsos <- ipsos[order(ipsos$Percent), ]

# Підсвітка окремих країн
highlight <- ipsos$Country %in% c("France", "South Korea", "Brazil", "Indonesia", "Turkey")
cols <- ifelse(highlight, "magenta", "grey80")

make_ipsos_barplot <- function() {
  with_par({
    par(mar = c(5, 11, 4, 2) + 0.1, las = 1)
    
    # Позиції барів (y-координати центрів)
    bp <- barplot(ipsos$Percent, horiz = TRUE, border = NA,
                  col = cols, xlim = c(0, 100), axes = FALSE,
                  names.arg = rep("", nrow(ipsos)))
    
    # --- ІНДИВІДУАЛЬНЕ №9: rect()-підкладка «смуг» ---
    # Малюємо напівпрозорі прямокутники 0–20–...–100 ПІД графіком
    # Для цього перший barplot вже “відкрив” координатну систему.
    y_min <- min(bp) - 1
    y_max <- max(bp) + 1
    
    rect(0,  y_min, 20, y_max, col = adjustcolor("skyblue", 0.15), border = NA)
    rect(20, y_min, 40, y_max, col = adjustcolor("skyblue", 0.25), border = NA)
    rect(40, y_min, 60, y_max, col = adjustcolor("skyblue", 0.15), border = NA)
    rect(60, y_min, 80, y_max, col = adjustcolor("skyblue", 0.25), border = NA)
    rect(80, y_min, 100, y_max, col = adjustcolor("skyblue", 0.15), border = NA)
    
    # Перемальовуємо барплот поверх підкладки (add=TRUE)
    barplot(ipsos$Percent, horiz = TRUE, border = NA,
            col = cols, xlim = c(0, 100), axes = FALSE, add = TRUE)
    
    # Осі та підписи шкали
    axis(1, at = seq(0, 100, 20), labels = seq(0, 100, 20))
    mtext("Відсоток, %", side = 1, line = 2.5, adj = 1, cex = 0.95)
    
    # Підписи країн (ліворуч) і значень (всередині/поряд зі стовпчиком)
    text(x = -2.5, y = bp, labels = ipsos$Country, xpd = NA, adj = 1, cex = 0.95)
    
    # Значення: для підсвічених робимо білим, для звичайних — чорним
    text(x = pmax(2, ipsos$Percent - 2.5), y = bp,
         labels = ipsos$Percent, adj = 1, cex = 0.9,
         col = ifelse(cols == "magenta", "white", "black"))
    
    # Опорна лінія (умовна “середня”)
    abline(v = 45, lwd = 2, col = "steelblue3", lty = 1)
    
    # Заголовок + пояснення
    title(main = "Релігійні переконання (I definitely believe...)",
          sub  = "Горизонтальний barplot із підкладкою rect() та опорною лінією 45%")
    
    # Легенда для підсвічених
    legend("bottomright", inset = 0.02, bty = "n",
           legend = c("Підсвічені країни", "Інші країни"),
           fill = c("magenta", "grey80"))
  })
}

# Експорт PNG (висока якість)
png(file.path(out_dir, "fig_lab3_task04_barplot_rect.png"),
    width = 1800, height = 1100, res = 180, bg = "white")
make_ipsos_barplot()
dev.off()

# Експорт PDF
grDevices::cairo_pdf(filename = file.path(out_dir, "fig_lab3_task04_barplot_rect.pdf"),
                     width = 10, height = 7, bg = "gray98")
make_ipsos_barplot()
dev.off()

# -------------------------------
# 5) Інформативний список результатів
# -------------------------------
cat("\nГотово! Файли збережено у папці:", out_dir, "\n")
print(list.files(out_dir))
