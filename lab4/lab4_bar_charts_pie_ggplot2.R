# ==========================================================
# Лабораторна робота №4
# Тема: Стовпчикові діаграми (bar charts) та кругові діаграми в R
#       (ggplot2 + base R pie + coord_polar)
#
# Етапи:
#  1) Побудувати різні види bar charts: прості, згруповані, стекові, 100% stacked,
#     з похибками (SE), з фасетами.
#  2) Підготувати дані: агрегація, pivot_longer, binning (cut).
#  3) Додати підписи, сортування категорій, відсоткові шкали, легенди і теми.
#  4) Побудувати pie/donut у base R та ggplot2 (coord_polar).
#  5) Зберегти графіки у PNG та PDF.
#  6) Індивідуальне завдання №9: binning Sepal.Length у iris + bar chart за Species.
# ==========================================================

# -------------------------------
# 0) Підготовка середовища
# -------------------------------
packages <- c("ggplot2", "dplyr", "tidyr", "scales")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

out_dir <- "plots_lab4"
if (!dir.exists(out_dir)) dir.create(out_dir)

save_both <- function(plot_obj, filename_base, w = 9, h = 6, dpi = 300) {
  # Зберігаємо і PNG, і PDF з однаковими розмірами
  ggsave(filename = file.path(out_dir, paste0(filename_base, ".png")),
         plot = plot_obj, width = w, height = h, dpi = dpi, bg = "white")
  ggsave(filename = file.path(out_dir, paste0(filename_base, ".pdf")),
         plot = plot_obj, width = w, height = h, dpi = dpi, bg = "white")
}

# Єдина тема для читабельності (без перевантаження)
theme_ua <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

# ==========================================================
# 1) Дані: mtcars -> підготовка факторів
# ==========================================================
mt <- mtcars %>%
  mutate(
    cyl = factor(cyl),
    am  = factor(am, labels = c("Автомат", "Механіка"))
  )

# ==========================================================
# 2) 4.1 Проста (one factor): частоти за cyl
# ==========================================================
mt_counts <- mt %>% count(cyl)

p1 <- ggplot(mt_counts, aes(x = cyl, y = n)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), vjust = -0.3, size = 4) +
  labs(
    title = "Кількість авто за числом циліндрів",
    x = "Циліндри",
    y = "Кількість"
  ) +
  theme_ua

save_both(p1, "fig_lab4_01_simple_counts")

# ==========================================================
# 3) 4.2 Згрупована (dodged): (cyl × КПП)
# ==========================================================
mt2_counts <- mt %>% count(cyl, am)

p2 <- ggplot(mt2_counts, aes(x = cyl, y = n, fill = am)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.75),
            vjust = -0.3, size = 3.7) +
  labs(
    title = "К-сть авто за циліндрами і типом КПП (згрупована)",
    x = "Циліндри",
    y = "Кількість",
    fill = "КПП"
  ) +
  theme_ua

save_both(p2, "fig_lab4_02_dodged_counts")

# ==========================================================
# 4) 4.3 Стекова (stacked): (cyl × КПП)
# ==========================================================
p3 <- ggplot(mt2_counts, aes(x = cyl, y = n, fill = am)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.6, color = "white") +
  labs(
    title = "Стекова діаграма: розподіл КПП у межах циліндрів",
    x = "Циліндри",
    y = "Кількість",
    fill = "КПП"
  ) +
  theme_ua

save_both(p3, "fig_lab4_03_stacked_counts")

# ==========================================================
# 5) 4.3 Нормована (100% stacked): частки + % шкала + підписи %
# ==========================================================
prop_df <- mt2_counts %>%
  group_by(cyl) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p4 <- ggplot(prop_df, aes(x = cyl, y = prop, fill = am)) +
  geom_col(position = "fill", width = 0.7, color = "white") +
  geom_text(aes(label = percent(prop, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "black") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Нормована (100%): частки КПП у межах циліндрів",
    x = "Циліндри",
    y = "Частка",
    fill = "КПП"
  ) +
  theme_ua

save_both(p4, "fig_lab4_04_fill_props_labels")

# ==========================================================
# 6) 4.4 Агрегована метрика + похибки (SE) для mpg за (cyl × КПП)
# ==========================================================
summ <- mt %>%
  group_by(cyl, am) %>%
  summarise(
    mean_mpg = mean(mpg),
    se = sd(mpg) / sqrt(n()),
    .groups = "drop"
  )

p5 <- ggplot(summ, aes(x = cyl, y = mean_mpg, fill = am)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_mpg - se, ymax = mean_mpg + se),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  geom_text(aes(label = round(mean_mpg, 1)),
            position = position_dodge(width = 0.8),
            vjust = -0.35, size = 3.6) +
  labs(
    title = "Середня витрата пального (mpg) з похибками (SE)",
    x = "Циліндри",
    y = "Середнє mpg",
    fill = "КПП"
  ) +
  theme_ua

save_both(p5, "fig_lab4_05_mean_with_se")

# ==========================================================
# 7) 4.5 Фасети + сортування категорій (вага як категорія)
# ==========================================================
mt3 <- mt %>%
  mutate(weight_cat = ifelse(wt < median(wt), "Легкі", "Важкі"))

mt3_counts <- mt3 %>% count(cyl, am, weight_cat) %>%
  group_by(cyl) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(cyl = reorder(cyl, total))

p6 <- ggplot(mt3_counts, aes(x = cyl, y = n, fill = am)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.75),
            vjust = -0.3, size = 3.4) +
  facet_wrap(~ weight_cat) +
  labs(
    title = "К-сть авто (циліндри × КПП) у фасетах за вагою",
    x = "Циліндри (відсортовано)",
    y = "Кількість",
    fill = "КПП"
  ) +
  theme_ua

save_both(p6, "fig_lab4_06_facets_sorted")

# ==========================================================
# 8) 5.1 Кругова діаграма (base R) + збереження PNG/PDF
# ==========================================================
x <- c(40, 25, 20, 15)
labels <- c("A", "B", "C", "D")
pct <- round(100 * x / sum(x))
labels_pct <- paste0(labels, " (", pct, "%)")

# PNG
png(file.path(out_dir, "fig_lab4_07_pie_base.png"),
    width = 1200, height = 800, res = 160, bg = "white")
par(mar = c(3, 3, 4, 3) + 0.1)
pie(x, labels = labels_pct, main = "Кругова діаграма (base R)")
dev.off()

# PDF
pdf(file.path(out_dir, "fig_lab4_07_pie_base.pdf"),
    width = 9, height = 6)
par(mar = c(3, 3, 4, 3) + 0.1)
pie(x, labels = labels_pct, main = "Кругова діаграма (base R)")
dev.off()

# ==========================================================
# 9) 5.2 Кругова діаграма (ggplot2 coord_polar) + donut
# ==========================================================
df_pie <- data.frame(cat = labels, value = x) %>%
  mutate(prop = value / sum(value),
         label = paste0(cat, " (", percent(prop, accuracy = 1), ")"))

p7 <- ggplot(df_pie, aes(x = "", y = value, fill = cat)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Кругова діаграма (ggplot2)", x = NULL, y = NULL, fill = "Категорія") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

save_both(p7, "fig_lab4_08_pie_ggplot", w = 7.5, h = 6)

p8 <- ggplot(df_pie, aes(x = 2, y = value, fill = cat)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  labs(title = "Donut-діаграма (ggplot2)", fill = "Категорія") +
  theme_void() +
  theme(plot.title = element_text(face = "bold"))

save_both(p8, "fig_lab4_09_donut_ggplot", w = 7.5, h = 6)

# ==========================================================
# 10) Індивідуальне завдання №9:
#     iris$Sepal.Length -> категорії (cut) + частоти за Species
# ==========================================================
iris2 <- iris %>%
  mutate(
    # 4 інтервали (можна змінити на 3, якщо треба)
    sepal_len_bin = cut(
      Sepal.Length,
      breaks = 4,
      include.lowest = TRUE
    )
  )

iris_counts <- iris2 %>% count(sepal_len_bin, Species)

# Стекова
p9 <- ggplot(iris_counts, aes(x = sepal_len_bin, y = n, fill = Species)) +
  geom_col(position = "stack", width = 0.75) +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 3.3, color = "white") +
  labs(
    title = "Iris: частоти за бінованим Sepal.Length (стекова)",
    x = "Інтервали Sepal.Length (binning)",
    y = "Кількість",
    fill = "Вид (Species)"
  ) +
  theme_ua +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_both(p9, "fig_lab4_10_iris_binning_stacked", w = 10, h = 6)

# Згрупована (dodged)
p10 <- ggplot(iris_counts, aes(x = sepal_len_bin, y = n, fill = Species)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.8),
            vjust = -0.25, size = 3.2) +
  labs(
    title = "Iris: частоти за бінованим Sepal.Length (згрупована)",
    x = "Інтервали Sepal.Length (binning)",
    y = "Кількість",
    fill = "Вид (Species)"
  ) +
  theme_ua +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_both(p10, "fig_lab4_11_iris_binning_dodged", w = 10, h = 6)

# 100% stacked (частки) + % підписи
iris_prop <- iris_counts %>%
  group_by(sepal_len_bin) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p11 <- ggplot(iris_prop, aes(x = sepal_len_bin, y = prop, fill = Species)) +
  geom_col(position = "fill", width = 0.75, color = "white") +
  geom_text(aes(label = percent(prop, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 3.1) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Iris: частки Species у межах bin Sepal.Length (100% stacked)",
    x = "Інтервали Sepal.Length (binning)",
    y = "Частка",
    fill = "Вид (Species)"
  ) +
  theme_ua +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_both(p11, "fig_lab4_12_iris_binning_fill", w = 10, h = 6)

# ==========================================================
# 11) Службовий вивід: що збережено
# ==========================================================
cat("\nГотово! Файли збережено у папці:", out_dir, "\n")
print(list.files(out_dir))
