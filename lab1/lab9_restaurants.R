# ==========================================================
# Лабораторна робота (Варіант 9) — ВАЛІДНА ВЕРСІЯ ГРАФІКІВ
# Датасет: ресторани (назва, тип кухні, рейтинг, ціновий діапазон, район)
# Вимоги: очищення, 3+ візуалізації, підписи UA, збереження PNG+PDF
# ==========================================================

library(ggplot2)
library(dplyr)

# 0) Папка для результатів
if (!dir.exists("plots")) dir.create("plots")

set.seed(9)

# ----------------------------------------------------------
# 1) Генерація датасету (більше даних -> читабельні фасети/boxplot)
# ----------------------------------------------------------
cuisines <- c("Європейська","Японська","Італійська","Американська","Індійська",
              "Азійська","Грузинська","Морепродукти","Здорова","Мексиканська")

districts <- c("Центр","Печерськ","Поділ","Оболонь","Лівий берег")

n <- 160  # достатній обсяг, щоб усюди були точки

restaurants_raw <- data.frame(
  name = paste("Ресторан", sprintf("%03d", 1:n)),
  cuisine = sample(cuisines, n, replace = TRUE),
  district = sample(districts, n, replace = TRUE),
  price_level = sample(1:4, n, replace = TRUE, prob = c(0.18, 0.44, 0.28, 0.10)),
  stringsAsFactors = FALSE
)

# Генеруємо рейтинг як число з "невеликими ефектами":
# - дорожчий сегмент трохи вищий
# - певні кухні/райони трохи вищі/нижчі
cuisine_effect <- setNames(runif(length(cuisines), -0.15, 0.20), cuisines)
district_effect <- setNames(runif(length(districts), -0.10, 0.15), districts)

restaurants_raw <- restaurants_raw %>%
  mutate(
    rating = 3.6 +
      0.20 * (price_level - 1) +
      cuisine_effect[cuisine] +
      district_effect[district] +
      rnorm(n, 0, 0.22)
  )

# Додаємо "штучні" проблеми для демонстрації очищення
restaurants_raw$rating[sample(1:n, 6)] <- NA
restaurants_raw$name[sample(1:n, 4)] <- paste0("  ", restaurants_raw$name[sample(1:n, 4)], "  ")
restaurants_raw <- rbind(restaurants_raw, restaurants_raw[1:3, ])  # дублікати
restaurants_raw$rating[sample(1:nrow(restaurants_raw), 2)] <- 6.2  # некоректні значення

# ----------------------------------------------------------
# 2) Очищення та підготовка типів даних
# ----------------------------------------------------------
restaurants <- restaurants_raw %>%
  mutate(
    name = trimws(name),
    cuisine = trimws(cuisine),
    district = trimws(district),
    rating = as.numeric(rating),
    rating = ifelse(rating < 1 | rating > 5, NA, rating)
  ) %>%
  distinct() %>%
  group_by(cuisine) %>%
  mutate(
    # заповнюємо пропуски медіаною по кухні
    rating = ifelse(is.na(rating), median(rating, na.rm = TRUE), rating)
  ) %>%
  ungroup() %>%
  mutate(
    # якщо раптом десь ще NA — заповнимо глобальною медіаною
    rating = ifelse(is.na(rating), median(rating, na.rm = TRUE), rating),
    
    # price_level -> фактор із читабельними лейблами
    price_level = factor(price_level, levels = 1:4, labels = c("₴","₴₴","₴₴₴","₴₴₴₴")),
    
    cuisine = factor(cuisine, levels = cuisines),
    district = factor(district, levels = districts)
  )

cat("Сирих рядків:", nrow(restaurants_raw), "\n")
cat("Після очищення:", nrow(restaurants), "\n")
cat("NA у рейтингу після очищення:", sum(is.na(restaurants$rating)), "\n")

# ----------------------------------------------------------
# 3) Візуалізація 1 — ФАСЕТИ (читабельно)
#    Ідея: фасетимо по кухні, а район на осі X, рейтинги по Y
# ----------------------------------------------------------
p1 <- ggplot(restaurants, aes(x = district, y = rating, color = price_level)) +
  geom_jitter(width = 0.18, alpha = 0.55, size = 1.4) +
  stat_summary(fun = mean, geom = "point", size = 2.5) +
  facet_wrap(~ cuisine, ncol = 4) +
  labs(
    title = "Рейтинги ресторанів по районах (фасети за типом кухні)",
    x = "Район",
    y = "Рейтинг",
    color = "Ціновий діапазон"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 25, hjust = 1)
  ) +
  coord_cartesian(ylim = c(3.0, 5.0))

print(p1)

# ----------------------------------------------------------
# 4) Візуалізація 2 — HEATMAP середнього рейтингу (кухня × район)
#    Це найчитабельніша відповідь на "по типах кухні та районах"
# ----------------------------------------------------------
heat <- restaurants %>%
  group_by(cuisine, district) %>%
  summarise(avg_rating = mean(rating), n = n(), .groups = "drop")

p2 <- ggplot(heat, aes(x = district, y = cuisine, fill = avg_rating)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", avg_rating)), size = 3.4) +
  labs(
    title = "Середній рейтинг (теплова карта): тип кухні × район",
    x = "Район",
    y = "Тип кухні",
    fill = "Середній рейтинг"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

print(p2)

# ----------------------------------------------------------
# 5) Візуалізація 3 — Середній рейтинг по кухнях (стовпчики)
# ----------------------------------------------------------
avg_by_cuisine <- restaurants %>%
  group_by(cuisine) %>%
  summarise(avg_rating = mean(rating), n = n(), .groups = "drop") %>%
  arrange(avg_rating)

p3 <- ggplot(avg_by_cuisine, aes(x = cuisine, y = avg_rating)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Середній рейтинг за типом кухні",
    x = "Тип кухні",
    y = "Середній рейтинг"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  coord_cartesian(ylim = c(3.0, 5.0))

print(p3)

# ----------------------------------------------------------
# 6) Збереження графіків у PNG та PDF
# ----------------------------------------------------------
ggsave("plots/plot1_facets_by_cuisine.png", p1, width = 14, height = 8, dpi = 200)
ggsave("plots/plot1_facets_by_cuisine.pdf", p1, width = 14, height = 8)

ggsave("plots/plot2_heatmap_cuisine_district.png", p2, width = 11, height = 7, dpi = 200)
ggsave("plots/plot2_heatmap_cuisine_district.pdf", p2, width = 11, height = 7)

ggsave("plots/plot3_avg_by_cuisine.png", p3, width = 10, height = 6, dpi = 200)
ggsave("plots/plot3_avg_by_cuisine.pdf", p3, width = 10, height = 6)

cat("\nГрафіки збережено у папку plots/ (PNG + PDF).\n")

# ----------------------------------------------------------
# 7) Імпорт/експорт CSV (без readr)
# ----------------------------------------------------------
write.csv(restaurants, "restaurants_clean.csv", row.names = FALSE)
restaurants_from_csv <- read.csv("restaurants_clean.csv", stringsAsFactors = FALSE)
cat("CSV експорт/імпорт виконано. Рядків після імпорту:", nrow(restaurants_from_csv), "\n")

# ----------------------------------------------------------
# 8) Короткі висновки (чернетка для звіту)
# ----------------------------------------------------------
best_cuisine <- avg_by_cuisine %>% slice_tail(n = 1)
best_district <- restaurants %>%
  group_by(district) %>%
  summarise(avg_rating = mean(rating), n = n(), .groups = "drop") %>%
  arrange(desc(avg_rating)) %>%
  slice(1)

cat("\n--- ВИСНОВКИ (чернетка) ---\n")
cat("Найвищий середній рейтинг серед кухонь має:", as.character(best_cuisine$cuisine),
    "—", round(best_cuisine$avg_rating, 2), "\n")
cat("Найвищий середній рейтинг серед районів має:", as.character(best_district$district),
    "—", round(best_district$avg_rating, 2), "\n")
cat("Теплова карта показує відмінності середніх рейтингів у розрізі «кухня × район».\n")
cat("---------------------------\n")
