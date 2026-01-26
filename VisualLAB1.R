# ==========================================================
# ЛАБОРАТОРНА: Візуалізація + структури даних + імпорт/експорт
# Пакети: ggplot2, dplyr, readxl, openxlsx
# (plotly, readr НЕ використовується)
# ==========================================================

# --- 1) Підключення пакетів ---
library(ggplot2)
library(dplyr)

# Якщо openxlsx встановлений — підключимо (не обов'язково)
has_openxlsx <- requireNamespace("openxlsx", quietly = TRUE)
if (has_openxlsx) library(openxlsx)

# --- 2) Створення датасету (структура: data frame / tibble) ---
restaurants <- data.frame(
  name = c(
    "Bistro Verde", "Kyiv Sushi Lab", "Trattoria Roma", "Burger Corner", "Curry House",
    "Saffron Grill", "La Piazza", "Pho Saigon", "Khinkali Point", "Ocean Grill",
    "Bakery&Coffee", "Steak District", "Pasta & Wine", "Green Bowl", "Taco Street",
    "Ramen Bar", "Meat & Fire", "Dolce Vita", "Spice Route", "Fish & Lemon"
  ),
  cuisine = c(
    "Європейська", "Японська", "Італійська", "Американська", "Індійська",
    "Індійська", "Італійська", "Азійська", "Грузинська", "Морепродукти",
    "Європейська", "Стейк-хаус", "Італійська", "Здорова", "Мексиканська",
    "Японська", "Стейк-хаус", "Італійська", "Індійська", "Морепродукти"
  ),
  rating = c(4.2, 4.7, 4.4, 4.0, 4.3, 4.6, 4.1, 4.2, 4.5, 4.4,
             4.0, 4.8, 4.3, 4.1, 4.2, 4.6, 4.7, 4.2, 4.4, 4.5),
  price_range = c(
    "₴₴", "₴₴₴", "₴₴₴", "₴₴", "₴₴",
    "₴₴₴", "₴₴", "₴₴", "₴₴", "₴₴₴",
    "₴", "₴₴₴₴", "₴₴₴", "₴₴", "₴₴",
    "₴₴₴", "₴₴₴₴", "₴₴₴", "₴₴₴", "₴₴₴"
  ),
  district = c(
    "Центр", "Центр", "Печерськ", "Поділ", "Оболонь",
    "Центр", "Поділ", "Лівий берег", "Печерськ", "Центр",
    "Оболонь", "Печерськ", "Центр", "Лівий берег", "Поділ",
    "Центр", "Печерськ", "Лівий берег", "Оболонь", "Поділ"
  ),
  stringsAsFactors = FALSE
)

# Перевірка структури даних
print(restaurants)
str(restaurants)
summary(restaurants)

# --- 3) Приведення типів даних (factor) ---
restaurants <- restaurants %>%
  mutate(
    cuisine = factor(cuisine),
    district = factor(district),
    price_range = factor(price_range, levels = c("₴", "₴₴", "₴₴₴", "₴₴₴₴"))
  )

# --- 4) Візуалізація: фасетні діаграми рейтингів ---
# 4.1 Фасети: кухня ~ район (точки)
p1 <- ggplot(restaurants, aes(x = price_range, y = rating)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.85) +
  facet_grid(cuisine ~ district) +
  labs(
    title = "Рейтинги ресторанів за типом кухні та районом",
    x = "Ціновий діапазон",
    y = "Рейтинг"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  ylim(3.5, 5.0)

print(p1)

# 4.2 Фасети: по кухнях (boxplot по районах)
p2 <- ggplot(restaurants, aes(x = district, y = rating)) +
  geom_boxplot(outlier.alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  facet_wrap(~ cuisine, ncol = 3) +
  labs(
    title = "Рейтинги по районах (фасети за типом кухні)",
    x = "Район",
    y = "Рейтинг"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  ylim(3.5, 5.0)

print(p2)

# --- 5) Експорт та імпорт даних (CSV без readr) ---
write.csv(restaurants, "restaurants.csv", row.names = FALSE)
restaurants_csv <- read.csv("restaurants.csv", stringsAsFactors = FALSE)
cat("\nCSV імпорт успішний, перші рядки:\n")
print(head(restaurants_csv))

# --- 6) (Опційно) Excel через openxlsx ---
if (has_openxlsx) {
  write.xlsx(restaurants, "restaurants.xlsx", overwrite = TRUE)
  restaurants_xlsx <- read.xlsx("restaurants.xlsx")
  cat("\nExcel імпорт успішний, перші рядки:\n")
  print(head(restaurants_xlsx))
} else {
  cat("\nopenxlsx не встановлений — Excel-частину пропускаємо (CSV достатньо).\n")
}
