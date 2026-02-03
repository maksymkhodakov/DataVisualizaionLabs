# ==============================================================================
# Лабораторна робота №6
# Візуалізація розподілів (Гістограми, KDE, Boxplot)
# ==============================================================================

# --- 1. ПІДГОТОВКА СЕРЕДОВИЩА ---
# Створюємо папку для графіків, якщо її немає
dir_name <- "plots_lab6"
if (!dir.exists(dir_name)) {
  dir.create(dir_name)
}

packages <- c("ggplot2", "dplyr", "gridExtra")
to_install <- setdiff(packages, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install)
lapply(packages, library, character.only = TRUE)

set.seed(123)

# --- 2. ПІДГОТОВКА ДАНИХ ---
data("faithful")
data("iris")
# Синтетичний двомодальний розподіл
df_mix <- data.frame(x = c(rnorm(400, 0, 1), rnorm(300, 3, 0.7)))

# --- 3. ГІСТОГРАМИ ТА KDE ---
# Порівняння параметрів бінінгу
p_bin1 <- ggplot(df_mix, aes(x)) + 
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white") +
  labs(title = "Малий binwidth (0.1)", subtitle = "Висока деталізація/Шум") + theme_minimal()

p_bin2 <- ggplot(df_mix, aes(x)) + 
  geom_histogram(binwidth = 0.7, fill = "steelblue", color = "white") +
  labs(title = "Великий binwidth (0.7)", subtitle = "Згладжування мод") + theme_minimal()

# KDE з лініями середнього та медіани
m <- mean(df_mix$x); md <- median(df_mix$x)
p_kde <- ggplot(df_mix, aes(x, y = after_stat(density))) +
  geom_histogram(binwidth = 0.3, fill = "grey90", color = "grey60") +
  geom_density(color = "darkblue", linewidth = 1) +
  geom_vline(aes(xintercept = m), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = md), color = "darkgreen", linetype = "dotted") +
  labs(title = "KDE + Статистичні показники", subtitle = "Червоний (--) : Mean, Зелений (..) : Median") + theme_minimal()

# --- 4. BOXPLOT ТА ПОРІВНЯННЯ ГРУП ---
p_box <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(outlier.colour = "red", width = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  labs(title = "Розподіл Sepal.Length (Boxplot)") + theme_minimal()

# --- 5. ІНДИВІДУАЛЬНЕ ЗАВДАННЯ №9: VIOLIN VS BOXPLOT ---
p_violin <- ggplot(iris, aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_violin(alpha = 0.5) +
  labs(title = "Violin Plot: Щільність форми") + theme_minimal()

p_box_ind <- ggplot(iris, aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_boxplot(width = 0.2, color = "black") +
  labs(title = "Boxplot: Квартилі та викиди") + theme_minimal()

# --- 6. КОМПОНУВАННЯ ТА ЗБЕРЕЖЕННЯ В ПАПКУ ---
# Основні графіки
main_results <- grid.arrange(p_bin1, p_bin2, p_kde, p_box, ncol = 2)
# Індивідуальне завдання
individual_results <- grid.arrange(p_violin, p_box_ind, ncol = 2)

# Збереження
ggsave(file.path(dir_name, "1_distribution_analysis.png"), main_results, width = 12, height = 10)
ggsave(file.path(dir_name, "2_violin_vs_boxplot.png"), individual_results, width = 10, height = 5)

print(paste("Графіки збережено у папку:", dir_name))

sessionInfo()
