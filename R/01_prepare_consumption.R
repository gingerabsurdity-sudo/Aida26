
# ============================================================
# КОНКУРС СБЕРИНДЕКС 2026
# Динамическая типология муниципальных экономик
# Свердловская область, 2023–2024 гг.
# ============================================================

# ============================================================
# 0. ПАКЕТЫ
# ============================================================

library(tidyverse)
library(arrow)
library(readxl)
library(janitor)
library(lubridate)
library(igraph)
library(mclust)
library(patchwork)
library(ggalluvial)
library(clusterCrit)

# ============================================================
# 1. СПРАВОЧНИК МУНИЦИПАЛЬНЫХ ОБРАЗОВАНИЙ
# ============================================================

dict <- read_excel(
  "raw/t_dict_municipal_districts.xlsx",
  col_types = replace(rep("guess", 18), 13, "text")
) %>%
  clean_names()

# ============================================================
# 2. МУНИЦИПАЛЬНЫЕ ОБРАЗОВАНИЯ СВЕРДЛОВСКОЙ ОБЛАСТИ, 2024
# ============================================================

dict_2024 <- dict %>%
  filter(
    region_code == 66,
    year_from <= 2024,
    year_to > 2024
  ) %>%
  select(
    territory_id,
    oktmo,
    municipal_district_name,
    municipal_district_name_short,
    municipal_district_type,
    municipal_district_center,
    municipal_district_center_lat,
    municipal_district_center_lon
  ) %>%
  distinct()

stopifnot(nrow(dict_2024) == 73)

# ============================================================
# 3. ДАННЫЕ О ПОТРЕБИТЕЛЬСКИХ РАСХОДАХ
# ============================================================

consumption <- read_parquet("raw/consumption.parquet")

# Оставляем только муниципальные образования
# Свердловской области из справочника 2024 года

consumption_so <- consumption %>%
  filter(territory_id %in% dict_2024$territory_id)

stopifnot(
  n_distinct(consumption_so$territory_id) == 69
)

# ============================================================
# 4. ПРОВЕРКА ПОЛНОТЫ ВРЕМЕННОЙ ПАНЕЛИ
# ============================================================

# Преобразуем месяц наблюдения в формат Date

consumption_so <- consumption_so %>%
  mutate(
    date = as.Date(paste0(date, "-01")),
    year = lubridate::year(date),
    month = lubridate::month(date)
  )

# Проверяем временной охват

stopifnot(
  min(consumption_so$date) == as.Date("2023-01-01"),
  max(consumption_so$date) == as.Date("2024-12-01"),
  n_distinct(consumption_so$date) == 24
)

# Число месяцев наблюдения для каждого МО

panel_completeness <- consumption_so %>%
  distinct(territory_id, date) %>%
  dplyr::count(
    territory_id,
    name = "n_months"
  ) %>%
  left_join(
    dict_2024 %>%
      select(
        territory_id,
        municipal_district_name_short
      ),
    by = "territory_id"
  ) %>%
  arrange(n_months)

# Полная сетка: 69 МО × 24 месяца

all_dates <- seq(
  as.Date("2023-01-01"),
  as.Date("2024-12-01"),
  by = "month"
)

missing_months <- expand_grid(
  territory_id = unique(consumption_so$territory_id),
  date = all_dates
) %>%
  anti_join(
    consumption_so %>%
      distinct(territory_id, date),
    by = c("territory_id", "date")
  ) %>%
  left_join(
    dict_2024 %>%
      select(
        territory_id,
        municipal_district_name_short
      ),
    by = "territory_id"
  ) %>%
  arrange(
    municipal_district_name_short,
    date
  )
# ============================================================
# 5. ХАРАКТЕР ПРОПУСКОВ В НЕПОЛНЫХ ВРЕМЕННЫХ РЯДАХ
# ============================================================

missing_summary <- missing_months %>%
  mutate(
    month_missing = format(date, "%Y-%m")
  ) %>%
  group_by(
    territory_id,
    municipal_district_name_short
  ) %>%
  summarise(
    n_missing = n(),
    missing = paste(month_missing, collapse = ", "),
    .groups = "drop"
  )

missing_summary %>%
  print(n = Inf)

# Проверяем комплектность категорий внутри каждого МО и месяца

category_check <- consumption_so %>%
  dplyr::count(territory_id, date, name = "n_categories")

table(category_check$n_categories)

# Для одной комбинации МО × месяц × категория
# должна существовать ровно одна строка

duplicate_check <- consumption_so %>%
  dplyr::count(territory_id, date, category) %>%
  filter(n > 1)

stopifnot(nrow(duplicate_check) == 0)

# ============================================================
# 6. ОСНОВНАЯ СБАЛАНСИРОВАННАЯ ПАНЕЛЬ
# ============================================================

balanced_ids <- panel_completeness %>%
  filter(n_months == 24) %>%
  pull(territory_id)

stopifnot(length(balanced_ids) == 64)

required_categories <- c(
  "Все категории",
  "Продовольствие",
  "Маркетплейсы",
  "Общественное питание",
  "Транспорт",
  "Здоровье"
)

required_category_check <- consumption_so %>%
  filter(territory_id %in% balanced_ids) %>%
  group_by(territory_id, date) %>%
  summarise(
    all_required = all(
      required_categories %in% category
    ),
    .groups = "drop"
  )

stopifnot(all(required_category_check$all_required))

panel_balanced <- consumption_so %>%
  filter(territory_id %in% balanced_ids) %>%
  select(
    territory_id,
    date,
    category,
    value
  ) %>%
  pivot_wider(
    names_from = category,
    values_from = value
  )

stopifnot(nrow(panel_balanced) == 64 * 24)

panel_balanced <- panel_balanced %>%
  mutate(
    share_food      = Продовольствие / `Все категории`,
    share_market    = Маркетплейсы / `Все категории`,
    share_catering  = `Общественное питание` / `Все категории`,
    share_transport = Транспорт / `Все категории`,
    share_health    = Здоровье / `Все категории`
  ) %>%
  left_join(
    dict_2024 %>%
      select(
        territory_id,
        municipal_district_name_short
      ),
    by = "territory_id"
  )

panel_balanced %>%
  summarise(
    food_mean      = mean(share_food),
    food_min       = min(share_food),
    food_max       = max(share_food),
    
    market_mean    = mean(share_market),
    market_min     = min(share_market),
    market_max     = max(share_market),
    
    catering_mean  = mean(share_catering),
    catering_min   = min(share_catering),
    catering_max   = max(share_catering),
    
    transport_mean = mean(share_transport),
    transport_min  = min(share_transport),
    transport_max  = max(share_transport),
    
    health_mean    = mean(share_health),
    health_min     = min(share_health),
    health_max     = max(share_health)
  )

panel_balanced %>%
  summarise(
    zero_food      = sum(share_food == 0),
    zero_market    = sum(share_market == 0),
    zero_catering  = sum(share_catering == 0),
    zero_transport = sum(share_transport == 0),
    zero_health    = sum(share_health == 0)
  )

# ============================================================
# 7. КОМПОЗИЦИОННОЕ ПРЕОБРАЗОВАНИЕ
# ============================================================

# Четыре категории, по которым будем определять
# экономическое сходство муниципалитетов

X <- panel_balanced %>%
  select(
    share_food,
    share_market,
    share_catering,
    share_transport
  )

# 1. Closure:
# нормируем четыре доли так, чтобы их сумма была равна 1

# CLR требует строго положительных компонентов
stopifnot(all(as.matrix(X) > 0))

X_closed <- X / rowSums(X)

stopifnot(
  max(abs(rowSums(X_closed) - 1)) < 1e-12
)

# 2. Логарифмируем компоненты

log_X <- log(X_closed)

# 3. Для каждого МО-месяца находим
# среднее логарифмов четырех компонентов

mean_log_X <- rowMeans(log_X)

# 4. CLR:
# из каждого логарифма вычитаем среднее
# логарифмов этой же композиции

X_clr <- log_X - mean_log_X

stopifnot(max(abs(rowSums(X_clr))) < 1e-10)

# ============================================================
# 8. ИТОГОВАЯ ПАНЕЛЬ CLR-ПРИЗНАКОВ
# ============================================================

panel_clr <- panel_balanced %>%
  select(
    territory_id,
    date,
    municipal_district_name_short
  ) %>%
  bind_cols(
    as_tibble(X_clr) %>%
      setNames(
        c(
          "clr_food",
          "clr_market",
          "clr_catering",
          "clr_transport"
        )
      )
  )

# Контроль размерности
stopifnot(nrow(panel_clr) == 64 * 24)

# CLR-координаты каждой композиции должны суммироваться к нулю
stopifnot(
  max(
    abs(
      rowSums(
        panel_clr %>%
          select(starts_with("clr_"))
      )
    )
  ) < 1e-10
)

glimpse(panel_clr)