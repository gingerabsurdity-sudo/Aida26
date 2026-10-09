# 05_external_interpretation.R
# Source objects: economic_groups, dict_2024, communities_all.

stopifnot(exists("economic_groups"), exists("dict_2024"),
          exists("communities_all"))
stopifnot(nrow(economic_groups) == 26L,
          !anyDuplicated(economic_groups$territory_id))
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(janitor)
library(readr)

years <- c(2023L, 2024L)
sector_vars <- c("Agriculture", "Industry", "Construction",
                 "Consumer_services", "Transport_IT",
                 "Business_services", "Public_social")
oktmo8 <- function(x) as.numeric(substr(gsub("[^0-9]", "", as.character(x)), 1, 8))

# 1. Crosswalk: exactly one 2024 OKTMO per selected territory.
economic_crosswalk <- economic_groups %>%
  left_join(dict_2024 %>%
              transmute(territory_id, oktmo8 = oktmo8(oktmo)),
            by = "territory_id")
stopifnot(nrow(economic_crosswalk) == 26L,
          !anyNA(economic_crosswalk$oktmo8),
          !anyDuplicated(economic_crosswalk$oktmo8))
print(economic_crosswalk %>% dplyr::count(group), n = Inf)

# 2. Population and salaries.
# Population: original annual Excel source (2023–2024).

population_file <- "raw/population.xlsx"
stopifnot(file.exists(population_file))

population_raw <- readxl::read_excel(population_file) %>%
  janitor::clean_names()

stopifnot(
  all(c("oktmo", "god", "naselenie") %in% names(population_raw))
)

population <- population_raw %>%
  dplyr::transmute(
    oktmo = oktmo,
    year = as.integer(god),
    population = as.numeric(naselenie)
  )

stopifnot(
  all(c("oktmo", "year", "population") %in% names(population))
)
population_ext <- population %>%
  transmute(oktmo8 = oktmo8(oktmo), year = as.integer(year),
            population = as.numeric(population)) %>%
  filter(year %in% years)
stopifnot(!anyDuplicated(population_ext[c("oktmo8", "year")]))

# Salaries: original annual Excel source (2023–2024).

salary_file <- "raw/salary_panel_2023_2024_sverdlovsk.xlsx"
stopifnot(file.exists(salary_file))

salary_raw <- readxl::read_excel(salary_file) %>%
  janitor::clean_names()

stopifnot(
  all(c("oktmo", "god", "srednaa_zarplata") %in% names(salary_raw))
)

salary_ext <- salary_raw %>%
  dplyr::transmute(
    oktmo8 = oktmo8(oktmo),
    year = as.integer(god),
    salary = as.numeric(srednaa_zarplata)
  )
salary_ext <- salary_ext %>% filter(year %in% years)
stopifnot(!anyDuplicated(salary_ext[c("oktmo8", "year")]))

economic_data <- economic_crosswalk %>%
  tidyr::crossing(year = years) %>%
  left_join(population_ext, by = c("oktmo8", "year")) %>%
  left_join(salary_ext, by = c("oktmo8", "year"))
stopifnot(nrow(economic_data) == 52L)
economic_coverage <- economic_data %>%
  group_by(group, year) %>%
  summarise(n = n(), n_population = sum(!is.na(population)),
            n_salary = sum(!is.na(salary)), .groups = "drop")
print(economic_coverage, n = Inf)

safe_median <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
economic_summary <- economic_data %>%
  group_by(group, year) %>%
  summarise(n = n(), n_population = sum(!is.na(population)),
            n_salary = sum(!is.na(salary)),
            population_median = safe_median(population),
            population_min = safe_min(population),
            population_max = safe_max(population),
            salary_median = safe_median(salary),
            salary_min = safe_min(salary),
            salary_max = safe_max(salary), .groups = "drop")
print(economic_summary, n = Inf, width = Inf)

# 3. Employment: 7 blocks, using annual January–December observations.
employment_raw <- readxl::read_excel(
  "raw/Отраслевая структура занятости.xlsx"
) %>%
  janitor::clean_names()
stopifnot(all(c("year", "indicator_period", "okved2", "oktmo",
                "municipality", "indicator_value") %in% names(employment_raw)))
employment_year <- employment_raw %>%
  filter(indicator_period == "Январь-декабрь", year %in% years) %>%
  mutate(year = as.integer(year), oktmo8 = oktmo8(oktmo),
         employment = readr::parse_number(
           as.character(indicator_value),
           locale = readr::locale(decimal_mark = ".")),
         is_total = okved2 == "Всего по обследуемым видам экономической деятельности")
employment_check <- employment_year %>%
  group_by(year, oktmo8) %>%
  summarise(total_employment = if (any(is_total))
              employment[which(is_total)[1]] else NA_real_,
            published_sector_sum = sum(employment[!is_total], na.rm = TRUE),
            n_published_sectors = sum(!is_total & !is.na(employment)),
            .groups = "drop") %>%
  mutate(coverage = published_sector_sum / total_employment)

# Original classification retained. Note Cyrillic/Latin letter variants.
employment_sectors <- employment_year %>%
  filter(!is_total) %>%
  mutate(sector = case_when(
    str_detect(okved2, "^Раздел [АA]") ~ "Agriculture",
    str_detect(okved2, "^Раздел [ВBСCDEЕ]") ~ "Industry",
    str_detect(okved2, "^Раздел F") ~ "Construction",
    str_detect(okved2, "^Раздел [GIS]") ~ "Consumer_services",
    str_detect(okved2, "^Раздел [НHJ]") ~ "Transport_IT",
    str_detect(okved2, "^Раздел [KLMN]") ~ "Business_services",
    str_detect(okved2, "^Раздел [OPQR]") ~ "Public_social",
    TRUE ~ NA_character_))
unclassified_sectors <- employment_sectors %>%
  filter(is.na(sector)) %>% distinct(okved2)
if (nrow(unclassified_sectors)) {
  print(unclassified_sectors, n = Inf)
  stop("Unclassified OKVED rows: check the source's alphabet and sector codes.")
}
employment_blocks <- employment_sectors %>%
  group_by(year, oktmo8, sector) %>%
  summarise(employment_sector = sum(employment, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(employment_check %>%
              select(year, oktmo8, total_employment, coverage),
            by = c("year", "oktmo8")) %>%
  mutate(share_sector = employment_sector / total_employment)
employment_structure <- employment_blocks %>%
  select(year, oktmo8, sector, share_sector, coverage) %>%
  pivot_wider(names_from = sector, values_from = share_sector,
              values_fill = 0)
for (s in sector_vars) if (!s %in% names(employment_structure))
  employment_structure[[s]] <- 0
employment_structure_75 <- employment_structure %>%
  filter(!is.na(coverage), coverage >= 0.75)
employment_structure_90 <- employment_structure %>%
  filter(!is.na(coverage), coverage >= 0.90)
employment_balanced_75 <- employment_structure_75 %>%
  group_by(oktmo8) %>% filter(n_distinct(year) == 2L) %>% ungroup()
employment_balanced_90 <- employment_structure_90 %>%
  group_by(oktmo8) %>% filter(n_distinct(year) == 2L) %>% ungroup()

employment_groups <- economic_crosswalk %>%
  select(territory_id, group, oktmo8) %>%
  inner_join(employment_balanced_75, by = "oktmo8")
employment_group_coverage <- employment_groups %>%
  group_by(group, year) %>%
  summarise(n_municipalities = n_distinct(territory_id),
            median_coverage = median(coverage), .groups = "drop")
sector_comparison <- employment_groups %>%
  group_by(group, year) %>%
  summarise(across(all_of(sector_vars),
                   ~ median(.x, na.rm = TRUE) * 100), .groups = "drop")
print(employment_group_coverage, n = Inf)
print(sector_comparison, n = Inf, width = Inf)

# 4. Independent external check of MONTHLY Louvain communities.
# Manhattan distance between seven published-sector shares.
# This is distinct from comparing the three stable economic groups.
employment_validation_run <- function(employment_balanced, B = 999L, seed = 123L) {
  employment_linked <- employment_balanced %>%
    inner_join(dict_2024 %>%
                 transmute(territory_id, oktmo8 = oktmo8(oktmo)),
               by = "oktmo8")
  stopifnot(!anyDuplicated(employment_linked[c("territory_id", "year")]))
  communities_employment <- communities_all %>%
    mutate(year = lubridate::year(date)) %>%
    inner_join(employment_linked %>%
                 select(territory_id, year, all_of(sector_vars)),
               by = c("territory_id", "year"))
  set.seed(seed)
  monthly <- communities_employment %>%
    group_by(date) %>% group_split() %>%
    map_dfr(function(x) {
      D <- as.matrix(dist(as.matrix(x[, sector_vars]), method = "manhattan"))
      ix <- which(upper.tri(D), arr.ind = TRUE)
      distances <- D[ix]
      labels <- x$community
      same <- labels[ix[, 1]] == labels[ix[, 2]]
      if (!any(same) || all(same))
        stop("Cannot calculate within/between distance for ", unique(x$date))
      within <- mean(distances[same])
      between <- mean(distances[!same])
      gap <- between - within
      perm_gaps <- replicate(B, {
        z <- sample(labels)
        ss <- z[ix[, 1]] == z[ix[, 2]]
        mean(distances[!ss]) - mean(distances[ss])
      })
      tibble(date = unique(x$date), n_municipalities = nrow(x),
             within = within, between = between, ratio = within / between,
             improvement_pct = 100 * (1 - within / between),
             observed_gap = gap, perm_mean = mean(perm_gaps),
             perm_sd = sd(perm_gaps),
             p_value = (sum(perm_gaps >= gap) + 1) / (B + 1))
    })
  list(linked = employment_linked, communities = communities_employment,
       monthly = monthly)
}
employment_validation_75 <- employment_validation_run(employment_balanced_75)
employment_validation_90 <- employment_validation_run(employment_balanced_90)
employment_validation_ratio <- employment_validation_75$monthly
permutation_results <- employment_validation_75$monthly %>%
  select(date, observed_gap, perm_mean, perm_sd, p_value)
employment_ratio_90 <- employment_validation_90$monthly
permutation_results_90 <- employment_validation_90$monthly %>%
  select(date, observed_gap, perm_mean, perm_sd, p_value)
employment_validation_summary <- bind_rows(
  employment_validation_ratio %>% mutate(threshold = "75%"),
  employment_ratio_90 %>% mutate(threshold = "90%")) %>%
  group_by(threshold) %>%
  summarise(n_months = n(), mean_ratio = mean(ratio),
            mean_improvement_pct = mean(improvement_pct),
            months_within_closer = sum(ratio < 1),
            months_p_below_005 = sum(p_value < 0.05),
            .groups = "drop")
print(employment_validation_summary, n = Inf)

employment_validation_75$monthly %>%
  dplyr::summarise(
    min_n = min(n_municipalities),
    max_n = max(n_municipalities),
    min_p = min(p_value),
    max_p = max(p_value)
  )

employment_validation_90$monthly %>%
  dplyr::summarise(
    min_n = min(n_municipalities),
    max_n = max(n_municipalities),
    min_p = min(p_value),
    max_p = max(p_value)
  )
