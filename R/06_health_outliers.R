# ============================================================
# ИСХОДНЫЕ СЕТЕВЫЕ ДАННЫЕ ДЛЯ HEALTH BENCHMARK
# ============================================================

# Все месячные списки ближайших соседей

neighbors_all <- dplyr::bind_rows(
  lapply(names(network_results), function(month) {
    network_results[[month]]$neighbors %>%
      dplyr::mutate(date = as.Date(month))
  })
)

# Параметры месячных сетей

network_dynamics <- dplyr::bind_rows(
  lapply(names(network_results), function(month) {
    result <- network_results[[month]]
    
    tibble::tibble(
      date = as.Date(month),
      n_communities = result$n_communities,
      modularity = result$modularity,
      n_edges = nrow(result$edges),
      mutual_edges = sum(result$edges$mutual),
      mutual_share = mean(result$edges$mutual),
      sigma = result$sigma,
      mean_distance = mean(result$edges$distance),
      median_distance = median(result$edges$distance)
    )
  })
)

# ============================================================
# ПОМЕСЯЧНЫЙ BENCHMARK РАСХОДОВ НА ЗДОРОВЬЕ
# ПО ЭКОНОМИЧЕСКИМ АНАЛОГАМ
# ============================================================

health_panel <- panel_balanced %>%
  dplyr::select(
    territory_id,
    date,
    municipal_district_name_short,
    share_health
  )

health_benchmark_monthly <- neighbors_all %>%
  dplyr::left_join(
    health_panel %>%
      dplyr::select(
        neighbor_id = territory_id,
        date,
        neighbor_health = share_health
      ),
    by = c("neighbor_id", "date")
  ) %>%
  dplyr::group_by(territory_id, date) %>%
  dplyr::summarise(
    benchmark_health = mean(
      neighbor_health,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    health_panel,
    by = c("territory_id", "date")
  ) %>%
  dplyr::mutate(
    health_gap = share_health - benchmark_health,
    health_gap_pct = 100 * health_gap / benchmark_health
  )

health_benchmark_monthly %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_months = dplyr::n_distinct(date),
    n_municipalities = dplyr::n_distinct(territory_id),
    missing_benchmark = sum(is.na(benchmark_health)),
    missing_gap = sum(is.na(health_gap)),
    min_gap = min(health_gap, na.rm = TRUE),
    max_gap = max(health_gap, na.rm = TRUE)
  )

# ============================================================
# ROBUSTNESS CHECK:
# DISTANCE-WEIGHTED HEALTH BENCHMARK
# ============================================================

sigma_monthly <- network_dynamics %>%
  dplyr::select(date, sigma)

health_benchmark_weighted <- neighbors_all %>%
  dplyr::left_join(
    sigma_monthly,
    by = "date"
  ) %>%
  dplyr::mutate(
    weight = exp(-distance / sigma)
  ) %>%
  dplyr::left_join(
    health_panel %>%
      dplyr::select(
        neighbor_id = territory_id,
        date,
        neighbor_health = share_health
      ),
    by = c("neighbor_id", "date")
  ) %>%
  dplyr::group_by(territory_id, date) %>%
  dplyr::summarise(
    benchmark_health_w = weighted.mean(
      neighbor_health,
      w = weight,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    health_panel,
    by = c("territory_id", "date")
  ) %>%
  dplyr::mutate(
    health_gap_w = share_health - benchmark_health_w,
    health_gap_pct_w =
      100 * health_gap_w / benchmark_health_w
  )

health_benchmark_weighted %>%
  dplyr::summarise(
    n_rows = dplyr::n(),
    n_months = dplyr::n_distinct(date),
    missing_benchmark = sum(is.na(benchmark_health_w)),
    missing_gap = sum(is.na(health_gap_w)),
    min_gap = min(health_gap_w, na.rm = TRUE),
    max_gap = max(health_gap_w, na.rm = TRUE)
  )

# ============================================================
# СРАВНЕНИЕ ОБЫЧНОГО И ВЗВЕШЕННОГО BENCHMARK
# ============================================================

benchmark_comparison <- health_benchmark_monthly %>%
  dplyr::select(
    territory_id,
    date,
    health_gap
  ) %>%
  dplyr::left_join(
    health_benchmark_weighted %>%
      dplyr::select(
        territory_id,
        date,
        health_gap_w
      ),
    by = c("territory_id", "date")
  )

benchmark_robustness_summary <- benchmark_comparison %>%
  dplyr::summarise(
    n_observations = dplyr::n(),
    
    spearman = cor(
      health_gap,
      health_gap_w,
      method = "spearman"
    ),
    
    pearson = cor(
      health_gap,
      health_gap_w,
      method = "pearson"
    ),
    
    same_sign_pct = 100 * mean(
      sign(health_gap) == sign(health_gap_w)
    )
  )

print(benchmark_robustness_summary)

# ============================================================
# УСТОЙЧИВОСТЬ HEALTH GAP НА УРОВНЕ МУНИЦИПАЛИТЕТОВ
# ============================================================

health_gap_by_mo <- health_benchmark_monthly %>%
  dplyr::group_by(
    territory_id,
    municipal_district_name_short
  ) %>%
  dplyr::summarise(
    n_months = dplyr::n(),
    mean_health = mean(share_health),
    mean_benchmark = mean(benchmark_health),
    mean_gap = mean(health_gap),
    median_gap = median(health_gap),
    sd_gap = sd(health_gap),
    mean_gap_pct = mean(health_gap_pct),
    median_gap_pct = median(health_gap_pct),
    positive_months = sum(health_gap > 0),
    negative_months = sum(health_gap < 0),
    positive_share = mean(health_gap > 0),
    min_gap = min(health_gap),
    max_gap = max(health_gap),
    .groups = "drop"
  )

health_gap_weighted_by_mo <- health_benchmark_weighted %>%
  dplyr::group_by(
    territory_id,
    municipal_district_name_short
  ) %>%
  dplyr::summarise(
    mean_gap_w = mean(health_gap_w),
    median_gap_w = median(health_gap_w),
    sd_gap_w = sd(health_gap_w),
    mean_gap_pct_w = mean(health_gap_pct_w),
    positive_months_w = sum(health_gap_w > 0),
    positive_share_w = mean(health_gap_w > 0),
    .groups = "drop"
  )

gap_robustness <- health_gap_by_mo %>%
  dplyr::left_join(
    health_gap_weighted_by_mo %>%
      dplyr::select(
        territory_id,
        mean_gap_w,
        mean_gap_pct_w,
        positive_months_w,
        positive_share_w
      ),
    by = "territory_id"
  ) %>%
  dplyr::mutate(
    same_sign = sign(mean_gap) == sign(mean_gap_w)
  )

gap_robustness %>%
  dplyr::summarise(
    n_municipalities = dplyr::n(),
    spearman = cor(mean_gap, mean_gap_w, method = "spearman"),
    same_sign = sum(same_sign),
    changed_sign = sum(!same_sign)
  )

gap_robustness %>%
  dplyr::count(same_sign, .drop = FALSE)

gap_robustness %>%
  dplyr::filter(is.na(same_sign) | !same_sign) %>%
  dplyr::select(
    municipal_district_name_short,
    mean_gap,
    mean_gap_w,
    same_sign
  )

# ============================================================
# ГОДОВЫЕ ПОКАЗАТЕЛИ ЗДОРОВЬЯ ПО УСТОЙЧИВЫМ ГРУППАМ
# ============================================================

health_groups <- health_panel %>%
  dplyr::mutate(
    year = as.integer(format(date, "%Y"))
  ) %>%
  dplyr::inner_join(
    economic_groups,
    by = "territory_id"
  ) %>%
  dplyr::group_by(
    territory_id,
    municipal_district_name_short,
    group,
    year
  ) %>%
  dplyr::summarise(
    health_mean = mean(share_health, na.rm = TRUE),
    health_sd = sd(share_health, na.rm = TRUE),
    n_months = dplyr::n(),
    .groups = "drop"
  )

health_group_summary <- health_groups %>%
  dplyr::group_by(group, year) %>%
  dplyr::summarise(
    n_municipalities = dplyr::n_distinct(territory_id),
    min_months = min(n_months),
    health_median_pct = 100 * median(health_mean),
    health_mean_pct = 100 * mean(health_mean),
    health_min_pct = 100 * min(health_mean),
    health_max_pct = 100 * max(health_mean),
    .groups = "drop"
  )

print(health_group_summary, n = Inf, width = Inf)


# ============================================================
# ОТКЛОНЕНИЯ ОТ МЕДИАНЫ ОСТАЛЬНЫХ МО ГРУППЫ
# ============================================================

health_group_gaps <- health_groups %>%
  dplyr::group_by(group, year) %>%
  dplyr::mutate(
    benchmark_loo = vapply(
      seq_along(health_mean),
      function(i) median(health_mean[-i], na.rm = TRUE),
      numeric(1)
    ),
    gap_pp = 100 * (health_mean - benchmark_loo)
  ) %>%
  dplyr::ungroup()

# Сопоставление отклонений за два года

health_gap_stability <- health_group_gaps %>%
  dplyr::select(
    territory_id,
    municipal_district_name_short,
    group,
    year,
    gap_pp
  ) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = gap_pp,
    names_prefix = "gap_"
  ) %>%
  dplyr::mutate(
    mean_gap_pp = (gap_2023 + gap_2024) / 2,
    same_direction = gap_2023 * gap_2024 > 0
  ) %>%
  dplyr::arrange(desc(abs(mean_gap_pp)))

health_gap_stability %>%
  dplyr::summarise(
    n_municipalities = dplyr::n(),
    same_direction = sum(same_direction),
    changed_direction = sum(!same_direction),
    max_abs_gap_pp = max(abs(mean_gap_pp))
  )

health_gap_stability %>%
  dplyr::count(same_direction, .drop = FALSE)

health_gap_stability %>%
  dplyr::filter(is.na(same_direction) | !same_direction) %>%
  dplyr::select(
    municipal_district_name_short,
    group,
    gap_2023,
    gap_2024,
    same_direction
  )

# ============================================================
# ПОМЕСЯЧНЫЕ ОТКЛОНЕНИЯ ВНУТРИ УСТОЙЧИВЫХ ГРУПП
# ============================================================

health_monthly_gaps <- health_panel %>%
  dplyr::inner_join(
    economic_groups,
    by = "territory_id"
  ) %>%
  dplyr::group_by(group, date) %>%
  dplyr::mutate(
    benchmark_loo = vapply(
      seq_along(share_health),
      function(i) median(share_health[-i], na.rm = TRUE),
      numeric(1)
    ),
    gap_pp = 100 * (share_health - benchmark_loo)
  ) %>%
  dplyr::ungroup()

# ============================================================
# УСТОЙЧИВОСТЬ НАПРАВЛЕНИЯ ОТКЛОНЕНИЙ
# ============================================================

health_monthly_stability <- health_monthly_gaps %>%
  dplyr::group_by(
    territory_id,
    municipal_district_name_short,
    group
  ) %>%
  dplyr::summarise(
    n_months = dplyr::n(),
    mean_gap_pp = mean(gap_pp, na.rm = TRUE),
    median_gap_pp = median(gap_pp, na.rm = TRUE),
    positive_months = sum(gap_pp > 0, na.rm = TRUE),
    negative_months = sum(gap_pp < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    direction_stability = pmax(
      positive_months,
      negative_months
    ) / n_months
  ) %>%
  dplyr::arrange(desc(abs(mean_gap_pp)))

health_monthly_stability %>%
  dplyr::summarise(
    n_municipalities = dplyr::n(),
    min_months = min(n_months),
    max_months = max(n_months),
    min_stability = min(direction_stability),
    max_stability = max(direction_stability)
  )

# ============================================================
# ФИНАЛЬНАЯ ДИАГНОСТИКА УСТОЙЧИВЫХ ОТКЛОНЕНИЙ HEALTH
# ============================================================

health_diagnostic_final <- health_monthly_stability %>%
  dplyr::filter(
    abs(mean_gap_pp) >= config$health$min_gap_pp,
    direction_stability >= config$health$min_months / 24
  ) %>%
  dplyr::mutate(
    direction = dplyr::if_else(
      mean_gap_pp > 0,
      "Выше группового ориентира",
      "Ниже группового ориентира"
    )
  ) %>%
  dplyr::arrange(desc(mean_gap_pp))

# Итоговая сводка

health_diagnostic_summary <- health_diagnostic_final %>%
  dplyr::summarise(
    n_municipalities = dplyr::n(),
    above = sum(mean_gap_pp > 0),
    below = sum(mean_gap_pp < 0),
    min_direction_stability = min(direction_stability),
    max_direction_stability = max(direction_stability)
  )

print(health_diagnostic_summary)

# Список муниципалитетов

health_diagnostic_final %>%
  dplyr::select(
    municipal_district_name_short,
    group,
    mean_gap_pp,
    positive_months,
    negative_months,
    direction_stability,
    direction
  ) %>%
  print(n = Inf, width = Inf)

# ============================================================
# КОНТРОЛЬ ВОСПРОИЗВОДИМОСТИ ФИНАЛЬНОЙ ДИАГНОСТИКИ
# ============================================================

stopifnot(
  nrow(health_diagnostic_final) == 12,
  sum(health_diagnostic_final$mean_gap_pp > 0) == 8,
  sum(health_diagnostic_final$mean_gap_pp < 0) == 4,
  all(health_diagnostic_final$n_months == 24),
  all(
    health_diagnostic_final$direction_stability >=
      config$health$min_months / 24
  ),
  all(
    abs(health_diagnostic_final$mean_gap_pp) >=
      config$health$min_gap_pp
  )
)

stopifnot(
  all(
    health_monthly_stability$positive_months +
      health_monthly_stability$negative_months ==
      health_monthly_stability$n_months
  )
)

print(health_diagnostic_summary, width = Inf)

health_diagnostic_final %>%
  dplyr::filter(
    (mean_gap_pp > 0 & positive_months < negative_months) |
      (mean_gap_pp < 0 & negative_months < positive_months)
  ) %>%
  dplyr::select(
    municipal_district_name_short,
    mean_gap_pp,
    positive_months,
    negative_months
  ) %>%
  print(n = Inf)

health_diagnostic_final %>%
  dplyr::filter(
    (mean_gap_pp > 0 & positive_months < negative_months) |
      (mean_gap_pp < 0 & negative_months < positive_months)
  ) %>%
  dplyr::select(
    municipal_district_name_short,
    mean_gap_pp,
    positive_months,
    negative_months
  ) %>%
  print(n = Inf)

