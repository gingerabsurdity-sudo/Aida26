# ============================================================
# 07_visualization.R
# ШАГ 1. Проверка исходных данных
# ============================================================

objects_map <- c(
  "dict_2024",
  "panel_clr",
  "economic_groups",
  "health_diagnostic_final"
)

data.frame(
  object = objects_map,
  exists = vapply(
    objects_map,
    exists,
    logical(1),
    envir = .GlobalEnv,
    inherits = FALSE
  )
)

# Проверяем географический файл
file.exists("raw/t_dict_municipal_districts_poly.gpkg")

# Проверяем доступные слои
if (file.exists("raw/t_dict_municipal_districts_poly.gpkg")) {
  sf::st_layers("raw/t_dict_municipal_districts_poly.gpkg")
}

# ============================================================
# 07_visualization.R
# ШАГ 2. Геометрия муниципалитетов на 2024 год
# ============================================================

library(sf)
library(dplyr)

# Путь к геометрии
path_gpkg <- "raw/t_dict_municipal_districts_poly.gpkg"

poly_raw <- sf::st_read(
  path_gpkg,
  layer = "t_dict_municipal_districts_poly",
  quiet = TRUE
) %>%
  janitor::clean_names()

# Проверяем структуру исходного слоя
names(poly_raw)

# Отбираем геометрию, действовавшую в 2024 году
poly_2024 <- poly_raw %>%
  dplyr::filter(
    year_from <= 2024,
    year_to > 2024
  ) %>%
  dplyr::mutate(
    territory_id = as.numeric(territory_id)
  ) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# Контроль
poly_2024 %>%
  sf::st_drop_geometry() %>%
  dplyr::summarise(
    n_polygons = dplyr::n(),
    n_territory_ids = dplyr::n_distinct(territory_id),
    missing_ids = sum(is.na(territory_id))
  )

# Проверяем, есть ли несколько геометрий у одного territory_id
poly_2024 %>%
  sf::st_drop_geometry() %>%
  dplyr::count(territory_id, name = "n") %>%
  dplyr::filter(n > 1) %>%
  dplyr::arrange(dplyr::desc(n))

# ============================================================
# 07_visualization.R
# ШАГ 3. Соответствие геометрии и аналитических данных
# ============================================================

panel_ids <- unique(panel_clr$territory_id)
group_ids <- unique(economic_groups$territory_id)
health_ids <- unique(health_diagnostic_final$territory_id)

geometry_ids <- unique(poly_2024$territory_id)

# Сколько идентификаторов непосредственно совпадает?
tibble::tibble(
  dataset = c(
    "Сбалансированная панель",
    "Три устойчивые группы",
    "Отклонения здоровья"
  ),
  n_municipalities = c(
    length(panel_ids),
    length(group_ids),
    length(health_ids)
  ),
  matched_geometry = c(
    sum(panel_ids %in% geometry_ids),
    sum(group_ids %in% geometry_ids),
    sum(health_ids %in% geometry_ids)
  )
)

# Смотрим формат идентификаторов
head(poly_2024$territory_id, 10)
head(panel_clr$territory_id, 10)

# Проверяем типы
class(poly_2024$territory_id)
class(panel_clr$territory_id)

# ============================================================
# 07_visualization.R
# ШАГ 4. Геометрия муниципалитетов панели
# ============================================================

poly_panel <- poly_2024 %>%
  dplyr::filter(
    territory_id %in% panel_clr$territory_id
  ) %>%
  dplyr::left_join(
    panel_clr %>%
      sf::st_drop_geometry() %>%
      dplyr::distinct(
        territory_id,
        municipal_district_name_short
      ),
    by = "territory_id"
  )

# Проверяем результат
poly_panel %>%
  sf::st_drop_geometry() %>%
  dplyr::summarise(
    n_polygons = dplyr::n(),
    n_municipalities = dplyr::n_distinct(territory_id),
    missing_names = sum(is.na(municipal_district_name_short)),
    invalid_geometry = sum(!sf::st_is_valid(poly_panel))
  )

# Проверяем пространственный охват
plot(sf::st_geometry(poly_panel),
     col = "grey85",
     border = "white",
     main = "Муниципалитеты сбалансированной панели")

# ============================================================
# 07_visualization.R
# ШАГ 5. Данные для итоговой карты
# ============================================================

map_final <- poly_panel %>%
  dplyr::left_join(
    economic_groups %>%
      dplyr::select(territory_id, group),
    by = "territory_id"
  ) %>%
  dplyr::left_join(
    health_diagnostic_final %>%
      dplyr::select(
        territory_id,
        mean_gap_pp,
        direction_stability,
        direction
      ),
    by = "territory_id"
  ) %>%
  dplyr::mutate(
    health_status = dplyr::case_when(
      is.na(group) ~ "Вне устойчивых групп",
      direction == "Выше группового ориентира" ~ "Выше",
      direction == "Ниже группового ориентира" ~ "Ниже",
      TRUE ~ "Без выраженного отклонения"
    )
  )

# Контроль состава карты
map_final %>%
  sf::st_drop_geometry() %>%
  dplyr::summarise(
    n_municipalities = dplyr::n(),
    n_groups = sum(!is.na(group)),
    health_high = sum(health_status == "Выше"),
    health_low = sum(health_status == "Ниже"),
    health_other = sum(
      health_status == "Без выраженного отклонения"
    )
  )

# Проверка численности групп
map_final %>%
  sf::st_drop_geometry() %>%
  dplyr::count(group, name = "n")

# ============================================================
# 07_visualization.R
# ШАГ 6. Карта трёх устойчивых групп
# ============================================================

library(ggplot2)

map_base_data <- map_final %>%
  dplyr::mutate(
    map_group = dplyr::case_when(
      group == "Городское ядро" ~ "Городское ядро",
      group == "Периферийный блок 1" ~ "Периферийный блок 1",
      group == "Периферийный блок 2" ~ "Периферийный блок 2",
      TRUE ~ "Другие МО панели"
    ),
    map_group = factor(
      map_group,
      levels = c(
        "Городское ядро",
        "Периферийный блок 1",
        "Периферийный блок 2",
        "Другие МО панели"
      )
    )
  )

group_palette <- c(
  "Городское ядро" = "#4C78A8",
  "Периферийный блок 1" = "#F2A541",
  "Периферийный блок 2" = "#59A14F",
  "Другие МО панели" = "#E8E8E8"
)

p_groups <- ggplot(map_base_data) +
  geom_sf(
    aes(fill = map_group),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values = group_palette,
    drop = FALSE,
    name = NULL
  ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  labs(
    title = "Три устойчивых сообщества муниципалитетов\nпо структуре потребительских расходов",
    subtitle = "Январь 2023 — декабрь 2024"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.width = grid::unit(16, "pt"),
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 10,
      color = "grey40",
      hjust = 0.5
    ),
    plot.margin = margin(12, 18, 10, 12)
  )

p_groups

# ============================================================
# 07_visualization.R
# ШАГ 7. Устойчивые отклонения расходов на здоровье
# ============================================================

health_points <- map_final %>%
  dplyr::filter(
    health_status %in% c("Выше", "Ниже")
  ) %>%
  sf::st_transform(3857) %>%
  sf::st_point_on_surface() %>%
  sf::st_transform(4326) %>%
  dplyr::mutate(
    health_symbol = dplyr::if_else(
      health_status == "Выше",
      "+",
      "\u2212"
    )
  )

# Проверяем точки
health_points %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    municipal_district_name_short,
    group,
    mean_gap_pp,
    direction_stability,
    health_status
  ) %>%
  dplyr::arrange(
    dplyr::desc(abs(mean_gap_pp))
  )

p_groups_health <- p_groups +
  
  geom_sf(
    data = health_points,
    shape = 21,
    size = 3.5,
    fill = "white",
    color = "#333333",
    stroke = 0.6
  ) +
  
  geom_sf_text(
    data = health_points,
    aes(label = health_symbol),
    size = 2.8,
    fontface = "bold",
    color = "#222222"
  ) +
  
  labs(
    caption = paste0(
      "+ / − — доля расходов на здоровье ",
      "устойчиво выше / ниже группового ориентира"
    )
  ) +
  
  theme(
    plot.caption = element_text(
      size = 9,
      color = "grey35",
      hjust = 0
    ),
    plot.caption.position = "plot"
  )

p_groups_health


# ============================================================
# 07_visualization.R
# ШАГ 8. Проверка согласованных сообществ
# ============================================================

# Структура готового объекта
dplyr::glimpse(aligned)

# Проверяем полноту данных
aligned %>%
  dplyr::summarise(
    rows = dplyr::n(),
    municipalities = dplyr::n_distinct(territory_id),
    months = dplyr::n_distinct(date),
    missing_labels = sum(is.na(aligned_community)),
    n_aligned_labels = dplyr::n_distinct(aligned_community)
  )

# Сколько сообществ в каждом месяце?
aligned %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(
    n_communities = dplyr::n_distinct(aligned_community),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# ============================================================
# 07_visualization.R
# ШАГ 9. Названия и порядок муниципалитетов
# ============================================================

# Иерархическая группировка по попарной устойчивости
hc_stability <- hclust(
  as.dist(1 - S),
  method = "average"
)

municipality_order <- as.numeric(
  rownames(S)[hc_stability$order]
)

# Присоединяем названия к согласованным сообществам

aligned_named <- aligned

# Проверяем названия и согласованные метки
aligned_named %>%
  dplyr::summarise(
    rows = dplyr::n(),
    municipalities = dplyr::n_distinct(territory_id),
    months = dplyr::n_distinct(date),
    missing_names = sum(is.na(municipal_district_name_short)),
    missing_labels = sum(is.na(aligned_community))
  )

# Контроль
tibble::tibble(
  indicator = c(
    "Муниципалитетов в порядке матрицы",
    "Строк aligned_named",
    "Пропущенных названий",
    "Пропущенных согласованных меток"
  ),
  value = c(
    length(municipality_order),
    nrow(aligned_named),
    sum(is.na(aligned_named$municipal_district_name_short)),
    sum(is.na(aligned_named$aligned_community))
  )
)

# Проверяем, совпадают ли множества ID
setequal(
  municipality_order,
  unique(aligned_named$territory_id)
)

group_positions <- tibble::tibble(
  territory_id = municipality_order,
  position = seq_along(municipality_order)
) %>%
  dplyr::inner_join(
    economic_groups %>%
      dplyr::select(territory_id, group),
    by = "territory_id"
  )

group_positions %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    n = dplyr::n(),
    first_position = min(position),
    last_position = max(position),
    contiguous = max(position) - min(position) + 1 == dplyr::n(),
    .groups = "drop"
  )

# ============================================================
# 07_visualization.R
# ШАГ 10. Матрица попарной устойчивости
# ============================================================

# Идентификаторы в порядке иерархической кластеризации
ordered_ids <- as.character(municipality_order)

# Названия в том же порядке
ordered_names <- municipality_names %>%
  dplyr::slice(
    match(municipality_order, territory_id)
  ) %>%
  dplyr::pull(municipal_district_name_short)

# Уточняем одинаковые сокращённые названия
ordered_names[municipality_order == 1987] <- "Камышловский ГО"
ordered_names[municipality_order == 2040] <- "Камышловский МР"

# Преобразуем матрицу S в таблицу для ggplot
matrix_plot <- as.data.frame(
  as.table(S),
  stringsAsFactors = FALSE
) %>%
  dplyr::rename(
    id_1 = Var1,
    id_2 = Var2,
    stability = Freq
  ) %>%
  dplyr::mutate(
    id_1 = factor(id_1, levels = ordered_ids),
    id_2 = factor(id_2, levels = rev(ordered_ids))
  )

# Базовая матрица
p_stability_clean <- ggplot(
  matrix_plot,
  aes(x = id_1, y = id_2, fill = stability)
) +
  geom_tile() +
  scale_fill_gradient(
    low = "#FFF7BC",
    high = "#006D2C",
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.50, 0.75, 1),
    name = "Доля месяцев\nсовместной принадлежности"
  ) +
  scale_x_discrete(
    labels = NULL,
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    labels = rev(ordered_names),
    expand = c(0, 0)
  ) +
  coord_equal() +
  labs(
    title = "Устойчивость совместной принадлежности муниципалитетов к сетевым сообществам",
    subtitle = "64 муниципалитета, январь 2023 — декабрь 2024",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(
      size = 6.2,
      color = "grey20"
    ),
    axis.ticks.y = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

p_stability_clean

# ============================================================
# 07_visualization.R
# Рамки трёх выбранных устойчивых сообществ
# ============================================================

group_rectangles <- group_positions %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    first = min(position),
    last = max(position),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    xmin = first - 0.5,
    xmax = last + 0.5,
    ymin = 64 - last + 0.5,
    ymax = 64 - first + 1.5
  )

p_stability_final <- p_stability_clean +
  
  # Белая подложка рамок
  geom_rect(
    data = group_rectangles,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    inherit.aes = FALSE,
    fill = NA,
    color = "white",
    linewidth = 2.4
  ) +
  
  # Цветные рамки сообществ
  geom_rect(
    data = group_rectangles,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      color = group
    ),
    inherit.aes = FALSE,
    fill = NA,
    linewidth = 1.1
  ) +
  
  scale_color_manual(
    values = group_palette,
    name = "Выбранное устойчивое сообщество"
  ) +
  
  theme(
    legend.position = "right"
  )

p_stability_final

# ============================================================
# 07_visualization.R
# ШАГ 11. Данные динамической матрицы
# ============================================================

dynamics_matrix_aligned <- aligned_named %>%
  dplyr::mutate(
    territory_id = factor(
      territory_id,
      levels = rev(municipality_order)
    )
  )

# Соответствие ID и названий
# Названия в порядке, согласованном с матрицей устойчивости
name_labels <- stats::setNames(
  ordered_names,
  as.character(municipality_order)
)

# Контроль
dynamics_matrix_aligned %>%
  dplyr::summarise(
    rows = dplyr::n(),
    municipalities = dplyr::n_distinct(territory_id),
    months = dplyr::n_distinct(date),
    missing_ids = sum(is.na(territory_id)),
    missing_labels = sum(is.na(aligned_community))
  )

# ============================================================
# 07_visualization.R
# ШАГ 12. Динамическая матрица сетевых сообществ
# ============================================================

p_dynamics_aligned <- ggplot(
  dynamics_matrix_aligned,
  aes(
    x = date,
    y = territory_id,
    fill = factor(aligned_community)
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_brewer(
    palette = "Set2",
    name = "Согласованное\nсообщество"
  ) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%m.%y",
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    labels = name_labels,
    expand = c(0, 0)
  ) +
  labs(
    title = "Динамика принадлежности муниципалитетов к сетевым сообществам",
    subtitle = "64 муниципалитета, январь 2023 — декабрь 2024",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    
    axis.text.y = element_text(
      size = 6.2,
      color = "grey20"
    ),
    axis.ticks.y = element_blank(),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    ),
    
    legend.position = "bottom",
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

p_dynamics_aligned

# ============================================================
# 07_visualization.R
# ШАГ 13. Правая полоса устойчивых групп
# ============================================================


group_strip <- tibble::tibble(
  territory_id = municipality_order
) %>%
  dplyr::left_join(
    economic_groups %>%
      dplyr::select(territory_id, group),
    by = "territory_id"
  ) %>%
  dplyr::mutate(
    group = dplyr::coalesce(group, "Другие МО панели"),
    territory_id = factor(
      territory_id,
      levels = rev(municipality_order)
    )
  )

# Палитра соответствует двум предыдущим рисункам

strip_palette <- group_palette
strip_palette["Другие МО панели"] <- "#F0F0F0"

# Проверяем состав полосы
group_strip %>%
  dplyr::count(group)

# Строим полосу
p_group_strip <- ggplot(
  group_strip,
  aes(
    x = 1,
    y = territory_id,
    fill = group
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = strip_palette,
    breaks = c(
      "Городское ядро",
      "Периферийный блок 1",
      "Периферийный блок 2"
    ),
    name = "Устойчивое объединение"
  ) +
  scale_y_discrete(
    limits = levels(dynamics_matrix_aligned$territory_id),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Группа"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 9,
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)
  )

p_group_strip

# ============================================================
# 07_visualization.R
# ШАГ 14. Итоговая динамическая матрица
# ============================================================

library(patchwork)

# Основная матрица без легенды номеров сообществ
p_dynamics_clean <- p_dynamics_aligned +
  guides(fill = "none") +
  labs(
    title = "Динамика принадлежности муниципалитетов к сетевым сообществам",
    subtitle = "64 муниципалитета, январь 2023 — декабрь 2024",
    caption = paste0(
      "Цвет — согласованная во времени метка сетевого сообщества. ",
      "Справа отмечена принадлежность к выделенным устойчивым группам."
    )
  ) +
  theme(
    plot.caption = element_text(
      size = 8,
      color = "grey35",
      hjust = 0
    )
  )

# Правая полоса
p_group_strip_clean <- p_group_strip +
  labs(title = "Устойчивая\nгруппа") +
  theme(
    plot.title = element_text(
      size = 7,
      face = "bold",
      hjust = 0.5,
      lineheight = 0.9
    ),
    legend.title = element_blank()
  )

# Объединяем
p_dynamics_final <-
  p_dynamics_clean +
  p_group_strip_clean +
  plot_layout(
    widths = c(20, 1.2),
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

p_dynamics_final

# ============================================================
# Сохранение итоговых рисунков
# ============================================================

dir.create("figures", showWarnings = FALSE)

ggsave(
  filename = "figures/fig_01_groups_health.png",
  plot = p_groups_health,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "figures/fig_02_pair_stability.png",
  plot = p_stability_final,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "figures/fig_03_community_dynamics.png",
  plot = p_dynamics_final,
  width = 16,
  height = 8,
  dpi = 300,
  bg = "white"
)