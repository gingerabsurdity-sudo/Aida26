config <- yaml::read_yaml("config.yml")

# ============================================================
# 9. ПОСТРОЕНИЕ МЕСЯЧНОЙ ЭКОНОМИЧЕСКОЙ СЕТИ
# ============================================================


build_month_network <- function(month_data, k_neighbors = 5) {
  
  # 1. CLR-признаки
  X <- month_data %>%
    select(
      clr_food,
      clr_market,
      clr_catering,
      clr_transport
    )
  
  # Каждый месяц должен содержать 64 МО
  stopifnot(nrow(X) == 64)
  
  # 2. Евклидовы расстояния в CLR-пространстве
  dist_matrix <- as.matrix(dist(X))
  
  rownames(dist_matrix) <- as.character(month_data$territory_id)
  colnames(dist_matrix) <- as.character(month_data$territory_id)
  
  # 3. Направленные k ближайших соседей
  neighbors <- vector("list", nrow(dist_matrix))
  
  for (i in seq_len(nrow(dist_matrix))) {
    
    d <- dist_matrix[i, ]
    d[i] <- NA_real_
    
    nearest <- order(
      d,
      na.last = NA
    )[seq_len(k_neighbors)]
    
    neighbors[[i]] <- tibble(
      territory_id = as.numeric(rownames(dist_matrix)[i]),
      neighbor_id = as.numeric(colnames(dist_matrix)[nearest]),
      distance = d[nearest],
      rank = seq_len(k_neighbors)
    )
  }
  
  neighbors <- bind_rows(neighbors)
  
  # Для каждого МО должно быть ровно k направленных соседств
  stopifnot(
    nrow(neighbors) == nrow(month_data) * k_neighbors
  )
  
  # 4. Union-kNN:
  # неориентированное ребро существует, если хотя бы одно
  # из двух МО входит в k ближайших соседей другого
  edges <- neighbors %>%
    mutate(
      from = as.character(pmin(territory_id, neighbor_id)),
      to   = as.character(pmax(territory_id, neighbor_id))
    ) %>%
    group_by(from, to) %>%
    summarise(
      distance = min(distance),
      mutual = n() == 2,
      .groups = "drop"
    )
  
  # 5. Масштаб расстояний
  sigma <- median(edges$distance)
  
  stopifnot(
    is.finite(sigma),
    sigma > 0
  )
  
  # 6. Вес ребра
  edges <- edges %>%
    mutate(
      weight = exp(-distance / sigma)
    )
  
  # 7. Вершины сети
  vertices <- month_data %>%
    transmute(
      name = as.character(territory_id),
      municipality = municipal_district_name_short
    )
  
  # 8. Неориентированный взвешенный граф
  graph <- graph_from_data_frame(
    d = edges,
    directed = FALSE,
    vertices = vertices
  )
  
  stopifnot(
    igraph::vcount(graph) == 64,
    !igraph::is_directed(graph),
    igraph::ecount(graph) > 0
  )
  
  # 9. Выделение сообществ методом Louvain
  set.seed(123)
  
  louvain <- cluster_louvain(
    graph,
    weights = E(graph)$weight
  )
  
  # 10. Принадлежность МО к сообществам
  communities <- tibble(
    territory_id = as.numeric(V(graph)$name),
    community = as.integer(membership(louvain))
  )
  
  # 11. Результаты
  list(
    neighbors = neighbors,
    edges = edges,
    graph = graph,
    communities = communities,
    sigma = sigma,
    modularity = modularity(louvain),
    n_communities = length(louvain)
  )
}


# ============================================================
# 10. ПОСТРОЕНИЕ СЕТЕЙ ДЛЯ ВСЕХ МЕСЯЦЕВ
# ============================================================

all_months <- sort(unique(panel_clr$date))

stopifnot(length(all_months) == 24)

network_results <- list()

for (current_month in all_months) {
  
  # В цикле for класс Date теряется — восстанавливаем его
  current_month <- as.Date(
    current_month,
    origin = "1970-01-01"
  )
  
  month_data <- panel_clr %>%
    filter(date == current_month)
  
  network_results[[as.character(current_month)]] <-
    build_month_network(
      month_data,
      k_neighbors = config$network$k
    )
}

stopifnot(length(network_results) == 24)

names(network_results)

communities_all <- imap_dfr(
  network_results,
  ~ .x$communities %>%
    mutate(
      date = as.Date(.y)
    )
)

communities_all <- communities_all %>%
  left_join(
    panel_clr %>%
      distinct(
        territory_id,
        municipal_district_name_short
      ),
    by = "territory_id"
  ) %>%
  select(
    date,
    territory_id,
    municipal_district_name_short,
    community
  ) %>%
  arrange(date, territory_id)

stopifnot(nrow(communities_all) == 64 * 24)

glimpse(communities_all)