# 03_cluster_validation.R
# Requires: panel_clr, network_results, build_month_network() from scripts 01-02.
# Libraries loaded in main script: dplyr, purrr, tibble, igraph, cluster, clusterCrit, mclust.

features <- c('clr_food', 'clr_market', 'clr_catering', 'clr_transport')
months <- sort(unique(panel_clr$date))
stopifnot(length(months) == 24L, length(network_results) == 24L,
          all(as.character(months) %in% names(network_results)))

# Original project definition of Scatter-Density between (S_Dbw).
calculate_sdbw <- function(X, groups) {
  groups <- as.integer(factor(groups))
  ids <- sort(unique(groups))
  stopifnot(length(ids) >= 2L, all(is.finite(X)))
  sd_all <- apply(X, 2, sd)
  norm_all <- sqrt(sum(sd_all^2))
  scatter <- vapply(ids, function(g) {
    Xg <- X[groups == g, , drop = FALSE]
    sqrt(sum(apply(Xg, 2, sd)^2))
  }, numeric(1))
  Scat <- mean(scatter) / norm_all
  radius <- mean(scatter)
  centers <- lapply(ids, function(g) colMeans(X[groups == g, , drop = FALSE]))
  density <- function(points, center) {
    distances <- sqrt(rowSums(sweep(points, 2, center, '-')^2))
    sum(distances <= radius)
  }
  cluster_density <- vapply(seq_along(ids), function(i) {
    density(X[groups == ids[i], , drop = FALSE], centers[[i]])
  }, numeric(1))
  pairs <- combn(seq_along(ids), 2)
  pair_scores <- apply(pairs, 2, function(pair) {
    i <- pair[1]; j <- pair[2]
    midpoint <- (centers[[i]] + centers[[j]]) / 2
    X_pair <- X[groups %in% ids[c(i, j)], , drop = FALSE]
    denominator <- max(cluster_density[i], cluster_density[j])
    if (denominator == 0) return(NA_real_)
    density(X_pair, midpoint) / denominator
  })
  Dens_bw <- mean(pair_scores)
  c(Scat = Scat, Dens_bw = Dens_bw, S_Dbw = Scat + Dens_bw)
}

# Weighted network MQ: each undirected edge is counted once.
calc_MQ <- function(graph, groups) {
  stopifnot(length(groups) == igraph::vcount(graph), !anyNA(groups))
  edges <- igraph::as_edgelist(graph, names = FALSE)
  w <- igraph::E(graph)$weight
  clusters <- sort(unique(groups))
  cf <- vapply(clusters, function(cl) {
    inside <- groups[edges[, 1]] == cl & groups[edges[, 2]] == cl
    boundary <- xor(groups[edges[, 1]] == cl, groups[edges[, 2]] == cl)
    mu <- sum(w[inside]); epsilon <- sum(w[boundary])
    denom <- mu + 0.5 * epsilon
    if (denom == 0) 0 else mu / denom
  }, numeric(1))
  sum(cf)
}

# Original unweighted AVI/AVU definition for undirected graphs.
calculate_avi_avu <- function(graph, membership) {
  stopifnot(length(membership) == igraph::vcount(graph),
            !anyNA(membership), !igraph::is_directed(graph))
  groups <- as.integer(factor(membership))
  K <- length(unique(groups))
  ends <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  M <- matrix(0, K, K)
  for (r in seq_len(nrow(ends))) {
    i <- groups[ends[r, 1]]; j <- groups[ends[r, 2]]
    M[i, j] <- M[i, j] + 1
    if (i == j) M[i, i] <- M[i, i] + 1 else M[j, i] <- M[j, i] + 1
  }
  AVI <- mean(diag(M) / rowSums(M))
  external <- rowSums(M) - diag(M)
  pairs <- combn(seq_len(K), 2)
  unifiability <- apply(pairs, 2, function(p) {
    i <- p[1]; j <- p[2]
    denominator <- external[i] + external[j] - M[i, j]
    if (denominator == 0) return(0)
    M[i, j] / denominator
  })
  AVU <- 2 * sum(unifiability) / K
  c(AVI = AVI, AVU = AVU)
}

# Attribute-space indices for the same X and partition.
calc_attribute_indices <- function(X, groups, distance_matrix = stats::dist(X)) {
  groups <- as.integer(factor(groups))
  k <- length(unique(groups)); n <- nrow(X)
  stopifnot(k > 1, k < n)
  SW <- mean(cluster::silhouette(groups, distance_matrix)[, 'sil_width'])
  grand_mean <- colMeans(X)
  W <- 0; B <- 0
  for (g in unique(groups)) {
    Xg <- X[groups == g, , drop = FALSE]
    center <- colMeans(Xg)
    W <- W + sum(sweep(Xg, 2, center, '-')^2)
    B <- B + nrow(Xg) * sum((center - grand_mean)^2)
  }
  CH <- (B / (k - 1)) / (W / (n - k))
  c(SW = SW, CH = CH, S_Dbw = unname(calculate_sdbw(X, groups)['S_Dbw']))
}

calc_network_indices <- function(graph, vertex_groups) {
  av <- calculate_avi_avu(graph, vertex_groups)
  AVI <- unname(av['AVI']); AVU <- unname(av['AVU'])
  c(AVI = AVI, AVU = AVU,
    MQ = calc_MQ(graph, vertex_groups),
    ANUI = AVI / (1 + AVI * AVU),
    Q = igraph::modularity(graph, membership = vertex_groups,
                          weights = igraph::E(graph)$weight))
}

# 1. Sensitivity to the number of neighbors, using the original graph builder.
sensitivity_results <- purrr::map_dfr(months, function(current_month) {
  month_data <- panel_clr %>%
    dplyr::filter(date == current_month) %>%
    dplyr::arrange(territory_id)
  X <- as.matrix(dplyr::select(month_data, dplyr::all_of(features)))
  reference <- network_results[[as.character(current_month)]]$communities
  reference_groups <- reference$community[match(month_data$territory_id, reference$territory_id)]
  stopifnot(!anyNA(reference_groups))
  purrr::map_dfr(config$network$sensitivity_k, function(k_neighbors) {
    result <- if (k_neighbors == config$network$k) network_results[[as.character(current_month)]] else
      build_month_network(month_data, k_neighbors = k_neighbors)
    alternative <- result$communities$community[
      match(month_data$territory_id, result$communities$territory_id)]
    stopifnot(!anyNA(alternative))
    tibble::tibble(date = current_month, k_neighbors = k_neighbors,
                   n_edges = nrow(result$edges), n_communities = result$n_communities,
                   modularity = result$modularity,
                   ARI_vs_k5 = mclust::adjustedRandIndex(reference_groups, alternative),
                   SW = mean(cluster::silhouette(alternative, stats::dist(X))[, 'sil_width']))
  })
})

sensitivity_summary <- sensitivity_results %>%
  dplyr::group_by(k_neighbors) %>%
  dplyr::summarise(n_months = dplyr::n(), mean_edges = mean(n_edges),
                   mean_communities = mean(n_communities),
                   min_communities = min(n_communities),
                   max_communities = max(n_communities),
                   mean_modularity = mean(modularity),
                   mean_ARI_vs_k5 = mean(ARI_vs_k5),
                   min_ARI_vs_k5 = min(ARI_vs_k5), mean_SW = mean(SW),
                   .groups = 'drop')
stopifnot(
  nrow(sensitivity_results) ==
    length(months) * length(config$network$sensitivity_k),
  
  all(sensitivity_summary$n_months == length(months)),
  
  all(
    sensitivity_results$ARI_vs_k5[
      sensitivity_results$k_neighbors == config$network$k
    ] == 1
  )
)
print(sensitivity_summary, width = Inf)

# 2. Comparison: ONE k-means run per month; all indices from the same partition.
comparison_methods_final <- purrr::map_dfr(months, function(current_month) {
  result <- network_results[[as.character(current_month)]]
  month_data <- panel_clr %>%
    dplyr::filter(date == current_month) %>%
    dplyr::arrange(territory_id)
  stopifnot(nrow(month_data) == 64L, !anyDuplicated(month_data$territory_id))
  X <- as.matrix(dplyr::select(month_data, dplyr::all_of(features)))
  lv_groups <- result$communities$community[
    match(month_data$territory_id, result$communities$territory_id)]
  stopifnot(!anyNA(lv_groups))
  k <- dplyr::n_distinct(lv_groups)
  set.seed(123)
  km <- stats::kmeans(X, centers = k, nstart = 50)
  km_groups <- km$cluster
  distance_matrix <- stats::dist(X)
  lv_attr <- calc_attribute_indices(X, lv_groups, distance_matrix)
  km_attr <- calc_attribute_indices(X, km_groups, distance_matrix)
  vertex_ids <- as.character(igraph::V(result$graph)$name)
  vertex_pos <- match(vertex_ids, as.character(month_data$territory_id))
  stopifnot(length(vertex_ids) == 64L, !anyNA(vertex_pos))
  lv_net <- calc_network_indices(result$graph, lv_groups[vertex_pos])
  km_net <- calc_network_indices(result$graph, km_groups[vertex_pos])
  stopifnot(abs(unname(lv_net['Q']) - result$modularity) < 1e-8)
  tibble::tibble(
    date = current_month, k = k,
    SW_louvain = unname(lv_attr['SW']), SW_kmeans = unname(km_attr['SW']),
    CH_louvain = unname(lv_attr['CH']), CH_kmeans = unname(km_attr['CH']),
    S_Dbw_louvain = unname(lv_attr['S_Dbw']), S_Dbw_kmeans = unname(km_attr['S_Dbw']),
    AVI_louvain = unname(lv_net['AVI']), AVI_kmeans = unname(km_net['AVI']),
    AVU_louvain = unname(lv_net['AVU']), AVU_kmeans = unname(km_net['AVU']),
    MQ_louvain = unname(lv_net['MQ']), MQ_kmeans = unname(km_net['MQ']),
    ANUI_louvain = unname(lv_net['ANUI']), ANUI_kmeans = unname(km_net['ANUI']),
    Q_louvain = unname(lv_net['Q']), Q_kmeans = unname(km_net['Q'])
  )
})
stopifnot(nrow(comparison_methods_final) == 24L,
          dplyr::n_distinct(comparison_methods_final$date) == 24L,
          all(is.finite(as.matrix(dplyr::select(comparison_methods_final, -date)))))

metric_names <- c('SW', 'CH', 'S_Dbw', 'AVI', 'AVU', 'MQ', 'ANUI', 'Q')
metric_directions <- c('max', 'max', 'min', 'max', 'min', 'max', 'max', 'max')
method_comparison_final <- tibble::tibble(
  metric = metric_names,
  direction = metric_directions,
  Louvain = vapply(metric_names, function(m) mean(comparison_methods_final[[paste0(m, '_louvain')]]), numeric(1)),
  Kmeans = vapply(metric_names, function(m) mean(comparison_methods_final[[paste0(m, '_kmeans')]]), numeric(1))
)
print(method_comparison_final, n = Inf)
cat('Months:', nrow(comparison_methods_final), '\n')
cat('k-means SW higher:', sum(comparison_methods_final$SW_kmeans > comparison_methods_final$SW_louvain), '/24\n')
cat('k-means CH higher:', sum(comparison_methods_final$CH_kmeans > comparison_methods_final$CH_louvain), '/24\n')
cat('Louvain MQ higher:', sum(comparison_methods_final$MQ_louvain > comparison_methods_final$MQ_kmeans), '/24\n')

# Optional output files
utils::write.csv(
  sensitivity_summary,
  "processed/sensitivity_summary.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  comparison_methods_final,
  "processed/comparison_methods_monthly.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  method_comparison_final,
  "processed/method_comparison_final.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)