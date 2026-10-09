# 04_temporal_stability.R
# Input: communities_all from 02_build_networks.R (64 municipalities x 24 months).


stopifnot(requireNamespace('dplyr', quietly = TRUE),
          requireNamespace('tidyr', quietly = TRUE),
          requireNamespace('purrr', quietly = TRUE),
          requireNamespace('igraph', quietly = TRUE),
          requireNamespace('gtools', quietly = TRUE))
stopifnot(nrow(communities_all) == 64L * 24L,
          dplyr::n_distinct(communities_all$date) == 24L,
          dplyr::n_distinct(communities_all$territory_id) == 64L,
          !anyDuplicated(communities_all[c('date', 'territory_id')]))

# 1. Flows between successive monthly partitions (raw monthly labels).
community_transitions <- communities_all %>%
  dplyr::arrange(territory_id, date) %>%
  dplyr::group_by(territory_id) %>%
  dplyr::mutate(next_date = dplyr::lead(date),
                next_community = dplyr::lead(community)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(next_date)) %>%
  dplyr::count(date, community, next_date, next_community,
               name = 'n_municipalities') %>%
  dplyr::arrange(date, community, dplyr::desc(n_municipalities))

transition_strength <- community_transitions %>%
  dplyr::group_by(date, community) %>%
  dplyr::mutate(source_size = sum(n_municipalities),
                share_of_source = n_municipalities / source_size,
                main_flow = n_municipalities == max(n_municipalities)) %>%
  dplyr::ungroup()

flow_check <- transition_strength %>%
  dplyr::group_by(date, community) %>%
  dplyr::summarise(source_size = dplyr::first(source_size),
                   max_n = max(n_municipalities),
                   n_max_flows = sum(n_municipalities == max_n),
                   .groups = 'drop')
flow_summary <- flow_check %>%
  dplyr::summarise(source_communities = dplyr::n(),
                   unique_main_flow = sum(n_max_flows == 1L),
                   tied_main_flow = sum(n_max_flows > 1L),
                   share_unique = mean(n_max_flows == 1L))

monthly_dominant <- transition_strength %>%
  dplyr::group_by(date, next_date) %>%
  dplyr::summarise(total = sum(n_municipalities),
                   dominant = sum(n_municipalities[main_flow]),
                   share = dominant / total, .groups = 'drop')
transition_month_summary <- monthly_dominant %>%
  dplyr::transmute(date, next_date,
                   municipalities_in_main_flows = dominant,
                   share_in_main_flows = share)
transition_summary <- tibble::tibble(
  n_monthly_transitions = nrow(monthly_dominant),
  total_transitions = sum(transition_strength$n_municipalities),
  dominant_transitions = sum(transition_strength$n_municipalities[
    transition_strength$main_flow]),
  dominant_share = dominant_transitions / total_transitions,
  min_monthly_share = min(monthly_dominant$share),
  max_monthly_share = max(monthly_dominant$share))
stopifnot(nrow(monthly_dominant) == 23L,
          all(monthly_dominant$total == 64L),
          transition_summary$total_transitions == 1472L)

# 2. Sequentially align community labels to maximize overlap.
# Aligned IDs are tracking labels, NOT permanent economic group types.
dates_all <- sort(unique(communities_all$date))
aligned <- communities_all %>%
  dplyr::filter(date == dates_all[1]) %>%
  dplyr::mutate(aligned_community = community)

for (i in seq.int(2L, length(dates_all))) {
  prev <- aligned %>%
    dplyr::filter(date == dates_all[i - 1L]) %>%
    dplyr::select(territory_id, aligned_community)
  curr <- communities_all %>% dplyr::filter(date == dates_all[i])
  tab <- table(prev$aligned_community[match(curr$territory_id, prev$territory_id)],
               curr$community)
  old_labels <- as.integer(rownames(tab))
  new_labels <- as.integer(colnames(tab))
  candidate_labels <- sort(unique(c(old_labels,
    seq_len(max(length(old_labels), length(new_labels))))))
  while (length(candidate_labels) < length(new_labels)) {
    candidate_labels <- c(candidate_labels, max(candidate_labels) + 1L)
  }
  perms <- gtools::permutations(n = length(candidate_labels),
                                r = length(new_labels), v = candidate_labels)
  scores <- apply(perms, 1, function(p) {
    sum(vapply(seq_along(new_labels), function(j) {
      if (p[j] %in% old_labels) {
        as.numeric(tab[as.character(p[j]), as.character(new_labels[j])])
      } else 0
    }, numeric(1)))
  })
  best <- perms[which.max(scores), ]
  mapping <- tibble::tibble(community = new_labels,
                            aligned_community = as.integer(best))
  curr <- curr %>% dplyr::left_join(mapping, by = 'community')
  aligned <- dplyr::bind_rows(aligned, curr)
}
communities_aligned <- aligned %>% dplyr::arrange(date, territory_id)
stopifnot(nrow(communities_aligned) == 1536L,
          !anyNA(communities_aligned$aligned_community))

# 3. Co-membership stability S_ij = fraction of 24 months in same community.
# This is NOT the persistence of direct kNN edges.
pair_stability <- communities_all %>%
  dplyr::select(date, territory_id, community) %>%
  dplyr::inner_join(communities_all %>%
    dplyr::transmute(date, territory_id_2 = territory_id,
                     community_2 = community),
    by = 'date', relationship = 'many-to-many') %>%
  dplyr::filter(territory_id < territory_id_2) %>%
  dplyr::group_by(territory_id, territory_id_2) %>%
  dplyr::summarise(months_together = sum(community == community_2),
                   months_observed = dplyr::n(),
                   stability = months_together / months_observed,
                   .groups = 'drop')

municipality_names <- panel_clr %>%
  dplyr::distinct(territory_id, municipal_district_name_short) %>%
  dplyr::arrange(territory_id)
ids <- as.character(municipality_names$territory_id)
S <- matrix(0, nrow = length(ids), ncol = length(ids),
            dimnames = list(ids, ids))
diag(S) <- 1
for (i in seq_len(nrow(pair_stability))) {
  a <- as.character(pair_stability$territory_id[i])
  b <- as.character(pair_stability$territory_id_2[i])
  S[a, b] <- pair_stability$stability[i]
  S[b, a] <- pair_stability$stability[i]
}
stopifnot(nrow(pair_stability) == choose(64L, 2L),
          all(pair_stability$months_observed == 24L),
          isTRUE(all.equal(S, t(S))), all(diag(S) == 1),
          all(S >= 0 & S <= 1))
S_upper <- S[upper.tri(S)]
pair_stability_summary <- tibble::tibble(
  n_pairs = length(S_upper), never_together = sum(S_upper == 0),
  stability_075_plus = sum(S_upper >= 0.75),
  always_together = sum(S_upper == 1))

# 4. Threshold graph (75%), connected components and maximal cliques.
stable_edges <- pair_stability %>% dplyr::filter(stability >= 0.75)
stable_graph <- igraph::graph_from_data_frame(
  stable_edges %>% dplyr::transmute(from = as.character(territory_id),
    to = as.character(territory_id_2), weight = stability),
  directed = FALSE, vertices = tibble::tibble(name = ids))
components <- igraph::components(stable_graph)
component_members <- tibble::tibble(
  territory_id = as.numeric(names(components$membership)),
  component = as.integer(components$membership))
core_summary <- component_members %>%
  dplyr::count(component, name = 'n_municipalities') %>%
  dplyr::arrange(dplyr::desc(n_municipalities))
core_quality <- pair_stability %>%
  dplyr::inner_join(component_members, by = 'territory_id') %>%
  dplyr::inner_join(component_members %>%
    dplyr::rename(territory_id_2 = territory_id, component_2 = component),
    by = 'territory_id_2') %>%
  dplyr::filter(component == component_2) %>%
  dplyr::group_by(component) %>%
  dplyr::summarise(n_pairs = dplyr::n(),
                   share_stable = mean(stability >= 0.75),
                   mean_stability = mean(stability),
                   min_stability = min(stability), .groups = 'drop') %>%
  dplyr::left_join(core_summary, by = 'component') %>%
  dplyr::arrange(dplyr::desc(n_municipalities))
cliques <- igraph::max_cliques(stable_graph, min = 3)
strict_cores <- purrr::map_dfr(seq_along(cliques), function(i) {
  tibble::tibble(core_id = i,
    territory_id = as.numeric(igraph::V(stable_graph)$name[as.integer(cliques[[i]])]))
})
core_sizes <- strict_cores %>%
  dplyr::count(core_id, name = 'n_municipalities') %>%
  dplyr::arrange(dplyr::desc(n_municipalities))

# Hierarchical ordering for visualizations, not the primary clustering model.
hc_stability <- stats::hclust(stats::as.dist(1 - S), method = 'average')
municipality_order <- as.numeric(rownames(S)[hc_stability$order])

# 5. The ORIGINAL selection used in the report: core 22 + component 7.
# These numeric IDs can change with igraph's enumeration order: verify membership!
stopifnot(22L %in% strict_cores$core_id, 7L %in% component_members$component)
component_7 <- component_members %>%
  dplyr::filter(component == 7L) %>%
  dplyr::left_join(municipality_names, by = 'territory_id') %>%
  dplyr::arrange(municipal_district_name_short)
ids_7 <- as.character(component_7$territory_id)
S7 <- S[ids_7, ids_7, drop = FALSE]
hc7 <- stats::hclust(stats::as.dist(1 - S7), method = 'average')
groups_7 <- stats::cutree(hc7, k = 2L)

economic_groups <- dplyr::bind_rows(
  strict_cores %>% dplyr::filter(core_id == 22L) %>%
    dplyr::transmute(territory_id, group = 'Городское ядро'),
  component_7 %>% dplyr::mutate(group = dplyr::if_else(
    groups_7[as.character(territory_id)] == 1L,
    'Периферийный блок 1', 'Периферийный блок 2')) %>%
    dplyr::select(territory_id, group))
expected_groups <- c('Городское ядро' = 9L,
                     'Периферийный блок 1' = 7L,
                     'Периферийный блок 2' = 10L)
actual_groups <- table(economic_groups$group)
print(actual_groups)
stopifnot(nrow(economic_groups) == 26L,
          !anyDuplicated(economic_groups$territory_id),
          identical(as.integer(actual_groups[names(expected_groups)]),
                    unname(expected_groups)))

# 6. Within-group and between-periphery pair stability.
group_stability_summary <- function(group_name) {
  group_ids <- economic_groups %>% dplyr::filter(group == group_name) %>%
    dplyr::pull(territory_id) %>% as.character()
  S_group <- S[group_ids, group_ids, drop = FALSE]
  values <- S_group[upper.tri(S_group)]
  tibble::tibble(group = group_name, n_municipalities = length(group_ids),
    n_pairs = length(values), mean_stability = mean(values),
    share_075_plus = mean(values >= 0.75),
    min_stability = min(values), max_stability = max(values))
}
group_summary <- purrr::map_dfr(names(expected_groups), group_stability_summary)
ids_p1 <- economic_groups %>%
  dplyr::filter(group == 'Периферийный блок 1') %>%
  dplyr::pull(territory_id) %>% as.character()
ids_p2 <- economic_groups %>%
  dplyr::filter(group == 'Периферийный блок 2') %>%
  dplyr::pull(territory_id) %>% as.character()
between_values <- as.vector(S[ids_p1, ids_p2, drop = FALSE])
peripheral_between_summary <- tibble::tibble(
  group_1 = 'Периферийный блок 1', group_2 = 'Периферийный блок 2',
  n_pairs = length(between_values),
  mean_stability = mean(between_values),
  share_075_plus = mean(between_values >= 0.75),
  min_stability = min(between_values),
  max_stability = max(between_values))

cat('\n--- FLOW SUMMARY ---\n'); print(flow_summary)
cat('\n--- TRANSITION SUMMARY ---\n'); print(transition_summary)
cat('\n--- PAIR STABILITY ---\n'); print(pair_stability_summary)
cat('\n--- GROUP STABILITY ---\n'); print(group_summary)
cat('\n--- BETWEEN P1 AND P2 ---\n'); print(peripheral_between_summary)
