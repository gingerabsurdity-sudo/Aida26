# ============================================================
# SberIndex Municipal Networks
# Полный запуск анализа
# ============================================================

source("R/01_prepare_consumption.R")
source("R/02_build_networks.R")
source("R/03_cluster_validation.R")
source("R/04_temporal_stability.R")
source("R/05_external_interpretation.R")
source("R/06_health_outliers.R")
source("R/07_visualization.R")

message("Все семь модулей успешно выполнены.")