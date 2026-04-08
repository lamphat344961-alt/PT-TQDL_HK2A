
# ============================================================
# Comparative Clustering in R
# Dataset: feature_for_clustering1.csv
# Author: OpenAI
# Mục tiêu:
# 1) K-Means: Elbow (WSS) + Silhouette để hỗ trợ chọn K
# 2) Hierarchical (Agglomerative - Ward): Dendrogram để chọn K
# 3) DBSCAN: k-NN distance plot để chọn eps tối ưu
# 4) Spectral Clustering: Eigengap Heuristic để gợi ý K
# 5) Đánh giá cụm bằng Silhouette, Davies-Bouldin, Calinski-Harabasz
# 6) Trực quan hóa kết quả và insight cluster
# ============================================================

# =========================
# 0. Cài / nạp package
# =========================
required_packages <- c(
  "ggplot2", "dplyr", "tidyr", "cluster", "factoextra", "dbscan",
  "kernlab", "scales", "patchwork", "viridis", "gridExtra"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# =========================
# 1. Tham số chính
# =========================
DATA_PATH <- "C:\\Users\\Admin\\Desktop\\TANPHAT\\Dự án_canhan\\TIKTOK_V2\\clustering\\feature_for_clustering_final.csv"   # đổi path nếu cần
ID_COL <- "CREATOR_ID"

# Tiền xử lý
DROP_NA_ROWS <- TRUE              # bám sát logic Python notebook của bạn
USE_MEDIAN_IMPUTE <- FALSE        # nếu FALSE và DROP_NA_ROWS = TRUE -> drop rows NA
SCALING_METHOD <- "minmax"        # "minmax" / "zscore"
USE_PCA_FOR_MODEL <- TRUE         # bám sát notebook Python
PCA_VARIANCE <- 0.95

# Dải K để dò tìm
K_RANGE <- 1:10

# DBSCAN
DBSCAN_MINPTS <- NULL             # nếu NULL -> tự đặt = max(5, round(log(n)))
DBSCAN_QUANTILE_FOR_EPS <- 0.98   # chỉ để fallback nếu knee detection không ổn

set.seed(42)

# =========================
# 2. Hàm tiện ích
# =========================

minmax_scale_df <- function(df) {
  out <- as.data.frame(lapply(df, function(x) {
    rng <- range(x, na.rm = TRUE)
    if (is.na(rng[1]) || is.na(rng[2]) || diff(rng) == 0) {
      return(rep(0, length(x)))
    } else {
      return((x - rng[1]) / (rng[2] - rng[1]))
    }
  }))
  names(out) <- names(df)
  out
}

zscore_scale_df <- function(df) {
  out <- scale(df)
  out <- as.data.frame(out)
  names(out) <- names(df)
  out
}

safe_mean <- function(x) if (length(x) == 0) NA_real_ else mean(x, na.rm = TRUE)

# Davies-Bouldin Index (càng nhỏ càng tốt)
davies_bouldin_index <- function(X, labels) {
  labels <- as.vector(labels)
  
  # Loại noise nếu có (0 hoặc -1)
  keep <- !(labels %in% c(0, -1))
  X <- X[keep, , drop = FALSE]
  labels <- labels[keep]
  
  k <- length(unique(labels))
  if (k < 2) return(NA_real_)
  
  centers <- lapply(sort(unique(labels)), function(cl) {
    colMeans(X[labels == cl, , drop = FALSE])
  })
  centers <- do.call(rbind, centers)
  rownames(centers) <- sort(unique(labels))
  
  S <- sapply(sort(unique(labels)), function(cl) {
    Xi <- X[labels == cl, , drop = FALSE]
    ci <- colMeans(Xi)
    mean(sqrt(rowSums((sweep(Xi, 2, ci, "-"))^2)))
  })
  
  M <- as.matrix(dist(centers))
  R <- matrix(NA_real_, nrow = k, ncol = k)
  
  for (i in 1:k) {
    for (j in 1:k) {
      if (i != j) {
        if (M[i, j] == 0) {
          R[i, j] <- NA_real_
        } else {
          R[i, j] <- (S[i] + S[j]) / M[i, j]
        }
      }
    }
  }
  
  Di <- apply(R, 1, function(x) max(x, na.rm = TRUE))
  mean(Di, na.rm = TRUE)
}

# Calinski-Harabasz Index (càng lớn càng tốt)
calinski_harabasz_index <- function(X, labels) {
  labels <- as.vector(labels)
  
  # Loại noise nếu có
  keep <- !(labels %in% c(0, -1))
  X <- X[keep, , drop = FALSE]
  labels <- labels[keep]
  
  n <- nrow(X)
  k <- length(unique(labels))
  if (k < 2 || k >= n) return(NA_real_)
  
  overall_mean <- colMeans(X)
  
  W <- 0
  B <- 0
  
  for (cl in unique(labels)) {
    Xi <- X[labels == cl, , drop = FALSE]
    ni <- nrow(Xi)
    ci <- colMeans(Xi)
    
    W <- W + sum(rowSums((sweep(Xi, 2, ci, "-"))^2))
    B <- B + ni * sum((ci - overall_mean)^2)
  }
  
  ((B / (k - 1)) / (W / (n - k)))
}

evaluate_clustering <- function(X, labels, model_name) {
  labels <- as.vector(labels)
  
  # loại noise để tính metric nội cụm
  keep <- !(labels %in% c(0, -1))
  X_eval <- X[keep, , drop = FALSE]
  labels_eval <- labels[keep]
  
  n_clusters <- length(unique(labels_eval))
  n_noise <- sum(labels %in% c(0, -1))
  
  silhouette_avg <- NA_real_
  if (n_clusters >= 2 && nrow(X_eval) > n_clusters) {
    dmat <- dist(X_eval)
    sil <- cluster::silhouette(as.integer(as.factor(labels_eval)), dmat)
    silhouette_avg <- mean(sil[, "sil_width"])
  }
  
  dbi <- davies_bouldin_index(as.matrix(X), labels)
  chi <- calinski_harabasz_index(as.matrix(X), labels)
  
  data.frame(
    Algorithm = model_name,
    n_clusters = n_clusters,
    n_noise = n_noise,
    Silhouette = silhouette_avg,
    Davies_Bouldin = dbi,
    Calinski_Harabasz = chi
  )
}

find_best_kmeans <- function(X, k_range = 1:10, nstart = 30) {
  wss <- numeric(length(k_range))
  sil <- numeric(length(k_range))
  
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    km <- kmeans(X, centers = k, nstart = nstart, iter.max = 100)
    wss[i] <- km$tot.withinss
    
    # Logic chống lỗi: Silhouette chỉ tính được từ 2 cụm trở lên
    if (k > 1) {
      ss <- cluster::silhouette(km$cluster, dist(X))
      sil[i] <- mean(ss[, "sil_width"])
    } else {
      sil[i] <- 0 # Mặc định k=1 thì Silhouette bằng 0
    }
  }
  
  k_sil <- k_range[which.max(sil)]
  
  elbow_df <- data.frame(K = k_range, WSS = wss)
  sil_df <- data.frame(K = k_range, Silhouette = sil)
  
  p1 <- ggplot(elbow_df, aes(x = K, y = WSS)) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous(breaks = k_range) +
    labs(
      title = "K-Means - Elbow Method (WSS theo K)",
      x = "Số cụm K", y = "Tổng Within-Cluster Sum of Squares (WSS)"
    ) +
    theme_minimal(base_size = 12)
  
  p2 <- ggplot(sil_df, aes(x = K, y = Silhouette)) +
    geom_line() +
    geom_point(size = 2) +
    geom_vline(xintercept = k_sil, linetype = "dashed") +
    scale_x_continuous(breaks = k_range) +
    labs(
      title = "K-Means - Average Silhouette theo K",
      x = "Số cụm K", y = "Average Silhouette"
    ) +
    theme_minimal(base_size = 12)
  
  print(p1 / p2)
  
  list(
    k_suggested = k_sil,
    wss = elbow_df,
    silhouette = sil_df
  )
}

find_best_hclust <- function(X, k_range = 2:10) {
  d <- dist(X)
  hc <- hclust(d, method = "ward.D2")
  
  plot(
    hc, labels = FALSE, hang = -1, cex = 0.6,
    main = "Hierarchical Clustering - Ward's Method Dendrogram",
    xlab = "", sub = "", lwd = 2 
  )
  
  sil_vals <- sapply(k_range, function(k) {
    labels <- cutree(hc, k = k)
    sil <- cluster::silhouette(labels, d)
    sil_matrix <- as.matrix(sil)
    # Kiểm tra số cột trước khi truy cập
    if (ncol(sil_matrix) < 3) return(0)
    
    return(mean(sil_matrix[, 3]))
  })
  
  sil_df <- data.frame(K = k_range, Silhouette = sil_vals)
  p <- ggplot(sil_df, aes(K, Silhouette)) +
    geom_line() + geom_point(size = 2) +
    geom_vline(xintercept = k_range[which.max(sil_vals)], linetype = "dashed") +
    scale_x_continuous(breaks = k_range) +
    labs(
      title = "Hierarchical - Average Silhouette theo K",
      x = "Số cụm K", y = "Average Silhouette"
    ) +
    theme_minimal(base_size = 12)
  print(p)
  
  list(
    hc = hc,
    k_suggested = k_range[which.max(sil_vals)],
    silhouette = sil_df
  )
}

suggest_dbscan_params <- function(X, minPts = NULL, q_fallback = 0.98) {
  n <- nrow(X)
  if (is.null(minPts)) minPts <- max(5, round(log(n)))
  
  kNN <- dbscan::kNNdist(X, k = minPts)
  kNN_sorted <- sort(kNN)
  
  idx <- seq_along(kNN_sorted)
  y <- (kNN_sorted - min(kNN_sorted)) / (max(kNN_sorted) - min(kNN_sorted) + 1e-12)
  x <- (idx - min(idx)) / (max(idx) - min(idx) + 1e-12)
  
  # Khoảng cách tới đường thẳng nối 2 đầu mút -> heuristic chọn "gối"
  dist_to_diag <- y - x
  knee_idx <- which.max(dist_to_diag)
  eps_knee <- kNN_sorted[knee_idx]
  
  if (!is.finite(eps_knee) || is.na(eps_knee) || eps_knee <= 0) {
    eps_knee <- as.numeric(quantile(kNN_sorted, q_fallback, na.rm = TRUE))
  }
  
  plot(
    kNN_sorted, type = "l",
    main = paste0("DBSCAN k-NN Distance Plot (k = minPts = ", minPts, ")"),
    xlab = "Points sorted by k-NN distance",
    ylab = paste0(minPts, "-NN distance")
  )
  abline(h = eps_knee, col = "red", lty = 2, lwd = 2)
  abline(v = knee_idx, col = "blue", lty = 3)
  
  message("DBSCAN suggested minPts = ", minPts)
  message("DBSCAN suggested eps (knee) = ", round(eps_knee, 6))
  
  list(minPts = minPts, eps = eps_knee, kNN_sorted = kNN_sorted)
}

eigengap_heuristic <- function(X, max_k = 10, sigma = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  
  dists <- as.matrix(dist(X))
  if (is.null(sigma)) {
    sigma <- median(dists[upper.tri(dists)], na.rm = TRUE)
  }
  sigma <- ifelse(is.na(sigma) || sigma <= 0, 1, sigma)
  
  W <- exp(-(dists^2) / (2 * sigma^2))
  diag(W) <- 0
  
  D <- diag(rowSums(W))
  D_inv_sqrt <- diag(1 / sqrt(diag(D) + 1e-12))
  L_sym <- diag(n) - D_inv_sqrt %*% W %*% D_inv_sqrt
  
  eig <- eigen(L_sym, symmetric = TRUE, only.values = TRUE)$values
  eig <- sort(Re(eig))
  
  max_k <- min(max_k, length(eig) - 1)
  gaps <- diff(eig[1:(max_k + 1)])
  k_suggested <- which.max(gaps)
  
  eig_df <- data.frame(
    Index = 1:(max_k + 1),
    Eigenvalue = eig[1:(max_k + 1)]
  )
  
  gap_df <- data.frame(
    K = 1:max_k,
    Eigengap = gaps
  )
  
  p1 <- ggplot(eig_df, aes(Index, Eigenvalue)) +
    geom_line() +
    geom_point(size = 2) +
    labs(
      title = "Spectral - Các trị riêng nhỏ nhất",
      x = "Thứ tự trị riêng", y = "Eigenvalue"
    ) +
    theme_minimal(base_size = 12)
  
  p2 <- ggplot(gap_df, aes(K, Eigengap)) +
    geom_line() +
    geom_point(size = 2) +
    geom_vline(xintercept = k_suggested, linetype = "dashed") +
    scale_x_continuous(breaks = 1:max_k) +
    labs(
      title = "Spectral - Eigengap Heuristic",
      x = "K (chọn theo khoảng cách lớn nhất giữa λ_k và λ_{k+1})",
      y = "Eigengap"
    ) +
    theme_minimal(base_size = 12)
  
  print(p1 / p2)
  
  list(k_suggested = k_suggested, eigenvalues = eig, gaps = gaps, sigma = sigma)
}

plot_cluster_pca <- function(X, labels, title = "Cluster Visualization") {
  pca <- prcomp(X, center = TRUE, scale. = FALSE)
  pca_df <- data.frame(
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    cluster = factor(labels)
  )
  
  ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(title = title, x = "PC1", y = "PC2", color = "Cluster") +
    theme_minimal(base_size = 12)
}

make_cluster_profile <- function(df_original_numeric, labels, top_n = 10, title = "Cluster Profile Heatmap") {
  tmp <- df_original_numeric
  tmp$cluster <- factor(labels)
  
  profile <- tmp %>%
    group_by(cluster) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # Chuẩn hóa theo cột để heatmap dễ đọc
  profile_long <- profile %>%
    tidyr::pivot_longer(-cluster, names_to = "feature", values_to = "value") %>%
    group_by(feature) %>%
    mutate(value_z = as.numeric(scale(value))) %>%
    ungroup()
  
  # chọn các feature phân hóa mạnh nhất giữa cụm
  feature_rank <- profile_long %>%
    group_by(feature) %>%
    summarise(spread = max(value_z, na.rm = TRUE) - min(value_z, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(spread)) %>%
    slice_head(n = min(top_n, n()))
  
  profile_top <- profile_long %>%
    filter(feature %in% feature_rank$feature)
  
  p <- ggplot(profile_top, aes(x = feature, y = cluster, fill = value_z)) +
    geom_tile() +
    scale_fill_viridis(option = "C", name = "Z-score") +
    labs(title = title, x = "Feature", y = "Cluster") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  list(plot = p, profile_table = profile)
}

# =========================
# 3. Nạp dữ liệu
# =========================
cat("\n", strrep("=", 90), "\n")
cat("BƯỚC 1. NẠP DỮ LIỆU VÀ THIẾT LẬP\n")
cat(strrep("=", 90), "\n")

df_raw <- read.csv(DATA_PATH, check.names = FALSE)

cat("Kích thước dữ liệu gốc:", dim(df_raw)[1], "rows x", dim(df_raw)[2], "cols\n")
cat("Tên cột:\n")
print(names(df_raw))

# Giữ ID riêng
creator_id <- NULL
if (ID_COL %in% names(df_raw)) {
  creator_id <- df_raw[[ID_COL]]
}

library(dplyr)

# Chỉ lấy numeric cho clustering
df_num <- df_raw %>%
  dplyr::select(where(is.numeric))

cat("\nSố cột numeric dùng cho clustering:", ncol(df_num), "\n")

# Xử lý NA
if (DROP_NA_ROWS) {
  before_n <- nrow(df_num)
  df_num <- na.omit(df_num)
  if (!is.null(creator_id)) creator_id <- creator_id[as.numeric(rownames(df_num))]
  rownames(df_num) <- NULL
  cat("Drop NA rows:", before_n - nrow(df_num), "rows bị loại.\n")
} else if (USE_MEDIAN_IMPUTE) {
  df_num <- df_num %>%
    mutate(across(everything(), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))
  cat("Đã median impute cho NA.\n")
}

cat("Kích thước sau xử lý NA:", dim(df_num)[1], "rows x", dim(df_num)[2], "cols\n")

# Scale
if (SCALING_METHOD == "minmax") {
  df_scaled <- minmax_scale_df(df_num)
} else {
  df_scaled <- zscore_scale_df(df_num)
}


cat("Scaling method:", SCALING_METHOD, "\n")

# PCA
if (USE_PCA_FOR_MODEL) {
  pca_model <- prcomp(df_scaled, center = TRUE, scale. = FALSE)
  cumvar <- cumsum((pca_model$sdev^2) / sum(pca_model$sdev^2))
  ncomp <- which(cumvar >= PCA_VARIANCE)[1]
  X_model <- pca_model$x[, 1:ncomp, drop = FALSE]
  cat("Dùng PCA cho modeling. Số chiều giữ lại =", ncomp,
      "để đạt", round(cumvar[ncomp] * 100, 2), "% phương sai.\n")
} else {
  X_model <- as.matrix(df_scaled)
  cat("Không dùng PCA cho modeling.\n")
}

X_model <- as.matrix(X_model)

# =========================
# 4. K-Means
# =========================
cat("\n", strrep("=", 90), "\n")
cat("BƯỚC 2. K-MEANS: ELBOW + SILHOUETTE\n")
cat(strrep("=", 90), "\n")

library(ggplot2)
library(patchwork)
km_search <- find_best_kmeans(X_model, k_range = K_RANGE, nstart = 30)
k_kmeans <- km_search$k_suggested
cat("K-Means - K gợi ý theo silhouette:", k_kmeans, "\n")

kmeans_model <- kmeans(X_model, centers = k_kmeans, nstart = 30, iter.max = 100)
labels_kmeans <- kmeans_model$cluster

# =========================
# 5. Hierarchical
# =========================
cat("\n", strrep("=", 90), "\n")
cat("BƯỚC 3. HIERARCHICAL CLUSTERING (WARD.D2)\n")
cat(strrep("=", 90), "\n")

dev.new()

hc_search <- find_best_hclust(X_model, k_range = K_RANGE)
k_hc <- hc_search$k_suggested
cat("Hierarchical - K gợi ý theo silhouette:", k_hc, "\n")

labels_hc <- cutree(hc_search$hc, k = k_hc)

# =========================
# 6. DBSCAN
# =========================
cat("\n", strrep("=", 90), "\n")
cat("BƯỚC 4. DBSCAN: K-NN DISTANCE CHỌN EPS\n")
cat(strrep("=", 90), "\n")

db_params <- suggest_dbscan_params(
  X_model,
  minPts = DBSCAN_MINPTS,
  q_fallback = DBSCAN_QUANTILE_FOR_EPS
)

k_dist <- dbscan::kNNdist(X_model, k = 100)

# 2. Lấy Eps ở mức phân vị cực thấp (1% hoặc 2%)
# Phân vị 1% nghĩa là chỉ có 1% các điểm có hàng xóm gần hơn mức này
eps_ <- quantile(sort(k_dist), 0.01)

# eps_ <- 0.2
minPts_ <- 100

db_model <- dbscan::dbscan(X_model, eps = eps_, minPts = minPts_)
labels_db <- db_model$cluster  # noise = 0

cat("DBSCAN - số cluster (không tính noise):", length(setdiff(unique(labels_db), 0)), "\n")
cat("DBSCAN - số noise:", sum(labels_db == 0), "\n")

# =========================
# 7. Spectral Clustering
# =========================
cat("\n", strrep("=", 90), "\n")
cat("BƯỚC 5. SPECTRAL CLUSTERING: EIGENGAP HEURISTIC\n")
cat(strrep("=", 90), "\n")


spec_search <- eigengap_heuristic(X_model, max_k = 10 )


k_spec <- spec_search$k_suggested
cat("Spectral - K gợi ý theo eigengap:", k_spec, "\n")

# specc dùng kernel RBF
spec_model <- kernlab::specc(as.matrix(X_model), centers = 2)
labels_spec <- as.integer(spec_model)

# =========================
# 8. Đánh giá mô hình
# =========================
cat("\n", strrep("=", 90), "\n")
cat("BƯỚC 6. ĐÁNH GIÁ CỤM\n")
cat(strrep("=", 90), "\n")

results_df <- dplyr::bind_rows(
  evaluate_clustering(X_model, labels_kmeans, "K-Means"),
  evaluate_clustering(X_model, labels_hc, "Hierarchical (Ward)"),
  evaluate_clustering(X_model, labels_db, "DBSCAN"),
  evaluate_clustering(X_model, labels_spec, "Spectral")
)

print(results_df)

# Biểu đồ so sánh metric
plot_metric_bar <- function(results_df, metric_name, better = c("higher", "lower")) {
  better <- match.arg(better)
  ggplot(results_df, aes(x = Algorithm, y = .data[[metric_name]], fill = Algorithm)) +
    geom_col() +
    geom_text(aes(label = round(.data[[metric_name]], 4)), vjust = -0.3, size = 3.5) +
    labs(
      title = paste0(metric_name, " (", ifelse(better == "higher", "càng lớn càng tốt", "càng nhỏ càng tốt"), ")"),
      x = "", y = metric_name
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1))
}

p_sil <- plot_metric_bar(results_df, "Silhouette", "higher")
p_dbi <- plot_metric_bar(results_df, "Davies_Bouldin", "lower")
p_chi <- plot_metric_bar(results_df, "Calinski_Harabasz", "higher")

print((p_sil / p_dbi) / p_chi)

# =========================
# 9. Trực quan hóa cụm
# =========================
cat("\n", strrep("=", 90), "\n")
cat("BƯỚC 7. TRỰC QUAN INSIGHT\n")
cat(strrep("=", 90), "\n")

p_km <- plot_cluster_pca(X_model, labels_kmeans, "K-Means - PCA Scatter")
p_hc <- plot_cluster_pca(X_model, labels_hc, "Hierarchical (Ward) - PCA Scatter")
p_db <- plot_cluster_pca(X_model, labels_db, "DBSCAN - PCA Scatter")
p_sp <- plot_cluster_pca(X_model, labels_spec, "Spectral - PCA Scatter")

print((p_km | p_hc) / (p_db | p_sp))

library(viridis)
library(ggplot2)
library(tidyr)
library(dplyr)

make_cluster_profile <- function(data, labels, n_top = 12, title = "Cluster Profile") {
  df_temp <- as.data.frame(data)
  df_temp$Cluster <- as.factor(labels)
  
  profile <- df_temp %>%
    group_by(Cluster) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(cols = -Cluster, names_to = "Feature", values_to = "MeanValue") %>%
    group_by(Cluster) %>%
    arrange(desc(abs(MeanValue))) %>% 
    # FIX TẠI ĐÂY: Sử dụng tên tham số n_top thay vì n
    slice_head(n = n_top) %>% 
    ungroup()
  
  # Vẽ biểu đồ Profile
  p <- ggplot(profile, aes(x = reorder(Feature, MeanValue), y = MeanValue, fill = Cluster)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~Cluster, scales = "free_y") +
    coord_flip() +
    labs(title = title, x = "Đặc trưng", y = "Giá trị trung bình (Scaled)") +
    theme_minimal()
  
  print(p)
  return(profile)
}

# Gọi hàm sau khi đã sửa
prof_km <- make_cluster_profile(df_scaled, labels_kmeans, n_top = 26, title = "K-Means - Top Cluster Profile")

# Heatmap profile
prof_km <- make_cluster_profile(df_scaled, labels_kmeans, n_top = 12, title = "K-Means - Top Cluster Profile")
prof_hc <- make_cluster_profile(df_scaled, labels_hc, n_top = 12, title = "Hierarchical - Top Cluster Profile")
prof_db <- make_cluster_profile(df_scaled, labels_db, n_top = 12, title = "DBSCAN - Top Cluster Profile")
prof_sp <- make_cluster_profile(df_scaled, labels_spec, n_top = 12, title = "Spectral - Top Cluster Profile")

print((prof_km$plot | prof_hc$plot) / (prof_db$plot | prof_sp$plot))


library(randomForest)
# Coi labels_spec là kết quả đúng, tìm xem cái gì tạo ra nó
rf_model <- randomForest(as.factor(labels_spec) ~ ., data = df_scaled)

# Vẽ biểu đồ quan trọng của biến
varImpPlot(rf_model, main = "Feature Importance - Spectral Clustering")

library(ggplot2)
library(tidyr)
library(dplyr)

plot_cluster_distribution <- function(data_scaled, labels, top_n = 12) {
  # 1. Chuẩn bị dữ liệu
  df_temp <- as.data.frame(data_scaled)
  df_temp$Cluster <- as.factor(labels)
  
  # 2. Tìm top_n feature có sự khác biệt trung bình giữa 2 cụm lớn nhất
  diff_features <- df_temp %>%
    group_by(Cluster) %>%
    summarise(across(where(is.numeric), mean), .groups = "drop") %>%
    pivot_longer(-Cluster, names_to = "feature", values_to = "mean_val") %>%
    group_by(feature) %>%
    summarise(diff = abs(diff(mean_val)), .groups = "drop") %>%
    arrange(desc(diff)) %>%
    slice_head(n = top_n) %>%
    pull(feature)
  
  # 3. Lọc dữ liệu theo top features và chuyển sang dạng long format
  df_long <- df_temp %>%
    select(Cluster, all_of(diff_features)) %>%
    pivot_longer(-Cluster, names_to = "Feature", values_to = "Value")
  
  # 4. Vẽ Boxplot để xem sự phân phối và các điểm dị biệt (outliers)
  p <- ggplot(df_long, aes(x = Cluster, y = Value, fill = Cluster)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    facet_wrap(~ Feature, scales = "free_y", ncol = 4) +
    scale_fill_manual(values = c("#E41A1C", "#377EB8")) + # Màu đỏ và xanh cho 2 cụm
    labs(title = "Phân phối 12 Đặc trưng phân hóa mạnh nhất giữa 2 cụm",
         subtitle = "Dữ liệu đã chuẩn hóa (Scaled)",
         x = "Cụm (Cluster)", y = "Giá trị") +
    theme_minimal() +
    theme(legend.position = "none", strip.text = element_text(face = "bold"))
  
  print(p)
}

# CHẠY LỆNH VẼ
plot_cluster_distribution(df_scaled, labels_spec, top_n = 12)








# =========================
# 10. Xuất kết quả
# =========================
cat("\n", strrep("=", 90), "\n")
cat("BƯỚC 8. XUẤT FILE KẾT QUẢ\n")
cat(strrep("=", 90), "\n")

cluster_output <- data.frame(
  row_id = 1:nrow(df_scaled),
  KMeans = labels_kmeans,
  Hierarchical_Ward = labels_hc,
  DBSCAN = labels_db,
  Spectral = labels_spec
)

if (!is.null(creator_id) && length(creator_id) == nrow(cluster_output)) {
  cluster_output <- cbind(CREATOR_ID = creator_id, cluster_output)
}

write.csv(cluster_output, "clustering_labels_comparison.csv", row.names = FALSE)
write.csv(results_df, "clustering_metrics_comparison.csv", row.names = FALSE)

write.csv(prof_km$profile_table, "cluster_profile_kmeans.csv", row.names = FALSE)
write.csv(prof_hc$profile_table, "cluster_profile_hierarchical.csv", row.names = FALSE)
write.csv(prof_db$profile_table, "cluster_profile_dbscan.csv", row.names = FALSE)
write.csv(prof_sp$profile_table, "cluster_profile_spectral.csv", row.names = FALSE)

cat("Đã xuất:\n")
cat("- clustering_labels_comparison.csv\n")
cat("- clustering_metrics_comparison.csv\n")
cat("- cluster_profile_kmeans.csv\n")
cat("- cluster_profile_hierarchical.csv\n")
cat("- cluster_profile_dbscan.csv\n")
cat("- cluster_profile_spectral.csv\n")

# =========================
# 11. Kết luận nhanh
# =========================
cat("\n", strrep("=", 90), "\n")
cat("KẾT LUẬN NHANH\n")
cat(strrep("=", 90), "\n")

best_sil <- results_df$Algorithm[which.max(results_df$Silhouette)]
best_dbi <- results_df$Algorithm[which.min(results_df$Davies_Bouldin)]
best_chi <- results_df$Algorithm[which.max(results_df$Calinski_Harabasz)]

cat("Model có Silhouette cao nhất:", best_sil, "\n")
cat("Model có Davies-Bouldin thấp nhất:", best_dbi, "\n")
cat("Model có Calinski-Harabasz cao nhất:", best_chi, "\n")

cat("\nLưu ý đọc kết quả:\n")
cat("- Silhouette cao hơn -> cụm tách biệt và chặt hơn.\n")
cat("- Davies-Bouldin thấp hơn -> cụm tốt hơn.\n")
cat("- Calinski-Harabasz cao hơn -> phân tách tốt hơn.\n")
cat("- DBSCAN có thể sinh noise (label = 0), đây không phải lỗi mà là đặc tính mô hình.\n")
cat("- K cuối cùng không nên chọn máy móc chỉ dựa vào 1 chỉ số; hãy kết hợp metric + trực quan + ý nghĩa business.\n")
