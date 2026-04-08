# ============================================================
# PHÂN TÍCH INSIGHT THEO CỤM CREATOR
# ============================================================
# File 1: clustering_labels_comparison.csv  — nhãn cụm (KMeans, Hierarchical, DBSCAN, Spectral)
# File 2: creator_features_final4.csv       — đặc trưng creator
# ============================================================

setwd("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/DA_cuoimon")

library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)   # ghép nhiều plot
library(dplyr)
# -----------------------------------------------------------
# 1. ĐỌC DỮ LIỆU
# -----------------------------------------------------------
labels   <- read.csv("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/DA_cuoimon/clustering_labels_comparison.csv")

features <- read.csv("C:\\Users\\Admin\\Desktop\\TANPHAT\\Dự án_canhan\\TIKTOK_V2\\clustering\\creator_features_final4.csv")

glimpse(labels)
glimpse(features)

# -----------------------------------------------------------
# 2. GỘP DỮ LIỆU
# -----------------------------------------------------------
df <- labels %>%
  left_join(features, by = "CREATOR_ID")

cat("Số dòng sau join:", nrow(df), "\n")
cat("Số dòng mất (không khớp):", nrow(labels) - nrow(df), "\n")

names(df)
library(dplyr)

# Cách 1: Chỉ định tên cột muốn bỏ
df <- df %>% 
  select(-KMeans, -Hierarchical_Ward, -DBSCAN,-row_id,-VIDEO_ID,-VIEW_COUNT,-LIKE_COUNT,
        -CATEGORY_TYPE, -COMMENT_COUNT,-SHARE_COUNT,-CREATE_TIME,-SAVE_COUNT,-hour,-VIDEO_COUNT_3M_y,-AVG_VIRAL_STRENGTH,-VIDEO_COUNT_y,-VIDEO_COUNT_x,-DAY_OF_WEEK,-HAS_SHOP_LINK )


write.csv(df, file ='data_for_vi.csv')

head(df, 20)

# Chuyển nhãn cụm thành factor có tên rõ ràng
df <- df %>%
  mutate(
    KMeans_f            = factor(KMeans,            labels = paste0("KM_", sort(unique(KMeans)))),
    Hierarchical_Ward_f = factor(Hierarchical_Ward, labels = paste0("HW_", sort(unique(Hierarchical_Ward)))),
    DBSCAN_f            = factor(DBSCAN,            labels = paste0("DB_", sort(unique(DBSCAN)))),
    Spectral_f          = factor(Spectral,          labels = paste0("SP_", sort(unique(Spectral))))
  )

# -----------------------------------------------------------
# 3. CHỌN THUẬT TOÁN CHÍNH ĐỂ PHÂN TÍCH (KMeans làm mặc định)
#    Đổi biến này nếu muốn dùng thuật toán khác
# -----------------------------------------------------------
CLUSTER_COL <- "Spectral_f"   # hoặc "Hierarchical_Ward_f", "Spectral_f", "DBSCAN_f"

df <- df %>% mutate(cluster = .data[[CLUSTER_COL]])

# -----------------------------------------------------------
# 4. THỐNG KÊ MÔ TẢ THEO CỤM
# -----------------------------------------------------------

## 4a. Số creator mỗi cụm + phân phối tier
cat("\n========== PHÂN BỔ CREATOR THEO CỤM ==========\n")

tier_by_cluster <- df %>%
  count(cluster, CREATOR_TIER) %>%
  group_by(cluster) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

print(tier_by_cluster)

## 4b. Bảng tổng hợp metrics chính theo cụm
key_metrics <- c(
  "FOLLOWERS", "ENGAGEMENT",
  "MEAN_VIEWS_3M", "MEAN_LIKES_3M", "MEAN_COMMENTS_3M", "MEAN_SHARES_3M",
  "MEAN_VIEWS_RATE_3M", "MEAN_LIKE_RATE_3M",
  "VIDEOS_PER_WEEK", "POSTING_CONSISTENCY",
  "VIRAL_MAGNITUDE", "AVG_VIRAL_STRENGTH",
  "COLLAB_SCORE", "VQSCORE",
  "PRICE_NUM", "HAS_SHOP_LINK"
)

cluster_summary <- df %>%
  group_by(cluster) %>%
  summarise(
    n_creators         = n(),
    across(
      all_of(key_metrics),
      list(
        median = ~median(.x, na.rm = TRUE),
        mean   = ~mean(.x,   na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  )

# Hiển thị dạng dọc cho dễ đọc
cluster_long <- cluster_summary %>%
  pivot_longer(
    cols      = -c(cluster, n_creators),
    names_to  = c("metric", "stat"),
    names_sep = "__"
  ) %>%
  pivot_wider(names_from = cluster, values_from = value)

cat("\n========== BẢNG MEDIAN THEO CỤM ==========\n")
cluster_long %>%
  filter(stat == "median") %>%
  select(-stat) %>%
  print(n = Inf)

# -----------------------------------------------------------
# 5. PROFILE TỪNG CỤM (tóm tắt tự động)
# -----------------------------------------------------------
cat("\n========== PROFILE CỤM ==========\n")

ref <- cluster_summary %>%
  select(cluster, n_creators, ends_with("__median")) %>%
  rename_with(~str_remove(.x, "__median"), ends_with("__median"))

# So sánh với median toàn bộ dataset
global_median <- df %>%
  summarise(across(all_of(key_metrics), ~median(.x, na.rm = TRUE))) %>%
  mutate(cluster = "OVERALL", n_creators = nrow(df))

combined <- bind_rows(ref, global_median)
print(combined)

# -----------------------------------------------------------
# 6. TRỰC QUAN HÓA
# -----------------------------------------------------------

theme_set(theme_minimal(base_size = 12))
pal <- c("#E63946", "#2A9D8F", "#E9C46A", "#264653", "#F4A261")  # tối đa 5 cụm

## 6a. Phân bổ tier trong từng cụm
p_tier <- tier_by_cluster %>%
  ggplot(aes(x = cluster, y = pct, fill = CREATOR_TIER)) +
  geom_col(position = "fill", width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Phân phối Creator Tier theo Cụm",
       x = "Cụm", y = "Tỷ lệ", fill = "Tier") +
  theme(legend.position = "bottom")

## 6b. Boxplot followers
p_followers <- df %>%
  ggplot(aes(x = cluster, y = FOLLOWERS, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = pal) +
  labs(title = "Followers (log scale)", x = "Cụm", y = "Followers") +
  theme(legend.position = "none")

## 6c. Engagement Rate
p_eng <- df %>%
  ggplot(aes(x = cluster, y = ENGAGEMENT, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = pal) +
  labs(title = "Engagement Rate (%)", x = "Cụm", y = "Engagement") +
  theme(legend.position = "none")

## 6d. Mean Views 3M
p_views <- df %>%
  ggplot(aes(x = cluster, y = MEAN_VIEWS_3M, fill = cluster)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = pal) +
  labs(title = "Mean Views 3 Tháng Gần Nhất (log)", x = "Cụm", y = "Views TB") +
  theme(legend.position = "none")

## 6e. Videos / tuần vs Posting Consistency
p_post <- df %>%
  ggplot(aes(x = VIDEOS_PER_WEEK, y = POSTING_CONSISTENCY, color = cluster)) +
  geom_point(alpha = 0.4, size = 1.2) +
  scale_color_manual(values = pal) +
  labs(title = "Tần suất đăng vs Độ nhất quán",
       x = "Videos / Tuần", y = "Posting Consistency",
       color = "Cụm") +
  theme(legend.position = "bottom")

## 6f. Viral Magnitude vs Avg Viral Strength
p_viral <- df %>%
  ggplot(aes(x = VIRAL_MAGNITUDE, y = AVG_VIRAL_STRENGTH, color = cluster)) +
  geom_point(alpha = 0.4, size = 1.2) +
  scale_color_manual(values = pal) +
  labs(title = "Viral Magnitude vs Avg Viral Strength",
       x = "Viral Magnitude", y = "Avg Viral Strength",
       color = "Cụm") +
  theme(legend.position = "bottom")

## 6g. Radar chart (spider) — profile so sánh cụm
# Chuẩn hóa 0-1 để vẽ radar
radar_metrics <- c("FOLLOWERS", "ENGAGEMENT", "MEAN_VIEWS_3M",
                   "VIDEOS_PER_WEEK", "POSTING_CONSISTENCY",
                   "VIRAL_MAGNITUDE", "COLLAB_SCORE", "VQSCORE")

radar_df <- df %>%
  group_by(cluster) %>%
  summarise(across(all_of(radar_metrics), ~median(.x, na.rm = TRUE))) %>%
  mutate(across(-cluster, ~rescale(.x)))  # chuẩn hóa min-max

radar_long <- radar_df %>%
  pivot_longer(-cluster, names_to = "metric", values_to = "value")

p_radar <- radar_long %>%
  ggplot(aes(x = metric, y = value, color = cluster, group = cluster)) +
  geom_line(size = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = pal) +
  coord_polar() +
  labs(title = "Radar: Profile Cụm (Median chuẩn hóa 0-1)",
       x = NULL, y = NULL, color = "Cụm") +
  theme(axis.text.x = element_text(size = 9))

## 6h. Heatmap: median metric ~ cụm
heatmap_df <- radar_long %>%
  group_by(cluster) %>%
  mutate(z = scale(value)[,1])   # chuẩn hóa z theo metric

p_heat <- heatmap_df %>%
  ggplot(aes(x = cluster, y = metric, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "#2A9D8F", mid = "white", high = "#E63946",
                       midpoint = 0.5) +
  labs(title = "Heatmap: Median chuẩn hóa từng metric theo Cụm",
       x = "Cụm", y = "Metric", fill = "Giá trị") +
  theme(axis.text.x = element_text(angle = 0))

# -----------------------------------------------------------
# 7. LƯU PLOTS
# -----------------------------------------------------------

# Ghép panel chính
panel_main <- (p_followers | p_eng | p_views) /
  (p_post       | p_viral)

ggsave("cluster_panel_main.png",  panel_main, width = 14, height = 9,  dpi = 150)
ggsave("cluster_tier_dist.png",   p_tier,     width = 7,  height = 5,  dpi = 150)
ggsave("cluster_radar.png",       p_radar,    width = 7,  height = 7,  dpi = 150)
ggsave("cluster_heatmap.png",     p_heat,     width = 8,  height = 6,  dpi = 150)

cat("\n✅ Đã lưu 4 file ảnh:\n",
    "  cluster_panel_main.png\n",
    "  cluster_tier_dist.png\n",
    "  cluster_radar.png\n",
    "  cluster_heatmap.png\n")

# -----------------------------------------------------------
# 8. SO SÁNH 4 THUẬT TOÁN CLUSTERING (Agreement Matrix)
# -----------------------------------------------------------
cat("\n========== AGREEMENT GIỮA CÁC THUẬT TOÁN ==========\n")

# Tỷ lệ creator cùng cụm trong KMeans vs Spectral (ví dụ)
agreement_table <- table(KMeans = df$KMeans, Spectral = df$Spectral)
cat("KMeans vs Spectral:\n")
print(agreement_table)

# Có thể tính Adjusted Rand Index với package mclust
# library(mclust)
# adjustedRandIndex(df$KMeans, df$Spectral)

# -----------------------------------------------------------
# 9. XUẤT BẢNG TỔNG HỢP RA CSV
# -----------------------------------------------------------
write_csv(cluster_long, "cluster_summary_table.csv")
cat("\n✅ Đã lưu bảng tổng hợp: cluster_summary_table.csv\n")

# Xuất dataframe đã gộp
write_csv(df, "creator_with_clusters.csv")
cat("✅ Đã lưu file gộp: creator_with_clusters.csv\n")

# -----------------------------------------------------------
# 10. PHÂN TÍCH THÊM: TOP FEATURE PHÂN BIỆT CỤM (ANOVA / KW)
# -----------------------------------------------------------
cat("\n========== TOP FEATURES PHÂN BIỆT CỤM (Kruskal-Wallis) ==========\n")

numeric_cols <- df %>%
  select(all_of(key_metrics)) %>%
  names()

kw_results <- map_dfr(numeric_cols, function(col) {
  tryCatch({
    test <- kruskal.test(df[[col]] ~ df$cluster)
    tibble(feature = col, p_value = test$p.value, statistic = test$statistic)
  }, error = function(e) tibble(feature = col, p_value = NA, statistic = NA))
})

kw_results <- kw_results %>%
  arrange(p_value) %>%
  mutate(significant = p_value < 0.05)

print(kw_results, n = Inf)