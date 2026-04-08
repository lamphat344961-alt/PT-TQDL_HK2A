# =========================================================
# 0) SETUP
# =========================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(stringr)
library(patchwork)
library(fmsb)

df <- read.csv("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/DA_cuoimon/data_for_vi.csv")



# Đổi tên cụm cho dễ đọc
df_plot <- df %>%
  mutate(
    Cluster = case_when(
      Spectral == 1 ~ "SP_1",
      Spectral == 2 ~ "SP_2",
      TRUE ~ paste0("SP_", Spectral)
    ),
    Cluster = factor(Cluster, levels = c("SP_1", "SP_2"))
  )

# ---------------------------------------------------------
# Kiểm tra cột cần dùng
# ---------------------------------------------------------
needed_cols <- c(
  "Cluster",
  "FOLLOWERS", "ENGAGEMENT", "MEAN_VIEWS_3M",
  "VIDEOS_PER_WEEK", "POSTING_CONSISTENCY",
  "COLLAB_SCORE", "VIRAL_MAGNITUDE",
  "CREATOR_TIER"
)

missing_cols <- setdiff(needed_cols, names(df_plot))
if (length(missing_cols) > 0) {
  stop("Thiếu các cột sau trong dữ liệu: ", paste(missing_cols, collapse = ", "))
}

# =========================================================
# 1) HEATMAP - MEDIAN CHUẨN HÓA 0-1 THEO CLUSTER
# =========================================================
# Ý nghĩa:
# - So sánh profile trung vị giữa các cụm
# - Chuẩn hóa từng metric về 0-1 để dễ nhìn pattern đối lập

heatmap_metrics <- c(
  "COLLAB_SCORE",
  "ENGAGEMENT",
  "FOLLOWERS",
  "MEAN_VIEWS_3M",
  "POSTING_CONSISTENCY",
  "VIDEOS_PER_WEEK",
  "VIRAL_MAGNITUDE"
)

heatmap_df <- df_plot %>%
  group_by(Cluster) %>%
  summarise(across(all_of(heatmap_metrics), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-Cluster, names_to = "Metric", values_to = "Median") %>%
  group_by(Metric) %>%
  mutate(
    Value_01 = ifelse(
      max(Median, na.rm = TRUE) == min(Median, na.rm = TRUE),
      0.5,
      (Median - min(Median, na.rm = TRUE)) / (max(Median, na.rm = TRUE) - min(Median, na.rm = TRUE))
    )
  ) %>%
  ungroup()

# Sắp xếp thứ tự metric để kể chuyện tốt hơn
metric_order <- c(
  "VQSCORE",          # nếu có thì sẽ tự hiện, nếu không có thì bỏ qua
  "VIRAL_MAGNITUDE",
  "VIDEOS_PER_WEEK",
  "POSTING_CONSISTENCY",
  "MEAN_VIEWS_3M",
  "FOLLOWERS",
  "ENGAGEMENT",
  "COLLAB_SCORE"
)

heatmap_df$Metric <- factor(
  heatmap_df$Metric,
  levels = rev(intersect(metric_order, unique(heatmap_df$Metric)))
)

p_heatmap <- ggplot(heatmap_df, aes(x = Cluster, y = Metric, fill = Value_01)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = round(Value_01, 2)), size = 4) +
  scale_fill_gradient2(
    low = "#2a9d8f",
    mid = "#f1f1f1",
    high = "#e63946",
    midpoint = 0.5,
    limits = c(0, 1),
    name = "Giá trị"
  ) +
  labs(
    title = "Heatmap: Median chuẩn hóa từng metric theo Cụm",
    x = "Cụm",
    y = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid = element_blank()
  )

print(p_heatmap)

library(ggplot2)
library(dplyr)
library(scales)

# tính median để annotate
median_df <- df_plot %>%
  group_by(Cluster) %>%
  summarise(med = median(FOLLOWERS, na.rm = TRUE))

p_views2 <- ggplot(df_plot, aes(x = Cluster, y = MEAN_VIEWS_3M, fill = Cluster)) +
  
  # boxplot chính
  geom_boxplot(alpha = 0.6, outlier.alpha = 0.2, width = 0.5) +
  
  # median point (rất quan trọng)
  geom_point(data = median_df, aes(x = Cluster, y = med),
             color = "black", size = 4) +
  
  # label median
  geom_text(data = median_df,
            aes(x = Cluster, y = med, label = round(med)),
            vjust = -1, size = 4) +
  
  scale_y_log10(
    labels = label_number(scale_cut = cut_si("")),
    breaks = log_breaks(n = 5)
  ) +
  
  labs(
    title = "Mean Views (3 tháng) theo Cluster",
    subtitle = "So sánh median và phân phối",
    x = "Cluster",
    y = "Views (log scale)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18)
  )

print(p_views2)


library(ggplot2)
library(dplyr)
library(scales)

median_df <- df_plot %>%
  group_by(Cluster) %>%
  summarise(med = median(FOLLOWERS, na.rm = TRUE), .groups = "drop")

p_followers2 <- ggplot(df_plot, aes(x = Cluster, y = FOLLOWERS, fill = Cluster)) +
  geom_boxplot(alpha = 0.6, outlier.alpha = 0.2, width = 0.5) +
  geom_point(
    data = median_df,
    aes(x = Cluster, y = med),
    color = "black",
    size = 4,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = median_df,
    aes(x = Cluster, y = med, label = label_number(scale_cut = cut_si(""))(med)),
    vjust = -1,
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_y_log10(
    labels = label_number(scale_cut = cut_si("")),
    breaks = log_breaks(n = 5)
  ) +
  labs(
    title = "Followers theo Cluster",
    subtitle = "So sánh median và phân phối",
    x = "Cluster",
    y = "Followers (log scale)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18)
  )

print(p_followers2)

library(ggplot2)
library(scales)
library(dplyr)

# xử lý tránh log lỗi
df_plot <- df_plot %>%
  mutate(PRICE_NUM = ifelse(PRICE_NUM <= 0, NA, PRICE_NUM))

p_price <- ggplot(df_plot, aes(x = Cluster, y = PRICE_NUM, fill = Cluster)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.25) +
  scale_y_log10(
    labels = label_number(scale_cut = cut_si("")),
    breaks = log_breaks(n = 5)
  ) +
  labs(
    title = "Price theo Cluster (log scale)",
    subtitle = "So sánh định giá giữa các nhóm creator",
    x = "Cluster",
    y = "Price"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

print(p_price)
# =========================================================
# 2) BOXPLOT - CONFIRM SEGMENTATION
# =========================================================
# 3 boxplot chính:
# - Followers
# - Engagement
# - Mean Views 3M

library(scales)

p_followers <- ggplot(df_plot, aes(x = Cluster, y = FOLLOWERS, fill = Cluster)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.35) +
  scale_y_log10(labels = label_number(scale_cut = cut_si(""))) +
  labs(
    title = "Followers",
    x = "Cụm",
    y = "Followers"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

p_engagement <- ggplot(df_plot, aes(x = Cluster, y =PRICE_NUM , fill = Cluster)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.35) +
  scale_y_log10(labels = label_number(scale_cut = cut_si(""))) +
  labs(
    title = "Price",
    x = "Cụm",
    y = "Price"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

p_views <- ggplot(df_plot, aes(x = Cluster, y = MEAN_VIEWS_3M, fill = Cluster)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.35) +
  scale_y_log10(labels = label_number(scale_cut = cut_si(""))) +
  labs(
    title = "Mean Views",
    x = "Cụm",
    y = "Views TB"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

(p_followers | p_engagement | p_views)

# =========================================================
# 3) SCATTER - FREQUENCY VS CONSISTENCY
# =========================================================
# Insight mong muốn:
# - SP_1: high frequency + high consistency
# - SP_2: organic pattern, consistency tăng dần theo frequency

p_freq_consistency <- ggplot(
  df_plot,
  aes(x = VIDEOS_PER_WEEK, y = POSTING_CONSISTENCY, color = Cluster)
) +
  geom_point(alpha = 0.45, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1) +
  labs(
    title = "Tần suất đăng vs Độ nhất quán",
    x = "Videos / Tuần",
    y = "Posting Consistency",
    color = "Cụm"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_freq_consistency)

# =========================================================
# 4) VIRAL PLOT
# =========================================================
# Có 2 cách:
# A. Boxplot Viral Magnitude để thấy spread
# B. Scatter với viral magnitude vs avg viral strength (nếu có AVG_VIRAL_STRENGTH)

p_viral_box <- ggplot(df_plot, aes(x = Cluster, y = VIRAL_MAGNITUDE, fill = Cluster)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.35) +
  labs(
    title = "Phân phối Viral Magnitude theo Cụm",
    x = "Cụm",
    y = "Viral Magnitude"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_viral_box)

# Nếu bạn có AVG_VIRAL_STRENGTH thì chạy thêm plot này:
if ("AVG_VIRAL_STRENGTH" %in% names(df_plot)) {
  p_viral_scatter <- ggplot(
    df_plot,
    aes(x = VIRAL_MAGNITUDE, y = AVG_VIRAL_STRENGTH, color = Cluster)
  ) +
    geom_point(alpha = 0.45, size = 2) +
    labs(
      title = "Viral Magnitude vs Avg Viral Strength",
      x = "Viral Magnitude",
      y = "Avg Viral Strength",
      color = "Cụm"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  print(p_viral_scatter)
}

# =========================================================
# 5) RADAR CHART - PROFILE TỔNG HỢP
# =========================================================
# Dùng median và chuẩn hóa 0-1 theo từng metric để nhìn profile đối lập

radar_metrics <- c(
  "FOLLOWERS",
  "ENGAGEMENT",
  "MEAN_VIEWS_3M",
  "VIDEOS_PER_WEEK",
  "POSTING_CONSISTENCY",
  "COLLAB_SCORE",
  "VIRAL_MAGNITUDE"
)

radar_df <- df_plot %>%
  group_by(Cluster) %>%
  summarise(across(all_of(radar_metrics), ~ median(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-Cluster, names_to = "Metric", values_to = "Median") %>%
  group_by(Metric) %>%
  mutate(
    Value_01 = ifelse(
      max(Median, na.rm = TRUE) == min(Median, na.rm = TRUE),
      0.5,
      (Median - min(Median, na.rm = TRUE)) / (max(Median, na.rm = TRUE) - min(Median, na.rm = TRUE))
    )
  ) %>%
  ungroup()

radar_wide <- radar_df %>%
  select(Cluster, Metric, Value_01) %>%
  pivot_wider(names_from = Metric, values_from = Value_01)

# fmsb yêu cầu 2 dòng đầu là max và min
radar_plot_df <- rbind(
  rep(1, ncol(radar_wide) - 1),
  rep(0, ncol(radar_wide) - 1),
  radar_wide %>% filter(Cluster == "SP_1") %>% select(-Cluster),
  radar_wide %>% filter(Cluster == "SP_2") %>% select(-Cluster)
)

colnames(radar_plot_df) <- colnames(radar_wide)[-1]
rownames(radar_plot_df) <- c("max", "min", "SP_1", "SP_2")

# Mở device mới để radar hiển thị rõ
op <- par(mar = c(2, 2, 3, 2))

radarchart(
  radar_plot_df,
  axistype = 1,
  pcol = c("#e63946", "#2a9d8f"),
  pfcol = c(alpha("#e63946", 0.20), alpha("#2a9d8f", 0.20)),
  plwd = 3,
  plty = 1,
  cglcol = "grey80",
  cglty = 1,
  cglwd = 1,
  axislabcol = "grey30",
  vlcex = 1.2,
  title = "Radar: Profile Cụm (Median chuẩn hóa 0-1)"
)

legend(
  "topright",
  legend = c("SP_1", "SP_2"),
  col = c("#e63946", "#2a9d8f"),
  lwd = 3,
  bty = "n",
  cex = 1.1
)

par(op)

# =========================================================
# 6) CREATOR TIER DISTRIBUTION
# =========================================================
# Mục tiêu:
# - cho thấy tier gần giống nhau giữa 2 cluster
# - follower tier không giải thích được segmentation

tier_df <- df_plot %>%
  filter(!is.na(CREATOR_TIER)) %>%
  mutate(
    CREATOR_TIER = factor(
      CREATOR_TIER,
      levels = c("Nano", "Micro", "Mid-tier", "Macro", "Mega")
    )
  ) %>%
  count(Cluster, CREATOR_TIER) %>%
  group_by(Cluster) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_tier <- ggplot(tier_df, aes(x = Cluster, y = prop, fill = CREATOR_TIER)) +
  geom_col(position = "fill", width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Phân phối Creator Tier theo Cụm",
    x = "Cụm",
    y = "Tỷ lệ",
    fill = "Tier"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_tier)

library(ggplot2)
library(scales)

p_scale_violin <- ggplot(df_plot, aes(x = Cluster, y = PRICE_NUM, fill = Cluster)) +
  geom_violin(alpha = 0.45, trim = FALSE) +
  geom_boxplot(width = 0.14, outlier.alpha = 0.2, alpha = 0.8) +
  scale_y_log10(
    labels = label_number(scale_cut = cut_si("")),
    breaks = log_breaks(n = 5)
  ) +
  labs(
    title = "Quy mô creator theo cụm",
    subtitle = "Phân phối PRICE thực tế",
    x = "Cụm",
    y = "PRICE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_scale_violin)

library(dplyr)
library(ggplot2)
library(scales)

scale_summary <- df_plot %>%
  group_by(Cluster) %>%
  summarise(
    median_followers = median(FOLLOWERS, na.rm = TRUE),
    q1 = quantile(FOLLOWERS, 0.25, na.rm = TRUE),
    q3 = quantile(FOLLOWERS, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p_scale_iqr <- ggplot(scale_summary, aes(x = Cluster, y = median_followers, color = Cluster)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.12, linewidth = 1.1) +
  scale_y_log10(
    labels = label_number(scale_cut = cut_si("")),
    breaks = log_breaks(n = 5)
  ) +
  labs(
    title = "Median follower scale theo cụm",
    subtitle = "Điểm là median, thanh dọc là IQR",
    x = "Cụm",
    y = "Followers (log scale)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_scale_iqr)

library(dplyr)
library(ggplot2)
library(scales)

df_scale_band <- df_plot %>%
  mutate(
    follower_band = case_when(
      FOLLOWERS < 5e4 ~ "<50K",
      FOLLOWERS < 1e5 ~ "50K-100K",
      FOLLOWERS < 5e5 ~ "100K-500K",
      FOLLOWERS < 1e6 ~ "500K-1M",
      TRUE ~ "1M+"
    ),
    follower_band = factor(
      follower_band,
      levels = c("<50K", "50K-100K", "100K-500K", "500K-1M", "1M+")
    )
  ) %>%
  count(Cluster, follower_band) %>%
  group_by(Cluster) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_scale_band <- ggplot(df_scale_band, aes(x = Cluster, y = prop, fill = follower_band)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Phân bố follower band theo cụm",
    subtitle = "Quy mô được chia trực tiếp từ followers",
    x = "Cụm",
    y = "Tỷ lệ",
    fill = "Follower band"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_scale_band)



p_scale_perf <- ggplot(df_plot, aes(x = FOLLOWERS, y = ENGAGEMENT, color = Cluster)) +
  geom_point(alpha = 0.35, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  scale_x_log10(
    labels = label_number(scale_cut = cut_si("")),
    breaks = log_breaks(n = 5)
  ) +
  labs(
    title = "Quy mô và hiệu quả theo cụm",
    x = "Followers (log scale)",
    y = "Engagement"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_scale_perf)


ggplot(df_plot, aes(x = Cluster, y = VIDEOS_PER_WEEK, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Productivity: Videos per Week")


ggplot(df_plot, aes(x = Cluster, y = FOLLOWERS, fill = Cluster)) +
  geom_violin() +
  scale_y_log10()

ggplot(df_plot, aes(x = Cluster, y = ENGAGEMENT, fill = Cluster)) +
  geom_boxplot()


ggplot(df_plot, aes(x = Cluster, y = MEAN_VIEWS_3M, fill = Cluster)) +
  geom_boxplot() +
  scale_y_log10()

ggplot(df_plot, aes(x = Cluster, y = COLLAB_SCORE, fill = Cluster)) +
  geom_boxplot() +
  scale_y_log10()


ggplot(df_plot, aes(x = Cluster, y = PRICE_NUM, fill = Cluster)) +
  geom_boxplot() +
  scale_y_log10()


df_plot <- df_plot %>%
  mutate(VALUE_SCORE = ENGAGEMENT / log1p(PRICE_NUM))

ggplot(df_plot, aes(x = Cluster, y = VALUE_SCORE, fill = Cluster)) +
  geom_boxplot()


library(dplyr)
library(ggplot2)
library(scales)

tier_df_stack <- df_plot %>%
  filter(!is.na(CREATOR_TIER)) %>%
  mutate(
    CREATOR_TIER = factor(
      CREATOR_TIER,
      levels = c("Nano", "Micro", "Mid-tier", "Macro", "Mega")
    )
  ) %>%
  count(CREATOR_TIER, Cluster) %>%
  group_by(CREATOR_TIER) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_tier_stack <- ggplot(tier_df_stack, aes(x = CREATOR_TIER, y = prop, fill = Cluster)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Phân bố Cluster trong từng Tier",
    x = "Creator Tier",
    y = "Tỷ lệ",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_tier_stack)

# =========================================================
# 7) PANEL GỘP (TÙY CHỌN) - TẠO MỘT HÌNH TỔNG HỢP
# =========================================================
panel_main <- (p_followers | p_engagement | p_views) /
  (p_freq_consistency | p_viral_box)

print(panel_main)

# =========================================================
# 8) BẢNG MEDIAN ĐỂ DIỄN GIẢI INSIGHT
# =========================================================
# Bảng này rất hữu ích để viết phần Results

summary_table <- df_plot %>%
  group_by(Cluster) %>%
  summarise(
    FOLLOWERS = median(FOLLOWERS, na.rm = TRUE),
    ENGAGEMENT = median(ENGAGEMENT, na.rm = TRUE),
    MEAN_VIEWS_3M = median(MEAN_VIEWS_3M, na.rm = TRUE),
    VIDEOS_PER_WEEK = median(VIDEOS_PER_WEEK, na.rm = TRUE),
    POSTING_CONSISTENCY = median(POSTING_CONSISTENCY, na.rm = TRUE),
    COLLAB_SCORE = median(COLLAB_SCORE, na.rm = TRUE),
    VIRAL_MAGNITUDE = median(VIRAL_MAGNITUDE, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_table)