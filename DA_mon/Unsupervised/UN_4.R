# ==============================================================================
# 1. THIẾT LẬP MÔI TRƯỜNG & ĐỌC DỮ LIỆU
# ==============================================================================
setwd("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/DA_cuoimon")

data <- read.csv("C:\Users\Admin\Desktop\TANPHAT\\Dự án_canhan\\TIKTOK_V2\\clustering\\dataset\\creator_features_final2.csv")

names(data)
cat("=== Cấu trúc dữ liệu gốc ===\n")
cat("Rows:", nrow(data), "| Cols:", ncol(data), "\n")

# ==============================================================================
# 2. LÀM SẠCH DỮ LIỆU
# ==============================================================================

# 2.1. Loại creator không có video (VIDEO_COUNT_x = NA → inactive)
data <- data[!is.na(data$VIDEO_COUNT_x), ]

# 2.2. Loại creator bất thường
data <- data[data$CREATOR_ID != "nhuhexii", ]

# 2.3. Fill NA có chủ đích
# VIEW_STD, VIEW_CV = 0: đúng toán học (1 video không có variance)
data$VIEW_STD[is.na(data$VIEW_STD)] <- 0
data$VIEW_CV[is.na(data$VIEW_CV)]   <- 0

# AVG_POST_GAP = mean: creator 1 video chưa có thói quen đăng bài
data$AVG_POST_GAP[is.na(data$AVG_POST_GAP)] <- mean(data$AVG_POST_GAP, na.rm = TRUE)

# 2.4. Fill mean cho RATE khi FOLLOWERS = 0
rate_cols <- c("MEAN_VIEWS_RATE_3M", "MEAN_LIKE_RATE_3M",
               "MEAN_COMMENT_RATE_3M", "MEAN_SHARE_RATE_3M", "MEAN_SAVE_RATE_3M")
for (col in rate_cols) {
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}

# 2.5. Kiểm tra NA sau xử lý
na_check <- colSums(is.na(data))
remaining_na <- na_check[na_check > 0]
if (length(remaining_na) == 0) {
  cat("NA:  không còn NA\n")
} else {
  cat("Các cột còn NA:\n")
  print(remaining_na)
}

# ==============================================================================
# 3. THỐNG KÊ & TRỰC QUAN HÓA TRƯỚC TRANSFORM
# ==============================================================================

if (!require("moments"))   install.packages("moments")
if (!require("ggplot2"))   install.packages("ggplot2")
if (!require("tidyr"))     install.packages("tidyr")
if (!require("ggcorrplot")) install.packages("ggcorrplot")
library(moments); library(ggplot2); library(tidyr); library(ggcorrplot)

# Loại cột không phải số và cột định danh khỏi data_final để thống kê và ko phải level creator
cols_remove_final <- c(
  "CREATOR_ID", "VIDEO_ID", "CREATE_TIME", "CATEGORY_y",
  "first_post", "last_post", "VIDEO_COUNT_y",
  "HAS_SHOP_LINK", "BROADCAST_SCORE", "CATEGORY_TYPE","COMMENT_COUNT", 
  "SAVE_COUNT", "SHARE_COUNT", "LIKE_COUNT", "VIEW_COUNT"
)
data_final <- data[, !(names(data) %in% cols_remove_final)]

cat("\n=== Các cột đưa vào phân tích (trước transform) ===\n")
print(names(data_final))


# 3.2. Histogram trước transform
pivot_longer(data_final, cols = everything(),
             names_to = "variable", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white", linewidth = 0.2) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histogram trước transform", x = "Giá trị", y = "Tần suất") +
  theme(strip.text = element_text(size = 6.5))

ggsave("histogram_before_transform.png", width = 18, height = 14, units = "in", dpi = 150)

# 3.4. Correlation matrix trước transform
corr_matrix <- cor(data_final, use = "complete.obs")
ggcorrplot(corr_matrix,
           hc.order = TRUE, type = "lower", lab = TRUE,
           lab_size = 1.5, method = "square",
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Correlation Matrix - TikTok Creator Features",
           tl.cex = 6, tl.srt = 90, digits = 1) +
  theme(
    axis.text.x = element_text(margin = margin(t = -5)),
    axis.text.y = element_text(margin = margin(r = -5)),
    plot.title  = element_text(size = 10, hjust = 0.5)
  )
# ggsave("correlation_clean.png", width = 15, height = 12, units = "in", dpi = 300)

cols_to_remove <- c("unique_days", "VIDEO_COUNT_x", "P90_VIEWS_3M", 
                    "MEAN_VIEWS_3M", "MAX_VIEWS_3M", "VIEW_MEAN",
                    "VIDEO_COUNT_3M_x","VIDEO_COUNT_3M_y","TOTAL_LIKES")

# Loại bỏ bằng cách dùng dấu ngoặc vuông [ , ]
data_final_2 <- data_final[, !(names(data_final) %in% cols_to_remove)]

names(data_final_2)

corr_matrix_2 <- cor(data_final_2, use = "complete.obs")
ggcorrplot(corr_matrix_2,
           hc.order = TRUE, type = "lower", lab = TRUE,
           lab_size = 1.5, method = "square",
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Correlation Matrix - TikTok Creator Features",
           tl.cex = 6, tl.srt = 90, digits = 1) +
  theme(
    axis.text.x = element_text(margin = margin(t = -5)),
    axis.text.y = element_text(margin = margin(r = -5)),
    plot.title  = element_text(size = 10, hjust = 0.5)
  )

# 3.3. Thống kê skew / ratio_99 / ratio_out / zero_pct
num_cols <- names(data_final_2)[sapply(data_final_2, is.numeric)]
num_cols <- setdiff(num_cols, c("HAS_SHOP_LINK", "HAS_BROADCAST_SCORE"))

stats_df <- do.call(rbind, lapply(num_cols, function(col) {
  x <- data[[col]]
  x <- x[!is.na(x)]
  
  q50  <- quantile(x, 0.50)
  q90  <- quantile(x, 0.90)
  q99  <- quantile(x, 0.99)
  q999 <- quantile(x, 0.999)
  
  ratio_90  <- ifelse(q50 > 0, q90  / q50, NA)
  ratio_99  <- ifelse(q50 > 0, q99  / q50, NA)
  ratio_out <- ifelse(q99 > 0, q999 / q99, NA)
  iqr_med   <- ifelse(median(x) > 0, IQR(x) / median(x), NA)
  zero_pct  <- mean(x == 0)
  skew      <- skewness(x)
  
  data.frame(
    col        = col,
    skewness   = round(skew, 2),
    ratio_90   = round(ratio_90, 1),
    ratio_99   = round(ratio_99, 1),
    ratio_out  = round(ratio_out, 2),
    zero_pct   = round(zero_pct * 100, 1),
    iqr_med    = round(iqr_med, 2)
  )
}))

print(stats_df, row.names = FALSE)
cat("\n=== Thống kê phân phối (dùng để quyết định transform) ===\n")
print(stats_df[order(-stats_df$ratio_out), ], row.names = FALSE)

# write.csv(stats_df, "stats_df_2.csv", row.names = FALSE)



# ==============================================================================
# 5. TRANSFORM & FEATURE SELECTION
# Rule: ratio_99 > 20 → log1p | 5–20 → sqrt | ≤5 → giữ
# Ratio_90 và Ratio_99 so với phân vị, nếu quá cao thì chứng tỏ các giá trị nhỏ chiếm đa số

# Ratio_out (Tỷ lệ ngoại lệ - Outliers)
#       ratio_out > 10 → winsorize p=0.95
#       ratio_out 5–10 → winsorize p=0.98
#       ratio_out 3–5  → winsorize p=0.99
# ==============================================================================

winsorize <- function(x, p) pmin(x, quantile(x, p, na.rm = TRUE))
transform_sqrt <- function(x) {
  return(sqrt(x))
}

# 2. Hàm Căn bậc ba (Cube Root)
# Cách dùng x^(1/3) trong R với số âm thường bị lỗi NaN, 
# nên dùng hàm sign() để giữ dấu.
transform_cbrt <- function(x) {
  return(sign(x) * abs(x)^(1/3))
}
# ------------------------------------------------------------------------------
# 5.1. LOG1P + WINSORIZE p=0.98 — ratio_99 > 20, ratio_out 5–10

# MEAN_SHARES_3M   r99=242.5 rout=6.83
# MAX_VIRAL_STRENGTH r99=56.9  rout=7.26
# MEAN_VIEWS_RATE_3M r99=53.8 rout=5.67
# MEAN_COMMENTS_3M r99=20.8  rout=6.22
# ------------------------------------------------------------------------------
cols_log_w98 <- c(
  "MEAN_SHARES_3M", "MAX_VIRAL_STRENGTH",
  "MEAN_VIEWS_RATE_3M", "MEAN_COMMENTS_3M"
)
for (col in cols_log_w98) data[[col]] <- transform_cbrt(winsorize(data[[col]], 0.95))

# ------------------------------------------------------------------------------
# 5.2. LOG1P + WINSORIZE p=0.99 — ratio_99 > 20, ratio_out 3–5
# P90_SHARES_3M  r99=164.3 rout=3.32
# MAX_SHARES_3M  r99=146.6 rout=4.06
# MEAN_SAVES_3M r99=50.8 rout=3.37
# MEAN_LIKES_3M r99=46.7 rout=3.48
# MAX_COMMENTS_3M r99=46.6 rout=3.67
# P90_SAVES_3M   r99=34.3  rout=3.06
# VIEW_P90      22.8      3.15
# VIEW_MEAN_V    32.3      3.77
# ------------------------------------------------------------------------------

names(data_final_2)

cols_log_w99 <- c(
  "P90_SHARES_3M", "MAX_SHARES_3M",
  "MEAN_SAVES_3M", "MEAN_LIKES_3M", "MAX_COMMENTS_3M",
  "P90_SAVES_3M","VIEW_P90","VIEW_MEAN_V"
)
for (col in cols_log_w99) data[[col]] <- transform_cbrt(winsorize(data[[col]], 0.95))

# ------------------------------------------------------------------------------
# 5.3. LOG1P THUẦN — ratio_99 > 20, ratio_out <= 3
# FOLLOWING_COUNT r99=108.8 | DIGG_COUNT r99=37.5
# MEAN_SHARE_RATE_3M r99=37.8 | P90_LIKES_3M r99=32.2
# MAX_SAVES_3M r99=31.6 | FOLLOWERS r99=30.9
# P90_COMMENTS_3M r99=29.5 | MAX_LIKES_3M r99=25.3
# VIEW_STD r99=21.5
# ------------------------------------------------------------------------------
cols_log <- c(
  "FOLLOWING_COUNT",  "MEAN_SHARE_RATE_3M",
  "P90_LIKES_3M", "MAX_SAVES_3M", "FOLLOWERS",
  "P90_COMMENTS_3M", "MAX_LIKES_3M", "VIEW_STD"
)
for (col in cols_log) data[[col]] <- transform_cbrt(data[[col]])

# ------------------------------------------------------------------------------
# 5.4. SQRT + WINSORIZE p=0.98 — 5 < ratio_99 <= 20, ratio_out 5–10
# MEAN_COMMENT_RATE_3M r99=15.0 rout=8.48
# AVG_VIRAL_STRENGTH     r99=13.4 rout=7.07
# ------------------------------------------------------------------------------
cols_sqrt_w98 <- c("MEAN_COMMENT_RATE_3M", "AVG_VIRAL_STRENGTH")

for (col in cols_sqrt_w98) data[[col]] <- winsorize(data[[col]], 0.90)

# ------------------------------------------------------------------------------
# 5.5. SQRT + WINSORIZE p=0.99 — 5 < ratio_99 <= 20, ratio_out 3–5
# VIRAL_MAGNITUDE r99=11.8 rout=4.59
# ------------------------------------------------------------------------------
cols_sqrt_w99 <- c("VIRAL_MAGNITUDE")
for (col in cols_sqrt_w99) data[[col]] <- winsorize(data[[col]], 0.95)

# ------------------------------------------------------------------------------
# 5.6. SQRT THUẦN — 5 < ratio_99 <= 20, ratio_out <= 3
# VIDEO_COUNT r99=16.4 | AVG_POST_GAP r99=13.7
# MEAN_SAVE_RATE_3M r99=12.0
# MEAN_LIKE_RATE_3M r99=5.5 | 
#VIDEOS_PER_WEEK      13.2      1.00 

# ------------------------------------------------------------------------------
cols_sqrt <- c(
  "VIDEO_COUNT"
)
for (col in cols_sqrt) data[[col]] <- sqrt(data[[col]])

# ------------------------------------------------------------------------------
# 5.7. GIỮ NGUYÊN — ratio_99 <= 5
# ENGAGEMENT r99=4.8 | VIEW_CV r99=3.8
# POSTING_CONSISTENCY r99=2.9 | COLLAB_SCORE r99=1.2
# HAS_BROADCAST_SCORE: binary 0/1
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 5.8. FEATURE SET CUỐI
# Loại không hợp lệ : PRICE_NUM (rout=16.37 cực mạnh, không cứu được)
# Loại corr ~1.0    : VIEW_MEAN_V, VIEW_P90, VIDEOS_PER_WEEK
# Loại corr > 0.9   : AVG_VIRAL_STRENGTH (↔ VIRAL_MAGNITUDE)
# ------------------------------------------------------------------------------
feature_cols <- c(
  # Scale & reach
  "FOLLOWERS",  
  "VIDEO_COUNT", "VIDEO_COUNT_3M_x",
  
  # View performance
  "VIEW_STD", "VIEW_CV","VIEW_MEAN_V" ,
  
  # Likes
  "MEAN_LIKES_3M",
  
  # Comments
  "MEAN_COMMENTS_3M", 
  
  # Shares
  "MEAN_SHARES_3M", 
  
  # Saves
  "MEAN_SAVES_3M", 
  
  
  # Posting behavior
  "AVG_POST_GAP", "POSTING_CONSISTENCY","VIDEOS_PER_WEEK",
  
  # Virality
  "VIRAL_MAGNITUDE", "MAX_VIRAL_STRENGTH",

)

data_cluster <- data[, c("CREATOR_ID", feature_cols)]

names(data_cluster)


# ------------------------------------------------------------------------------
# 5.9. KIỂM TRA
# ------------------------------------------------------------------------------
num_cols2 <- names(data_cluster)[sapply(data_cluster, is.numeric)]
num_cols2 <- setdiff(num_cols, c("HAS_SHOP_LINK", "HAS_BROADCAST_SCORE"))

stats_df_2 <- do.call(rbind, lapply(num_cols2, function(col) {
  x <- data[[col]]
  x <- x[!is.na(x)]
  
  q50  <- quantile(x, 0.50)
  q90  <- quantile(x, 0.90)
  q99  <- quantile(x, 0.99)
  q999 <- quantile(x, 0.999)
  
  ratio_90  <- ifelse(q50 > 0, q90  / q50, NA)
  ratio_99  <- ifelse(q50 > 0, q99  / q50, NA)
  ratio_out <- ifelse(q99 > 0, q999 / q99, NA)
  iqr_med   <- ifelse(median(x) > 0, IQR(x) / median(x), NA)
  zero_pct  <- mean(x == 0)
  skew      <- skewness(x)
  
  data.frame(
    col        = col,
    skewness   = round(skew, 2),
    ratio_90   = round(ratio_90, 1),
    ratio_99   = round(ratio_99, 1),
    ratio_out  = round(ratio_out, 2),
    zero_pct   = round(zero_pct * 100, 1),
    iqr_med    = round(iqr_med, 2)
  )
}))

print(stats_df_2, row.names = FALSE)
cat("\n=== Thống kê phân phối (dùng để quyết định transform) ===\n")
print(stats_df_2[order(-stats_df_2$ratio_out), ], row.names = FALSE)

# ------------------------------------------------------------------------------
# 5.10. HISTOGRAM SAU TRANSFORM
# ------------------------------------------------------------------------------
library(ggplot2); library(tidyr)
pivot_longer(data_cluster[, feature_cols], cols = everything(),
             names_to = "variable", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "#5DCAA5", color = "white", linewidth = 0.2) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histogram sau transform", x = "Giá trị", y = "Tần suất") +
  theme(strip.text = element_text(size = 6.5))
ggsave("histogram_after_transform.png", width = 18, height = 14, units = "in", dpi = 150)

cat("Done. Features:", length(feature_cols), "\n")



write.csv(data_cluster, "data_clustering_final5.csv", row.names = FALSE)