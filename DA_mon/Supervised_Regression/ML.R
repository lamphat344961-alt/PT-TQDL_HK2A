# ==============================================================================
# PHẦN 1: CÀI ĐẶT CÁC THƯ VIỆN (Chỉ cài nếu máy chưa có)
# ==============================================================================
required_packages <- c(
  "tidyverse",   # Xử lý dữ liệu (dplyr, readr) & Trực quan hóa (ggplot2)
  "tidymodels",  # Framework ML chính (tương đương scikit-learn)
  "glmnet",      # Cho Ridge và Lasso Regression
  "rpart",       # Cho Decision Tree
  "ranger",      # Cho Random Forest (tối ưu tốc độ)
  "xgboost",     # Cho XGBoost
  "patchwork",   # Hỗ trợ sắp xếp biểu đồ
  "vip"          # Xem độ quan trọng của các biến (Feature Importance)
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# ==============================================================================
# PHẦN 2: KHAI BÁO THƯ VIỆN (Tương đương đoạn import trong Python)
# ==============================================================================

# 1. Xử lý dữ liệu và Tiện ích
library(tidyverse)   # Tương đương: import pandas, numpy
library(magrittr)    # Tương đương: hỗ trợ pipeline/pipe
options(warn = -1)   # Tương đương: warnings.filterwarnings('ignore')

# 2. Trực quan hóa dữ liệu
library(ggplot2)     # Tương đương: import matplotlib, seaborn
library(patchwork)   # Tiện ích bổ trợ cho trực quan hóa

# 3. Tiền xử lý và Xây dựng Pipeline (Tidymodels)
library(tidymodels)  # Bao gồm: rsample (split), recipes (preprocess), workflows (pipeline)
library(recipes)     # Tương đương: ColumnTransformer, Scaler, Imputer, Encoder

# 4. Các mô hình hồi quy (Engines)
library(glmnet)      # Tương đương: LinearRegression, Ridge, Lasso
library(rpart)       # Tương đương: DecisionTreeRegressor
library(ranger)      # Tương đương: RandomForestRegressor
library(xgboost)     # Tương đương: XGBRegressor
library(readr)
# 5. Đánh giá mô hình (Metrics)
# Đã nằm trong tidymodels (yardstick), dùng hàm metric_set() để gọi
# Tương đương: mean_absolute_error, mean_squared_error, r2_score

# Thông báo hoàn tất
cat("\n--- Đã cài đặt và load tất cả thư viện thành công! ---\n")

# Đọc dataset 
# Đọc file CSV
df <- read_csv("C:/Users/Admin/Desktop/PAPERs/ML/PT-TQDL_HK2A/DA_cuoimon/Python/job_salary_prediction_dataset.csv")
# Xem cấu trúc dữ liệu (tương đương df.info() trong Python)


# Xem kích thước dữ liệu (Rows, Columns)
cat("Dataset Shape:\n")
dim(df)

# Xem thông tin chi tiết cấu trúc dữ liệu
cat("\nDataset Info:\n")
glimpse(df)

# Kiểm tra missing value 
# Kiểm tra giá trị thiếu trên từng cột
cat("\nMissing Values per Column:\n")
colSums(is.na(df))

# Kiểm tra tổng số hàng bị trùng lặp
cat("\nDuplicated Rows:\n")
sum(duplicated(df))

# Thống kê mô tả cho các biến số
cat("\nNumerical Features Summary Stats:\n")
summary(df)

# TRỰC QUAN HÓA BẰNG BOXPLOT  
    # Thiết lập danh sách cột số
num_cols <- c('experience_years', 'skills_count', 'certifications', 'salary')

    # Vẽ Boxplot cho các cột số
df %>%
  select(all_of(num_cols)) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "value") %>%
  ggplot(aes(x = "", y = value)) +
  geom_boxplot(fill = "skyblue", outlier.size = 2) +
  facet_wrap(~column, scales = "free", ncol = 2) +
  labs(title = "Boxplots of Numerical Features", x = NULL, y = "Value") +
  theme_minimal()


# == 2. KIỂM TRA PHÂN PHỐI LƯƠNG (SKEWNESS )
#---------
  # Vẽ biểu đồ Histogram kèm đường KDE (mật độ) và các đường chỉ số trung bình/trung vị.

# Tính toán trung bình và trung vị
salary_mean <- mean(df$salary, na.rm = TRUE)
salary_median <- median(df$salary, na.rm = TRUE)

# Vẽ biểu đồ Histogram
ggplot(df, aes(x = salary)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "black", size = 1) +
  geom_vline(aes(xintercept = salary_mean, color = "Trung bình"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = salary_median, color = "Trung vị"), linetype = "solid", size = 1) +
  scale_color_manual(name = "Chỉ số", values = c("Trung bình" = "red", "Trung vị" = "yellow")) +
  labs(title = "Phân phối của Salary (Kiểm tra độ lệch)", x = "Salary", y = "Mật độ") +
  theme_minimal()

# == 3. HÀM TÍNH TOÁN CHI TIẾT OUTLIERS THEO IQR
#-------------------------------------------------
  # Đoạn code này định nghĩa hàm và lặp qua các cột để in báo cáo.
  # Hàm báo cáo Outliers
report_outliers <- function(data, column) {
  values <- data[[column]]
  
  # Tính toán IQR
  stats <- quantile(values, probs = c(0.25, 0.75), na.rm = TRUE)
  Q1 <- stats[1]
  Q3 <- stats[2]
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Lọc ngoại lai
  outliers <- values[values < lower_bound | values > upper_bound]
  outliers <- outliers[!is.na(outliers)]
  
  # In báo cáo
  cat(paste0("--- Báo cáo cho cột: ", column, " ---\n"))
  cat(sprintf("- Ngưỡng dưới: %.2f, Ngưỡng trên: %.2f\n", lower_bound, upper_bound))
  cat(sprintf("- Số lượng Outliers: %d (%.2f%%)\n", length(outliers), length(outliers)/nrow(data)*100))
  
  if (length(outliers) > 0) {
    cat(sprintf("- Giá trị lớn nhất ngoại lai: %.2f\n", max(outliers)))
    cat(sprintf("- Giá trị nhỏ nhất ngoại lai: %.2f\n", min(outliers)))
  }
  cat(paste0(strrep("-", 40), "\n"))
}

# Chạy báo cáo cho từng cột
walk(num_cols, ~report_outliers(df, .x))

# == 4. KIỂM TRA TÍNH NHẤT QUÁN Ở CÁC CỘT 
# -------------

# 1. Định nghĩa danh sách các cột phân loại
cat_cols <- c('job_title', 'education_level', 'industry', 'company_size', 'location', 'remote_work')

# 2. TRỰC QUAN HÓA BẤT NHẤT ĐỊNH DẠNG (Bằng ggplot2)
# Chuyển dữ liệu sang dạng dọc để vẽ lưới (facet)
df_long <- df %>%
  select(all_of(cat_cols)) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "value")


ggplot(df_long, aes(y = fct_infreq(value), fill = column)) +
  geom_bar() +
  facet_wrap(~column, scales = "free", ncol = 2) +
  scale_fill_viridis_d() +
  labs(title = "Phân phối giá trị trong các biến phân loại", x = "Số lượng", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

# == 5. KIỂM TRA BẤT NHẤT CHI TIẾT (Vòng lặp kiểm tra chuẩn hóa)
# -------------

cat("\n--- KIỂM TRA TÍNH BẤT NHẤT CHI TIẾT ---\n")
for (col in cat_cols) {
  # Lấy giá trị duy nhất hiện tại
  unique_vals <- unique(df[[col]])
  
  # Chuẩn hóa giả định (viết thường + xóa khoảng trắng)
  normalized_vals <- unique(trimws(tolower(df[[col]])))
  
  cat(paste0("Cột [", col, "]:\n"))
  cat(paste0("  - Số giá trị duy nhất hiện tại: ", length(unique_vals), "\n"))
  cat(paste0("  - Số giá trị sau khi chuẩn hóa: ", length(normalized_vals), "\n"))
  
  if (length(unique_vals) != length(normalized_vals)) {
    cat(paste0("  => CẢNH BÁO: Phát hiện sự bất nhất về định dạng trong cột ", col, "!\n"))
  } else {
    cat(paste0("  => Cột ", col, " sạch sẽ về định dạng.\n"))
  }
  cat(paste0(strrep("-", 40), "\n"))
}

# == 6. KIỂM TRA LOGIC (Kinh nghiệm âm)
# -------------

inconsistent_exp <- df %>% filter(experience_years < 0)
cat(paste0("\nSố dòng có kinh nghiệm âm: ", nrow(inconsistent_exp), "\n"))

# ==============================================================================
# KIỂM TRA MẤT CÂN BẰNG DỮ LIỆU (DATA BALANCE CHECK)
# ==============================================================================

# 1. Khai báo các thư viện cần thiết
library(tidyverse)
library(tidytext)  # Bắt buộc để dùng reorder_within và scale_y_reordered

# 2. Định nghĩa danh sách các cột phân loại
cat_cols <- c('job_title', 'education_level', 'industry', 'company_size', 'location', 'remote_work')

# 3. BÁO CÁO CHI TIẾT SỐ LƯỢNG VÀ TỶ LỆ % (Console Output)
cat("--- BÁO CÁO MẤT CÂN BẰNG (Categorical Features) ---\n")

for (col in cat_cols) {
  # Tính toán tần suất và tỷ lệ
  counts <- table(df[[col]])
  percentages <- prop.table(counts) * 100
  
  # Tạo bảng tổng hợp và sắp xếp giảm dần
  summary_df <- data.frame(
    So_luong = as.numeric(counts),
    Ty_le_Phan_tram = as.numeric(percentages),
    row.names = names(counts)
  )
  summary_df <- summary_df[order(-summary_df$So_luong), ]
  
  cat(paste0("\nPhân phối của cột [", col, "]:\n"))
  print(summary_df)
  
  # Cảnh báo nếu có nhóm quá lớn (>50%) hoặc quá nhỏ (<5%)
  max_pct <- max(summary_df$Ty_le_Phan_tram)
  min_pct <- min(summary_df$Ty_le_Phan_tram)
  
  if (max_pct > 50) {
    cat(sprintf("  [!] Cảnh báo: Có nhóm chiếm ưu thế quá lớn (%.2f%%)\n", max_pct))
  }
  if (min_pct < 5) {
    cat(sprintf("  [!] Lưu ý: Có nhóm dữ liệu rất ít (%.2f%%)\n", min_pct))
  }
  cat(paste0(strrep("-", 40), "\n"))
}

  # 4. TRỰC QUAN HÓA BẰNG BIỂU ĐỒ CỘT (Visual Output)
  # Chuẩn bị dữ liệu cho ggplot
  df_plot <- df %>%
  select(all_of(cat_cols)) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "value") %>%
  group_by(column, value) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(column) %>%
  mutate(percent = n / sum(n) * 100)

# Vẽ biểu đồ lưới (Facet Wrap)
ggplot(df_plot, aes(y = reorder_within(value, n, column), x = n, fill = column)) +
  geom_col() +
  # Thêm nhãn % vào cuối mỗi cột
  geom_text(aes(label = sprintf("%.1f%%", percent)), 
            hjust = -0.1, size = 3, fontface = "bold") +
  # Chia biểu đồ theo từng biến số
  facet_wrap(~column, scales = "free", ncol = 2) +
  # Sắp xếp thứ tự thanh biểu đồ từ cao xuống thấp trong từng ô
  scale_y_reordered() + 
  scale_fill_viridis_d(option = "magma") +
  labs(title = "Phân phối các lớp trong các biến phân loại", 
       subtitle = "Kiểm tra sự cân bằng giữa các nhóm dữ liệu",
       x = "Số lượng dòng dữ liệu", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 10)) +
  # Mở rộng trục X để hiển thị đầy đủ nhãn phần trăm
  expand_limits(x = max(df_plot$n) * 1.3)




# ==============================================================================
# KIỂM TRA ĐỘ LỆCH (SKEWNESS) VÀ BIẾN ĐỔI DỮ LIỆU (HOÀN CHỈNH)
# ==============================================================================

# 1. Khai báo thư viện
library(tidyverse)
library(e1071)      # Để tính toán skewness
library(patchwork)  # Để ghép biểu đồ

# 2. Danh sách các cột số
num_cols <- c('experience_years', 'skills_count', 'certifications', 'salary')

# 3. BÁO CÁO CHỈ SỐ SKEWNESS CHI TIẾT
cat("--- BÁO CÁO ĐỘ LỆCH (SKEWNESS) ---\n")
for (col in num_cols) {
  val <- e1071::skewness(df[[col]], na.rm = TRUE)
  
  # Phân loại mức độ lệch
  status <- "Đối xứng (Normal)"
  if (val > 0.5) {
    status <- "Lệch phải (Positive Skew)"
  } else if (val < -0.5) {
    status <- "Lệch trái (Negative Skew)"
  }
  
  cat(sprintf("%s: %.4f -> %s\n", col, val, status))
}

# 4. TRỰC QUAN HÓA PHÂN PHỐI VỚI THANG ĐO RIÊNG BIỆT (SCALES = FREE)
# Chuẩn bị dữ liệu dạng dọc
df_num_long <- df %>%
  select(all_of(num_cols)) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "value")

# Tính toán Mean và Median cho từng nhóm để vẽ đường thẳng
df_stats <- df_num_long %>%
  group_by(column) %>%
  summarise(
    m_mean = mean(value, na.rm = TRUE),
    m_median = median(value, na.rm = TRUE),
    m_skew = e1071::skewness(value, na.rm = TRUE)
  )

# Vẽ biểu đồ Histogram lưới
ggplot(df_num_long, aes(x = value)) +
  # Vẽ Histogram và đường mật độ (KDE)
  geom_histogram(aes(y = ..density..), bins = 25, fill = "salmon", alpha = 0.6, color = "white") +
  geom_density(color = "black", size = 0.8) +
  # Thêm đường Mean và Median từ bảng thống kê df_stats
  geom_vline(data = df_stats, aes(xintercept = m_mean, color = "Mean"), linetype = "dashed", size = 1) +
  geom_vline(data = df_stats, aes(xintercept = m_median, color = "Median"), linetype = "solid", size = 1) +
  # Chia lưới với thang đo tự do (QUAN TRỌNG: scales = "free")
  facet_wrap(~column, scales = "free", ncol = 2) +
  scale_color_manual(name = "Chỉ số", values = c("Mean" = "blue", "Median" = "green")) +
  labs(title = "Phân phối thực tế của các biến số",
       subtitle = "Đường nét đứt (Blue): Mean | Đường nét liền (Green): Median",
       x = "Giá trị", y = "Mật độ") +
  theme_minimal() +
  theme(legend.position = "top", 
        strip.text = element_text(face = "bold", size = 12))

# 5. SO SÁNH TÁC DỤNG CỦA LOG-TRANSFORM (Cho biến Salary)
# Biểu đồ gốc
p1 <- ggplot(df, aes(x = salary)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density() +
  labs(title = "Salary Gốc", 
       subtitle = sprintf("Skewness: %.4f", e1071::skewness(df$salary, na.rm = TRUE))) +
  theme_minimal()

# Biểu đồ sau khi Log1p
p2 <- ggplot(df, aes(x = log1p(salary))) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "purple", alpha = 0.7) +
  geom_density() +
  labs(title = "Salary sau Log-transform", 
       subtitle = sprintf("Skewness: %.4f", e1071::skewness(log1p(df$salary), na.rm = TRUE))) +
  theme_minimal()

# Hiển thị so sánh cạnh nhau
p1 + p2


# =======================
# The Experience-Salary Correlation
#=================
library(ggplot2)
# . Vẽ biểu đồ tương quan giữa Kinh nghiệm và Lương
ggplot(df, aes(x = experience_years, y = salary)) +
  # Vẽ các điểm dữ liệu (tương đương scatter_kws)
  geom_point(alpha = 0.3, color = "#1e3c72") +
  # Vẽ đường hồi quy tuyến tính (tương đương line_kws trong sns.regplot)
  geom_smooth(method = "lm", color = "red", se = TRUE) + 
  # Thiết lập tiêu đề và nhãn
  labs(
    title = "Impact of Experience on Salary Growth",
    x = "Years of Experience",
    y = "Salary"
  ) +
  # Giao diện sạch sẽ
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold"))
#======
# Median salary across different job locations
#========
library(tidyverse)

# 1. Tính toán mức lương trung vị theo từng Location
# Lưu ý: Nếu bạn đang dùng log_salary, hãy chuyển ngược lại (exp) để con số thực tế hơn,
# hoặc để nguyên log_salary nếu muốn đồng nhất với các phần trước.
location_summary <- train_data %>%
  group_by(location) %>%
  summarise(median_sal = median(log_salary, na.rm = TRUE)) %>%
  arrange(desc(median_sal))

# 2. Vẽ biểu đồ cột
ggplot(location_summary, aes(x = reorder(location, median_sal), y = median_sal)) +
  geom_col(fill = "#377EB8", color = "black", width = 0.7) + 
  coord_flip() + # Xoay ngang để tên các địa phương dễ đọc hơn
  theme_minimal() +
  labs(
    title = "Median Log-Salary by Geographic Location",
    subtitle = "Analysis of regional wage differentials",
    x = "Location",
    y = "Median Log Salary"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# 3. Lưu ảnh cho báo cáo
ggsave("median_salary_location.png", width = 8, height = 6, dpi = 300)

#======
# Average salary for top job titles
#====
library(tidyverse)
library(ggplot2)

# Giả sử train_data là tập dữ liệu huấn luyện của bạn

# 1. Tính toán mức lương trung bình và sắp xếp Top 10 Job Titles
job_summary <- train_data %>%
  # Nhóm theo chức danh công việc
  group_by(job_title) %>%
  # Tính mức lương trung vị (hoặc trung bình) cho log_salary
  # Nên dùng median để giảm ảnh hưởng của nhiễu (outliers)
  summarise(median_log_sal = median(log_salary, na.rm = TRUE),
            count = n()) %>%
  # Chỉ lấy các job title có số lượng quan sát đáng kể (ví dụ > 5)
  # để đảm bảo tính đại diện
  filter(count > 5) %>%
  # Sắp xếp giảm dần theo mức lương
  arrange(desc(median_log_sal)) %>%
  # Chỉ lấy Top 10 (hoặc Top 15) chức danh cao nhất
  head(10)

# 2. Vẽ biểu đồ cột nằm ngang
ggplot(job_summary, aes(x = reorder(job_title, median_log_sal), y = median_log_sal)) +
  # Vẽ cột với màu xanh chuyên nghiệp, viền đen
  geom_col(fill = "#377EB8", color = "black", width = 0.7) + 
  # Xoay ngang biểu đồ để tên chức danh dễ đọc
  coord_flip() + 
  # Sử dụng theme tối giản
  theme_minimal() +
  # Thêm nhãn và tiêu đề
  labs(
    title = "Top 10 Highest Paying Job Titles",
    subtitle = "Based on median log-transformed salary",
    x = "Job Titles",
    y = "Median Log Salary"
  ) +
  # Tùy chỉnh font chữ để đẹp hơn khi chèn vào báo cáo
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 11), # Tăng kích thước chữ trục Y (tên job)
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 3. Lưu ảnh chất lượng cao cho báo cáo
ggsave("top_salary_jobtitles.png", width = 8, height = 6, dpi = 300)

#===========
# Role & Education
# ==========
# 1. Khai báo thư viện
library(tidyverse)

# 2. Xử lý dữ liệu (Tương đương pivot_table)
# Tính lương trung bình theo Job Title và Education Level
heatmap_data <- df %>%
  group_by(job_title, education_level) %>%
  summarise(avg_salary = mean(salary, na.rm = TRUE), .groups = 'drop')

# 3. Vẽ biểu đồ Heatmap
ggplot(heatmap_data, aes(x = education_level, y = job_title, fill = avg_salary)) +
  # Vẽ các ô màu
  geom_tile(color = "white") +
  # Thêm nhãn số vào từng ô (tương đương annot=True, fmt=".0f")
  geom_text(aes(label = round(avg_salary, 0)), size = 3) +
  # Thiết lập bảng màu (YlGnBu tương đương palette "YlGnBu" trong R)
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "Average Salary") +
  # Thiết lập tiêu đề và nhãn
  labs(
    title = "Average Salary by Job Title and Education Level",
    x = "Education Level",
    y = "Job Title"
  ) +
  # Giao diện sạch sẽ
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1) # Xoay nhãn trục X cho dễ đọc
  )

# ============ 
# Location vs. Remote Work
# ==============
# 1. Khai báo thư viện
library(ggplot2)

# 2. Vẽ biểu đồ Boxplot phân nhóm
ggplot(df, aes(x = location, y = salary, fill = remote_work)) +
  # Vẽ Boxplot
  geom_boxplot(outlier.alpha = 0.5) +
  # Thiết lập bảng màu (Set2 tương đương palette='Set2' trong Seaborn)
  scale_fill_brewer(palette = "Set2") +
  # Thiết lập tiêu đề và nhãn
  labs(
    title = "Salary Variance by Location and Work Model",
    x = "Location",
    y = "Salary",
    fill = "Remote Work"
  ) +
  # Tùy chỉnh giao diện
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    # Xoay nhãn trục X 45 độ (tương đương plt.xticks(rotation=45))
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    # Vị trí chú thích (tương đương loc='upper right')
    legend.position = "top" 
  )
  
# =========
# MA TRẬN TƯƠNG QUAN GIỮA CÁC BIẾN 
#=============
#=============
# 1. Cài đặt và khai báo thư viện
if(!require(ggcorrplot)) install.packages("ggcorrplot")
library(ggcorrplot)
library(RColorBrewer) # Cần thư viện này để lấy màu chuẩn

# 2. Tính toán ma trận tương quan (chỉ lấy các cột dạng số)
corr_matrix <- df %>% 
  select(where(is.numeric)) %>% 
  cor(use = "complete.obs")

# Lấy 3 màu đại diện: Cực âm (-1), Trung tính (0), Cực dương (+1)
# Bảng màu "RdBu" (Red-Blue)
my_colors <- brewer.pal(n = 3, name = "RdBu")

# 3. Vẽ Heatmap với bảng màu MỚI (RdBu)
ggcorrplot(corr_matrix, 
           hc.order = TRUE,           # Sắp xếp biến
           type = "lower",            # Chỉ hiện nửa dưới
           lab = TRUE,                # Hiện chỉ số
           lab_size = 3,              # Kích thước chữ
           method = "square",         # Hình dạng các ô
           # THAY ĐỔI TẠI ĐÂY: Dùng 3 màu [Đỏ sẫm, Trắng, Xanh dương sẫm]
           colors = c(my_colors[3], "white", my_colors[1]), 
           title = "Correlation Heatmap (Màu RdBu dễ nhìn)",
           ggtheme = theme_minimal()) +
  # Xoay nhãn trục X 45 độ cho dễ đọc
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ===========
# Feature Engineering
#=========
# ==============================================================================
# BƯỚC 1: TIỀN XỬ LÝ DỮ LIỆU TỔNG THỂ (Tạo df_final)
# ==============================================================================
df_final <- df %>%
  # 1. Log transform salary ngay từ đầu để dùng làm biến mục tiêu
  mutate(log_salary = log1p(salary)) %>%
  
  # 2. Tạo biến binary cho Remote (Sửa lỗi is_remote not found)
  # Lưu ý: Kiểm tra lại tên cột gốc là 'remote_work' hay 'remote_ratio'
  mutate(is_remote = if_else(remote_work == "Yes", 1, 0)) %>%
  
  # 3. Chuyển các cột Ordinal thành dạng số (Encoded)
  mutate(
    education_level_encoded = case_when(
      education_level == "High School" ~ 0,
      education_level == "Diploma" ~ 1,
      education_level == "Bachelor" ~ 2,
      education_level == "Master" ~ 3,
      education_level == "PhD" ~ 4,
      TRUE ~ -1
    ),
    company_size_encoded = case_when(
      company_size == "Startup" ~ 0,
      company_size == "Small" ~ 1,
      company_size == "Medium" ~ 2,
      company_size == "Large" ~ 3,
      company_size == "Enterprise" ~ 4,
      TRUE ~ -1
    )
  )

# ==============================================================================
# BƯỚC 2: CHIA DỮ LIỆU (70/15/15)
# ==============================================================================
set.seed(42)
data_split <- initial_split(df_final, prop = 0.70, strata = log_salary)
train_data <- training(data_split)
temp_data  <- testing(data_split)

val_test_split <- initial_split(temp_data, prop = 0.50, strata = log_salary)
val_data  <- training(val_test_split)
test_data <- testing(val_test_split)

# ==============================================================================
# BƯỚC 3: ĐỊNH NGHĨA RECIPE (Quy trình chuẩn hóa)
# ==============================================================================
num_features <- c("experience_years", "skills_count", "certifications", "skills_per_year", "certs_per_year")

salary_recipe <- recipe(log_salary ~ ., data = train_data) %>%
  # Loại bỏ các cột gốc không dùng nữa
  step_rm(salary, remote_work, education_level, company_size) %>%
  
  # Feature Engineering bên trong Recipe (Tự động áp dụng cho Val/Test)
  step_mutate(
    skills_per_year = skills_count / (experience_years + 1),
    certs_per_year = certifications / (experience_years + 1),
    experience_level = case_when(
      experience_years <= 2 ~ "entry",
      experience_years <= 5 ~ "junior",
      experience_years <= 10 ~ "mid",
      experience_years <= 15 ~ "senior",
      TRUE ~ "expert"
    ),
    skills_level = case_when(
      skills_count <= 5 ~ "low",
      skills_count <= 10 ~ "mid",
      skills_count <= 15 ~ "high",
      TRUE ~ "expert"
    )
  ) %>%
  
  # Chuyển Level thành Factor để mã hóa Ordinal
  step_mutate(
    experience_level = factor(experience_level, levels = c("entry", "junior", "mid", "senior", "expert")),
    skills_level = factor(skills_level, levels = c("low", "mid", "high", "expert"))
  ) %>%
  step_integer(experience_level, skills_level) %>%
  
  # One-hot Encoding cho Job Title, Industry, Location
  step_dummy(job_title, industry, location, one_hot = TRUE) %>%
  
  # Chuẩn hóa các cột số
  step_normalize(all_of(num_features))

# Thực thi Recipe
salary_prep <- prep(salary_recipe)
train_transformed <- bake(salary_prep, new_data = NULL)
val_transformed   <- bake(salary_prep, new_data = val_data)
test_transformed  <- bake(salary_prep, new_data = test_data)
# KIỂM TRA LẠI KẾT QUẢ
cat("Tổng số dữ liệu ban đầu:", nrow(df_final), "dòng\n")
cat("-> Tập Train (70%):", nrow(train_data), "dòng\n")
cat("-> Tập Validation (15%):", nrow(val_data), "dòng\n")
cat("-> Tập Test (15%):", nrow(test_data), "dòng\n")

# Nếu bạn muốn kiểm tra tỷ lệ % để đảm bảo chia đúng
cat("\nTỷ lệ phân chia:\n")
cat("- Train:", round(nrow(train_data) / nrow(df_final) * 100, 1), "%\n")
cat("- Validation:", round(nrow(val_data) / nrow(df_final) * 100, 1), "%\n")
cat("- Test:", round(nrow(test_data) / nrow(df_final) * 100, 1), "%\n")

# ============
# MODELING
# ==============
 
# RANDOM FOREST 
#================== 
# 1. Khai báo thư viện cần thiết
library(tidymodels)
library(ranger) # Engine cho Random Forest tốc độ cao

# ==========================================
# 1. Khởi tạo Model (Random Forest)
# ==========================================
# n_estimators tương đương với trees
# random_state tương đương với set.seed()
# n_jobs=-1 được engine "ranger" tự động tối ưu hóa
rf_spec <- rand_forest(
  trees = 200,
  mode = "regression"
) %>%
  set_engine("ranger", seed = 42)

# ==========================================
# 2. Train trên tập Train
# ==========================================
# Giả sử train_transformed là dữ liệu đã qua bước 'bake'
rf_fit <- rf_spec %>%
  fit(log_salary ~ ., data = train_transformed)

# ==========================================
# 3. Evaluate trên Validation (để tuning)
# ==========================================
y_val_pred <- predict(rf_fit, new_data = val_transformed) %>%
  bind_cols(val_transformed) # Kết hợp kết quả dự báo với thực tế

val_results <- y_val_pred %>%
  metrics(truth = log_salary, estimate = .pred)

# Tính toán riêng lẻ để in giống Python
val_mae  <- mae(y_val_pred, truth = log_salary, estimate = .pred)$.estimate
val_rmse <- rmse(y_val_pred, truth = log_salary, estimate = .pred)$.estimate
val_r2   <- rsq(y_val_pred, truth = log_salary, estimate = .pred)$.estimate

cat("📊 Validation Results:\n")
cat(sprintf("MAE  : %.2f\n", val_mae))
cat(sprintf("RMSE : %.2f\n", val_rmse))
cat(sprintf("R2   : %.4f\n", val_r2))

# ==========================================
# 4. Evaluate trên Test (Kết quả cuối cùng)
# ==========================================
y_test_pred <- predict(rf_fit, new_data = test_transformed) %>%
  bind_cols(test_transformed)

test_mae  <- mae(y_test_pred, truth = log_salary, estimate = .pred)$.estimate
test_rmse <- rmse(y_test_pred, truth = log_salary, estimate = .pred)$.estimate
test_r2   <- rsq(y_test_pred, truth = log_salary, estimate = .pred)$.estimate

cat("\n🏆 Test Results (Final):\n")
cat(sprintf("MAE  : %.2f\n", test_mae))
cat(sprintf("RMSE : %.2f\n", test_rmse))
cat(sprintf("R2   : %.4f\n", test_r2))

# ============
# Linear Regression
#==========
# 1. Khai báo thư viện (nếu chưa khai báo)
library(tidymodels)
library(glmnet)

# ==========================================
# 1. Khởi tạo Model (Ridge Regression)
# ==========================================
# mixture = 0 đại diện cho Ridge Regression
# penalty = 1.0 tương đương với alpha=1.0 trong Scikit-learn
ridge_spec <- linear_reg(
  penalty = 1.0, 
  mixture = 0
) %>%
  set_engine("glmnet")

# ==========================================
# 2. Train trên tập Train
# ==========================================
# Sử dụng dữ liệu đã qua xử lý (train_transformed)
ridge_fit <- ridge_spec %>%
  fit(log_salary ~ ., data = train_transformed)

# ==========================================
# 3. Đánh giá trên Validation
# ==========================================
y_val_pred <- predict(ridge_fit, new_data = val_transformed) %>%
  bind_cols(val_transformed)

val_mae  <- mae(y_val_pred, truth = log_salary, estimate = .pred)$.estimate
val_rmse <- rmse(y_val_pred, truth = log_salary, estimate = .pred)$.estimate
val_r2   <- rsq(y_val_pred, truth = log_salary, estimate = .pred)$.estimate

cat("📊 Validation Results (Ridge Regression):\n")
cat(sprintf("MAE  : %.2f\n", val_mae))
cat(sprintf("RMSE : %.2f\n", val_rmse))
cat(sprintf("R2   : %.4f\n", val_r2))

# ==========================================
# 4. Đánh giá trên Test (Kết quả cuối)
# ==========================================
y_test_pred <- predict(ridge_fit, new_data = test_transformed) %>%
  bind_cols(test_transformed)

test_mae  <- mae(y_test_pred, truth = log_salary, estimate = .pred)$.estimate
test_rmse <- rmse(y_test_pred, truth = log_salary, estimate = .pred)$.estimate
test_r2   <- rsq(y_test_pred, truth = log_salary, estimate = .pred)$.estimate

cat("\n🏆 Test Results (Ridge Regression):\n")
cat(sprintf("MAE  : %.2f\n", test_mae))
cat(sprintf("RMSE : %.2f\n", test_rmse))
cat(sprintf("R2   : %.4f\n", test_r2))

# =========
# Ridge Regression
# ================
# 1. Khai báo thư viện (nếu chưa có)
library(tidymodels)
library(glmnet)

# -----------------------------
# 2. Định nghĩa Ridge Model
# -----------------------------
# mixture = 0 là Ridge, penalty = 1.0 là alpha trong Python
ridge_spec <- linear_reg(penalty = 1.0, mixture = 0) %>%
  set_engine("glmnet")

# -----------------------------
# 3. Huấn luyện (Fit)
# -----------------------------
# Lưu ý: Sử dụng các bảng đã qua xử lý (transformed)
ridge_fit <- ridge_spec %>%
  fit(log_salary ~ ., data = train_transformed)

# -----------------------------
# 4. Dự báo (Predict)
# -----------------------------
y_train_pred <- predict(ridge_fit, new_data = train_transformed) %>% bind_cols(train_transformed)
y_val_pred   <- predict(ridge_fit, new_data = val_transformed)   %>% bind_cols(val_transformed)
y_test_pred  <- predict(ridge_fit, new_data = test_transformed)  %>% bind_cols(test_transformed)

# -----------------------------
# 5. Đánh giá (Evaluation)
# -----------------------------
# Tính R2 cho từng tập
train_r2 <- rsq(y_train_pred, truth = log_salary, estimate = .pred)$.estimate
val_r2   <- rsq(y_val_pred,   truth = log_salary, estimate = .pred)$.estimate
test_r2  <- rsq(y_test_pred,  truth = log_salary, estimate = .pred)$.estimate

# Tính RMSE cho tập Test
test_rmse <- rmse(y_test_pred, truth = log_salary, estimate = .pred)$.estimate

# In kết quả giống định dạng Python
cat("=== Ridge Regression ===\n")
cat(sprintf("Train R2: %.4f\n", train_r2))
cat(sprintf("Validation R2: %.4f\n", val_r2))
cat(sprintf("Test R2: %.4f\n", test_r2))
cat(sprintf("Test RMSE: %.4f\n", test_rmse))

# =========
#  Decision TREE REGRESSION 
# ==========
# 1. Khai báo thư viện (nếu chưa có)
library(tidymodels)
library(rpart)

# -----------------------------
# 2. Định nghĩa Decision Tree Model
# -----------------------------
# tree_depth tương đương với max_depth
# Nếu muốn để None (mọc tự do), bạn có thể bỏ qua tham số này hoặc để giá trị lớn
dt_spec <- decision_tree(
  tree_depth = 30, # Tương đương để mọc sâu
  min_n = 2        # Số lượng mẫu tối thiểu để tách nút
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# -----------------------------
# 3. Huấn luyện (Fit)
# -----------------------------
dt_fit <- dt_spec %>%
  fit(log_salary ~ ., data = train_transformed)

# -----------------------------
# 4. Dự báo (Predict)
# -----------------------------
y_train_pred <- predict(dt_fit, new_data = train_transformed) %>% bind_cols(train_transformed)
y_val_pred   <- predict(dt_fit, new_data = val_transformed)   %>% bind_cols(val_transformed)
y_test_pred  <- predict(dt_fit, new_data = test_transformed)  %>% bind_cols(test_transformed)

# -----------------------------
# 5. Đánh giá (Evaluation)
# -----------------------------
# Tính R2
train_r2 <- rsq(y_train_pred, truth = log_salary, estimate = .pred)$.estimate
val_r2   <- rsq(y_val_pred,   truth = log_salary, estimate = .pred)$.estimate
test_r2  <- rsq(y_test_pred,  truth = log_salary, estimate = .pred)$.estimate

# Tính RMSE cho tập Test
test_rmse <- rmse(y_test_pred, truth = log_salary, estimate = .pred)$.estimate

# In kết quả
cat("=== Decision Tree Regression ===\n")
cat(sprintf("Train R2: %.4f\n", train_r2))
cat(sprintf("Validation R2: %.4f\n", val_r2))
cat(sprintf("Test R2: %.4f\n", test_r2))
cat(sprintf("Test RMSE: %.4f\n", test_rmse))

# ===========
# Gradient Boosting
# =================
# 1. Khai báo thư viện (nếu chưa có)
library(tidymodels)
library(xgboost)

# ---------------------------------------------------------
# 2. Định nghĩa Gradient Boosting Model (Sử dụng Boost Tree)
# ---------------------------------------------------------
# trees = n_estimators (số lượng cây)
# learn_rate = learning_rate (tốc độ học)
# tree_depth = max_depth (độ sâu tối đa)
gb_spec <- boost_tree(
  trees = 500,
  learn_rate = 0.05,
  tree_depth = 5
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# ---------------------------------------------------------
# 3. Huấn luyện (Fit)
# ---------------------------------------------------------
# Sử dụng dữ liệu đã qua xử lý (transformed)
gb_fit <- gb_spec %>%
  fit(log_salary ~ ., data = train_transformed)

# ---------------------------------------------------------
# 4. Dự báo (Predict)
# ---------------------------------------------------------
y_train_pred <- predict(gb_fit, new_data = train_transformed) %>% bind_cols(train_transformed)
y_val_pred   <- predict(gb_fit, new_data = val_transformed)   %>% bind_cols(val_transformed)
y_test_pred  <- predict(gb_fit, new_data = test_transformed)  %>% bind_cols(test_transformed)

# ---------------------------------------------------------
# 5. Đánh giá (Evaluation)
# ---------------------------------------------------------
# Tính R2 cho từng tập
train_r2 <- rsq(y_train_pred, truth = log_salary, estimate = .pred)$.estimate
val_r2   <- rsq(y_val_pred,   truth = log_salary, estimate = .pred)$.estimate
test_r2  <- rsq(y_test_pred,  truth = log_salary, estimate = .pred)$.estimate

# Tính RMSE cho tập Test
test_rmse <- rmse(y_test_pred, truth = log_salary, estimate = .pred)$.estimate

# In kết quả theo định dạng bạn muốn
cat("=== Gradient Boosting Regression ===\n")
cat(sprintf("Train R2: %.4f\n", train_r2))
cat(sprintf("Validation R2: %.4f\n", val_r2))
cat(sprintf("Test R2: %.4f\n", test_r2))
cat(sprintf("Test RMSE: %.4f\n", test_rmse))

# ===========
# XG BOOST 
# ==========
## 1. Khai báo thư viện (nếu chưa có)
library(tidymodels)
library(xgboost)

# ---------------------------------------------------------
# 2. Chuẩn bị dữ liệu DMatrix (XGBoost cần định dạng này để Early Stop)
# ---------------------------------------------------------
# Tách X (biến độc lập) và y (biến mục tiêu)
dtrain <- xgb.DMatrix(
  data = as.matrix(train_transformed %>% select(-log_salary)), 
  label = train_transformed$log_salary
)

dval <- xgb.DMatrix(
  data = as.matrix(val_transformed %>% select(-log_salary)), 
  label = val_transformed$log_salary
)

dtest <- xgb.DMatrix(
  data = as.matrix(test_transformed %>% select(-log_salary)), 
  label = test_transformed$log_salary
)

# ---------------------------------------------------------
# 3. Định nghĩa tham số & Huấn luyện
# ---------------------------------------------------------
params <- list(
  objective = "reg:squarederror",
  eta = 0.05,            # learning_rate
  max_depth = 5,         # tree_depth
  subsample = 0.8,       # sample_size
  colsample_bytree = 0.8,# mtry
  alpha = 0.1,           # L1
  lambda = 1             # L2
)

# Huấn luyện với Early Stopping
xgb_model_final <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(train = dtrain, val = dval),
  early_stopping_rounds = 20,
  print_every_n = 50,
  verbose = 1
)

# ---------------------------------------------------------
# 4. Dự báo & Đánh giá trên tập Test
# ---------------------------------------------------------
# Dự báo
y_pred_test <- predict(xgb_model_final, dtest)

# Tạo bảng kết quả
results_test <- tibble(
  actual = test_transformed$log_salary,
  predicted = y_pred_test
)

# Tính toán các chỉ số
test_r2 <- rsq(results_test, truth = actual, estimate = predicted)$.estimate
test_rmse <- rmse(results_test, truth = actual, estimate = predicted)$.estimate

# In kết quả
cat("\n=== XGBoost Regression (Fixed & Optimized) ===\n")
cat(sprintf("Test R2       : %.4f\n", test_r2))
cat(sprintf("Test RMSE     : %.4f\n", test_rmse))
#=========
# So sánh 6 model
#=========

library(tidyverse)
library(tidymodels)
library(tictoc) # Thư viện để đo thời gian chính xác

# ---------------------------------------------------------
# 1. Định nghĩa danh sách các Model Spec
# ---------------------------------------------------------
# Chúng ta gom tất cả cấu hình vào một list
model_specs <- list(
  "Linear Regression" = linear_reg() %>% set_engine("lm"),
  
  "Ridge Regression"  = linear_reg(penalty = 1.0, mixture = 0) %>% set_engine("glmnet"),
  
  "Decision Tree"     = decision_tree(tree_depth = 30) %>% set_engine("rpart") %>% set_mode("regression"),
  
  "Random Forest"     = rand_forest(trees = 200, min_n = 10) %>% set_engine("ranger") %>% set_mode("regression"),
  
  "Gradient Boosting" = boost_tree(trees = 500, learn_rate = 0.05, tree_depth = 5) %>% set_engine("xgboost") %>% set_mode("regression"),
  
  "XGBoost"           = boost_tree(trees = 500, learn_rate = 0.05, tree_depth = 5) %>% 
    set_engine("xgboost", lambda = 1, alpha = 0.1) %>% set_mode("regression")
)

# ---------------------------------------------------------
# 2. Vòng lặp Train, Predict và Đánh giá
# ---------------------------------------------------------
results_list <- list()

for (name in names(model_specs)) {
  cat("Đang huấn luyện mô hình:", name, "...\n")
  
  # Đo thời gian huấn luyện
  tic() 
  model_fit <- model_specs[[name]] %>% fit(log_salary ~ ., data = train_transformed)
  exec_time <- toc(quiet = TRUE)
  time_val <- exec_time$toc - exec_time$tic
  
  # Dự báo trên các tập
  y_train_pred <- predict(model_fit, new_data = train_transformed) %>% pull(.pred)
  y_val_pred   <- predict(model_fit, new_data = val_transformed)   %>% pull(.pred)
  y_test_pred  <- predict(model_fit, new_data = test_transformed)  %>% pull(.pred)
  
  # Tính toán Metrics
  train_r2 <- rsq_vec(train_transformed$log_salary, y_train_pred)
  val_r2   <- rsq_vec(val_transformed$log_salary, y_val_pred)
  test_r2  <- rsq_vec(test_transformed$log_salary, y_test_pred)
  test_rmse <- rmse_vec(test_transformed$log_salary, y_test_pred)
  
  # Lưu kết quả
  results_list[[name]] <- tibble(
    Model = name,
    `Train R2` = round(train_r2, 4),
    `Validation R2` = round(val_r2, 4),
    `Test R2` = round(test_r2, 4),
    `Test RMSE` = round(test_rmse, 4),
    `Time (s)` = round(time_val, 4)
  )
}

# ---------------------------------------------------------
# 3. Tổng hợp và In bảng so sánh
# ---------------------------------------------------------
final_results <- bind_rows(results_list) %>%
  arrange(desc(`Test R2`))

print(final_results)

# ==========
# K- Fold cho model Random Forest
# ==============================================================================
# 1. KHỞI TẠO CẤU HÌNH (LƯU Ý: Chạy lại các bước này nếu đã Clear Workspace)
# ==============================================================================
library(tidyverse)
library(tidymodels)
library(ranger) # Engine cho Random Forest

# Cấu hình Model Random Forest (trees=100 để chạy nhanh hơn khi Cross-Val)
rf_spec <- rand_forest(trees = 100) %>%
  set_engine("ranger", num.threads = parallel::detectCores()) %>% 
  set_mode("regression")

# ==============================================================================
# 2. THIẾT LẬP WORKFLOW (Đóng gói Recipe và Model)
# ==============================================================================
# salary_recipe là cái recipe thô bạn đã định nghĩa ở các bước đầu
rf_workflow <- workflow() %>%
  add_recipe(salary_recipe) %>% 
  add_model(rf_spec)

# ==============================================================================
# 3. CHẠY K-FOLD CROSS-VALIDATION (k=5)
# ==============================================================================
set.seed(42) # Để kết quả chia fold luôn cố định
# Sử dụng train_data gốc (chưa qua xử lý)
folds <- vfold_cv(train_data, v = 5, strata = log_salary)

cat("Đang bắt đầu chạy 5-Fold Cross-Validation... Vui lòng đợi trong giây lát.\n")

cv_results <- rf_workflow %>%
  fit_resamples(
    resamples = folds,
    metrics = metric_set(rsq, rmse, mae),
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )

# ==============================================================================
# 4. TỔNG HỢP VÀ TÍNH TOÁN CHỈ SỐ
# ==============================================================================
# Lấy danh sách kết quả R2 của từng Fold
cv_metrics_all <- collect_metrics(cv_results, summarize = FALSE) %>%
  filter(.metric == "rsq")

# Tính toán các giá trị thống kê quan trọng
mean_score <- mean(cv_metrics_all$.estimate)
std_score  <- sd(cv_metrics_all$.estimate)

# ==============================================================================
# 5. TRỰC QUAN HÓA (BIỂU ĐỒ BOXPLOT + JITTER)
# ==============================================================================
ggplot(cv_metrics_all, aes(x = "", y = .estimate)) +
  # Vẽ Boxplot màu xanh nhạt
  geom_boxplot(fill = "#a2d2ff", width = 0.3, alpha = 0.7) +
  # Vẽ các điểm số của từng Fold (Swarm plot style)
  geom_jitter(color = "#00308f", size = 4, width = 0.05) +
  # Vẽ đường kẻ ngang biểu thị giá trị trung bình
  geom_hline(yintercept = mean_score, color = "red", linetype = "dashed", size = 1) +
  # Tinh chỉnh nhãn và giao diện
  labs(
    title = "K-Fold Cross-Validation Results (k=5) - Random Forest",
    subtitle = paste0("Mean R2: ", round(mean_score, 4), " (+/- ", round(std_score, 4), ")"),
    y = "R2 Score",
    x = ""
  ) +
  coord_cartesian(ylim = c(0, 1)) + # Giới hạn trục Y từ 0 đến 1
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# In kết quả chi tiết ra màn hình Console
cat("\n--- KẾT QUẢ CHI TIẾT ---")
cat(sprintf("\nScores từng fold: %s", paste(round(cv_metrics_all$.estimate, 4), collapse = ", ")))
cat(sprintf("\nR2 Trung bình: %.4f", mean_score))
cat(sprintf("\nĐộ lệch chuẩn (Std): %.4f\n", std_score))
#======
# IMPORTANCE FEATURES
#======
# Cài đặt nếu chưa có: install.packages(c("vip", "ggplot2", "ranger"))
library(tidyverse)
library(vip)
library(ggplot2)

# 1. Huấn luyện mô hình Random Forest (sử dụng engine 'ranger')
# LƯU Ý: Phải thiết lập importance = "impurity" hoặc "permutation" để tính được độ quan trọng
rf_final_fit <- rand_forest(mtry = 5, trees = 500) %>%
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression") %>%
  fit(log_salary ~ ., data = train_data)

# 2. Vẽ biểu đồ Feature Importance
feature_importance_plot <- vip(rf_final_fit, 
                               num_features = 10,       # Hiển thị top 10 biến quan trọng nhất
                               geom = "col",            # Vẽ dạng cột (column)
                               fill = "#377EB8",        # Màu xanh chuyên nghiệp (giống bài mẫu)
                               aesthetics = list(color = "black", size = 0.5)) + 
  theme_minimal() +
  labs(
    title = "Feature Importance for Salary Prediction",
    subtitle = "Based on Random Forest (Mean Decrease in Impurity)",
    x = "Predictor Variables",
    y = "Importance Score"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

# 3. Hiển thị và lưu ảnh
print(feature_importance_plot)
ggsave("feature_importance_rf.png", width = 8, height = 6, dpi = 300)



# BẢNG KẾT QUẢ K FOLD 
#========
install.packages("kableExtra")
library(knitr)
library(kableExtra)

# --- BƯỚC 1: Tạo bảng dữ liệu tổng hợp ---
summary_table <- tibble(
  `Fold` = c("Fold 1", "Fold 2", "Fold 3", "Fold 4", "Fold 5", "TRUNG BÌNH (Mean)", "Độ lệch chuẩn (Std)"),
  `R-Squared Score` = c(
    round(cv_metrics_all$.estimate, 4), 
    round(mean_score, 4), 
    round(std_score, 4)
  )
)

# --- BƯỚC 2: In bảng đẹp ra Viewer ---
summary_table %>%
  kable(
    align = "cl", 
    caption = "BẢNG KẾT QUẢ 5-FOLD CROSS-VALIDATION (RANDOM FOREST)"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "bordered", "condensed"),
    full_width = FALSE,
    font_size = 14
  ) %>%
  # Tô màu dòng cuối cùng (Trung bình và Std) để làm nổi bật
  row_spec(6:7, bold = TRUE, color = "white", background = "#00308f") %>%
  column_spec(1, bold = TRUE, border_right = TRUE)


# =====
# K Fold cho XG BOOST 
library(tidyverse)
library(tidymodels)
library(xgboost)
library(tictoc)

# ---------------------------------------------------------
# 1. Định nghĩa Model Spec & Workflow
# ---------------------------------------------------------
xgb_spec <- boost_tree(
  trees = 500,
  learn_rate = 0.05,
  tree_depth = 5
) %>%
  set_engine("xgboost", nthreads = parallel::detectCores()) %>%
  set_mode("regression")

# Gom vào workflow với recipe bạn đã định nghĩa (salary_recipe)
xgb_workflow <- workflow() %>%
  add_recipe(salary_recipe) %>%
  add_model(xgb_spec)

# ---------------------------------------------------------
# 2. Khởi tạo K-Fold (k=5)
# ---------------------------------------------------------
set.seed(42)
folds <- vfold_cv(train_data, v = 5, strata = log_salary)

# ---------------------------------------------------------
# 3. Chạy Cross-Validation thủ công để đo thời gian từng Fold
# ---------------------------------------------------------
fold_results <- list()

cat(sprintf("%-10s | %-12s | %-10s\n", "Fold", "R2 Score", "Time (s)"))
cat(paste0(rep("-", 35), collapse = ""), "\n")

for (i in 1:nrow(folds)) {
  # Lấy dữ liệu của fold hiện tại
  fold_split <- folds$splits[[i]]
  
  # Đo thời gian huấn luyện
  tic()
  fold_fit <- xgb_workflow %>% fit(data = training(fold_split))
  ex_time <- toc(quiet = TRUE)
  time_val <- ex_time$toc - ex_time$tic
  
  # Dự đoán trên tập kiểm tra của fold (assessment data)
  holdout_data <- assessment(fold_split)
  y_pred <- predict(fold_fit, new_data = holdout_data) %>% pull(.pred)
  y_true <- holdout_data$log_salary
  
  # Tính R2 sử dụng rsq_vec
  score_r2 <- rsq_vec(y_true, y_pred)
  
  # Lưu kết quả vào list
  fold_results[[i]] <- tibble(
    Fold = paste("Fold", i),
    R2 = score_r2,
    Time = time_val
  )
  
  cat(sprintf("Fold %-5d | %-12.4f | %-10.4f\n", i, score_r2, time_val))
}

# Tổng hợp kết quả thành một dataframe
xgb_cv_df <- bind_rows(fold_results)

# ---------------------------------------------------------
# 4. Trực quan hóa kết quả (R2 Score) - Tương đương Boxplot + Swarmplot
# ---------------------------------------------------------
p1 <- ggplot(xgb_cv_df, aes(x = "", y = R2)) +
  geom_boxplot(fill = "#ff9a00", width = 0.3, alpha = 0.7) +
  geom_jitter(color = "#d00000", size = 4, width = 0.05) +
  geom_hline(yintercept = mean(xgb_cv_df$R2), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "K-Fold Cross-Validation (k=5) - XGBoost Performance",
       subtitle = paste("Mean R2:", round(mean(xgb_cv_df$R2), 4)),
       y = "R2 Score", x = "") +
  theme_minimal()

print(p1)

# ---------------------------------------------------------
# 5. Trực quan hóa thời gian chạy (Training Time)
# ---------------------------------------------------------
# Sử dụng mã màu Hex #008080 để thay thế cho "teal" bị lỗi
p2 <- ggplot(xgb_cv_df, aes(x = Fold, y = Time)) +
  geom_col(fill = "#008080", alpha = 0.7) +
  labs(title = "Training Time per Fold - XGBoost",
       x = "Fold Number", y = "Time (seconds)") +
  theme_minimal()

print(p2)

cat(sprintf("\nThời gian trung bình mỗi fold: %.4f giây\n", mean(xgb_cv_df$Time)))

# =========
# LossValidation của GradientBoostRegression
#============
library(tidyverse)
library(xgboost)

library(tidyverse)
library(xgboost)

# --- BƯỚC 1: Thử lấy log bằng 2 cách phổ biến nhất ---
# Cách 1: Dùng hàm get_log (phổ biến ở bản mới)
eval_log <- tryCatch({
  xgb.get.evaluation.log(xgb_model_final)
}, error = function(e) {
  # Cách 2: Nếu cách 1 lỗi, truy cập trực tiếp vào thuộc tính ẩn
  return(xgb_model_final$evaluation_log)
})

# --- BƯỚC 2: Kiểm tra cấu hình watchlist (CỰC KỲ QUAN TRỌNG) ---
# Nếu eval_log vẫn NULL, nghĩa là lúc train bạn chưa truyền watchlist đúng cách
if (is.null(eval_log)) {
  cat("CẢNH BÁO: Vẫn không thấy log. Bình hãy chạy lại lệnh train này trước nhé:\n")
  
  # Chạy lại đoạn này để đảm bảo có watchlist
  xgb_model_final <- xgb.train(
    params = list(objective = "reg:squarederror", eta = 0.05, max_depth = 5),
    data = dtrain,
    nrounds = 300,
    watchlist = list(train = dtrain, test = dval), # Phải có dòng này mới có Log
    verbose = 0
  )
  
  # Lấy lại log sau khi đã train chuẩn
  eval_log <- xgb_model_final$evaluation_log
}

# --- BƯỚC 3: Vẽ biểu đồ ---
if (!is.null(eval_log)) {
  loss_data <- eval_log %>%
    pivot_longer(cols = contains("rmse"), names_to = "Dataset", values_to = "Loss")
  
  ggplot(loss_data, aes(x = iter, y = Loss, color = Dataset)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c("train_rmse" = "blue", "test_rmse" = "red")) +
    labs(title = "XGBoost Learning Curve", x = "Iterations", y = "RMSE") +
    theme_minimal()
} else {
cat("Lỗi hệ thống: Không thể trích xuất log. Bình hãy kiểm tra lại biến dtrain và dval.")
}

library(ggplot2)

# Vẽ biểu đồ So sánh Thực tế và Dự báo
ggplot(results_test, aes(x = actual, y = predicted)) +
  # Vẽ các điểm dữ liệu với độ trong suốt để tránh bị chồng lấp quá dày
  geom_point(alpha = 0.4, color = "#377EB8") + 
  # Vẽ đường chéo 45 độ (Dự báo hoàn hảo)
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Actual vs. Predicted Salary (XGBoost)",
    subtitle = paste("Test R2:", round(test_r2, 4), "| Test RMSE:", round(test_rmse, 4)),
    x = "Actual Log Salary",
    y = "Predicted Log Salary"
  ) +
  theme_minimal() +
  # Đảm bảo trục X và Y có cùng tỉ lệ để đường 45 độ chuẩn xác
  coord_fixed(ratio = 1) 

# Lưu ảnh chất lượng cao
ggsave("actual_vs_predicted_xgboost.png", width = 7, height = 7, dpi = 300)
