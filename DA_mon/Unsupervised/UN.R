setwd("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/DA_cuoimon")

dataset_tiktokers <- read.csv("C:\\Users\\Admin\\Desktop\\TANPHAT\\Dự án_canhan\\TIKTOK_V2\\clustering\\creator_features.csv")

head(dataset_tiktokers)

str(dataset_tiktokers)
summary(dataset_tiktokers)

tong_so_na_ban_dau <- sum(is.na(dataset_tiktokers))
tong_so_na_ban_dau

col_na <- colSums(is.na(dataset_tiktokers))
col_na


library(dplyr)

# 1. Định nghĩa các giá trị giả mạo missing
missing_values <- c('', ' ', 'null', 'Null', 'NULL', 'N/A', 'n/a', 'NaN', 'None', '-')

# 2. Replace toàn bộ thành NA
dataset_tiktokers <- dataset_tiktokers %>%
  mutate(across(everything(), ~ ifelse(. %in% missing_values, NA, .)))

# 3. Các cột cần ép về numeric
numeric_columns <- c('FOLLOWERS', 'VIEW_COUNT', 'LIKE_COUNT', 
                     'COMMENT_COUNT', 'SHARE_COUNT', 'ENGAGEMENT')

# 4. Ép kiểu numeric (coerce lỗi → NA)
dataset_tiktokers <- dataset_tiktokers %>%
  mutate(across(all_of(numeric_columns), ~ as.numeric(.)))

# 5. Đếm số lượng NA theo cột
null_counts <- colSums(is.na(dataset_tiktokers))

cat("Số lượng giá trị Null ở mỗi cột sau khi làm sạch bước 1:\n")

# Chỉ in các cột có NA > 0
print(null_counts[null_counts > 0])


# Danh sách các cột cần xóa
cac_cot_can_xoa <- c('PRICE', 'CATEGORY_x')

# Xóa cột (tự động ignore nếu cột không tồn tại)
dataset_tiktokers <- dataset_tiktokers %>%
  select(-any_of(cac_cot_can_xoa))

# In danh sách cột còn lại
cat("Danh sách các cột sau khi xóa:\n")
print(colnames(dataset_tiktokers))


# Bước 1: Tạo HAS_SHOP_LINK (0/1)
dataset_tiktokers <- dataset_tiktokers %>%
  mutate(HAS_SHOP_LINK = as.integer(!is.na(ANCHOR_TYPES)))

# Bước 2: Kiểm tra kiểu dữ liệu
cat("Kiểu dữ liệu của cột HAS_SHOP_LINK:\n")
print(class(dataset_tiktokers$HAS_SHOP_LINK))

# Bước 3: Xem 10 dòng đầu
print(dataset_tiktokers %>% 
        select(ANCHOR_TYPES, HAS_SHOP_LINK) %>% 
        head(10))

dataset_tiktokers <- dataset_tiktokers %>%
  mutate(
    HAS_BROADCAST_SCORE = as.integer(!is.na(BROADCAST_SCORE)),
    BROADCAST_SCORE = ifelse(is.na(BROADCAST_SCORE), median(BROADCAST_SCORE, na.rm = TRUE), BROADCAST_SCORE)
  )
dataset_tiktokers[,'HAS_BROADCAST_SCORE']

dataset_tiktokers$CATEGORY_y[is.na(dataset_tiktokers$CATEGORY_y)] <- "UnKnown"





library(mice)

cols_impute <- c("COLLAB_SCORE", "VQSCORE", "BITRATE")
extra_cols  <- c("FOLLOWERS", "VIEW_COUNT", "LIKE_COUNT", "COMMENT_COUNT", "SHARE_COUNT", "ENGAGEMENT")

use_cols <- c(cols_impute, extra_cols)
use_cols <- use_cols[use_cols %in% names(dataset_tiktokers)]

dataset_tiktokers$COLLAB_SCORE_missing <- as.integer(is.na(dataset_tiktokers$COLLAB_SCORE))
dataset_tiktokers$VQSCORE_missing      <- as.integer(is.na(dataset_tiktokers$VQSCORE))
dataset_tiktokers$BITRATE_missing      <- as.integer(is.na(dataset_tiktokers$BITRATE))

for (col in use_cols) {
  dataset_tiktokers[[col]] <- as.numeric(dataset_tiktokers[[col]])
}

data_impute <- dataset_tiktokers[, use_cols, drop = FALSE]

init <- mice(data_impute, maxit = 0, printFlag = FALSE)
meth <- init$method
pred <- init$predictorMatrix

meth[] <- ""
meth["COLLAB_SCORE"] <- "pmm"
meth["VQSCORE"]      <- "pmm"
meth["BITRATE"]      <- "pmm"

diag(pred) <- 0

imp <- mice(data_impute, method = meth, predictorMatrix = pred, m = 1, maxit = 5, seed = 42, printFlag = FALSE)
comp <- complete(imp)

dataset_tiktokers$COLLAB_SCORE <- comp$COLLAB_SCORE
dataset_tiktokers$VQSCORE      <- comp$VQSCORE
dataset_tiktokers$BITRATE      <- comp$BITRATE

summary(dataset_tiktokers)

for (col in cols_to_fix) {
  if (col %in% names(dataset_tiktokers)) {
    upper_limit <- quantile(dataset_tiktokers[[col]], 0.95, na.rm = TRUE)
    dataset_tiktokers[[col]][dataset_tiktokers[[col]] > upper_limit] <- upper_limit
  }
}


for (col in cols_to_fix) {
  if (col %in% names(dataset_tiktokers)) {
    dataset_tiktokers[[col]] <- sqrt(dataset_tiktokers[[col]])
  }
}


library(dplyr)

data_creator_notlog <- dataset_tiktokers %>%
  #1. Chọn các cột cần thiết (Thêm CREATOR_ID vào đầu để định danh)
  select(
    CREATOR_ID, 
    FOLLOWERS, FOLLOWING_COUNT, 
    ENGAGEMENT, TOTAL_LIKES, DIGG_COUNT, VIDEO_COUNT, 
    COLLAB_SCORE, HAS_BROADCAST_SCORE, PRICE_NUM,

  ) %>%
  # 2. Chỉ giữ lại các dòng có CREATOR_ID duy nhất
  # .keep_all = TRUE giúp giữ lại tất cả các cột đã select ở trên
  distinct(CREATOR_ID, .keep_all = TRUE)


# Giữ lại các dòng có FOLLOWERS khác 0
dataset_tiktokers <- dataset_tiktokers[dataset_tiktokers$FOLLOWERS != 0, ]

# 3. Lưu file
write.csv(data_creator_notlog, "data_creator_notlog.csv", row.names = FALSE)


summary(
  data_creator_notlog )












# --- 3.1 Khai báo nhóm biến ---
heavy_cols <- c(
  "FOLLOWERS","VIEW_COUNT","LIKE_COUNT","COMMENT_COUNT",
  "SHARE_COUNT","SAVE_COUNT","TOTAL_LIKES","DIGG_COUNT","BITRATE"
)

light_count <- c("VIDEO_COUNT","FOLLOWING_COUNT")

score_cols <- c("COLLAB_SCORE","VQSCORE")

sensitive_cols <- c("ENGAGEMENT","PRICE_NUM")

# Giữ các cột tồn tại
all_cols <- c(heavy_cols, light_count, score_cols, sensitive_cols)
all_cols <- all_cols[all_cols %in% names(dataset_tiktokers)]

# --- 3.2 Ép numeric ---
for (col in all_cols) {
  dataset_tiktokers[[col]] <- as.numeric(dataset_tiktokers[[col]])
}

# --- 3.3 Log cho heavy-tail ---
for (col in heavy_cols) {
  if (col %in% names(dataset_tiktokers)) {
    dataset_tiktokers[[col]] <- log1p(dataset_tiktokers[[col]])
  }
}

# --- 3.4 Log (tùy chọn) cho count nhẹ ---
for (col in light_count) {
  if (col %in% names(dataset_tiktokers)) {
    dataset_tiktokers[[col]] <- log1p(dataset_tiktokers[[col]])
  }
}

# --- 3.5 Winsorize nhẹ cho biến nhạy ---
winsorize <- function(x, p = 0.99) {
  upper <- quantile(x, p, na.rm = TRUE)
  lower <- quantile(x, 1 - p, na.rm = TRUE)  # thường không cần, nhưng để an toàn
  x[x > upper] <- upper
  x[x < lower] <- lower
  return(x)
}

if ("ENGAGEMENT" %in% names(dataset_tiktokers)) {
  dataset_tiktokers$ENGAGEMENT <- winsorize(dataset_tiktokers$ENGAGEMENT, 0.99)
}

if ("PRICE_NUM" %in% names(dataset_tiktokers)) {
  dataset_tiktokers$PRICE_NUM <- g(dataset_tiktokers$PRICE_NUM)
  dataset_tiktokers$PRICE_NUM <- winsorize(dataset_tiktokers$PRICE_NUM, 0.995)
}

# --- 3.6 Scale ---
cols_for_model <- all_cols
cols_for_model <- cols_for_model[cols_for_model %in% names(dataset_tiktokers)]

data_scaled <- scale(dataset_tiktokers[, cols_for_model])




cols <- c(
  "FOLLOWERS", "FOLLOWING_COUNT", "ENGAGEMENT", "TOTAL_LIKES",
  "DIGG_COUNT", "VIDEO_COUNT", "COLLAB_SCORE", "VIEW_COUNT",
  "LIKE_COUNT", "COMMENT_COUNT", "SHARE_COUNT", "SAVE_COUNT",
  "VQSCORE", "BITRATE", "PRICE_NUM"
)

cols <- cols[cols %in% names(dataset_tiktokers)]

# Chia layout: 3 hàng × 5 cột
par(mfrow = c(3, 5), mar = c(2,2,2,1))

for (col in cols) {
  boxplot(
    log1p(dataset_tiktokers[[col]]),
    main = col,
    col = "lightblue",
    outline = TRUE
  )
}


par(mfrow = c(3, 5), mar = c(2,2,2,1))

for (col in cols) {
  hist(
    dataset_tiktokers[[col]],
    main = col,
    col = "lightblue",
    breaks = 30
  )
}

dataset_tiktokers


# Checklist 
cols_2 <- c(
  "CREATOR_ID","VIDEO_ID","FOLLOWERS", "FOLLOWING_COUNT", 
  "ENGAGEMENT", "TOTAL_LIKES",
  "DIGG_COUNT", "VIDEO_COUNT", "COLLAB_SCORE", "VIEW_COUNT",
  "LIKE_COUNT", "COMMENT_COUNT", "SHARE_COUNT", "SAVE_COUNT",
  "VQSCORE", "BITRATE", "PRICE_NUM",'HAS_BROADCAST_SCORE','HAS_SHOP_LINK',
  'CATEGORY_y'
)

colSums(is.na(dataset_tiktokers[, cols_2]))

str(dataset_tiktokers[, cols_2])

dataset_tiktokers$HAS_BROADCAST_SCORE <- factor(dataset_tiktokers$HAS_BROADCAST_SCORE)
dataset_tiktokers$HAS_SHOP_LINK       <- factor(dataset_tiktokers$HAS_SHOP_LINK)

summary(factor(dataset_tiktokers$CATEGORY_TYPE) )
dataset_tiktokers$CATEGORY_TYPE <- factor(dataset_tiktokers$CATEGORY_TYPE)

dataset_tiktokers$VIDEO_ID <- as.character(dataset_tiktokers$VIDEO_ID)

tags <- unique(dataset_tiktokers['CATEGORY_y'])
#writeLines(as.character(tags), "tags_output.txt")




# =========================
# 1) TAXONOMY GROUPS
# =========================

group_map <- list(
  ENTERTAINMENT = c(
    "Comedy", "Lip Syncing", "Dance", "Skits", "Selfie",
    "Movies & TV", "Music", "Animation & Cosplay",
    "Theater & Stage", "Talent Showcase", "Supernatural & Horror",
    "Street Interviews & Social Experiments", "Slime, ASMR, & Aesthetics"
  ),
  
  "Comedy", "Lip Syncing", "Dance", "Skits", "Selfie",  "Daily Life",
  
  => 2 group =  "Comedy" , "Daily Life", 
  

  onehot 107 
  
  
  LIFESTYLE = c(
    "Daily Life", "Family", "Love & Romantic Relationships",
    "Campus Life", "Life Motivation", "Recreation Facility"
  ),
  
  FOOD = c(
    "Cooking & Recipes", "Mukbang & Food Tasting",
    "Food Display & Reviews", "Restaurant Exploration",
    "Food & Beverage", "Beverages & Production"
  ),
  
  BEAUTY = c(
    "Beauty Tutorials & Tips", "Skincare", "Hair Design & Care",
    "Haircare", "Cosmetics", "Beauty & Personal Care",
    "Other Beauty & Personal Care", "Nail Art & Care",
    "Fragrances & Perfumes", "Oral Care", "Feminine Care",
    "Health & Wellness", "Health", "Wig & Hair Styling"
  ),
  
  FASHION = c(
    "Outfits", "Apparel & Accessories", "Other Apparel & Accessories",
    "Women's Clothing", "Men's Clothing", "Women's Shoes",
    "Men's Shoes", "Clothing Accessories", "Ordinary Jewelry",
    "Watches", "Bags", "Fashion and Body Art"
  ),
  
  GAMING = c(
    "Video Games", "Games", "Non-video Games",
    "Action Games", "RPG Games", "Shooting Games"
  ),
  
  TECH = c(
    "Tech Products & Tests", "Tech & Electronics",
    "Software & Apps", "Apps", "Photography & Special Effects",
    "Wearable Tech Devices"
  ),
  
  COMMERCE = c(
    "E-Commerce (Non-app)", "Household Products",
    "Appliances", "Life Services"
  ),
  
  EDUCATION = c(
    "Education", "School Education", "Humanities & Science",
    "Career Development", "Jobs & Careers",
    "Business & Finance", "Business Services", "Financial Services", "Art"
  ),
  
  NEWS = c(
    "News & Entertainment", "Social News & Events",
    "Entertainment News", "Sports News"
  ),
  
  PETS_NATURE = c(
    "Pet Tips & Care", "Pets", "Wildlife & Exotic Pets",
    "Nature & Scenery", "Fishing, Hunting, & Camping"
  ),
  
  SPORTS = c(
    "Fitness", "Traditional Sports",
    "Extreme Sports", "Sports & Outdoor"
  ),
  
  TRAVEL = c(
    "Travel", "Automobiles",
    "Vehicle & Transportation", "Other transportation"
  ),
  
  FAMILY_KIDS = c(
    "Baby, Kids & Maternity", "Babies"
  ),
  
  HOME_DIY = c(
    "Home Improvement", "Home & Garden", "DIY & Life Hacks"
  ),
  
  UNKNOWN = c(
    "UnKnown"
  )
)

# =========================
# 2) BUILD EXACT LOOKUP
# =========================

# kiểm tra 1 tag không bị gán vào nhiều group
all_tags <- unlist(group_map, use.names = FALSE)
dup_tags <- unique(all_tags[duplicated(all_tags)])

if (length(dup_tags) > 0) {
  stop(
    paste(
      "Các tag sau đang xuất hiện ở nhiều hơn 1 group:",
      paste(dup_tags, collapse = ", ")
    )
  )
}

# reverse lookup exact match
tag_to_group <- setNames(
  rep(names(group_map), times = sapply(group_map, length)),
  unlist(group_map, use.names = FALSE)
)

# =========================
# 3) HELPER FUNCTIONS
# =========================

split_tags_exact <- function(x) {
  if (is.na(x) || trimws(x) == "") return(character(0))
  # split theo dấu phẩy + khoảng trắng
  # chú ý: với tag có dấu phẩy bên trong như "Baby, Kids & Maternity"
  # dữ liệu của bạn đang lưu theo dạng chuỗi category ghép bằng ", "
  # nên ta cần khôi phục đúng tag bằng exact matching theo dictionary
  raw_parts <- unlist(strsplit(x, ",\\s*"))
  raw_parts <- trimws(raw_parts)
  raw_parts <- raw_parts[raw_parts != ""]
  raw_parts
}

# vì có các tag bản thân chứa dấu phẩy như "Baby, Kids & Maternity",
# ta cần parse lại dựa trên dictionary known tags thay vì split thuần.
parse_tags_by_dictionary <- function(x, known_tags_sorted) {
  if (is.na(x) || trimws(x) == "") return(character(0))
  
  s <- trimws(x)
  out <- character(0)
  
  while (nchar(s) > 0) {
    matched <- FALSE
    
    for (tag in known_tags_sorted) {
      # exact ở đầu chuỗi
      if (startsWith(s, tag)) {
        out <- c(out, tag)
        s <- sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", tag)), "", s)
        s <- sub("^,\\s*", "", s)
        s <- trimws(s)
        matched <- TRUE
        break
      }
    }
    
    if (!matched) {
      # nếu không match được exact dictionary, tách thô phần đầu để debug
      next_piece <- sub(",.*$", "", s)
      out <- c(out, paste0("UNMAPPED::", trimws(next_piece)))
      s <- sub("^[^,]+,\\s*", "", s)
      if (identical(s, next_piece)) break
      s <- trimws(s)
    }
  }
  
  unique(out)
}

# sort known tags theo độ dài giảm dần để match tag dài trước
known_tags_sorted <- names(tag_to_group)
known_tags_sorted <- known_tags_sorted[order(nchar(known_tags_sorted), decreasing = TRUE)]

map_tags_to_groups <- function(x, tag_to_group, known_tags_sorted) {
  tags <- parse_tags_by_dictionary(x, known_tags_sorted)
  
  if (length(tags) == 0) return("UNKNOWN")
  
  # nếu có unmapped debug token
  unmapped_debug <- grepl("^UNMAPPED::", tags)
  clean_tags <- tags[!unmapped_debug]
  
  groups <- unique(unname(tag_to_group[clean_tags]))
  groups <- groups[!is.na(groups)]
  
  # nếu không map được group nào
  if (length(groups) == 0) groups <- "UNKNOWN"
  
  paste(sort(unique(groups)), collapse = ", ")
}

extract_unmapped_tags <- function(x, known_tags_sorted) {
  tags <- parse_tags_by_dictionary(x, known_tags_sorted)
  unmapped <- tags[grepl("^UNMAPPED::", tags)]
  sub("^UNMAPPED::", "", unmapped)
}

# =========================
# 4) APPLY MAPPING
# =========================

dataset_tiktokers$CATEGORY_GROUP <- sapply(
  dataset_tiktokers$CATEGORY_y,
  map_tags_to_groups,
  tag_to_group = tag_to_group,
  known_tags_sorted = known_tags_sorted
)

# =========================
# 5) ONE-HOT ENCODING
# =========================

group_names <- names(group_map)

for (grp in group_names) {
  col_name <- paste0("grp_", grp)
  dataset_tiktokers[[col_name]] <- as.integer(
    sapply(
      strsplit(dataset_tiktokers$CATEGORY_GROUP, ",\\s*"),
      function(v) grp %in% v
    )
  )
}

# số group mỗi dòng
dataset_tiktokers$CATEGORY_GROUP_COUNT <- sapply(
  strsplit(dataset_tiktokers$CATEGORY_GROUP, ",\\s*"),
  function(v) length(unique(v[v != ""]))
)

# =========================
# 6) QUALITY CHECKS
# =========================

# 6.1 kiểm tra phân phối group
cat("Tần suất group:\n")
print(sort(colSums(dataset_tiktokers[paste0("grp_", group_names)]), decreasing = TRUE))

# 6.2 lấy tất cả tag chưa map được exact
all_unmapped <- unlist(
  lapply(dataset_tiktokers$CATEGORY_y, extract_unmapped_tags, known_tags_sorted = known_tags_sorted),
  use.names = FALSE
)

all_unmapped <- sort(table(all_unmapped), decreasing = TRUE)

cat("\nCác tag chưa được map (nếu có):\n")
print(all_unmapped)

# 6.3 xem vài dòng kết quả
cat("\nPreview mapping:\n")
print(
  head(
    data.frame(
      CATEGORY_y = dataset_tiktokers$CATEGORY_y,
      CATEGORY_GROUP = dataset_tiktokers$CATEGORY_GROUP
    ),
    20
  )
)


dataset_tiktokers$CATEGORY_GROUP 

apply(dataset_tiktokers[, cols_2], 2, var, na.rm = TRUE)

str(dataset_tiktokers)
cor_matrix <- cor(dataset_tiktokers[, cols_for_model], use = "complete.obs")



# Cài đặt nếu bạn chưa có
# install.packages("corrplot")
library(corrplot)

# 1. Tính toán ma trận tương quan
cor_matrix <- cor(dataset_tiktokers[, cols_for_model], use = "complete.obs")
par(mfrow = c(1, 1))

#install.packages("ggcorrplot")
library(ggcorrplot)

# Tính toán ma trận
cor_matrix <- cor(dataset_tiktokers[, cols_for_model], use = "complete.obs")

# Vẽ với kích thước chữ nhỏ hơn
ggcorrplot(cor_matrix, 
           type = "lower",        # Chỉ hiện 1 nửa để không gian thoáng hơn
           hc.order = TRUE,      # Nhóm các biến tương quan (Cực kỳ quan trọng để dễ nhìn)
           colors = c("#6D9EC1", "white", "#E46726"),
           # --- Chìa khóa xử lý chữ to ---
           # Giảm fontsize để tránh chèn ép
           tl.cex = 8,           # Kích thước nhãn tên biến (ví dụ: VIEW_COUNT)
           lab = TRUE,           # Hiện số tương quan
           lab_size = 2,         # Kích thước con số trong ô (giảm xuống RẤT nhỏ)
           # ----------------------------
           title = "TikTokers Correlation Matrix"
) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # Xoay nhãn trục X


names(dataset_tiktokers)



library(dplyr)

data_creator1 <- dataset_tiktokers %>%
#1. Chọn các cột cần thiết (Thêm CREATOR_ID vào đầu để định danh)
 select(
    CREATOR_ID, 
    FOLLOWERS, FOLLOWING_COUNT, 
    ENGAGEMENT, TOTAL_LIKES, DIGG_COUNT, VIDEO_COUNT, 
    COLLAB_SCORE, HAS_BROADCAST_SCORE, PRICE_NUM, TIER_LABEL,
    starts_with("grp_"), 
    CATEGORY_GROUP_COUNT
  ) %>%
  # 2. Chỉ giữ lại các dòng có CREATOR_ID duy nhất
  # .keep_all = TRUE giúp giữ lại tất cả các cột đã select ở trên
distinct(CREATOR_ID, .keep_all = TRUE)

# 3. Lưu file
write.csv(data_creator1, "data_creator_unique2.csv", row.names = FALSE)

#a <- read.csv('data_creator_unique.csv')
#View(a)










