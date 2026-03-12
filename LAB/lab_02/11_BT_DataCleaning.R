###########Giới thiệu dataset 
# Đây là bộ dữ liệu y tế cực kỳ giá trị từ kho UCI, được thu thập từ năm 1992 đến 1995.
# tên là Myocardial Infarction Complications (Biến chứng nhồi máu cơ tim)

# Load data 
news <- read.csv("Myocardial infarction complications Database.csv")
View(news)
#Hiểu Data

# Đọc 10 dòng đầu tiên 
head(news,10)

# Xem phân phối, cấu trúc tổng quát các cột
summary(news)
str(news)


# Xem số lượng cột/dòng
length(news) 
nrow(news)

dim(news) # Xem cả 2 thông tin 

# Xét Missing Value 
news[!complete.cases(news),]

# Đếm số lượng Missing 
#Tổng số lượng NA trong toàn bộ
sum(is.na(news))

# Số  dòng có ít nhất 1 NA 
sum(!complete.cases(news))

# ==> Số lượng NA cực kỳ lớn, không có bệnh nhân nào được ghi chép đầy đủ

# Xem số lượng NA của từng col
col_na <- colSums(is.na(news))
col_na
# Xem top 10 col có NA nhiều nhất 
sort(col_na, decreasing = TRUE)[1:10]

# Giữ lại những cột có tỷ lệ NA < 80% (tức là < 0.8)
news <- news[, colMeans(is.na(news)) < 0.8]


# Chỉ giữ lại các dòng có số lượng NA ít hơn 40% tổng số cột
# is.na trả về TRUE/FALSE, rowSums cộng lại để biết số lượng NA mỗi dòng

news <- news[rowSums(is.na(news)) < (ncol(news) * 0.4 ),]

# Xử lý AGE
summary(news$AGE)
str(news$AGE)
# xử lý bằng median cho AGE -> không xóa -> tránh mất dữ liệu
news[is.na(news$AGE), "AGE"] <- median(news$AGE, na.rm = TRUE)

# Xử lý INF_ANAM Số lần nhồi máu cơ tim trước đây
summary(factor(news$INF_ANAM))
unique(news$INF_ANAM)

news$INF_ANAM[is.na(news$INF_ANAM)] <- median(news$INF_ANAM, na.rm = TRUE)

colSums(is.na(news))

# Xử lý GB Huyết áp cao
news[is.na(news$GB),"GB"] <- median(news$GB, na.rm = TRUE)

#Xử lý  SIM_GIPERT  
news[is.na(news$SIM_GIPERT),"SIM_GIPERT"] <- median(news$SIM_GIPERT, na.rm = TRUE)

# XỬ lý bệnh lý về phổi zab_leg_01,2,3,4,6, vê mặc y tế thì gán = 0
zab_cols <- c("zab_leg_01", "zab_leg_02", "zab_leg_03", "zab_leg_04", "zab_leg_06")
news[zab_cols] <- lapply(news[zab_cols] , function (x) {x[is.na(x) ] <- 0  
                                                            return (x) 
                                                            })


# Xử lý IM_PG_P Nhồi máu thất phải thường đi kèm với Nhồi máu cơ tim thành dưới (Inferior MI).
news$IM_PG_P[is.na(news$IM_PG_P) & news$inf_im > 0 ] <- 1

#Xử lý NITR_S Nitrates thường được dùng cho các ca có Đau thắt ngực (STENOK_AN) hoặc Phù phổi (OTEK_LANC)
# Có phù phổi và thiếu NITR_S -> Gán là 1 
news$NITR_S[is.na(news$NITR_S) & news$OTEK_LANC == 1] <- 1
# Các ca thiếu còn lại -> Gán là 0
news$NITR_S[is.na(news$NITR_S)] <- 0

#Xử lý Fibrinolytic therapy (fibr_ter_01 đến fibr_ter_08) các loại thuốc "làm tan cục máu đông" để tái thông mạch vành
fibr_cols <- c("fibr_ter_01", "fibr_ter_02", "fibr_ter_03", "fibr_ter_05", "fibr_ter_06", "fibr_ter_07", "fibr_ter_08")

news[fibr_cols] <- lapply(news[fibr_cols], function (x) { x[is.na(x)] <- median(x, na.rm = TRUE)})

# Xử lý NA_R_1_n Sử dụng thuốc giảm đau gây nghiện
news$NA_R_1_n[is.na(news$NA_R_1_n)] <-median(news$NA_R_1_n, na.rm = TRUE)

# Xử lý Nhóm nr (Rối loạn nhịp)
specific_arrhythmia <- c("nr_01", "nr_02", "nr_03", "nr_04", "nr_07", "nr_08","np_09","np_10")
# XỬ lý nr_11 
# Gán nr11 bằng 1 cho những dòng có ít nhất một triệu chứng cụ thể là 1
news[rowSums(news[specific_arrhythmia], na.rm = TRUE) > 0, "nr11"] <- 1
# Kiểm tra những ca Có tiền sử rối loạn nhịp nói chung
news[rowSums(news[c("nr_01", "nr_02", "nr_03", "nr_04", "nr_07", "nr_08","np_09","np_10")]) > 0 & news$nr_11 == 1 , ]
news[specific_arrhythmia] <- lapply(news[specific_arrhythmia] , function (x) {x[is.na(x)] <- -1 
                                                                                            return (x) 
                                                                                } )
# có rối loạn nhưng ko biết loại 

#Xử lý Nhóm np (Rối loạn dẫn truyền/Block)
# Danh sách các cột Block dẫn truyền
np_cols <- c("np_01", "np_04", "np_05", "np_07", "np_08", "np_09", "np_10")

news[np_cols] <- lapply(news[np_cols], function(x) {
  x[is.na(x) | x == 63 ] <- median(x, na.rm = TRUE)
  return(x)
})

# Nhóm nội tiết (endocr)
# Danh sách các cột nội tiết
endocr_cols <- c("endocr_01", "endocr_02", "endocr_03")

news[endocr_cols] <- lapply(news[endocr_cols], function(x) {
  x[is.na(x)] <- 0
  return(x)
})

# Danh sách các cột thuốc giảm đau theo ngày
pain_meds <- c("NA_R_1_n", "NA_R_2_n", "NA_R_3_n", 
               "NOT_NA_1_n", "NOT_NA_2_n", "NOT_NA_3_n")

# Gán NA = 0 cho tất cả các cột này vì tử vong hoặc không nhận thuốc
news[pain_meds] <- lapply(news[pain_meds], function(x) {
  x[is.na(x)] <- 0
  return(x)
})

# Xử lý Thuốc dùng bởi Đội cấp cứu (LID_KB)
news[is.na(news$LID_KB),"LID_KB"] <- -1 # Trường hợp khác  


colSums(is.na(news))
View(news)


#Xử lý STENOK_AN , FK_STENOK

#FK_STENOK (Cột 6): Phân loại chức năng của đau thắt ngực trong năm qua. Giá trị 0 nghĩa là "không có đau thắt ng ", 1 là đau loại 1 .
#STENOK_AN (Cột 5): Tiền sử đau thắt ngực gắng sức. Giá trị 0 là "chưa bao giờ", và 1 là "trong vòng 1 năm qua"

news$FK_STENOK[is.na(news$FK_STENOK) & news$STENOK_AN == 0] <- 0 
news$FK_STENOK[is.na(news$FK_STENOK) & news$STENOK_AN > 0 ] <- median(news$FK_STENOK, na.rm = TRUE)

news$STENOK_AN[is.na(news$STENOK_AN) & news$FK_STENOK == 0] <- 0 
news$STENOK_AN[is.na(news$STENOK_AN) & news$FK_STENOK > 0 ] <- median(news$STENOK_AN, na.rm = TRUE) 



colSums(is.na(news))






