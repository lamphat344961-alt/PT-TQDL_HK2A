setwd("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/LAB/lab_02")

# ============================================================
# BƯỚC 0: ĐỌC VÀ KHÁM PHÁ DỮ LIỆU BAN ĐẦU
# ============================================================

# Đọc file CSV vào R
du_lieu <- read.csv("Myocardial infarction complications Database.csv")

# Xem 6 dòng đầu tiên để biết dữ liệu trông như thế nào
head(du_lieu)

# Xem 10 dòng đầu tiên
head(du_lieu, 10)

# Xem cấu trúc tổng quát: kiểu dữ liệu từng cột
str(du_lieu)

# Xem thống kê tóm tắt: min, max, mean, số NA...
summary(du_lieu)


# Liệt kê tất cả các cột nhị phân (chỉ có giá trị 0 và 1)
cac_cot_nhi_phan <- c(
  # Nhân khẩu học
  "SEX", "SIM_GIPERT",
  
  # Loạn nhịp tiền sử (nr)
  "nr_11", "nr_01", "nr_02", "nr_03", "nr_04", "nr_07", "nr_08",
  
  # Block dẫn truyền tiền sử (np)
  "np_01", "np_04", "np_05", "np_07", "np_08", "np_09", "np_10",
  
  # Bệnh lý nội tiết
  "endocr_01", "endocr_02", "endocr_03",
  
  # Bệnh phổi mãn tính
  "zab_leg_01", "zab_leg_02", "zab_leg_03", "zab_leg_04", "zab_leg_06",
  
  # Biến chứng nhập viện
  "O_L_POST", "K_SH_POST", "MP_TP_POST", "SVT_POST", "GT_POST", "FIB_G_POST",
  
  # Vị trí nhồi máu thất phải
  "IM_PG_P",
  
  # Nhịp cơ bản ECG (ritm_ecg_p)
  "ritm_ecg_p_01", "ritm_ecg_p_02", "ritm_ecg_p_04",
  "ritm_ecg_p_06", "ritm_ecg_p_07", "ritm_ecg_p_08",
  
  # Loạn nhịp ECG (n_r_ecg_p)
  "n_r_ecg_p_01", "n_r_ecg_p_02", "n_r_ecg_p_03", "n_r_ecg_p_04",
  "n_r_ecg_p_05", "n_r_ecg_p_06", "n_r_ecg_p_08", "n_r_ecg_p_09", "n_r_ecg_p_10",
  
  # Block dẫn truyền ECG (n_p_ecg_p)
  "n_p_ecg_p_01", "n_p_ecg_p_03", "n_p_ecg_p_04", "n_p_ecg_p_05",
  "n_p_ecg_p_06", "n_p_ecg_p_07", "n_p_ecg_p_08", "n_p_ecg_p_09",
  "n_p_ecg_p_10", "n_p_ecg_p_11", "n_p_ecg_p_12",
  
  # Thuốc tiêu sợi huyết (fibr_ter)
  "fibr_ter_01", "fibr_ter_02", "fibr_ter_03", "fibr_ter_05",
  "fibr_ter_06", "fibr_ter_07", "fibr_ter_08"
  
)

# Chuyển tất cả một lần bằng lapply
# levels = c(0, 1): 0 là nhóm tham chiếu (Không)
# labels = c("Khong", "Co"): đặt nhãn dễ đọc
du_lieu[cac_cot_nhi_phan] <- lapply(
  du_lieu[cac_cot_nhi_phan],
  function(x) factor(x, levels = c(0, 1), labels = c("Khong", "Co"))
)

# Kiểm tra kích thước ban đầu: bao nhiêu dòng, bao nhiêu cột
dim(du_lieu)

# Đếm tổng số ô bị thiếu (NA) trong toàn bộ bảng
tong_so_na_ban_dau <- sum(is.na(du_lieu))
tong_so_na_ban_dau

# Xem số lượng NA của từng col
col_na <- colSums(is.na(du_lieu))
col_na
# Xem top 10 col có NA nhiều nhất 
sort(col_na, decreasing = TRUE)[1:10]


# ============================================================
# BƯỚC 1: XÓA CÁC CỘT QUÁ NHIỀU GIÁ TRỊ THIẾU (> 60%)
# ============================================================
# Lý do: Nếu một cột thiếu quá nhiều (ví dụ 95-99%), 
# thì dù ta điền vào cũng không có ý nghĩa thực tế.
# Tốt hơn là bỏ hẳn cột đó đi.

# Giữ lại những cột có tỷ lệ NA < 80% (tức là < 0.8)
du_lieu <- du_lieu[, colMeans(is.na(du_lieu)) < 0.6]



# ============================================================
# BƯỚC 2: XÓA CÁC DÒNG QUÁ NHIỀU GIÁ TRỊ THIẾU (> 40%)
# ============================================================
# Lý do: Nếu một hồ sơ bệnh nhân thiếu hơn 40% thông tin,
# hồ sơ đó không còn đáng tin cậy để phân tích.

# Tính ngưỡng: 40% số cột hiện tại
nguong_dong <- ncol(du_lieu) * 0.4

# Đếm số NA trong mỗi dòng, chỉ giữ những dòng có ít NA hơn ngưỡng
du_lieu <- du_lieu[rowSums(is.na(du_lieu)) < nguong_dong, ]


# ============================================================
# BƯỚC 3: XỬ LÝ BIẾN NHÂN KHẨU HỌC
# ============================================================

# --- Cột AGE: Tuổi bệnh nhân ---
# Kiểm tra xem cột AGE có bị NA không
sum(is.na(du_lieu$AGE))

# Điền bằng Trung vị (Median)
# Lý do dùng Median: Tuổi không phân phối chuẩn, Median ít bị ảnh hưởng bởi giá trị cực đoa

gia_tri_median_tuoi <- median(du_lieu$AGE, na.rm = TRUE)

du_lieu$AGE[is.na(du_lieu$AGE)] <- gia_tri_median_tuoi

# Kiểm tra lại: 
sum(is.na(du_lieu$AGE))


# --- Cột SEX: Giới tính (1 = Nam, 0 = Nữ) ---
# Kiểm tra
sum(is.na(du_lieu$SEX))


# --- Cột INF_ANAM: Tiền sử nhồi máu cũ (1 = Có, 0 = Không) ---
# Kiểm tra
sum(is.na(du_lieu$INF_ANAM))

# Điền bằng 0: Đa số bệnh nhân trong tập này là lần đầu bị nhồi máu
du_lieu$INF_ANAM[is.na(du_lieu$INF_ANAM)] <- median(du_lieu$INF_ANAM, na.rm = TRUE)

# Kiểm tra lại
sum(is.na(du_lieu$INF_ANAM))


# --- Cột GB: Tăng huyết áp (Hypertension) ---

sum(is.na(du_lieu$GB))
du_lieu$GB[is.na(du_lieu$GB)] <- median(du_lieu$GB, na.rm = TRUE)

sum(is.na(du_lieu$GB))


# --- Cột SIM_GIPERT: Tăng huyết áp thứ phát ---

sum(is.na(du_lieu$SIM_GIPERT))
du_lieu$SIM_GIPERT[is.na(du_lieu$SIM_GIPERT)] <- median(du_lieu$SIM_GIPERT, na.rm = TRUE)

sum(is.na(du_lieu$SIM_GIPERT))



# ============================================================
# BƯỚC 4: XỬ LÝ NHÓM BIẾN NỘI TIẾT (ENDOCRINE)
# ============================================================

# --- endocr_01: Đái tháo đường (Diabetes Mellitus) ---
# Đái tháo đường làm hỏng mạch máu nhỏ, khiến nhồi máu nặng hơn
sum(is.na(du_lieu$endocr_01))
du_lieu$endocr_01[is.na(du_lieu$endocr_01)] <- 0

sum(is.na(du_lieu$endocr_01))

# --- endocr_02: Béo phì (Obesity) ---
sum(is.na(du_lieu$endocr_02))
du_lieu$endocr_02[is.na(du_lieu$endocr_02)] <- 0

sum(is.na(du_lieu$endocr_02))

# --- endocr_03: Nhiễm độc giáp (Thyrotoxicosis) ---
# Gây nhịp tim nhanh, tăng nguy cơ loạn nhịp
sum(is.na(du_lieu$endocr_03))
du_lieu$endocr_03[is.na(du_lieu$endocr_03)] <- 0

sum(is.na(du_lieu$endocr_03))



# ============================================================
# BƯỚC 5: XỬ LÝ NHÓM BIẾN BỆNH PHỔI MÃN TÍNH
# ============================================================

# --- zab_leg_01: Viêm phế quản mãn tính ---
sum(is.na(du_lieu$zab_leg_01))

du_lieu$zab_leg_01[is.na(du_lieu$zab_leg_01)] <- 0

# --- zab_leg_02: Hen phế quản (Bronchial Asthma) ---
sum(is.na(du_lieu$zab_leg_02))

du_lieu$zab_leg_02[is.na(du_lieu$zab_leg_02)] <- 0

# --- zab_leg_03: Lao phổi (Tuberculosis) ---
sum(is.na(du_lieu$zab_leg_03))

du_lieu$zab_leg_03[is.na(du_lieu$zab_leg_03)] <- 0

# --- zab_leg_04: Khí phế thũng (Emphysema) ---
sum(is.na(du_lieu$zab_leg_04))

du_lieu$zab_leg_04[is.na(du_lieu$zab_leg_04)] <- 0

# --- zab_leg_06: Các bệnh phổi khác ---
sum(is.na(du_lieu$zab_leg_06))

du_lieu$zab_leg_06[is.na(du_lieu$zab_leg_06)] <- 0

# du_lieu <- lapply (du_lieu(c()), funciton (x) {x[is.na(x)] <- 0 return (x) } )


# ============================================================
# BƯỚC 6: XỬ LÝ VỊ TRÍ NHỒI MÁU (INFARCTION LOCATION)
# ============================================================
# Vị trí nhồi máu quyết định mức độ nguy hiểm và biến chứng.
# Nhồi máu thành TRƯỚC (ant_im) thường nặng nhất.

# --- ant_im: Thành trước (Anterior) ---
# Liên quan đến động mạch LAD - hay gặp nhất và nguy hiểm nhất

sum(is.na(du_lieu$ant_im))
du_lieu$ant_im[is.na(du_lieu$ant_im)] <- 0

# --- lat_im: Thành bên (Lateral) ---

sum(is.na(du_lieu$lat_im))
du_lieu$lat_im[is.na(du_lieu$lat_im)] <- 0

# --- inf_im: Thành dưới (Inferior) ---
# Thường do tắc động mạch vành phải (RCA)

sum(is.na(du_lieu$inf_im))
du_lieu$inf_im[is.na(du_lieu$inf_im)] <- 0

# --- post_im: Thành sau (Posterior) ---

sum(is.na(du_lieu$post_im))
du_lieu$post_im[is.na(du_lieu$post_im)] <- 0


# du_lieu <- lapply (du_lieu(c()), funciton (x) {x[is.na(x)] <- 0 return (x) } )

# --- IM_PG_P: Nhồi máu thất phải (Right Ventricle) ---
# Logic đặc biệt: Nếu bệnh nhân bị nhồi máu thành dưới (inf_im > 0)
# thì khả năng cao cũng bị nhồi máu thất phải (cùng mạch máu RCA nuôi)

du_lieu$IM_PG_P[is.na(du_lieu$IM_PG_P) & du_lieu$inf_im > 0] <- 1

# Những ca còn lại (không có inf_im) thì gán = 0
du_lieu$IM_PG_P[is.na(du_lieu$IM_PG_P)] <- 0

# Kiểm tra kết quả logic
sum(is.na(du_lieu$IM_PG_P))



# ============================================================
# BƯỚC 7: XỬ LÝ NHÓM BIẾN LOẠN NHỊP TIM (ARRHYTHMIA)
# ============================================================
# Loạn nhịp là biến chứng cực kỳ nguy hiểm sau nhồi máu.
# Đặc biệt: Rung thất (nr_07) có thể gây tử vong trong vài phút.

# --- nr_01: Ngoại tâm thu nhĩ (Atrial Premature Beats) ---

sum(is.na(du_lieu$nr_01))
du_lieu$nr_01[is.na(du_lieu$nr_01)] <- 0

# --- nr_02: Ngoại tâm thu thất (Ventricular Premature Beats) ---

# Dấu hiệu tim đang bị kích ứng mạnh
sum(is.na(du_lieu$nr_02))
du_lieu$nr_02[is.na(du_lieu$nr_02)] <- 0

# --- nr_03: Rung nhĩ kịch phát (Paroxysmal Atrial Fibrillation) ---

sum(is.na(du_lieu$nr_03))
du_lieu$nr_03[is.na(du_lieu$nr_03)] <- 0

# --- nr_04: Rung nhĩ mãn tính (Chronic Atrial Fibrillation) ---

sum(is.na(du_lieu$nr_04))
du_lieu$nr_04[is.na(du_lieu$nr_04)] <- 0

# --- nr_07: Rung thất (Ventricular Fibrillation) ---
# NGUY HIỂM NHẤT: Tim rung lên không bơm được máu, ngừng tim ngay lập tức

sum(is.na(du_lieu$nr_07))
du_lieu$nr_07[is.na(du_lieu$nr_07)] <- 0

# --- nr_08: Nhịp nhanh thất kịch phát (Paroxysmal Ventricular Tachycardia) ---

sum(is.na(du_lieu$nr_08))
du_lieu$nr_08[is.na(du_lieu$nr_08)] <- 0

# du_lieu <- lapply (du_lieu(c()), funciton (x) {x[is.na(x)] <- 0 return (x) } )


# --- nr_11: Tiền sử loạn nhịp nói chung ---
# Logic: Nếu bệnh nhân có bất kỳ loại loạn nhịp nào ở trên thì nr_11 = 1

specific_arrhythmia <- c("nr_01", "nr_02", "nr_03", "nr_04", "nr_07", "nr_08","np_09","np_10")

du_lieu[specific_arrhythmia] <- lapply(du_lieu[specific_arrhythmia] , function (x) {x[is.na(x)] <- -1 
                                                                            return (x) 
                                                                                            } )
du_lieu$nr_11[is.na(du_lieu$nr_11)] <- 0

# Kiểm tra
sum(is.na(du_lieu$nr_11))


# ============================================================
# BƯỚC 8: XỬ LÝ NHÓM BIẾN BLOCK DẪN TRUYỀN (CONDUCTION BLOCK)
# ============================================================
# Block dẫn truyền: Xung điện bị nghẽn, không truyền được từ nhĩ xuống thất.
# Tùy mức độ, có thể cần đặt máy tạo nhịp khẩn cấp.

# --- np_01: Block AV độ 1 (Nhẹ nhất, thường không cần can thiệp) ---

sum(is.na(du_lieu$np_01))
du_lieu$np_01[is.na(du_lieu$np_01)] <- 0

# --- np_04: Block AV hoàn toàn độ 3 (Nặng nhất, cần máy tạo nhịp) ---

sum(is.na(du_lieu$np_04))
du_lieu$np_04[is.na(du_lieu$np_04)] <- 0

# --- np_05: Block nhánh trái phân nhánh trước ---

sum(is.na(du_lieu$np_05))
du_lieu$np_05[is.na(du_lieu$np_05)] <- 0

# --- np_07: Block nhánh trái không hoàn toàn ---

sum(is.na(du_lieu$np_07))
du_lieu$np_07[is.na(du_lieu$np_07)] <- 0

# --- np_08: Block nhánh trái hoàn toàn (Left Bundle Branch Block) ---

sum(is.na(du_lieu$np_08))
du_lieu$np_08[is.na(du_lieu$np_08)] <- 0

# --- np_09: Block nhánh phải không hoàn toàn ---

sum(is.na(du_lieu$np_09))
du_lieu$np_09[is.na(du_lieu$np_09)] <- 0

# --- np_10: Block nhánh phải hoàn toàn (Right Bundle Branch Block) ---

sum(is.na(du_lieu$np_10))
du_lieu$np_10[is.na(du_lieu$np_10)] <- 0


# du_lieu <- lapply (du_lieu(c()), funciton (x) {x[is.na(x)] <- 0 return (x) } )


# ============================================================
# BƯỚC 9: XỬ LÝ BIẾN ĐAU THẮT NGỰC (ANGINA)
# ============================================================
# STENOK_AN: Bệnh nhân có tiền sử đau thắt ngực không? (1 = Có, 0 = Không)
# FK_STENOK: Phân độ chức năng của đau thắt ngực (0, 1, 2, 3, 4)

# Logic bước 1: Nếu biết phân độ FK > 0 tức là có đau thắt ngực → STENOK_AN = 1
du_lieu$STENOK_AN[is.na(du_lieu$STENOK_AN) & du_lieu$FK_STENOK > 0] <- 1


# Logic bước 2: Nếu STENOK_AN = 0 (không có đau thắt ngực) → phân độ FK = 0
du_lieu$FK_STENOK[is.na(du_lieu$FK_STENOK) & du_lieu$STENOK_AN == 0] <- 0

# Các ca còn lại không rõ: điền 0 (không có tiền sử)

du_lieu$STENOK_AN[is.na(du_lieu$STENOK_AN)] <- 0
du_lieu$FK_STENOK[is.na(du_lieu$FK_STENOK)] <- 0




# ============================================================
# BƯỚC 10: XỬ LÝ THUỐC GIẢM ĐAU OPIOID (NARCOTIC ANALGESICS)
# ============================================================

sum(is.na(du_lieu$NA_R_1_n))
du_lieu$NA_R_1_n[is.na(du_lieu$NA_R_1_n)] <- 0

sum(is.na(du_lieu$NA_R_2_n))
du_lieu$NA_R_2_n[is.na(du_lieu$NA_R_2_n)] <- 0

sum(is.na(du_lieu$NA_R_3_n))
du_lieu$NA_R_3_n[is.na(du_lieu$NA_R_3_n)] <- 0


# du_lieu <- lapply (du_lieu(c()), funciton (x) {x[is.na(x)] <- 0 return (x) } )


# ============================================================
# BƯỚC 11: XỬ LÝ THUỐC GIẢM ĐAU KHÔNG OPIOID (NON-NARCOTIC)
# ============================================================
# NOT_NA_1_n, NOT_NA_2_n, NOT_NA_3_n: Số lần dùng NSAID ngày 1, 2, 3

sum(is.na(du_lieu$NOT_NA_1_n))
du_lieu$NOT_NA_1_n[is.na(du_lieu$NOT_NA_1_n)] <- 0

sum(is.na(du_lieu$NOT_NA_2_n))
du_lieu$NOT_NA_2_n[is.na(du_lieu$NOT_NA_2_n)] <- 0

sum(is.na(du_lieu$NOT_NA_3_n))
du_lieu$NOT_NA_3_n[is.na(du_lieu$NOT_NA_3_n)] <- 0



# ============================================================
# BƯỚC 12: XỬ LÝ THUỐC TIÊU SỢI HUYẾT (FIBRINOLYTIC THERAPY)
# ============================================================
# Đây là nhóm thuốc "phá" cục máu đông, phải dùng sớm (trong 6-12 giờ đầu).
# fibr_ter_01 đến fibr_ter_07: Các loại thuốc tiêu sợi huyết khác nhau

sum(is.na(du_lieu$fibr_ter_01))
du_lieu$fibr_ter_01[is.na(du_lieu$fibr_ter_01)] <- 0

sum(is.na(du_lieu$fibr_ter_02))
du_lieu$fibr_ter_02[is.na(du_lieu$fibr_ter_02)] <- 0

sum(is.na(du_lieu$fibr_ter_03))
du_lieu$fibr_ter_03[is.na(du_lieu$fibr_ter_03)] <- 0

sum(is.na(du_lieu$fibr_ter_05))
du_lieu$fibr_ter_05[is.na(du_lieu$fibr_ter_05)] <- 0

sum(is.na(du_lieu$fibr_ter_07))
du_lieu$fibr_ter_07[is.na(du_lieu$fibr_ter_07)] <- 0


# du_lieu <- lapply (du_lieu(c()), funciton (x) {x[is.na(x)] <- 0 return (x) } )

# ============================================================
# BƯỚC 13: XỬ LÝ HUYẾT ÁP ĐO TẠI ICU (CONTINUOUS VARIABLES)
# ============================================================
# S_AD_ORIT: Huyết áp tâm thu tại phòng hồi sức ICU (mmHg)
# D_AD_ORIT: Huyết áp tâm trương tại ICU (mmHg)
# Huyết áp < 90 mmHg là dấu hiệu sốc tim - rất nguy hiểm.

# Xem phân phối trước khi điền
median(du_lieu$S_AD_ORIT, na.rm = TRUE)
median(du_lieu$D_AD_ORIT, na.rm = TRUE)

# Số NA trong từng cột
sum(is.na(du_lieu$S_AD_ORIT))
sum(is.na(du_lieu$D_AD_ORIT))

# Điền bằng Median (phù hợp hơn Mean vì có thể có giá trị cực đoan)
du_lieu$S_AD_ORIT[is.na(du_lieu$S_AD_ORIT)] <- median(du_lieu$S_AD_ORIT, na.rm = TRUE)
du_lieu$D_AD_ORIT[is.na(du_lieu$D_AD_ORIT)] <- median(du_lieu$D_AD_ORIT, na.rm = TRUE)



# ============================================================
# BƯỚC 14: XỬ LÝ CHỈ SỐ ĐIỆN GIẢI VÀ MEN GAN
# ============================================================

# --- K_BLOOD: Kali máu (mmol/L) ---
# Kali bình thường: 3.5 - 5.0 mmol/L
# Kali thấp hoặc cao đều gây loạn nhịp tim nguy hiểm

median(du_lieu$K_BLOOD, na.rm = TRUE)
sum(is.na(du_lieu$K_BLOOD))

du_lieu$K_BLOOD[is.na(du_lieu$K_BLOOD)] <- median(du_lieu$K_BLOOD, na.rm = TRUE)
sum(is.na(du_lieu$K_BLOOD))

# --- Na_BLOOD: Natri máu (mmol/L) ---
# Natri bình thường: 135 - 145 mmol/L
# Rối loạn Natri gây phù não và suy tim

median(du_lieu$Na_BLOOD, na.rm = TRUE)
sum(is.na(du_lieu$Na_BLOOD))

du_lieu$Na_BLOOD[is.na(du_lieu$Na_BLOOD)] <- median(du_lieu$Na_BLOOD, na.rm = TRUE)
sum(is.na(du_lieu$Na_BLOOD))

# --- ALT_BLOOD: Men gan Alanine Aminotransferase ---
# Tăng cao: Dấu hiệu suy gan, thường gặp khi có suy đa tạng sau nhồi máu

median(du_lieu$ALT_BLOOD, na.rm = TRUE)
sum(is.na(du_lieu$ALT_BLOOD))

du_lieu$ALT_BLOOD[is.na(du_lieu$ALT_BLOOD)] <- median(du_lieu$ALT_BLOOD, na.rm = TRUE)
 sum(is.na(du_lieu$ALT_BLOOD))

# --- AST_BLOOD: Men gan Aspartate Aminotransferase ---
# AST tăng cả trong nhồi máu cơ tim lẫn tổn thương gan
 
median(du_lieu$AST_BLOOD, na.rm = TRUE)
 sum(is.na(du_lieu$AST_BLOOD))
 
du_lieu$AST_BLOOD[is.na(du_lieu$AST_BLOOD)] <- median(du_lieu$AST_BLOOD, na.rm = TRUE)
sum(is.na(du_lieu$AST_BLOOD))



colSums(is.na(du_lieu))



# Kiểm tra chéo với biến tử vong LET_IS
cat("=== KIỂM TRA CHÉO VỚI BIẾN TỬ VONG ===\n")
cat("Số bệnh nhân TỬ VONG (LET_IS = 1):", sum(du_lieu$LET_IS == 1, na.rm = TRUE), "\n")
cat("Số bệnh nhân CÒN SỐNG (LET_IS = 0):", sum(du_lieu$LET_IS == 0, na.rm = TRUE), "\n\n")

# O_L_POST: Phù phổi cấp (Acute Pulmonary Edema) - NA = 12
cat("NA trong O_L_POST:", sum(is.na(du_lieu$O_L_POST)), "\n")
du_lieu$O_L_POST[is.na(du_lieu$O_L_POST)] <- 0
cat("NA sau xử lý O_L_POST:", sum(is.na(du_lieu$O_L_POST)), "\n\n")

# K_SH_POST: Sốc tim (Cardiogenic Shock) - NA = 15
# Đây là biến chứng nguy hiểm nhất, tỷ lệ tử vong > 50%
cat("NA trong K_SH_POST:", sum(is.na(du_lieu$K_SH_POST)), "\n")
du_lieu$K_SH_POST[is.na(du_lieu$K_SH_POST)] <- 0
cat("NA sau xử lý K_SH_POST:", sum(is.na(du_lieu$K_SH_POST)), "\n\n")

# MP_TP_POST: Rung nhĩ (Atrial Fibrillation) - NA = 14
cat("NA trong MP_TP_POST:", sum(is.na(du_lieu$MP_TP_POST)), "\n")
du_lieu$MP_TP_POST[is.na(du_lieu$MP_TP_POST)] <- 0
cat("NA sau xử lý MP_TP_POST:", sum(is.na(du_lieu$MP_TP_POST)), "\n\n")

# SVT_POST: Nhịp nhanh trên thất (Supraventricular Tachycardia) - NA = 12
cat("NA trong SVT_POST:", sum(is.na(du_lieu$SVT_POST)), "\n")
du_lieu$SVT_POST[is.na(du_lieu$SVT_POST)] <- 0
cat("NA sau xử lý SVT_POST:", sum(is.na(du_lieu$SVT_POST)), "\n\n")

# GT_POST: Loạn nhịp thất (Ventricular Arrhythmia) - NA = 12
cat("NA trong GT_POST:", sum(is.na(du_lieu$GT_POST)), "\n")
du_lieu$GT_POST[is.na(du_lieu$GT_POST)] <- 0
cat("NA sau xử lý GT_POST:", sum(is.na(du_lieu$GT_POST)), "\n\n")

# FIB_G_POST: Rung thất (Ventricular Fibrillation) - NA = 12
# Rung thất gây ngừng tim ngay lập tức, cần sốc điện cấp cứu
cat("NA trong FIB_G_POST:", sum(is.na(du_lieu$FIB_G_POST)), "\n")
du_lieu$FIB_G_POST[is.na(du_lieu$FIB_G_POST)] <- 0
cat("NA sau xử lý FIB_G_POST:", sum(is.na(du_lieu$FIB_G_POST)), "\n\n")

colSums(is.na(du_lieu))


# IM_PG_P: Nhồi máu thất phải (Right Ventricle) - NA = 1
# Logic chéo: Nếu có nhồi máu thành dưới (inf_im > 0)
# → khả năng cao cũng bị nhồi máu thất phải (cùng mạch RCA nuôi)
cat("NA trong IM_PG_P:", sum(is.na(du_lieu$IM_PG_P)), "\n")
du_lieu$IM_PG_P[is.na(du_lieu$IM_PG_P) & du_lieu$inf_im > 0] <- 1
du_lieu$IM_PG_P[is.na(du_lieu$IM_PG_P)] <- 0
cat("NA sau xử lý IM_PG_P:", sum(is.na(du_lieu$IM_PG_P)), "\n\n")



# ============================================================
# NHÓM 4: RỐI LOẠN NHỊP TRÊN ECG - THIẾU THEO KHỐI (NA: 115 - 152)
# Các cột: ritm_ecg_p_xx, n_r_ecg_p_xx, n_p_ecg_p_xx
# ============================================================
# Lý do thiếu theo khối (Block missing):
# 152 bệnh nhân không kịp đo ECG 12 chuyển đạo vì quá cấp bách.
# Toàn bộ nhóm biến ECG bị thiếu cùng một lúc.
# Hướng xử lý:
# - Loạn nhịp bất thường → gán 0 (không có bằng chứng)
# - ritm_ecg_p_01 (Nhịp xoang): Nếu tất cả nhịp khác = 0 → gán 1
#   (Logic: Tim phải có nhịp nào đó, không có bất thường = nhịp xoang bình thường)

# --- Phần A: Nhịp cơ bản (ritm_ecg_p) ---

# Bước 1: Gán tất cả nhịp BẤT THƯỜNG = 0 trước
# ritm_ecg_p_02: Nhịp nhĩ (Atrial Rhythm) - NA = 152
cat("NA trong ritm_ecg_p_02:", sum(is.na(du_lieu$ritm_ecg_p_02)), "\n")
du_lieu$ritm_ecg_p_02[is.na(du_lieu$ritm_ecg_p_02)] <- 0
cat("NA sau xử lý ritm_ecg_p_02:", sum(is.na(du_lieu$ritm_ecg_p_02)), "\n\n")

# ritm_ecg_p_04: Nhịp bộ nối (AV Junctional Rhythm) - NA = 152
cat("NA trong ritm_ecg_p_04:", sum(is.na(du_lieu$ritm_ecg_p_04)), "\n")
du_lieu$ritm_ecg_p_04[is.na(du_lieu$ritm_ecg_p_04)] <- 0
cat("NA sau xử lý ritm_ecg_p_04:", sum(is.na(du_lieu$ritm_ecg_p_04)), "\n\n")

# ritm_ecg_p_06: Rung nhĩ (Atrial Fibrillation) - NA = 152
cat("NA trong ritm_ecg_p_06:", sum(is.na(du_lieu$ritm_ecg_p_06)), "\n")
du_lieu$ritm_ecg_p_06[is.na(du_lieu$ritm_ecg_p_06)] <- 0
cat("NA sau xử lý ritm_ecg_p_06:", sum(is.na(du_lieu$ritm_ecg_p_06)), "\n\n")

# ritm_ecg_p_07: Cuồng nhĩ (Atrial Flutter) - NA = 152
cat("NA trong ritm_ecg_p_07:", sum(is.na(du_lieu$ritm_ecg_p_07)), "\n")
du_lieu$ritm_ecg_p_07[is.na(du_lieu$ritm_ecg_p_07)] <- 0
cat("NA sau xử lý ritm_ecg_p_07:", sum(is.na(du_lieu$ritm_ecg_p_07)), "\n\n")

# ritm_ecg_p_08: Nhịp thất tự phát (Ventricular Rhythm) - NA = 152
cat("NA trong ritm_ecg_p_08:", sum(is.na(du_lieu$ritm_ecg_p_08)), "\n")
du_lieu$ritm_ecg_p_08[is.na(du_lieu$ritm_ecg_p_08)] <- 0
cat("NA sau xử lý ritm_ecg_p_08:", sum(is.na(du_lieu$ritm_ecg_p_08)), "\n\n")

# Bước 2: Xử lý ritm_ecg_p_01 (Nhịp xoang bình thường) - NA = 152
# Logic: Nếu tất cả các nhịp bất thường đều = 0
# → Tim đang đập theo nhịp xoang bình thường → Gán = 1
cat("NA trong ritm_ecg_p_01:", sum(is.na(du_lieu$ritm_ecg_p_01)), "\n")

du_lieu$ritm_ecg_p_01[
  is.na(du_lieu$ritm_ecg_p_01) &
    du_lieu$ritm_ecg_p_02 == 0 &
    du_lieu$ritm_ecg_p_04 == 0 &
    du_lieu$ritm_ecg_p_06 == 0 &
    du_lieu$ritm_ecg_p_07 == 0 &
    du_lieu$ritm_ecg_p_08 == 0
] <- 1

# Các ca còn lại vẫn không xác định được → Gán 0
du_lieu$ritm_ecg_p_01[is.na(du_lieu$ritm_ecg_p_01)] <- 0

cat("NA sau xử lý ritm_ecg_p_01:", sum(is.na(du_lieu$ritm_ecg_p_01)), "\n\n")


# --- Phần B: Loạn nhịp cụ thể (n_r_ecg_p) - Tất cả NA = 115 ---
# Gán 0 = Không ghi nhận loạn nhịp này

cat("=== Xử lý nhóm n_r_ecg_p (Loạn nhịp cụ thể) - NA = 115 ===\n")

# n_r_ecg_p_01: Ngoại tâm thu nhĩ (Atrial Premature Beats)
du_lieu$n_r_ecg_p_01[is.na(du_lieu$n_r_ecg_p_01)] <- 0

# n_r_ecg_p_02: Nhịp nhanh nhĩ kịch phát (Paroxysmal Atrial Tachycardia)
du_lieu$n_r_ecg_p_02[is.na(du_lieu$n_r_ecg_p_02)] <- 0

# n_r_ecg_p_03: Cuồng nhĩ (Atrial Flutter)
du_lieu$n_r_ecg_p_03[is.na(du_lieu$n_r_ecg_p_03)] <- 0

# n_r_ecg_p_04: Rung nhĩ (Atrial Fibrillation)
du_lieu$n_r_ecg_p_04[is.na(du_lieu$n_r_ecg_p_04)] <- 0

# n_r_ecg_p_05: Ngoại tâm thu thất (Ventricular Premature Beats)
du_lieu$n_r_ecg_p_05[is.na(du_lieu$n_r_ecg_p_05)] <- 0

# n_r_ecg_p_06: Nhịp nhanh thất kịch phát (Paroxysmal Ventricular Tachycardia)
du_lieu$n_r_ecg_p_06[is.na(du_lieu$n_r_ecg_p_06)] <- 0

# n_r_ecg_p_08: Rung thất (Ventricular Fibrillation) - NGUY HIỂM NHẤT
du_lieu$n_r_ecg_p_08[is.na(du_lieu$n_r_ecg_p_08)] <- 0

# n_r_ecg_p_09: Nhịp bộ nối (AV Junctional Rhythm)
du_lieu$n_r_ecg_p_09[is.na(du_lieu$n_r_ecg_p_09)] <- 0

# n_r_ecg_p_10: Nhịp thất thoát (Ventricular Escape Rhythm)
du_lieu$n_r_ecg_p_10[is.na(du_lieu$n_r_ecg_p_10)] <- 0

cat("Kiểm tra NA còn lại (phải = 0):", sum(is.na(du_lieu$n_r_ecg_p_01)), "\n\n")


# --- Phần C: Block dẫn truyền (n_p_ecg_p) - Tất cả NA = 115 ---
# Gán 0 = Không ghi nhận block dẫn truyền này

cat("=== Xử lý nhóm n_p_ecg_p (Block dẫn truyền) - NA = 115 ===\n")

# n_p_ecg_p_01: Block AV độ 1 (nhẹ nhất, thường không cần can thiệp)
du_lieu$n_p_ecg_p_01[is.na(du_lieu$n_p_ecg_p_01)] <- 0

# n_p_ecg_p_03: Block AV độ 2 Mobitz I (Wenckebach)
du_lieu$n_p_ecg_p_03[is.na(du_lieu$n_p_ecg_p_03)] <- 0

# n_p_ecg_p_04: Block AV hoàn toàn độ 3 (nặng nhất, cần máy tạo nhịp)
du_lieu$n_p_ecg_p_04[is.na(du_lieu$n_p_ecg_p_04)] <- 0

# n_p_ecg_p_05: Block nhánh trái phân nhánh trước (Left Anterior Fascicular Block)
du_lieu$n_p_ecg_p_05[is.na(du_lieu$n_p_ecg_p_05)] <- 0

# n_p_ecg_p_06: Block nhánh trái phân nhánh sau (Left Posterior Fascicular Block)
du_lieu$n_p_ecg_p_06[is.na(du_lieu$n_p_ecg_p_06)] <- 0

# n_p_ecg_p_07: Block nhánh trái không hoàn toàn (Incomplete LBBB)
du_lieu$n_p_ecg_p_07[is.na(du_lieu$n_p_ecg_p_07)] <- 0

# n_p_ecg_p_08: Block nhánh trái hoàn toàn (Complete LBBB)
# Đây chính là nguyên nhân gây NA cho nhóm vị trí nhồi máu ở trên
du_lieu$n_p_ecg_p_08[is.na(du_lieu$n_p_ecg_p_08)] <- 0

# n_p_ecg_p_09: Block nhánh phải không hoàn toàn (Incomplete RBBB)
du_lieu$n_p_ecg_p_09[is.na(du_lieu$n_p_ecg_p_09)] <- 0

# n_p_ecg_p_10: Block nhánh phải hoàn toàn (Complete RBBB)
du_lieu$n_p_ecg_p_10[is.na(du_lieu$n_p_ecg_p_10)] <- 0

# n_p_ecg_p_11: Block hai nhánh (Bifascicular Block)
du_lieu$n_p_ecg_p_11[is.na(du_lieu$n_p_ecg_p_11)] <- 0

# n_p_ecg_p_12: Block ba nhánh (Trifascicular Block)
du_lieu$n_p_ecg_p_12[is.na(du_lieu$n_p_ecg_p_12)] <- 0

cat("Kiểm tra NA còn lại (phải = 0):", sum(is.na(du_lieu$n_p_ecg_p_01)), "\n\n")



# ============================================================
# NHÓM 5: THUỐC TIÊU SỢI HUYẾT BỔ SUNG (NA: 10)
# Các cột: fibr_ter_06, fibr_ter_08
# ============================================================
# Hướng xử lý: Gán 0
# Lý do: Nếu không có ghi nhận sử dụng → Không dùng thuốc này

# fibr_ter_06 - NA = 10
cat("NA trong fibr_ter_06:", sum(is.na(du_lieu$fibr_ter_06)), "\n")
du_lieu$fibr_ter_06[is.na(du_lieu$fibr_ter_06)] <- 0
cat("NA sau xử lý fibr_ter_06:", sum(is.na(du_lieu$fibr_ter_06)), "\n\n")

# fibr_ter_08 - NA = 10
cat("NA trong fibr_ter_08:", sum(is.na(du_lieu$fibr_ter_08)), "\n")
du_lieu$fibr_ter_08[is.na(du_lieu$fibr_ter_08)] <- 0
cat("NA sau xử lý fibr_ter_08:", sum(is.na(du_lieu$fibr_ter_08)), "\n\n")



# ============================================================
# NHÓM 6: CHỈ SỐ XÉT NGHIỆM MÁU (NA: 125 - 375)
# Các cột: GIPO_K (369), GIPER_NA (375), NA_BLOOD (375),
#           ROE (203), L_BLOOD (125)
# ============================================================
# Hướng xử lý: Dùng MICE (KHÔNG gán 0)
# Lý do: Đây là chỉ số liên tục (điện giải, tốc độ máu lắng, bạch cầu).
# Gán 0 sẽ sai hoàn toàn về mặt sinh học.
# Ví dụ: Không ai có Natri máu = 0 mmol/L và còn sống.
# MICE dự đoán giá trị dựa trên tuổi, giới tính và kết cục tử vong.

# Cài và nạp thư viện mice (chỉ cần cài 1 lần)
install.packages("mice")
library(mice)

# Xem NA trước khi chạy MICE
cat("=== NA TRƯỚC KHI MICE ===\n")
cat("GIPO_K :", sum(is.na(du_lieu$GIPO_K)), "\n")
cat("GIPER_NA:", sum(is.na(du_lieu$GIPER_NA)), "\n")
cat("NA_BLOOD:", sum(is.na(du_lieu$NA_BLOOD)), "\n")
cat("ROE     :", sum(is.na(du_lieu$ROE)), "\n")
cat("L_BLOOD :", sum(is.na(du_lieu$L_BLOOD)), "\n\n")

# Tạo bảng con chỉ gồm các cột cần điền + biến hỗ trợ dự đoán
cac_cot_mice   <- c("GIPO_K", "GIPER_NA", "NA_BLOOD", "ROE", "L_BLOOD")
cac_bien_phu   <- c("AGE", "SEX", "LET_IS")

bang_mice <- du_lieu[, c(cac_cot_mice, cac_bien_phu)]

# Chạy MICE
# method = "pmm": Predictive Mean Matching (phù hợp với biến liên tục)
# m = 5: Tạo 5 bộ dữ liệu hoàn chỉnh
# maxit = 5: Số vòng lặp hội tụ
# seed = 42: Đặt seed để kết quả tái lặp được
set.seed(42)
ket_qua_mice <- mice(bang_mice, m = 5, maxit = 5, method = "pmm", printFlag = FALSE)

# Lấy bộ dữ liệu hoàn chỉnh thứ nhất
bang_da_dien <- complete(ket_qua_mice, 1)

# Ghi lại giá trị đã điền vào bảng chính du_lieu
du_lieu$GIPO_K   <- bang_da_dien$GIPO_K
du_lieu$GIPER_NA <- bang_da_dien$GIPER_NA
du_lieu$NA_BLOOD <- bang_da_dien$NA_BLOOD
du_lieu$ROE      <- bang_da_dien$ROE
du_lieu$L_BLOOD  <- bang_da_dien$L_BLOOD

# Kiểm tra sau MICE
cat("=== NA SAU KHI MICE ===\n")
cat("GIPO_K :", sum(is.na(du_lieu$GIPO_K)), "\n")
cat("GIPER_NA:", sum(is.na(du_lieu$GIPER_NA)), "\n")
cat("NA_BLOOD:", sum(is.na(du_lieu$NA_BLOOD)), "\n")
cat("ROE     :", sum(is.na(du_lieu$ROE)), "\n")
cat("L_BLOOD :", sum(is.na(du_lieu$L_BLOOD)), "\n\n")



# ============================================================
# NHÓM 7: THỜI GIAN VÀ THUỐC CẤP CỨU TIỀN BỆNH VIỆN (NA: 126 - 686)
# Các cột: TIME_B_S (126), NA_KB (657), NOT_NA_KB (686), LID_KB (677)
# ============================================================

# --- TIME_B_S: Thời gian từ lúc đau đến khi nhập viện (phút) - NA = 126 ---
# Hướng xử lý: Gán Median
# Lý do: Bệnh nhân không nhớ hoặc không ai biết giờ đau chính xác.
# Dùng Median vì phân phối thời gian thường lệch phải (skewed right).

cat("TIME_B_S - Median:", median(du_lieu$TIME_B_S, na.rm = TRUE), "phút\n")
cat("NA trong TIME_B_S:", sum(is.na(du_lieu$TIME_B_S)), "\n")

du_lieu$TIME_B_S[is.na(du_lieu$TIME_B_S)] <- median(du_lieu$TIME_B_S, na.rm = TRUE)

cat("NA sau xử lý TIME_B_S:", sum(is.na(du_lieu$TIME_B_S)), "\n\n")


# --- NA_KB, NOT_NA_KB, LID_KB: Thuốc dùng bởi đội cấp cứu lưu động ---
# NA = 657, 686, 677 (~55-58% tổng số dòng)
# Hướng xử lý: Chuyển thành biến 3 lớp (KHÔNG xóa bỏ)
# Lý do: ~60% bệnh nhân TỰ ĐẾN bệnh viện, không qua đội cấp cứu ngoài.
# → Không thể biết họ có dùng thuốc hay không → Tạo lớp thứ 3 "Không rõ".
#   0 = Không dùng thuốc này
#   1 = Có dùng thuốc này
#   2 = Không rõ / Tự đến viện (không qua đội cấp cứu lưu động)

cat("=== CHUYỂN ĐỔI THUỐC CẤP CỨU THÀNH BIẾN 3 LỚP ===\n\n")

# NA_KB: Morphine/Narcotic dùng bởi đội cấp cứu - NA = 657
cat("NA trong NA_KB:", sum(is.na(du_lieu$NA_KB)), "\n")
du_lieu$NA_KB[is.na(du_lieu$NA_KB)] <- 2
cat("Phân phối NA_KB sau xử lý:\n")
print(table(du_lieu$NA_KB))
cat("\n")

# NOT_NA_KB: NSAID dùng bởi đội cấp cứu - NA = 686
cat("NA trong NOT_NA_KB:", sum(is.na(du_lieu$NOT_NA_KB)), "\n")
du_lieu$NOT_NA_KB[is.na(du_lieu$NOT_NA_KB)] <- 2
cat("Phân phối NOT_NA_KB sau xử lý:\n")
print(table(du_lieu$NOT_NA_KB))
cat("\n")

# LID_KB: Lidocaine dùng bởi đội cấp cứu - NA = 677
cat("NA trong LID_KB:", sum(is.na(du_lieu$LID_KB)), "\n")
du_lieu$LID_KB[is.na(du_lieu$LID_KB)] <- 2
cat("Phân phối LID_KB sau xử lý:\n")
print(table(du_lieu$LID_KB))
cat("\n")



# ============================================================
# NHÓM 8: ĐIỀU TRỊ TẠI ICU VÀ CƠN ĐAU TÁI PHÁT (NA: 9 - 130)
# Các cột: R_AB_1_n, R_AB_2_n, R_AB_3_n, NITR_S,
#           LID_S_n, B_BLOK_S_n, ANT_CA_S_n, GEPAR_S_n,
#           ASP_S_n, TIKL_S_n, TRENT_S_n
# ============================================================
# Hướng xử lý: Gán 0
# Lý do: Tỷ lệ thiếu TĂNG DẦN theo ngày (ngày 1 ít NA, ngày 2-3 nhiều hơn).
# Đây là bằng chứng bệnh nhân đã TỬ VONG hoặc ỔN ĐỊNH RA VIỆN trước ngày đó.
# Nếu không còn ở ICU → Không có thêm cơn đau → Không có thêm thuốc → Gán 0.

# R_AB_1_n: Số cơn đau tái phát ngày 1 - NA = 16
cat("NA trong R_AB_1_n:", sum(is.na(du_lieu$R_AB_1_n)), "\n")
du_lieu$R_AB_1_n[is.na(du_lieu$R_AB_1_n)] <- 0
cat("NA sau xử lý R_AB_1_n:", sum(is.na(du_lieu$R_AB_1_n)), "\n\n")

# R_AB_2_n: Số cơn đau tái phát ngày 2 - NA = 108
cat("NA trong R_AB_2_n:", sum(is.na(du_lieu$R_AB_2_n)), "\n")
du_lieu$R_AB_2_n[is.na(du_lieu$R_AB_2_n)] <- 0
cat("NA sau xử lý R_AB_2_n:", sum(is.na(du_lieu$R_AB_2_n)), "\n\n")

# R_AB_3_n: Số cơn đau tái phát ngày 3 - NA = 128
cat("NA trong R_AB_3_n:", sum(is.na(du_lieu$R_AB_3_n)), "\n")
du_lieu$R_AB_3_n[is.na(du_lieu$R_AB_3_n)] <- 0
cat("NA sau xử lý R_AB_3_n:", sum(is.na(du_lieu$R_AB_3_n)), "\n\n")

# NITR_S: Nitroglycerin tại ICU - NA = 9
# Nitroglycerin giãn mạch vành, giảm đau ngực cấp
cat("NA trong NITR_S:", sum(is.na(du_lieu$NITR_S)), "\n")
du_lieu$NITR_S[is.na(du_lieu$NITR_S)] <- 0
cat("NA sau xử lý NITR_S:", sum(is.na(du_lieu$NITR_S)), "\n\n")

# LID_S_n: Lidocaine tại ICU - NA = 10
# Lidocaine chống loạn nhịp thất
cat("NA trong LID_S_n:", sum(is.na(du_lieu$LID_S_n)), "\n")
du_lieu$LID_S_n[is.na(du_lieu$LID_S_n)] <- 0
cat("NA sau xử lý LID_S_n:", sum(is.na(du_lieu$LID_S_n)), "\n\n")

# B_BLOK_S_n: Beta-blocker tại ICU - NA = 11
# Beta-blocker giảm nhịp tim, bảo vệ cơ tim khỏi thiếu oxy
cat("NA trong B_BLOK_S_n:", sum(is.na(du_lieu$B_BLOK_S_n)), "\n")
du_lieu$B_BLOK_S_n[is.na(du_lieu$B_BLOK_S_n)] <- 0
cat("NA sau xử lý B_BLOK_S_n:", sum(is.na(du_lieu$B_BLOK_S_n)), "\n\n")

# ANT_CA_S_n: Thuốc chẹn kênh canxi tại ICU - NA = 13
cat("NA trong ANT_CA_S_n:", sum(is.na(du_lieu$ANT_CA_S_n)), "\n")
du_lieu$ANT_CA_S_n[is.na(du_lieu$ANT_CA_S_n)] <- 0
cat("NA sau xử lý ANT_CA_S_n:", sum(is.na(du_lieu$ANT_CA_S_n)), "\n\n")

# GEPAR_S_n: Heparin tại ICU - NA = 17
# Heparin chống đông máu, ngăn huyết khối lan rộng thêm
cat("NA trong GEPAR_S_n:", sum(is.na(du_lieu$GEPAR_S_n)), "\n")
du_lieu$GEPAR_S_n[is.na(du_lieu$GEPAR_S_n)] <- 0
cat("NA sau xử lý GEPAR_S_n:", sum(is.na(du_lieu$GEPAR_S_n)), "\n\n")

# ASP_S_n: Aspirin tại ICU - NA = 17
# Aspirin chống kết tập tiểu cầu, giảm huyết khối
cat("NA trong ASP_S_n:", sum(is.na(du_lieu$ASP_S_n)), "\n")
du_lieu$ASP_S_n[is.na(du_lieu$ASP_S_n)] <- 0
cat("NA sau xử lý ASP_S_n:", sum(is.na(du_lieu$ASP_S_n)), "\n\n")

# TIKL_S_n: Ticlopidine tại ICU - NA = 16
cat("NA trong TIKL_S_n:", sum(is.na(du_lieu$TIKL_S_n)), "\n")
du_lieu$TIKL_S_n[is.na(du_lieu$TIKL_S_n)] <- 0
cat("NA sau xử lý TIKL_S_n:", sum(is.na(du_lieu$TIKL_S_n)), "\n\n")

# TRENT_S_n: Trental (Pentoxifylline) tại ICU - NA = 16
cat("NA trong TRENT_S_n:", sum(is.na(du_lieu$TRENT_S_n)), "\n")
du_lieu$TRENT_S_n[is.na(du_lieu$TRENT_S_n)] <- 0
cat("NA sau xử lý TRENT_S_n:", sum(is.na(du_lieu$TRENT_S_n)), "\n\n")





colSums(is.na(du_lieu))
