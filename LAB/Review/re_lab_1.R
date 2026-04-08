setwd("C:\\Users\\Admin\\Desktop\\TANPHAT\\hocotruong\\Năm ba 2025-2026\\HK2_A\\Phantichvatrucquandulieu\\LAB\\Review")

ten_du_an <- c("NLP", "CVAE", "TikTok_Clustering", "Logistics_App", "GDP_Forecast")

ngay_bat_dau <- as.Date(c("2020-10-20","2020-10-10","2020-3-6","2020-8-10","2020-12-30"))

so_ngay<- Sys.Date() - ngay_bat_dau[1]

so_ngay

orderd_factor <- rep (c("Low","Medium","High"), time =2, length.out = 5 )

orderd_factor <- factor(orderd_factor ,
                        levels = c("Low","Medium","High") ,
                        ordered = TRUE 
                        )

qlda <- data.frame(orderd_factor,ngay_bat_dau,ten_du_an)

summary(qlda)

######################################

set.seed(123)
diem <- floor(runif(15, min =0 , max = 10))
diem_so <- matrix(diem, nrow=5, ncol = 3)




rownames(diem_so) <- ten_du_an
colnames(diem_so) <-c ("Technical", "Feasibility", "Impact")

class(diem_so) 
dim(diem_so) # Phải hiện ra [1] 5 3

tong_diem <- rowSums(diem_so)
diem_so <- cbind(diem_so,tong_diem)

str(diem_so)

thong_tin_sv <- list(
  ten_sv = "Nguyễn Văn A",             
  mssv = "21012345",                   
  bang_diem = diem_so                   
)

diem_tk_clustering <- thong_tin_sv$bang_diem["TikTok_Clustering", "Technical"]

###################################################



df_profect  <- data.frame(orderd_factor,ngay_bat_dau,ten_du_an,tong_diem)
colnames(df_profect[1]) <- 'ten_du_an'

pj_uu_tien <- df_profect[df_profect$orderd_factor == 'High' & df_profect$tong_diem >15,]

df_profect <- df_profect[order(-df_profect$tong_diem), ]


df_profect$ngay_bat_dau[2] <- NA

col_na <- colSums(is.na(df_profect))

df_profect[!complete.cases(df_profect)]


df_profect$ngay_bat_dau[is.na(df_profect$ngay_bat_dau)] <- as.Date("2025-2-10")

















