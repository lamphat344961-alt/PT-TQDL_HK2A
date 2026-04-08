
setwd("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/LAB/lab_02")

#Bước 1 Load và khám phá dữ liệu
#Load() dữ liệu  .csv
alcohol <- read.csv("student-alcohol.csv")

alcoho <- read.csv("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/LAB/lab_02")

#Xem head()
head(alcohol)

#Kiểm tra cấu trúc
str(alcohol)

#Loại bỏ cột đầu tiên (nếu là ID )
head(alcohol[,-1]) #Xem trước khi xóa 
alcohol <- alcohol[,-1]


# Lưu ý 
#[,-1] lấy tất cả các cột và loại bỏ cột 1
# luôn kiểm tra trc khi xóa để tránh mất dữ liệu quan trọng

#Bước 2 Xử lý Missing data
# tìm các dòng có dữ liệu thiếu 

alcohol[!complete.cases(),]

# Đếm số dòng bị thiếu
length(alcohol[!complete.cases(alcohol),]) # Nó sẽ trả về số lượng cột
length(alcohol)

# ===> dùng sum()

sum(is.na(alcohol))

sum(!complete.cases(alcohol))

colSums(is.na(alcohol)) # tổng NA trên từng cột



# Xử lý biến số numeric-age
summary(alcohol$age)

# Tính trung binh bỏ qua NA
median(alcohol$age, na.rm = TRUE)

#Điền missing value bằng median 
alcohol$age[is.na(alcohol$age)] <- median(alcohol$age, na.rm = TRUE)

#Kiểm tra có còn NA không
alcohol$age[is.na(alcohol$age)]

#Xử lý biến phân loại - Mjob
alcohol[!complete.cases(alcohol), ]

#Đếm số dòng bị thiếu
length(alcohol[!complete.cases(alcohol),])

#Điền gia trị "orther" cho dòng 63 (phần bị thiếu)
alcohol$Mjob[63] <- "orther"

# Kiểm tra lại
alcohol[!complete.cases(alcohol), ]

# Bước 3 chuyển đổi Categori thành Factor

str(alcohol)

# Biến nhị phân đơn giản (B variables)
#School : GP / MS
summary(factor(alcohol$school)) # cho school thành factor thử 
                               #  để kiểm tra có nên chuyển nó không

alcohol$school <- factor(alcohol$school, 
                         levels = c("GP","MS"),
                         labels =  c("Gabriel Pereira", "Mousino da Silveira")
                         )

str(alcohol)

summary(factor(alcohol$sex))

alcohol$sex <- factor(alcohol$sex, 
                      levels = c("F", "M"), 
                      labels = c("female", "male")
                      )

str(alcohol)
summary(factor(alcohol$address))

alcohol$address <- factor (alcohol$address , 
                           levels = c("R", "U"),  
                           labels = c("rural", "urban")
                           )


summary(factor(alcohol$famsize))

alcohol$famsize <- factor(alcohol$famsize, 
                          levels = c("GT3", "LE3"), 
                          labels = c("more than 3", "less or equal to 3"))


summary(factor(alcohol$Pstatus))
alcohol$Pstatus <- factor(alcohol$Pstatus, 
                          levels = c("A", "T"), 
                          labels = c("living apart", "living together")
                          )

# Ordinal Factors (Biến có thứ tự)
# Mother's education - Trình độ học vấn của mẹ
summary(factor(alcohol$Medu))
alcohol$Medu<-factor(alcohol$Medu,
                     levels = c(0,1,2,3,4),
                     labels = c("none", "primary", "primary higher", "secondary", "higher"),
                     ordered = TRUE
)

# Father's education - Trình độ học vấn của cha
summary(factor(alcohol$Fedu))

alcohol$Fedu <- factor(alcohol$Fedu, 
                       levels = c(0, 1, 2, 3, 4), 
                       labels = c("none", "primary", "primary higher", 
                                  "secondary", "higher"), 
                       ordered = TRUE)

# Reason to choose this school - Lý do chọn trường
summary(factor(alcohol$reason))
alcohol$reason<-factor(alcohol$reason)

# Kiểm tra cấu trúc sau khi chuyển đổi
str(alcohol)
summary(alcohol)

summary(factor(alcohol$guardian))
alcohol$guardian <- factor(alcohol$guardian)

summary(factor(alcohol$traveltime))

alcohol$traveltime <- factor (alcohol$traveltime , 
                              levels = c(1,2,3,4) ,
                              labels = c("0-15 min", "15-30 min",
                                         "30-60 min", "above 60 min"),
                              ordered = TRUE
                              )

# Study time - Thời gian học mỗi tuần
summary(alcohol$studytime)
summary(factor(alcohol$studytime))

# Ý nghĩa: 1 - <2 giờ, 2 - 2-5 giờ, 3 - 5-10 giờ, 4 - >10 giờ
alcohol$studytime <- factor(alcohol$studytime, 
                            levels = c(1, 2, 3, 4), 
                            labels = c("0-2 hours", "2-5 hours",
                                       "5-10 hours", "above 10 hours"),
                            ordered = TRUE)

# Các biến nhị phân
str(alcohol)


# Nhận xét: Có 8 biến cùng dạng yes/no → lặp code 8 lần rất tốn công!

#BƯỚC 4: Tự động hóa với lapply()
# Liệt kê tất cả các biến binary (yes/no)
binaryVariables <- c("schoolsup", "famsup", "paid", "activities", 
                     "nursery", "higher", "internet", "romantic")

alcohol[,binaryVariables]

# hàm áp dụng list nhiều biến lapply()

lapply(alcohol[,binaryVariables] , summary)
lapply(alcohol[,binaryVariables], factor)

lapply(alcohol[,binaryVariables], function (x) {summary(factor(x))}) # xử dụng trong trường hợp : muốn áp dụng một hàm cho cả list


# Vấn đề: Biến internet có nhiều level (0, 1, NO, YES, no, yes) → lỗi nhập liệu!

# Giải pháp: Chuẩn hóa
alcohol$internet[alcohol$internet==0]<-"no"
alcohol$internet[alcohol$internet=="NO"]<-"no"
alcohol$internet[alcohol$internet==1]<-"yes"
alcohol$internet[alcohol$internet=="YES"]<-"yes"


summary(factor(alcohol$internet))


lapply(alcohol[,binaryVariables], function (x) {summary(factor(x))})

alcohol[,binaryVariables] <- lapply(alcohol[,binaryVariables] , factor ) 

leveledVariables <- c("famrel", "freetime", "goout", "Dalc", "Walc")

lapply(alcohol[,leveledVariables], summary)

# Ý nghĩa:
#   
#   freetime: Thời gian rảnh sau giờ học
# goout: Tần suất đi chơi với bạn bè
# Dalc: Mức độ uống rượu trong ngày thường (Daily Alcohol Consumption)
# Walc: Mức độ uống rượu cuối tuần (Weekend Alcohol Consumption)
# Scale: 1 = rất thấp, 2 = thấp, 3 = trung bình, 4 = cao, 5 = rất cao
# 
# 

alcohol[,leveledVariables] <- lapply(alcohol[,leveledVariables], factor, levels = c(1, 2, 3, 4, 5), 
                                                                        labels = c("very bad", "bad", "average",
                                                                                   "good", "very good"),
                                                                        ordered = TRUE
                                                                        )

# Các tham số của factor được đặt sau dấu phẩy (đi kèm và áp dụng cho function trước đó)
#-> bắt buộc do lapply() quy định, sẽ khác với khi dùng factor cho 1 cột  
                                     

str(alcohol)

# Health status - Tình trạng sức khỏe
summary(alcohol$health)


alcohol$health <- factor(alcohol$health , 
                         levels = c(1, 2, 3, 4, 5), 
                         labels = c("very bad", "bad", "average",
                                    "good", "very good"),
                         ordered = TRUE
                         )

# BƯỚC 6: Kiểm tra kết quả cuối cùng
# Xem cấu trúc dữ liệu sau khi clean
str(alcohol)

# Tóm tắt thống kê
summary(alcohol)

length(alcohol[!complete.cases(alcohol), ] ) # Trả về số cột của DF = với ncol()

nrow(alcohol[!complete.cases(alcohol),]) # trả về số dòng của DF

sum(is.na(alcohol)) # đếm số dòng bị NA trong alcohol


