### Phần 1 ############################
# # Nhiệm vụ
# # 1. Tạo và thêm tên duy nhất vào năm vector có độ dài 8. 
# Làm cho các kiểu dữ liệu của chúng đa dạng.

# 1. Vector số thực (Numeric) - Ví dụ: Chi phí cho girlfriend hoặc tiết kiệm
vec_1 <- c(500, 1200, 450, 300, 800, 1000, 200, 50)

# 2. Vector chuỗi ký tự (Character) - Ví dụ: Các môn học Data Science
vec_2 <- c("R", "Python", "SQL", "NLP", "CVAE", "Dart", "Flutter", "Anki")

# 3. Vector logic (Logical) - Ví dụ: Trạng thái hoàn thành bài tập
vec_3 <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)

# 4. Vector số nguyên (Integer) - Ví dụ: Số giờ tự học mỗi tuần
vec_4 <- c(2L, 4L, 3L, 5L, 2L, 6L, 1L, 0L)

# 5. Vector định danh (Factor) - Ví dụ: Mức độ ưu tiên dự án
vec_5 <- factor(c("Low", "High", "Med", "High", "Low", "Med", "High", "Low"))



# Tạo một dataframe có tên "mySet1" từ các vector đã tạo.
# # a) Hiển thị dòng thứ 5 của dataframe đã tạo.
# # b) Thay đổi tên của cột thứ hai của dataframe 
# mySet1 thành "column02"
# # c) Hiển thị 7 dòng đầu tiên của dataframe mySet1. 
# Sử dụng hai phương pháp khác nhau - với chỉ số và với một hàm.

# Tạo dataframe
mySet1 <- data.frame(vec_1,vec_2,vec_3,vec_4,vec_5)


head(mySet1)
colnames(mySet1)[2] <- "column2"
mySet1[0:7,]
head(mySet1,7)

# 2. Sử dụng bộ dữ liệu iris. 
# Sử dụng chỉ số để hiển thị giá trị của mỗi 
# dòng thứ 3 giữa quan sát thứ 40 và 120.
# Cố gắng sử dụng một dòng lệnh duy nhất 
# (rút gọn mã để nó vừa trong một dòng duy nhất, 
# không có bất kỳ bước trung gian nào)
iris[seq(from = 40 , to = 120 , by = 3), ]


# 3. Sử dụng bộ dữ liệu có sẵn "women".
# a) Thay đổi kiểu của cột đầu tiên thành kiểu ký tự.
# b) Thêm hai dòng mới vào bộ dữ liệu với các số tự tạo. Đảm bảo rằng bạn không làm mất các kiểu của biến trong dataframe chính trong quá trình này.
# c) Thêm biến mới vào bộ dữ liệu và đặt tên là "shoe_size". Sử dụng hàm runif để tạo các giá trị cho biến này. Kích thước giày phải là số nguyên giữa 35 và 42.

women

women$height <- as.character(women$height)

# nối DF thì phải có cùng tên cột, -> tạo và đặt tên cột trong 1 dòng 

new <- data.frame(height = as.character(c(10,100))
                  , weight = c(222,333)
                  )

new
women <- rbind(women, new)  # rbind nối DF 

nrow(women) # nrow sẽ trả về số dòng của DF

runif(nrow(women), min = 35 , max = 42 )


women$shoe_size <- floor(runif(nrow(women), min = 35, max = 42))






### Các bài tập ######################################################################
data()


### Sử dụng bộ dữ liệu có sẵn CO2 cho các nhiệm vụ sau:
# 1. In giá trị CO2 uptake từ lớn nhất đến nhỏ nhất.
View(CO2)
str(CO2)

sort(CO2$uptake, decreasing =  TRUE)

# 2. Hiển thị các dòng của bộ dữ liệu CO2 có Type là Quebec và Treatment là chilled.

CO2[CO2$Type=="Quebec" & CO2$Treatment== "chilled", ]

# 3. Hiển thị các dòng của bộ dữ liệu CO2 có uptake lớn hơn 40 và 
# được sắp xếp theo giá trị conc từ nhỏ nhất đến lớn nhất.
# Điểm thưởng nếu giữ toàn bộ mã trong một dòng duy nhất. Nếu cần tạo
# một đối tượng trung gian, hãy đặt tên là 'temp'.

temp <- CO2[CO2$uptake > 40, ]

temp <- temp[order(temp$conc), ]

temp

res <- CO2[CO2$uptake > 40, ][order(CO2[CO2$uptake > 40, ]$conc), ]
res


# 4. Làm thế nào để sắp xếp ngẫu nhiên bộ dữ liệu CO2? GỢI Ý: Bạn có thể cần tạo
# một vector với các chỉ số ngẫu nhiên từ kết quả của order(runif(...)).
# Tham khảo phần "Picking random rows from data".

v1 <-  order(runif(10,0,10))
v1

CO2[v1,]


CO2[order(runif(nrow(CO2))),] 


# Điểm thưởng nếu viết mã trong một dòng duy nhất không có đối tượng trung gian.
### Chạy mã này trước khi thực hiện các nhiệm vụ tiếp theo
set.seed(123)
missCO2 <- CO2
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"uptake"] <- NA
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"conc"] <- NA
missCO2$weight <- paste0(as.integer(runif(nrow(missCO2))*30),"kg")




# 5. Hiển thị các dòng của bộ dữ liệu missCO2 có ít nhất một giá trị bị thiếu.

missCO2[!complete.cases(missCO2),]
# Giải thích:
# - complete.cases(missCO2) trả về TRUE cho các dòng không có giá trị NA
# - Toán tử ! phủ định kết quả, trả về TRUE cho các dòng có ít nhất một giá trị NA
# - missCO2[chỉ số, ] chọn các dòng thỏa mãn điều kiện

# 6. Điền các giá trị uptake bị thiếu với giá trị 20.

missCO2$uptake <- as.integer(missCO2$uptake)
missCO2[is.na(missCO2$uptake),"uptake"] <- 20



missCO2[is.na(missCO2$uptake), 'uptake'] <- 20 
missCO2$uptake[is.na(missCO2$uptake)] <-20 


# 7. Điền các giá trị conc bị thiếu với giá trị trung bình của conc.

missCO2[is.na(missCO2$conc), "conc"] <- mean(missCO2$conc, na.rm = TRUE)#na.rm = TRUE giúp loại bỏ NA khi tính TB, 
                                                             #nếu không thì mean sẽ trả về NA nếu cột đó có NA
missCO2



# 8. Trích xuất giá trị số từ biến weight và lưu trong cột mới
# "weightNumber". Điểm thưởng nếu giữ mã trong một dòng,
# không sử dụng đối tượng trung gian nào.

women
str(women)

women$weightNumber <- as.numeric(women$weight)

# Lưu ý : Kiểm tra môi trường làm việc getwd() , thiết lập môi trường làm việc setwd() tránh mất file 
setwd("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/LAB/lab_01")

# Lưu DATA FRAME vào một tệp CSV
write.csv(women,file = "women.csv") # định dạng quốc dân, cách nhau bằng dấu ,


# Lưu DATA FRAME vào một tệp RDS (định dạng R)
saveRDS(women , file = "women_RDS.rds") # định dạng nhị phân riêng của R, dùng readRDS("women_RDS.rds") để đọc
  
  
# Lưu nhiều đối tượng vào một tệp RData
save(women , missCO2, file  = "multiple_datasets.RData") # một định dạng kiểu "nhà kho" dùng lưu trữ nhiều đối tượng cùng lúc,
                                                          # dùng load() (lần sau dùng thì nó vẫn có tên cũ  women , missCO2 )

write.csv(women, file ='test.csv')

saveRDS(women, file = ('test2.rds'))
save(women, missCO2, file = 'multitest.RData')

data()
View()
