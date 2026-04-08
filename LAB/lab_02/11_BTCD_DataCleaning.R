setwd("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/LAB/lab_02")

### LẤY ĐIỂM THỰC HÀNH
### Các bài tập ######################################################################
# 1. Đọc mô tả dữ liệu phân tích tính cách khách hàng và tải 
# vào R (file clients.csv) với tên biến là "clients".
clients <- read.csv("clients.csv")
View(clients)

# Chỉnh sửa lại lỗi nhập liệu 

# Có thể giữ lại, 
shift_left_from <- function(df, row_id, start_col) {
  end_col <- ncol(df)
  df[row_id, start_col:end_col] <- c(df[row_id, (start_col+1):end_col], 2)
  df
}

col_mar <- which(names(clients) == "Marital_Status")

cycle_rows <- which(clients$Marital_Status == "Cycle")

for(i in cycle_rows){
  clients <- shift_left_from(clients, i, col_mar)
}

clients$Education[clients$Education == "2n"] <- "2n Cycle"


#phức tạp hơn nên bỏ
is_datetime <- function(x){
  grepl("^\\d{2}-\\d{2}-\\d{4}$", x)
}
clients <- clients[ !is_datetime(clients$Teenhome), ]


View(clients)


# 2. Xem qua cấu trúc dữ liệu và kiểm tra các lớp (classes) đã được gán 
# cho các biến trong bộ dữ liệu.
head(clients)

summary(clients)
View(clients)
str(clients)
length(clients) # 30 col

clients$Dt_Customer <- as.Date(clients$Dt_Customer,format = "%d-%m-%Y")

lst_int <- c("Income","Kidhome","Teenhome","Recency")
clients[,lst_int] <- lapply(clients[,lst_int], as.integer) 



# 3. Kiểm tra xem có giá trị nào bị thiếu trong bộ dữ liệu không.
# a) Những biến nào có chứa giá trị bị thiếu?

col_na <- colSums(is.na(clients))
col_na  # số NA trên từng cột
col_na[col_na > 0] # các cột NA > 0

list_na <- col_na[col_na > 0]

clients[ , names(list_na)] # Xem giá trị các cột đang có NA 


lapply(clients[, names(list_na)], summary) # Xemm summary() các cột đang NA 

head(clients[list_na,])

# b) Điền các giá trị bị thiếu bằng giá trị trung bình hoặc trung vị của biến đó.
# Trước khi điền, hãy xem xét bản chất của biến. Nếu là số nguyên (ví dụ: năm sinh),
# thì hãy điền giá trị phù hợp với bản chất của biến (chúng ta không muốn năm sinh là 1995.832, phải không? ;)).
# c) Bạn sử dụng đoạn mã nào để điền các giá trị bị thiếu của Year_Birth (nếu có)?
clients$Year_Birth[is.na(clients$Year_Birth)] <- median(clients[,"Year_Birth"], na.rm = TRUE)
clients$Year_Birth[!complete.cases(clients$Year_Birth)]


data[is.na(data$col),'col'] <- median(data$col, na.rm =TRUE )

data$col[!complete.cases(data$col)]



clients$MntWines[is.na(clients$MntWines)] <- mean(clients[,"MntWines"], na.rm = TRUE)
clients$MntWines[!complete.cases(clients$MntWines)]

clients$Response[is.na(clients$Response)] <- median(clients[,"Response"], na.rm = TRUE)
clients$Response[!complete.cases(clients$Response)]

# 4. a) Kiểm tra xem tất cả các giá trị bị thiếu đã được điền đầy đủ chưa. Nếu chưa, lặp lại bước 3.
# b) Bạn sẽ dùng đoạn mã nào để hiển thị tất cả các dòng vẫn còn chứa dữ liệu bị thiếu?

colSums(is.na(clients))

# 5. a) Xem xét những biến nào nên chuyển đổi thành kiểu "factor"?
# Gợi ý: Đây thường là các biến văn bản có một số giá trị cụ thể và lặp lại.
# Chúng cũng có thể là các biến được biểu diễn bằng số nhưng không mang "ý nghĩa số học"
# - ví dụ: biến "education" với các giá trị 2, 3, 4 thực chất đại diện cho các cấp độ
# giáo dục liên tiếp (ý nghĩa logic) thay vì số năm học tập chính xác (ý nghĩa số học).
str(clients)

lapply(clients[, names(clients)], function (x) {summary(factor(x))})
lapply(clients[, names(clients)], factor)


# b) Bạn sẽ dùng đoạn mã ngắn nhất nào để chuyển đổi biến Marital_Status?
summary (factor (clients[,"Marital_Status"] ) )

clients$Marital_Status <- factor(clients$Marital_Status) 
                                 
str(clients$Marital_Status)
# 6. a) Xem xét biến nào trong số các biến đã xác định ở trên nên được
# chuyển đổi thành kiểu 'ordered factor' (biến phân loại có thứ tự).
# Gợi ý: Biến kiểu 'ordered factor' nên chứa các mức có thứ tự logic
# - ví dụ: biến 'education' với các giá trị 'primary', 'secondary'
# và 'tertiary'. Trong trường hợp này, việc giữ thứ tự các mức là quan trọng.
# Một ví dụ điển hình khác của biến ordered factor là các câu trả lời
# khảo sát sử dụng thang đo Likert (https://en.wikipedia.org/wiki/Likert_scale).
binaryVariables <- c("AcceptedCmp3","AcceptedCmp4","AcceptedCmp5","AcceptedCmp2","AcceptedCmp1")

clients[,binaryVariables] <- lapply(clients[,binaryVariables], factor, 
                                    levels = c(0,1), 
                                    labels = c("0","1"),
                                    ordered = TRUE )


# b) Bạn sẽ dùng đoạn mã nào để chuyển đổi biến Education? Giả sử rằng
# 2n nghĩa là giáo dục trung học và graduation tương đương với bảo vệ bằng cử nhân.
summary (factor (clients[,"Education"] ) )
clients$Education  <- factor(clients$Education ,
                             levels = c("Basic","Graduation","2n Cycle","Master","PhD"),
                             ordered = TRUE) 

# 7. Chuyển đổi các biến đã xác định trong bước 5 và 6 thành các lớp thích hợp.
summary (factor (clients[,"Complain"] ) )
clients$Complain <- factor(clients$Complain)

summary (factor (clients[,"Response"] ) )
clients$Response <- factor(clients$Response)

lapply(clients[, binaryVariables], function (x) {summary(factor(x))})

# 8. Lưu kết quả để tham khảo sau này! Sử dụng file RData với tên "clientsInR".

save(clients, file = "clientsInR.RData")

load("clientsInR.RData")

save(data, file = 'data.RData')











