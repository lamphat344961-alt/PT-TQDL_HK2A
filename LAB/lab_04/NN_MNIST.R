# =============================================================
# MẠNG NƠ-RON NHẬN DẠNG CHỮ SỐ VIẾT TAY – MNIST
# Dùng package keras (bọc TensorFlow) – không viết backprop thủ công
# Kiến trúc: Input(784) -> Dense(128,ReLU) -> Dense(64,ReLU) -> Dense(10,Softmax)
# =============================================================
# Cài đặt lần đầu (chạy 1 lần duy nhất):
#install.packages("keras")
#library(keras)
#install_keras()   # tự cài TensorFlow + Python env
#
# Nếu dùng GPU:
#   install_keras(tensorflow = "gpu")
# =============================================================


# ================================================================
# PHẦN 0: PACKAGE
# ================================================================
install.packages(c("keras", "scales"))
library(keras)
library(ggplot2)
library(scales)


# ================================================================
# PHẦN 1: TẢI VÀ TIỀN XỬ LÝ DỮ LIỆU MNIST
# ================================================================
# keras có hàm tải MNIST tích hợp sẵn, không cần package ngoài
# Trả về list gồm train và test, mỗi tập có $x (ảnh) và $y (nhãn)

cat("Dang tai MNIST...\n")
mnist <- dataset_mnist()

X_train_full <- mnist$train$x
y_train_full <- mnist$train$y   # vector 0–9, dài 60000
X_test       <- mnist$test$x
y_test       <- mnist$test$y    # vector 0–9, dài 10000

# Reshape: từ (n, 28, 28) -> (n, 784) và chuẩn hóa [0, 1]
# array_reshape() của keras dùng C-order (row-major), đúng cho ảnh
X_train_full <- array_reshape(X_train_full, c(nrow(X_train_full), 784)) / 255
X_test       <- array_reshape(X_test,       c(nrow(X_test),       784)) / 255

cat(sprintf("Train full : %d x %d\n", nrow(X_train_full), ncol(X_train_full)))
cat(sprintf("Test       : %d x %d\n", nrow(X_test), ncol(X_test)))


# ================================================================
# PHẦN 2: CHIA TRAIN / VALIDATION / TEST
# ================================================================
# Train (80% of 60k = 48000): cập nhật trọng số
# Validation (20% = 12000) : early stopping, chọn hyperparameter
# Test (10000 riêng biệt)   : đánh giá cuối cùng

set.seed(123)
n_total   <- nrow(X_train_full)
val_idx   <- sample(n_total, floor(n_total * 0.2))
train_idx <- setdiff(seq_len(n_total), val_idx)

X_train <- X_train_full[train_idx, ]
y_train <- y_train_full[train_idx]

X_val   <- X_train_full[val_idx, ]
y_val   <- y_train_full[val_idx]

cat(sprintf("Train : %d | Val : %d | Test : %d\n",
            nrow(X_train), nrow(X_val), nrow(X_test)))

# One-hot encoding: keras yêu cầu nhãn dạng ma trận (n, 10)
# to_categorical() chuyển vector nhãn 0-9 -> ma trận one-hot
Y_train <- to_categorical(y_train, 10)
Y_val   <- to_categorical(y_val,   10)
Y_test  <- to_categorical(y_test,  10)


# ================================================================
# PHẦN 3: XÂY DỰNG MÔ HÌNH
# ================================================================
# keras_model_sequential(): mô hình tuyến tính (lớp nối tiếp nhau)
# layer_dense(): fully-connected layer
#   units       = số neuron
#   activation  = hàm kích hoạt ('relu', 'softmax', 'sigmoid', ...)
#   input_shape = chỉ cần khai báo ở lớp đầu tiên
# layer_dropout(): tắt ngẫu nhiên một tỷ lệ neuron trong lúc train
#   -> giảm overfitting, tăng tổng quát hóa

model <- keras_model_sequential(name = "MNIST_NN") %>%
  
  # Lớp ẩn 1: 784 -> 128, ReLU
  layer_dense(units = 128, activation = "relu",
              input_shape = 784,
              name = "hidden1") %>%
  
  # Dropout 20%: mỗi bước train tắt ngẫu nhiên 20% neuron lớp 1
  layer_dropout(rate = 0.2, name = "dropout1") %>%
  
  # Lớp ẩn 2: 128 -> 64, ReLU
  layer_dense(units = 64, activation = "relu",
              name = "hidden2") %>%
  
  # Dropout 20%
  layer_dropout(rate = 0.2, name = "dropout2") %>%
  
  # Lớp output: 64 -> 10, Softmax (xác suất 10 chữ số)
  layer_dense(units = 10, activation = "softmax",
              name = "output")

# Tổng quan kiến trúc
summary(model)


# ================================================================
# PHẦN 4: COMPILE MÔ HÌNH
# ================================================================
# compile() thiết lập:
#   optimizer: thuật toán tối ưu
#     "adam"  = Adaptive Moment Estimation, tự điều chỉnh learning rate
#               thường tốt hơn SGD thuần cho bài phân loại ảnh
#   loss    : hàm lỗi
#     "categorical_crossentropy" = Cross-Entropy cho nhiều lớp (one-hot)
#     "sparse_categorical_crossentropy" = dùng khi nhãn là số nguyên (không one-hot)
#   metrics : chỉ số hiển thị trong lúc train (không ảnh hưởng tối ưu)

model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss      = "categorical_crossentropy",
  metrics   = c("accuracy")
)


# ================================================================
# PHẦN 5: EARLY STOPPING CALLBACK
# ================================================================
# Callback: hàm được gọi tự động sau mỗi epoch
# callback_early_stopping():
#   monitor  : theo dõi chỉ số nào ("val_loss" hoặc "val_accuracy")
#   patience : số epoch chờ không cải thiện trước khi dừng
#   min_delta: mức cải thiện tối thiểu được coi là có ý nghĩa
#   restore_best_weights: TRUE = khôi phục trọng số tốt nhất khi dừng
#                         (tránh dùng trọng số đã bắt đầu overfit)

callbacks_list <- list(
  callback_early_stopping(
    monitor              = "val_loss",
    patience             = 8,
    min_delta            = 1e-4,
    restore_best_weights = TRUE,
    verbose              = 1
  ),
  # callback_reduce_lr_on_plateau(): giảm learning rate khi val_loss không cải thiện
  # factor=0.5: giảm lr xuống còn 50% ; min_lr: không giảm dưới ngưỡng này
  callback_reduce_lr_on_plateau(
    monitor  = "val_loss",
    factor   = 0.5,
    patience = 4,
    min_lr   = 1e-6,
    verbose  = 1
  )
)


# ================================================================
# PHẦN 6: HUẤN LUYỆN
# ================================================================
# fit() chạy toàn bộ vòng lặp train (feedforward + backprop + cập nhật)
# Trả về object "history" chứa loss và metrics theo từng epoch
# validation_data: tập val để tính val_loss và val_accuracy sau mỗi epoch

cat("\nBat dau huan luyen...\n")
history <- model %>% fit(
  x                = X_train,
  y                = Y_train,
  epochs           = 50,
  batch_size       = 256,
  validation_data  = list(X_val, Y_val),
  callbacks        = callbacks_list,
  verbose          = 1   # 1 = hiện progress bar; 0 = im lặng; 2 = 1 dòng/epoch
)


# ================================================================
# PHẦN 7: ĐÁNH GIÁ TRÊN TẬP TEST
# ================================================================
# evaluate() chạy feedforward trên test set, trả về loss và metrics
cat("\nDanh gia tren tap Test...\n")
test_eval <- model %>% evaluate(X_test, Y_test, verbose = 0)
cat(sprintf("Test Loss     : %.4f\n", test_eval["loss"]))
cat(sprintf("Test Accuracy : %.4f (%.2f%%)\n",
            test_eval["accuracy"], test_eval["accuracy"] * 100))

# Dự đoán nhãn từng mẫu (argmax của softmax output)
Y_hat    <- model %>% predict(X_test, verbose = 0)  # (10000, 10)
y_pred   <- apply(Y_hat, 1, which.max) - 1L          # 0-indexed

# Accuracy từng chữ số
per_class_acc <- sapply(0:9, function(cls) {
  idx <- y_test == cls
  mean(y_pred[idx] == cls)
})

# Confusion matrix
cm <- table(True = y_test, Predicted = y_pred)

# Bảng tổng hợp
cat(strrep("=", 54), "\n")
cat("  KET QUA DANH GIA\n")
cat(strrep("=", 54), "\n")
n_epochs_run <- length(history$metrics$loss)
metrics_df <- data.frame(
  Chi_so  = c("Test Accuracy", "Test Loss",
              "Epochs thuc chay",
              paste0("Accuracy chu so ", 0:9)),
  Gia_tri = c(
    sprintf("%.4f (%.2f%%)", test_eval["accuracy"], test_eval["accuracy"]*100),
    sprintf("%.4f", test_eval["loss"]),
    sprintf("%d", n_epochs_run),
    sprintf("%.2f%%", per_class_acc * 100)
  )
)
print(metrics_df, row.names = FALSE)
cat(strrep("=", 54), "\n")


# ================================================================
# PHẦN 8: ĐỒ THỊ
# ================================================================

# --- 8A: Loss Curve Train vs Validation ---
# history$metrics chứa: loss, accuracy, val_loss, val_accuracy
n_ep    <- length(history$metrics$loss)

df_loss <- data.frame(
  epoch = rep(seq_len(n_ep), 2),
  loss  = c(history$metrics$loss, history$metrics$val_loss),
  tap   = factor(rep(c("Train", "Validation"), each = n_ep),
                 levels = c("Train", "Validation"))
)

# Tìm epoch có val_loss tốt nhất (nơi early stopping khôi phục về)
best_epoch <- which.min(history$metrics$val_loss)

p_loss <- ggplot(df_loss, aes(x = epoch, y = loss, color = tap)) +
  geom_line(linewidth = 1.0) +
  geom_vline(xintercept = best_epoch,
             color = "tomato", linetype = "dashed", linewidth = 0.8) +
  annotate("text",
           x = best_epoch + 0.3,
           y = max(df_loss$loss) * 0.92,
           label = sprintf("Best\nEpoch %d", best_epoch),
           color = "tomato", hjust = 0, size = 3.5) +
  scale_color_manual(values = c("Train" = "#185FA5", "Validation" = "#D85A30")) +
  scale_x_continuous(breaks = pretty_breaks(n = 8)) +
  labs(
    title    = "Loss Function: Train vs Validation",
    subtitle = sprintf("MNIST keras | Best Val Loss: %.4f | Epochs: %d",
                       min(history$metrics$val_loss), n_ep),
    x = "Epoch", y = "Cross-Entropy Loss", color = "Tap du lieu"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title      = element_text(face = "bold", size = 14),
        plot.subtitle   = element_text(color = "gray45", size = 11),
        legend.position = "top")

print(p_loss)


# --- 8B: Accuracy Curve Train vs Validation ---
df_acc <- data.frame(
  epoch    = rep(seq_len(n_ep), 2),
  accuracy = c(history$metrics$accuracy, history$metrics$val_accuracy) * 100,
  tap      = factor(rep(c("Train", "Validation"), each = n_ep),
                    levels = c("Train", "Validation"))
)

p_acc <- ggplot(df_acc, aes(x = epoch, y = accuracy, color = tap)) +
  geom_line(linewidth = 1.0) +
  geom_vline(xintercept = best_epoch,
             color = "tomato", linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(values = c("Train" = "#185FA5", "Validation" = "#D85A30")) +
  scale_x_continuous(breaks = pretty_breaks(n = 8)) +
  labs(
    title    = "Accuracy: Train vs Validation",
    subtitle = sprintf("Best Val Accuracy: %.2f%%",
                       max(history$metrics$val_accuracy) * 100),
    x = "Epoch", y = "Accuracy (%)", color = "Tap du lieu"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title      = element_text(face = "bold", size = 14),
        plot.subtitle   = element_text(color = "gray45", size = 11),
        legend.position = "top")

print(p_acc)


# --- 8C: Confusion Matrix Heatmap ---
cm_df          <- as.data.frame(cm)
total_per_true <- tapply(cm_df$Freq, cm_df$True, sum)
cm_df$pct      <- cm_df$Freq / total_per_true[as.character(cm_df$True)] * 100

p_cm <- ggplot(cm_df, aes(x = Predicted, y = True, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(Freq > 0,
                               sprintf("%d\n%.0f%%", Freq, pct), "")),
            size = 2.6, color = "gray15", fontface = "bold") +
  scale_fill_gradient(low = "#EAF3DE", high = "#27500A",
                      name = "% / hang",
                      labels = function(x) paste0(x, "%")) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Confusion Matrix – MNIST Test Set",
    subtitle = sprintf("Test Accuracy: %.2f%% | %d mau",
                       test_eval["accuracy"] * 100, nrow(X_test)),
    x = "Du doan (Predicted)", y = "Thuc te (True)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray45", size = 11),
        panel.grid    = element_blank())

print(p_cm)


# --- 8D: Per-class Accuracy ---
df_cls <- data.frame(chu_so   = factor(0:9),
                     accuracy = per_class_acc * 100)

p_cls <- ggplot(df_cls, aes(x = chu_so, y = accuracy, fill = accuracy)) +
  geom_col(width = 0.65, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", accuracy)),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  geom_hline(yintercept = test_eval["accuracy"] * 100,
             color = "tomato", linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = 10.4, y = test_eval["accuracy"] * 100,
           label = sprintf("TB\n%.1f%%", test_eval["accuracy"] * 100),
           color = "tomato", hjust = 1, size = 3.2) +
  scale_fill_gradient(low = "#B5D4F4", high = "#0C447C", guide = "none") +
  scale_y_continuous(limits = c(0, 108), expand = c(0, 0)) +
  labs(title    = "Accuracy theo tung chu so (0-9)",
       subtitle = "Duong ke do = accuracy trung binh tren test set",
       x = "Chu so", y = "Accuracy (%)") +
  theme_minimal(base_size = 13) +
  theme(plot.title         = element_text(face = "bold", size = 14),
        plot.subtitle      = element_text(color = "gray45", size = 11),
        panel.grid.major.x = element_blank())

print(p_cls)

cat("\nHoan tat! 4 bieu do: Loss | Accuracy | Confusion Matrix | Per-class Acc\n")