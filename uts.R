# Import Dataset
hepatitis <- read.csv("C:/Users/lenovo/Downloads/hepatitis.data", header=FALSE)
View(hepatitis)

# Pemanggilan Library
library(randomForest)
library(caret)
library(psych)
library(party)
library(mice)

str(hepatitis)
summary(databaru)
databaru <- na.omit(hepatitis)

p <- function(x) {sum(is.na(x))/length(x)*100}
apply(hepatitis, 2, p)
md.pattern(hepatitis)

# Menghilangkan Missing value
databaru$V4 = ifelse(is.na(databaru$V4),
                      ave(databaru$V4, FUN = function(x) mean(x, na.rm = TRUE)),
                      databaru$V4)

hepatitis$V6 = ifelse(is.na(hepatitis$V6),
                      ave(hepatitis$V6, FUN = function(x) mean(x, na.rm = TRUE)),
                      hepatitis$V6)

#Menjadikan variabel kategorik sebagai faktor
hepatitis$V1 <- as.factor(hepatitis$V1)
hepatitis$V3 <- as.factor(hepatitis$V3)
hepatitis$V4 <- as.factor(hepatitis$V4)
hepatitis$V5 <- as.factor(hepatitis$V5)
hepatitis$V6 <- as.factor(hepatitis$V6)
hepatitis$V7 <- as.factor(hepatitis$V7)
hepatitis$V8 <- as.factor(hepatitis$V8)
hepatitis$V9 <- as.factor(hepatitis$V9)
hepatitis$V10 <- as.factor(hepatitis$V10)
hepatitis$V11 <- as.factor(hepatitis$V11)
hepatitis$V12 <- as.factor(hepatitis$V12)
hepatitis$V13 <- as.factor(hepatitis$V13)
hepatitis$V14 <- as.factor(hepatitis$V14)
hepatitis$V20 <- as.factor(hepatitis$V20)

# Partisi data menjadi data training dan testing
inTrain <- createDataPartition(y=hepatitis$V1, p=0.75, list = FALSE)
train <- hepatitis[inTrain]
test <- hepatitis[-inTrain]

# Membuat model Random Fores
output.forest <- randomForest(V1 ~ ., data = train)
output.forest

importance(output.forest)

# Bentuk plot dari variabel importance
varImpPlot(output.forest)

# Validasi model dengam data test
Prediksi <- predict(output.forest, test)

# confusion matrix
CM <- table(test$V1, Perdiksi)

# Akurasi
accuracy <- (sum(diag(CM)))/sum(CM)

summary(hepatitis$V2)









