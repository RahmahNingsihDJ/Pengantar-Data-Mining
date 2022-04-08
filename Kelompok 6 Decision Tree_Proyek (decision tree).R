#DECISION TREE (POHON KEPUTUSAN)
#pengambilan data
data1=read.csv("Bengaluru_House_Data.csv")
data1
dim(data1)
View(data1)
help("na.omit")

#checking missing value
colSums(is.na(data1))

#mengatasi missing value (menghilangkan data yang tidak lengkap)
data1_baru=na.omit(data1)
summary(data1_baru)
colSums(is.na(data1_baru))
dim(data1_baru)
View(data1_baru)
str(data1_baru)

#mengambil data yang numerik
databaru=data.frame(as.factor(data1_baru$area_type),as.numeric(data1_baru$total_sqft),data1_baru$bath,data1_baru$balcony,data1_baru$price)
databaru
str(databaru)
View(databaru)

#merename data
colnames(databaru)[1]="area_type"
colnames(databaru)[2]="total_sqft"
colnames(databaru)[3]="bath"
colnames(databaru)[4]="balcony"
colnames(databaru)[5]="price"

#membagi data menjadi data training dan data testing
str(databaru)
set.seed(1234)
bagidata <- sample(2, nrow(databaru), replace=TRUE, prob=c(0.7, 0.3))
data_training <- databaru[bagidata==1,]
data_testing <- databaru[bagidata==2,]

#Membangun model
library(rpart)
fit <- rpart(area_type~., data = data_training)
summary(fit)
fit$variable.importance
barplot(fit$variable.importance)

#membuat plot decision tree
library(rattle)
fancyRpartPlot(fit, font=2)

#EVALUASI MODEL
# prediksi testing
prediksi = predict(fit, newdata = data_testing, type = "class")
# Confusion matrix
table(prediksi, data_testing$area_type)
library(caret)
confusionMatrix(data=prediksi,reference=data_testing$area_type)