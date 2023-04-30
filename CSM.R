library(readxl)
library(utf8)
library(tidyverse)
library(plyr)

###### 1111 #######

# doc du lieu
new_data <- read_excel("2014 and 2015 CSM dataset.xlsx")
View(new_data)

###### 2222 #######
# lam sach du lieu

data_use <- new_data %>% as_tibble() %>%
select(Genre, Sequel, Budget, Screens, Views, Gross)
View(data_use)

# tim cac N/A
apply(is.na(data_use), 2, which)

# xu ly cac du lieu khuyet
for(i in c(1:2)) {
  data_use[is.na(data_use[,i]), i] <- median(as.numeric(unlist(data_use[,i])),
                                             na.rm = TRUE)
}
for(i in c(3:6)) {
  data_use[is.na(data_use[,i]), i] <- mean(as.numeric(unlist(data_use[,i])),
                                           na.rm = TRUE)
}
View(data_use)

########  3333 ###########
# lam ro du lieu

# thong ke du lieu
# thong ke bien lien tuc

cot <- c("mean", "median", "sd", "min", "max")
hang <- c("Budget", "Screens", "Views", "Gross")
tp <- c()
for (i in hang) {
  cotm <- c(mean(data_use[[i]]), median(data_use[[i]]), sd(data_use[[i]]),
            min(data_use[[i]]), max(data_use[[i]]))
  tp <- rbind(tp,cotm)
}

tp <- as.data.frame(tp)
colnames(tp) <- cot
rownames(tp) <- hang
data_analysis <- tp
View(data_analysis)

# thong ke bien phan loai
b1_3.1.2 = table(data_use$Genre, dnn = "Genre")
View(b1_3.1.2)
b2_3.1.2 = table(data_use$Sequel, dnn = "Sequel")
View(b2_3.1.2)

# ve do thi cua bien Gross
par(mar=c(3.9,3.9,3.9,3.9))
hist(data_use$Gross, xlab = "Value", main = "Histogram of movies gross",
     ylim=c(0,200), breaks = 7, labels = T, col = "lightblue")

# ve bieu do hop de thay su tuong quan giua bien Gross voi cac bien phan loai khac

boxplot(data = data_use, Gross ~ Genre, col = "cornsilk",
        main = "Boxplot of gross for each genre",
        xlab = "Genre", ylab = "Value of gross")
boxplot(data = data_use, Gross ~ Sequel, col = "darkgoldenrod1",
        main = "Boxplot of gross for each sequel",
        xlab = "Sequel", ylab = "Value of gross")

# bieu do phan phoi giua bien Gross voi cac bien lien tuc khac

pairs(data = data_use, Gross ~ Budget, main = "Gross & Budget pairs graph",
      pch = 16, col = "lightblue")
pairs(data = data_use, Gross ~ Screens, main = "Gross & Screens pairs graph",
      pch = 16, col = "lightblue")
pairs(data = data_use, Gross ~ Views, main = "Gross & Views pairs graph",
      pch = 16, col = "lightblue")

########## 4444 ##########
library(BMA)

X = data_use[, c('Genre', 'Sequel', 'Budget', 'Screens', 'Views')]
Y = data_use$Gross

result <- bicreg(X, Y, strict = FALSE, OR = 20)
summary(result)

# bieu do xac suat xuat hien cua cac bien trong cac mo hinh
imageplot.bma(result)

# chon mo hinh hoi quy boi
# mo hinh toi uu se loai bo cac bien Genre, Views
optimal_model <-lm(Gross ~ Sequel + Budget + Screens, data = data_use)
summary(optimal_model)

##########  555 ########
# kiem tra cac gia dinh

library(performance)
check_model(optimal_model)

#########  6666 #########
# du doan

# du doan doanh thu cao or ko cao
High_gross <- function(x){
  if(x >= 100000000) return("High gross")
  else return("Not high gross")
}
ket_qua <- c(apply(data_use["Gross"], MARGIN = 1, FUN = High_gross))
data_use <- cbind (data_use, ket_qua)
View(data_use)

# thong ke ti le
ti_le = prop.table(table(data_use$Gross >= 100000000))
ti_le

# Kiem dinh ket qua du doan thong qua mo hinh hoi quy

newtab <- data_use %>% as_tibble() %>% select(Gross, Sequel, Budget, Screens, Views)
pred_Gross <- predict(optimal_model)
newtab <- cbind(newtab, pred_Gross)
view(newtab)

# so sanh kq
ti_le1 = prop.table(table(newtab$pred_Gross >= 100000000))
So_sanh <- data.frame(cbind(ti_le, ti_le1))
colnames(So_sanh) <- c("Thuc te","Du doan")
rownames(So_sanh) <- c("Not high gross", "High gross")
View(So_sanh)


