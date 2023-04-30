library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggpubr)

#################### 11111 ##############
# file -> import dataset -> from text (readr)

auto_mpg <- read_delim("auto_mpg.csv", delim = ";", 
                       quote = "'", escape_double = FALSE, na = "?", 
                       comment = "#", trim_ws = TRUE)

#--------------------lam sach du lieu---------------------
apply(is.na(auto_mpg),2,sum)   #Dem so luong NA
apply(is.na(auto_mpg),2,which) #cac cot bi khuyet, xuat dong bi khuyet
auto_mpg = na.omit(auto_mpg)   #xoa du lieu bi khuyet
apply(is.na(auto_mpg),2,sum)   #Dem so luong NA

#xet du lieu ngoai lai
temp <- auto_mpg[,c(1:8)]

# boxplot.stats( temp[[i]])$out => xem cac gia tri ngoai lai
# length(boxplot.stats( temp[[i]])$out) => dem so luong ngoai lai
# neu cot nao co ngoai lai => in ra

for(i in 1:ncol(temp)){
    if(length(boxplot.stats( temp[[i]])$out)!=0)
        cat(names(temp[i]),'la bien co gia tri ngoai lai\n')
}

#######  nhin ro hon 2 cac bien co outlier #######

qplot(y=temp$horsepower,x='',geom='boxplot',
      col=I('darkblue'),fill=I('lightblue'),ylab='value of Horsepower',
      xlab='Horsepower',main='Boxplot of Horsepower')

qplot(y=temp$acceleration,x='',geom='boxplot',
      col=I('darkblue'),fill=I('lightblue'),ylab='Value of Acceleration',
      xlab='Acceleration',main='Boxplot of Acceleration')


#----------------loai bo outliers --------------

# quantile(temp[[i]], 0.75) => phan vi 75% Q3
# upper_outliers => ngoai lai phia tren = Q3 + 1.5*IQR

# quantile(temp[[i]], 0.25) => phan vi 25% Q1
# lower_outliers => ngoai lai phia duoi = Q1 - 1.5*IQR

# subset => tach du lieu rieng ra

for(i in 1:ncol(temp))
{
    upper_outliers= quantile(temp[[i]], 0.75) +1.5 *IQR(temp[[i]])
    lower_outliers= quantile(temp[[i]], 0.25) -1.5 *IQR( temp[[i]])
    
    while(length(boxplot.stats( temp[[i]])$out)!=0)
    {
        temp <- subset(temp, temp[[i]]<=upper_outliers &  temp[[i]]>=lower_outliers)
        upper_outliers= quantile( temp[[i]], 0.75) +1.5 *IQR( temp[[i]])
        lower_outliers= quantile( temp[[i]], 0.25) -1.5 *IQR( temp[[i]])
    }
}

# kiem tra lai so luong cac ngoai lai (8 cot)

for(i in 1:ncol(temp)){
    print(boxplot.stats( temp[[i]])$out)
}

# ve bieu do de kiem tra ro hon

qplot(y=temp$horsepower,x='',geom='boxplot',
      col=I('darkblue'),fill=I('lightblue'),ylab='Value of Horsepower',
      xlab='Horsepower',main='Boxplot of Horsepower')

qplot(y=temp$acceleration,x='',geom='boxplot',
      col=I('darkblue'),fill=I('lightblue'),ylab='Value of Acceleration',
      xlab='Acceleration',main='Boxplot of Acceleration')

#------------------thuc hien tinh toan thong ke-----------------

# thong ke du lieu
# thong ke bien lien tuc

cot <- c("mean", "median", "sd", "min", "max")
hang <- c('mgp','displacement','horsepower','weight','acceleration')
statistic <- c()
for(i in hang){
    cotm <- c(mean(temp[[i]]), median(temp[[i]]), sd(temp[[i]]),
              min(temp[[i]]), max(temp[[i]]))
    statistic <-rbind(statistic,cotm)
}
statistic <-as.data.frame(statistic)
rownames(statistic) <-hang
colnames(statistic) <-cot
statistic

# thong ke bien phan loai
stat_cylinders = table(temp$cylinders, dnn = "cylinders")
View(stat_cylinders)

stat_model_year = table(temp$model_year, dnn = "model_year")
View(stat_model_year)

stat_origin = table(temp$origin, dnn = "origin")
View(stat_origin)

stat_car_name = table(auto_mpg$car_name, dnn = "car_name")
View(stat_car_name)

# Ve bieu do bien mgp

par(mar=c(3.9,3.9,3.9,3.9))
hist(temp$mgp, xlab = "MGP", main = " histogram graph",
     ylim=c(0,100), labels = T, col = "lightblue")

# ve bieu do hop de thay su tuong quan giua bien mgp voi cac bien phan loai khac

boxplot(data = temp, mgp ~ cylinders, col = "cornsilk",
        main = "Boxplot of Mgp for each cylinders",
        xlab = "cylinders", ylab = "Value of Mgp")

boxplot(data = temp, mgp ~ model_year, col = "darkgoldenrod1",
        main = "Boxplot of Mgp for each model_year",
        xlab = "model_year", ylab = "Value of Mgp")

boxplot(data = temp, mgp ~ origin, col = "darkolivegreen1",
        main = "Boxplot of Mgp for each origin",
        xlab = "origin", ylab = "Value of Mgp")

boxplot(data = auto_mpg, mgp ~ car_name, col = "darksalmon",
        main = "Boxplot of Mgp for each car_name",
        xlab = "car_name", ylab = "Value of Mgp")

# bieu do phan phoi giua bien mgp voi cac bien lien tuc khac

pairs(data = temp, mgp ~ displacement, main = "mgp & displacement pairs graph", pch = 16,
      col = "lightblue")

pairs(data = temp, mgp ~ horsepower, main = "mgp & horsepower pairs graph", pch = 16,
      col = "lightblue")

pairs(data = temp, mgp ~ weight, main = "mgp & weight pairs graph",
      pch = 16, col = "lightblue")

pairs(data = temp, mgp ~ acceleration, main = "mgp & acceleration pairs graph",
      pch = 16, col = "lightblue")

###############   2222  ###############
# chia du lieu thanh 2 phan

auto_mpg1 <- temp[1:200,]
auto_mpg2 <- temp[201:342,]

################   3333  ###############
# (pValue>0.05 la khong co y nghia ve mat thong ke) 

# dung phuong phap bayes chon mo hinh toi uu
# X => cac bien doc lap
# Y => bien phu thuoc mpg

library(BMA)
X = auto_mpg1[, c('cylinders','displacement','horsepower','weight'
                  ,'acceleration','model_year','origin')]

Y = auto_mpg1$mgp

result <- bicreg(X, Y, strict = FALSE, OR = 20)

summary(result)

# bieu do xac suat xuat hien cua cac bien trong cac mo hinh

imageplot.bma(result)

# mo hinh hoi quy boi
# mo hinh toi uu se loai bo cac bien cylinders, displacement, horsepower, acceleration
optimal_model <-lm(mgp ~ weight + model_year + origin, data = auto_mpg1)
summary(optimal_model)

#############  4444 #############
# Kiem tra cac gia dinh (gia thiet) cua mo hinh.

#install.packages("see")
#install.packages("patchwork")
#install.packages("performance")

library(performance)
check_model(optimal_model)

############  6666  ############
# dung ham predict de dua ra du bao

predict_mpg = predict(optimal_model, auto_mpg2, interval = "prediction")

############  7777  ############

# them cot gia tri thuc te de so sanh
# Cot fit dai dien cho gia tri bien phu thuoc uoc luong tu mo hinh
# Cot lwr và upr la nhung gia tri gioi han khoang tin cay 95%
# Cot mgp la gia tri thuc te thu thap duoc

predict_mpg = data.frame(predict_mpg, auto_mpg2["mgp"]) 

suitable = predict_mpg["mgp"] >= predict_mpg["lwr"] & predict_mpg["mgp"] <= predict_mpg["upr"]
sum(suitable)
sum(!suitable)

# ve bieu do de thay Su khac biet giua du bao va thuc te

predict_mpg%>%gather(mgp,fit,key="Y",value="DLCO")%>%
  ggplot(aes(x=DLCO, fill=Y))+
  geom_density(alpha=0.3)+
  scale_fill_manual(values=c("blue","red"))+
  theme_bw() 


