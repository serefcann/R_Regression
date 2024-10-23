# rastgele secilmis 8 ailenin aylik geliri ve aylik kulturel harcamalari
# verilerini kullanarak x ve y arasindaki iliskiyi veren regresyon
# dogrusunu tahmin ediniz.
# selin benim sevgilim

x <- c(850,1000,1400,1750,2000,2200,2500,3400)
y <- c(80,90,175,225,250,275,300,345)
plot(x,y,xlab="aylik gelir",ylab="aylik harcama")

sxx <- sum((x-mean(x))^2)
sxy <- sum(x*y) - (length(x)*mean(x)*mean(y))
beta1 <- sxy/sxx
beta0 <- mean(y) - beta1*mean(x)
beta0; beta1

# 9.03129 + 0.110447*x

plot(x,y,xlab="aylik gelir",ylab="aylik harcama")
abline(lm(y~x),col="red")
model <- lm(y~x)
modelpred <- predict(model)

dataz <- data.frame("actual"=y,"prediction"=modelpred)
dataz

error <- y-modelpred
dataz <- data.frame("actual"=y,"prediction"=modelpred,"artiklar"=error)
dataz

# dataz$selam <- error
# dataz

sum(dataz$artiklar)
sum(error)

# ODEV

x <- c(15.50,23.75,8.00,17.00,5.50,19.00,24.00,2.50,7.50,11.00,
       13.00,3.75,25.00,9.75,22.00,18.00,6.00,12.50,2.00,21.50)
y <- c(2158.70,1678.15,2316.00,2061.30,2207.50,1708.30,1784.70,2575.00,
       2357.90,2256.70,2165.20,2399.55,1779.80,2336.75,1765.30,2053.50,
       2414.40,2200.50,2654.20,1753.70)

plot(x,y,xlab = "iticinin yasi",ylab = "kesme dayanimi")

sxx<- sum((x-mean(x))^2)
sxy<- sum(x*y)-sum(length(x)*mean(x)*mean(y))
b1<- sxy/sxx
b0<- mean(y)-b1*mean(x)
b0; b1

plot(x,y,xlab = "iticinin yasi",ylab = "kesme dayanimi")
abline(lm(y~x),col='blue')

model<- lm(y~x)
pred<- predict(model)

df<- data.frame("gercek_kesme_dayanimi"=y,"tahmin"=pred)
error <- y-pred
df$hata_fark = error
df





x1 <- c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
y <- c(16.68,11.5,12.03,14.88,13.75,18.11,8,17.83,79.24,21.5,40.33,21,13.5,
        19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75)
data <- data.frame(y,x1)
head(data)
data
plot(x1,y,xlab = 'teslim hacmi',ylab = "teslim suresi")
plot(y~x1,ylab = "teslim suresi",xlab = "teslim hacmi")
abline(lm(y~x1),col = 'red')

sxy <- sum(x1*y)-length(x1)*mean(x1)*mean(y)
sxx <- sum((x1-mean(x1))^2)
beta1_hat <- sxy/sxx
beta0_hat <- mean(y) - beta1_hat*mean(x1)
rbind(c(beta0_hat,beta1_hat))
# yhat = 3.32 + 2.17*x

model1 <- lm(y~x1)
pred <- predict(model1)
error <- y - pred

data$predictions <- pred
data$error <- error
data

# varsayimlar
round(sum(error)) # 0
sum(error)
sum(y);sum(pred) # esit

round(sum(x1*error)) # 0
round(sum(pred*error)) # 0

# regresyonun standart hatasi # sigma sapka
SSE<- sum((error)^2)
sigmakaresapka <- SSE/(length(x1)-2) 
sigmasapka <- sqrt(sigmakaresapka)
sigmasapka

# Katsayilara iliskin guven araliklari 
se_beta1 <- sqrt(sigmakaresapka/sxx)
se_beta0 <- sqrt(sigmakaresapka*((1/length(x1)) + (mean(x1)^2/sxx)))

beta0_interval <- c(beta0_hat-(qt(1-0.025,length(x1)-2)*se_beta0),
                    beta0_hat+(qt(1-0.025,length(x1)-2))*se_beta0)
beta1_interval <- c(beta1_hat-(qt(1-0.025,length(x1)-2)*se_beta1),
                    beta1_hat+(qt(1-0.025,length(x1)-2))*se_beta1)
beta0_interval
beta1_interval
confint(model1)

# ortalama yanit icin guven araligi
x0<- c(3,6,mean(x1),9,10,15,20,25,30)
y0_hat <- beta0_hat+beta1_hat*x0

var_ortalama_yanit <- sigmakaresapka*(1/length(x1) + ((x0-mean(x1))^2)/sxx)
alt_sinir <- y0_hat - (qt(1-0.025,25-2))*sqrt(var_ortalama_yanit)
# 25-2 = length(x1) - 2
ust_sinir <- y0_hat + (qt(1-0.025,25-2))*sqrt(var_ortalama_yanit)
rbind(alt_sinir,ust_sinir)
u <- ust_sinir - alt_sinir
paste("x0=",x0,"degeri icin ortalama yanit icin %95 guven araligi: ",
      "[",alt_sinir,ust_sinir,"]",u)

library(ggplot2)
x0<-x1
y_hat<-y
var_ortalama_yanit1 <- sigmakaresapka*((1/length(x1)) + ((x0-mean(x1))^2)/sxx)
alt_sinir1 <- y0_hat - (qt(1-0.025,25-2)*sqrt(var_ortalama_yanit1))
ust_sinir1 <- y0_hat + (qt(1-0.025,25-2)*sqrt(var_ortalama_yanit1))
data11<- data.frame(x1,y,alt_sinir1,ust_sinir1)
data11
ggplot(data=data11,aes(x=x1,y=y)) + 
  geom_smooth(method = "lm",se = F,col="red") +
  geom_point(data=data11, aes(x=x1,y=y),col="black") +
  geom_line(data=data11,aes(x1,y=alt_sinir1),col="blue") +
  geom_line(data=data11,aes(x=x1,y=ust_sinir1),col="darkblue")




# ANOVA Varyans analizi

SSE <- sum(error^2)
SSR <- beta1_hat*sxy
SST <- sum((y-mean(y))^2); SST
# SST = SSR+SSE
R2 <- SSR/SST;R2 # R2 hesaplama

#basit dogrusal
anova_table <- rbind(c(1,SSR,(SSR/1),(SSR/1)/(SSE/(length(x1)-2))),
                     c((length(y)-2),SSE,SSE/(length(x1)-2),((SSR/1)/(SSE/(length(x1)-2)))),
                       c((length(x1)-1),SST,SST/(length(x1)-1),((SSR/1)/(SSE/(length(x1)-2)))))
colnames(anova_table)<- c("sd","karaler top","karaler ort","Fhesap")
rownames(anova_table)<- c("regresyon","hata","genel")
anova_table
anova(model1) # 0.025'ten kucuk oldugu icin H0 red Pr(>F)

summary(model1)
# aykiri deger varsa medyan tercih edilmeli
# adjusted R2 biraz daha cezalandirilmis
























