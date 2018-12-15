library(flexmix)
library(gclus)
library(xtable)
library(PerformanceAnalytics)
library(ggplot2)

dataset <- read.csv("tdata.csv")

dataset1 <- dataset
length(dataset1)

#removing claims outliers
summary(dataset1)
bench <- 0.3885 + 1.5*IQR(dataset1$Claims)
bench
data.claims <- dataset1[dataset1$Claims <= bench, ]

# #removing claims Beneficiaries
# summary(data.claims)
# #bench1 <- -0.1374 + 1.5*IQR(data.claims$Beneficiaries)
# bench1 = 0.18
# data.claims <- data.claims[data.claims$Beneficiaries < bench1, ]
# 
# #removing claims Beneficiaries
# summary(data.claims)
# bench1 <- -0.07837 + 1.5*IQR(data.claims$Beneficiaries)
# bench1
# data.claims <- data.claims[data.claims$Beneficiaries < bench1, ]
# 
# #removing claims Female
# summary(data.claims)
# bench2 <- 0.41671 + 1.5*IQR(data.claims$Female_Ratio)
# bench3 <- -0.37715 - 1.5*IQR(data.claims$Female_Ratio)
# bench3
# bench2
# data.claims <- data.claims[data.claims$Female_Ratio > bench3, ]
# 
# #removing claims Age
# summary(data.claims)
# bench4 <- 0.3083 + 1.5*IQR(data.claims$Age)
# bench4
# data.claims <- data.claims[data.claims$Age < bench4, ]
# 
# #removing claims Pens_ratio
# summary(data.claims)
# bench5 <- 0.09187 + 1.5*IQR(data.claims$Pens_ratio)
# bench5
# data.claims <- data.claims[data.claims$Pens_ratio < bench5, ]
# 
# #removing claims Depen_ratio
# summary(data.claims)
# bench6 <- 0.60735 + 1.5*IQR(data.claims$Depen_ratio)
# bench6
# bench7 <- -0.42845 - 1.5*IQR(data.claims$Depen_ratio)
# bench7
# data.claims <- data.claims[data.claims$Depen_ratio > bench7, ]

write.csv(data.claims, file = "cleanData.csv")
dataset1 <- read.csv("cleanData.csv") 

#exploratory data analysis
plot(dataset$Beneficiaries, dataset$Claims, col="Red")
plot(dataset$Age, dataset$Claims, col="Red")

boxplot(dataset$Claims)
boxplot(dataset$Beneficiaries)
boxplot(dataset$Female_Ratio)
boxplot(dataset$Age)
boxplot(dataset$Pens_ratio)
boxplot(dataset$Depen_ratio)


#correlation matrix
mcor<- round(cor(dataset), 3)
upper.tri(mcor)
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper
print(xtable(upper))

chart.Correlation(dataset, histogram=TRUE, pch=19)

chart.Correlation(dataset1, histogram=TRUE, pch=19)

pairs(dataset[, 1:6])


set.seed(12345)

#*******************************************************************************
model1 <-  flexmix(Claims ~ (Beneficiaries+Female_Ratio+Age+Pens_ratio+Depen_ratio),
               data = dataset, k = 3)
model1

parameters(model1, component= 1)
parameters(model1, component= 2)
parameters(model1, component= 3)

clusters(model1)

summary(model1)

model2 <- refit(model1)

summary(model2)

plot(model2)

par(new=T)

# #*******************************************************************************
 set.seed(12345)
# 
# model3 <-  flexmix(Claims ~ (Beneficiaries+Female_Ratio+Age+Pens_ratio+Depen_ratio),
#                    data = dataset1, k = 3, model = FLXglm(family = 'gaussian'))
# model3
model3 <-  flexmix(Claims ~ Age,
                   data = dataset1, k = 3, model = FLXglm(family = 'gaussian'))

 summary(model3)
# 
 model4 <- refit(model3)
# 
# summary(model4)
# 
 clusters(model3)
# 
# 
# parameters(model3, component= 1)
# parameters(model1, component= 2)
# parameters(model1, component= 3)
# 
ypred2 <- predict(model3, dataset1)


c11 <- ypred2$Comp.1
c21 <- ypred2$Comp.2
c31 <- ypred2$Comp.3

dataAge <- dataset1
dataAge[["c11"]] <- c11
dataAge[["c21"]] <- c21
dataAge[["c31"]] <- c31
# 
dataAge[["g"]] <- clusters(model3)
write.csv(dataAge, file = "clusters.csv")
# 
dataAge <- read.csv("clusters.csv")
plot(dataAge$Age,  dataAge$Claims, col=c("red","green3","blue")[clusters(model3)], 
      main="prediction of components", xlab="Age", ylab="Claims")
abline(lm(Claims~Age, data = dataAge[dataAge$g == 1, ]), col="red",lwd=1)
abline(lm(Claims~Age, data = dataAge[dataAge$g == 2, ]),  col="green3",lwd=1)
abline(lm(Claims~Age, data = dataAge[dataAge$g == 3, ]),  col="blue",lwd=1)
# 

summary(dataAge[dataAge$g == 1, ])
# #*******************************************************************************

ypred <- predict(model1, dataset)

ypred

c1 <- ypred$Comp.1
c1
c2 <- ypred$Comp.2
c3 <- ypred$Comp.3
#******************************************************************************
dataset[["g"]] <- clusters(model1)
#****************************************************************************
plot(dataset$Pens_ratio,  dataset$Claims, col=c("red","green3","blue")[dataset$g], 
     main="prediction of components", xlab="Pens_ratio", ylab="Claims")

abline(lm(Claims~Pens_ratio, data = dataset[dataset$g == 1, ]), col="red",lwd=1)
abline(lm(Claims~Pens_ratio, data = dataset[dataset$g == 2, ]),  col="green3",lwd=1)
abline(lm(Claims~Pens_ratio, data = dataset[dataset$g == 3, ]),  col="blue",lwd=1)

summary(dataset[dataset$g == 3, ])
