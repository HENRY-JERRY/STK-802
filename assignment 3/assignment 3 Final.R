library(flexmix)
library(gclus)
library(xtable)
library(PerformanceAnalytics)
library(ggplot2)

#import the dataset
dataset <- read.csv("tdata.csv")


#correlation matrix
mcor<- round(cor(dataset), 3)
upper.tri(mcor)
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)


#export the correlations to latex 
print(xtable(upper))


#plots - EDA
chart.Correlation(dataset, histogram=TRUE, pch=19)
pairs(dataset[, 1:6])


Training the model using flemix
#*******************************************************************************
set.seed(12345)
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


#prediction of values - It is not necessary anyway
# #*******************************************************************************
ypred <- predict(model1, dataset)
ypred

c1 <- ypred$Comp.1
c2 <- ypred$Comp.2
c3 <- ypred$Comp.3


#Adding another column with cluster labels
#******************************************************************************
dataset[["g"]] <- clusters(model1)


#plotting scatter digrams
#Keep changing the variable names - I didnt write a function to plot
#E.g Age should be changed to Pen_ratio
#****************************************************************************
plot(dataset$Age,  dataset$Claims, col=c("red","green3","blue")[dataset$g], 
     main="prediction of components", xlab="Age", ylab="Claims")

abline(lm(Claims~Age, data = dataset[dataset$g == 1, ]),  col="red",lwd=1)
abline(lm(Claims~Age, data = dataset[dataset$g == 2, ]),  col="green3",lwd=1)
abline(lm(Claims~Age, data = dataset[dataset$g == 3, ]),  col="blue",lwd=1)



