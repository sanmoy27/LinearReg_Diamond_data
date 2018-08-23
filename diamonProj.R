#Library
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(plotly)
library(Hmisc)
library(corrgram)
library(caTools)

setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\ML-R")

#Read CSV
dataset <- read.csv("./data/diamonds.csv", header = T, stringsAsFactors = F)

#Read First 6 recs
head(dataset)

#Read Last 6 Recs
tail(dataset)

#Structure of the data frame
str(dataset)
glimpse(dataset)

#No of Cols
ncol(dataset)

#No of rows
nrow(dataset)

#Names
names(dataset)


#Detect NAs
detectNAs<-function(x){
  return(sum(is.na(x)))
}
lapply(dataset, detectNAs)

#DetectSpace
detectSpace <- function(x){
  if(class(x)=="character")
    return(sum(str_trim(x)==""))
  else
    return("Not a Character")
}
sapply(dataset, detectSpace)


#Clean Data
dataset$cut[str_trim(dataset$cut)==""] <- NA
dataset$color[str_trim(dataset$color)==""] <- NA
dataset$clarity[str_trim(dataset$clarity)==""] <- NA

which(is.na(dataset$cut))
which(is.na(dataset$color))
which(is.na(dataset$clarity))

View(dataset)

#Remove NA's
datasetGood <- na.omit(dataset)
nrow(datasetGood)

png("./plots/diamondDist.png", width = 500, height = 500, res = 72)
#Freq dist of Price
p1<- ggplot(datasetGood, aes(x=price))+
  geom_histogram(binwidth = 1)+
  labs(title="Price Distribution")+
  labs(x="Price")

p2<-ggplot(datasetGood, aes(x=carat))+
  geom_histogram(binwidth=0.1)+
  labs(title="Carat Distribution")+
  labs(x="Carat")

p3<-ggplot(datasetGood, aes(x=depth))+
  geom_histogram(binwidth=0.5)+
  labs(title="Depth Distribution")+
  labs(x="Depth")

p4<-ggplot(datasetGood, aes(x=table))+
  geom_histogram(binwidth=0.5)+
  labs(title="Table Distribution")+
  labs(x="Table")

grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
dev.off()

#Comparision of cut with price
g1<-ggplot(datasetGood, aes(x=color, y=price))+
  geom_point(position="jitter")+
  labs(title="Color v/s Price")+
  labs(x="Color")+
  labs(y="Price")

g2<-ggplot(datasetGood, aes(x=color, y=price, fill=color))+
  geom_bar(stat="identity", position="dodge")+
  labs(title="Color v/s Price")+
  labs(x="Color")+
  labs(y="Price")


g3<-ggplot(datasetGood, aes(x=cut, fill=color))+
  geom_bar(stat="count", position="dodge")+
  labs(title="Count of Color by Cut")+
  labs(x="Cut")+
  labs(y="Color")

g4<-ggplot(datasetGood, aes(x=cut, fill=clarity))+
  geom_bar(stat="count", position="dodge")+
  labs(title="Count of Clarity by Cut")+
  labs(x="Cut")+
  labs(y="Clarity")

grid.arrange(g1, g2, g3, g4, nrow=2, ncol=2)

#Chisq Test
#H0: Clarity is independent on cut
#H1: Clarity is dependent on cut
cTable <- table(datasetGood$cut, datasetGood$clarity)
cTable
chisq.test(cTable)
#Reject Null Hypothesis at significance level=0.05


#Annova
datasetAnn <- datasetGood[, 1:7]
#H0: Price is independent on cut and clarity
#H1: Price is dependent on cut and clarity
datasetAnn$cut <- factor(datasetAnn$cut, levels=c("Fair", "Good", "Ideal", "Premium", "Very Good"), labels = c(1,2,3,4,5))
datasetAnn$clarity <- factor(datasetAnn$clarity, levels=c("I1", "IF", "SI1", "SI2", "VS1", "VS2", "VVS1", "VVS2"), labels = c(1,2,3,4,5,6,7,8))
datasetAnn$color <- factor(datasetAnn$color, levels=c("D", "E", "F", "G", "H", "I", "J"), labels = c(1,2,3,4,5,6,7))

annova=aov(price ~ (cut + clarity + cut*clarity), data=datasetAnn)
summary(annova)
#Accept Alt Hypothesis, both cut and clarity is dependent on price


#H0: Price is independent on color and cut
#H1: Price is dependent on color and cut
annova=aov(price ~ (cut + color + cut*color), data=datasetAnn)
summary(annova)
#Accept Alt Hypothesis, both cut and color is dependent on price

#H0: Price is independent on color and clarity
#H1: Price is dependent on color and clarity
annova=aov(price ~ (color + clarity + color*clarity), data=datasetAnn)
summary(annova)
#Accept Alt Hypothesis, both clarity and color is dependent on price


#Box plot
b1 <- ggplot(datasetAnn, aes(x=cut, y=price)) +
  geom_boxplot(aes(fill=cut)) +
  labs(title="cut & price") +
  labs(x="Cut") +
  labs(y="Price")

b2 <- ggplot(datasetAnn, aes(x=color, y=price)) +
  geom_boxplot(aes(fill=color)) +
  labs(title="color & price") +
  labs(x="Color") +
  labs(y="Price")


b3 <- ggplot(datasetAnn, aes(x=clarity, y=price)) +
  geom_boxplot(aes(fill=clarity)) +
  labs(title="clarity & price") +
  labs(x="Clarity") +
  labs(y="Price")

grid.arrange(b1, b2, b3, nrow=3, ncol=1)



#Correlation
rcorr(as.matrix(datasetAnn))
#Correlation Plot
col.corrgram <- function(ncol){   
  colorRampPalette(c("darkgoldenrod4", "burlywood1",
                     "darkkhaki", "darkgreen"))(ncol)} 
corrgram(datasetAnn, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt, 
         main="Correlogram of Car Mileage Data (PC2/PC1 Order)")


#Multiple Regression
#DependentVariable=Price
datasetAnn <- datasetAnn[c(2,3,4,1,5,6,7)]
set.seed(123)

#Split the data into training and test set
split<-sample.split(datasetAnn$price, SplitRatio = 0.8)
training_set<-subset(datasetAnn, split==TRUE)
test_set<-subset(datasetAnn, split==FALSE)

#Scaling
training_set[,4:7]<-scale(training_set[,4:7])
test_set[,4:7]<-scale(test_set[, 4:7])

#Build Model
regressor1<- lm(formula=price~., data=training_set)
summary(regressor1)

#predict on test data
cat("\014")
y_pred<-predict(regressor1, newdata = test_set)
y_pred

#Linear Regression
#Dependent=Price
#Independent=carat
datasetReg<-subset(datasetAnn, select = c(4,7))
set.seed(123)

#Split the data into training and test set
split<-sample.split(datasetReg$price, SplitRatio = 0.8)
training_set2<-subset(datasetReg, split==TRUE)
test_set2<-subset(datasetReg, split==FALSE)


#Scaling
training_set2[,1:2]<-scale(training_set2[,1:2])
test_set2[,1:2]<-scale(test_set2[, 1:2])

#Build Model
regressor2<- lm(formula=price~carat, data=training_set2)
summary(regressor2)

#predict on test data
cat("\014")
y_pred<-predict(regressor2, newdata = test_set2)
y_pred

#Plot regression
#Training Data
r1<-ggplot(training_set2, aes(x=carat, y=price))+
  geom_point()+geom_smooth(method="lm", se=FALSE)+
  labs(title="TrainingData-Carat v/s Price")+
  labs("Carat")+
  labs("Price")

#Test Data
r2<-ggplot()+
  geom_point(aes(x=test_set2$carat, y=test_set2$price), color="red")+geom_line(aes(x=training_set2$carat, y=predict(regressor2, newdata = training_set2)))+
  labs(title="TestData-Carat v/s Price")+
  labs("Carat")+
  labs("Price")

grid.arrange(r1, r2, nrow=2, ncol=1)