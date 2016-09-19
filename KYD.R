## Know Your Data (KYD) - For Tree Inventory Model Analysis
##Bern Romey

lidarData <- read.csv("BA_lidarData.csv")

str(lidarData)
names(lidarData)
summary(lidarData)

ResponseV <- lidarData$BAtotal
plot(lidarData$HMean,ResponseV)
cor(lidarData$HMean, lidarData$BAtotal)
corrMat <- cor(lidarData[,-1])
print(round(corrMat,2))
range(lidarData$HMax)
hist(lidarData$BAtotal, freq = F)
var(lidarData$BAtotal)
sd(lidarData$BAtotal)

source('cor.matrix.r')
cor.matrix(lidarData)
boxplot(lidarData[,-1])
