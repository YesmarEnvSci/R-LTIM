## Know Your Data (KYD)
##Bern Romey

lidardata <- read.csv("BA_lidarData.csv")

str(lidardata)
names(lidardata)
summary(lidardata)

ResponseV <- lidardata$BAtotal
plot(lidardata$HMean,ResponseV)
cor(lidardata$HMean, lidardata$BAtotal)
corrMat <- cor(lidardata[,-1])
print(round(corrMat,2))
range(lidardata$HMax)
hist(lidardata$BAtotal, freq = F)
var(lidardata$BAtotal)
sd(lidardata$BAtotal)

