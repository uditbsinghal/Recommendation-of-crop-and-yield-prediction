#part 1
cat("Select City Number:\n\n1 AHMEDNAGAR\n2 AKOLA\n3 AMRAVATI\n4 AURANGABAD\n5 BEED\n6 BHANDARA\n7 BULDHANA\n8 CHANDRAPUR\n9 DHULE\n10 GADCHIROLI\n11 GONDIA\n12 HINGOLI\n13 JALGAON\n14 JALNA\n15 KOLHAPUR\n16 LATUR\n17 NAGPUR\n18 NANDED\n19 NANDURBAR\n20 NASHIK\n21 OSMANABAD\n22 PARBHANI\n23 PUNE\n24 RAIGAD\n25 RATNAGIRI\n26 SANGLI\n27 SATARA\n28 SINDHUDURG\n29 SOLAPUR\n30 THANE\n31 WARDHA\n32 WASHIM\n33 YAVATMAL\n34 MUMBAI\n\nChoose Corresponding Number to Your City from the Given List:\n")

readinteger <- function()
{ 
  n <- readline(prompt="Enter City Number: ")
  n <- as.integer(n)
  if (is.na(n)){
    n <- readinteger()
  }
  return(n)
}
y <- readinteger()

#part 2
cityname <- switch(y,"AHMEDNAGAR","AKOLA","AMRAVATI","AURANGABAD","BEED","BHANDARA","BULDHANA","CHANDRAPUR","DHULE","GADCHIROLI","GONDIA","HINGOLI","JALGAON","JALNA","KOLHAPUR","LATUR","NAGPUR","NANDED","NANDURBAR","NASHIK","OSMANABAD","PARBHANI","PUNE","RAIGAD","RATNAGIRI","SANGLI","SATARA","SINDHUDURG","SOLAPUR","THANE","WARDHA","WASHIM","YAVATMAL","MUMBAI")
print(cityname)

data1 <- read.csv("C:/Users/Lenovo/Desktop/Mini_Project/District_ph.csv")
data2 <- read.csv("C:/Users/Lenovo/Desktop/Mini_Project/Crop_ph.csv")

subval1 <- subset(data1, data1$City == cityname, select = c("City","pH_min","pH_max"))
print(subval1)

#part 3
#install.packages('e1071')
#install.packages('dplyr')

result1 <- subset(data2,data2$pH_Req <= subval1$pH_min & data2$pH_max_Range >= subval1$pH_max, select = c("Crop"))

par(mfrow=c(2, 3))

boxplot(data1$pH_max, main="Max_pH", sub=paste("Outlier rows: ", boxplot.stats(data1$pH_max)$out)) 
boxplot(data1$pH_min, main="Min_pH", sub=paste("Outlier rows: ", boxplot.stats(data1$pH_min)$out)) 
boxplot(data2$pH_Req, main="Req_pH", sub=paste("Outlier rows: ", boxplot.stats(data2$pH_Req)$out)) 

boxplot(data2$pH_max_Range, main="Max_pH_Range", sub=paste("Outlier rows: ", boxplot.stats(data2$pH_max_Range)$out)) 
boxplot(data2$pH_min_Range, main="Min_pH_Range", sub=paste("Outlier rows: ", boxplot.stats(data2$pH_min_Range)$out)) 

#part 4
library(e1071)
par(mfrow=c(1, 2))  
plot(density(data1$pH_min), main="Density Plot: City_pH_min", ylab="Plot", sub=paste("Skewness:", round(e1071::skewness(data1$pH_min), 2)))  
polygon(density(data1$pH_min), col="red")
plot(density(data2$pH_Req), main="Density Plot: Crop_pH_Req", ylab="Plot", sub=paste("Skewness:", round(e1071::skewness(data2$pH_Req), 2)))  
polygon(density(data2$pH_Req), col="red")
print(result1)

#part 5
rainfallavailabledata <- read.csv("C:/Users/Lenovo/Desktop/Mini_Project/MaharashtrastateRainfall.csv")
head(rainfallavailabledata)
subvalrainfall <- subset(rainfallavailabledata, rainfallavailabledata$District == cityname, select = c("State","District","Year","January","February","March","April","May","June","July","August","September","October","November","December","Annual_Total"))
convertednumbers <- as.numeric(as.vector(subvalrainfall$Annual_Total))
totalrainfall = mean(convertednumbers,na.rm=TRUE)
avgrainfall = totalrainfall/12
print(avgrainfall)
par(mfrow=c(1, 1))  
plot(density(as.numeric(subvalrainfall$Annual_Total)), main="Density Plot: City_Annual_Rainfall", ylab="Plot", sub=paste("Skewness:", round(e1071::skewness(as.numeric(subvalrainfall$Annual_Total)), 2)))  
polygon(density(as.numeric(subvalrainfall$Annual_Total)), col="red")

#part 6
croprainfalltempdata <- read.csv("C:/Users/Lenovo/Desktop/Mini_Project/CropRequiredTemperature.csv")
head(croprainfalltempdata)
result2 <- subset(croprainfalltempdata, (croprainfalltempdata$min_rainfall<=avgrainfall & croprainfalltempdata$max_rainfall>=avgrainfall), select = c("Crop"))
par(mfrow=c(1, 2))
plot(density(as.numeric(croprainfalltempdata$max_rainfall)), main="Density Plot: Crop_Max_Rainfall", ylab="Plot", sub=paste("Skewness:", round(e1071::skewness(as.numeric(croprainfalltempdata$max_rainfall)), 2)))  
polygon(density(as.numeric(croprainfalltempdata$max_rainfall)), col="red")
plot(density(as.numeric(croprainfalltempdata$min_rainfall)), main="Density Plot: Crop_Min_Rainfall", ylab="Plot", sub=paste("Skewness:", round(e1071::skewness(as.numeric(croprainfalltempdata$min_rainfall)), 2)))  
polygon(density(as.numeric(croprainfalltempdata$min_rainfall)), col="red")
print(result2)

#part 7
areaproductiondata <- read.csv("C:/Users/Lenovo/Desktop/Mini_Project/CropData.csv")
head(areaproductiondata)
subval <- subset(areaproductiondata, areaproductiondata$District_Name == cityname, select = c("State_Name","District_Name","Crop_Year","Season","Crop","Area","Production"))
print(subval)
par(mfrow=c(1, 1))  
plot((areaproductiondata$Area/1000)[areaproductiondata$District_Name==cityname],(areaproductiondata$Production/1000)[areaproductiondata$District_Name==cityname],main = "Area vs Production")
#abline(lm(subval$Area~subval$Production))

#part 8
plot((areaproductiondata$Crop_Year)[areaproductiondata$District_Name==cityname],(areaproductiondata$Production/1000)[areaproductiondata$District_Name==cityname],main = "year vs Production")

#part 9
max((subval$Production)/(subval$Area),na.rm=TRUE) 
expectedcropname <- subval[which.max(subval$Production/subval$Area),]
print(expectedcropname)

#part10
avgcrop<- mean((subval$Production)/(subval$Area),na.rm=TRUE)
print(avgcrop)
result3 <- subset(subval, (subval$Production/subval$Area)>avgcrop, select = c("Crop"))
print(unique(result3))

#part 11
unusefulcrop <- subval[which.min(subval$Production/subval$Area),]
print(unusefulcrop)

#part12
library(dplyr)
cat("Crop Predicted By Considering PH Value and Rainfall Factors:\t")
print(unique(intersect(as.vector(result2), as.vector(result1))))

cat("Crop Predicted By Considering Rainfall and Area-Production Factors:\t")
print(unique(intersect(as.vector(result2), as.vector(result3))))

cat("Crop Predicted By Considering PH Value and Area-Production Factors:\t")
print(unique(intersect(as.vector(result1), as.vector(result3))))

cat("Crop Predicted By Considering All Three Factors Factors:\t")
rs12<-unique(intersect(as.vector(result2), as.vector(result1)))
rs3<-as.vector(result3)
print(unique(intersect(rs12,rs3)))