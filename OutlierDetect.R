#Importing dataset
sales_data<- read.csv("/Users/abdullahsaka/Desktop/sales_data.csv",header=TRUE,sep=";")
head(sales_data)
sum(is.na(sales_data$Sales))

#Preprocessing(find blank values and fulfill these rows)
index_NA=which(is.na(sales_data$Sales))
index_NA=array(index_NA)
outlier_week=sales_data$Week[c(index_NA)]
outlier_week=array(outlier_week)
for (i in 1:length(index_NA))  {
  sales_data[index_NA[i],3] = mean(sales_data[sales_data$Week==outlier_week[i],3],na.rm=TRUE)
}

#Twitter Anomaly Detection
install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(devtools)
library(AnomalyDetection)
anoms1 <- AnomalyDetectionVec(sales_data$Sales, max_anoms=0.1,period=4, plot = TRUE)
anoms1
anoms1$plot
write.csv(anoms1$anoms,file = "outliers.csv",row.names=FALSE)


# Box-Whisker Method
ggplot(data = sales_data, mapping = aes(x = " ", y = Sales)) +
  geom_boxplot(color="darkblue") +
  stat_summary(geom = "text", label = "----", size = 10, color = "red")  +
  ggtitle(expression(atop("Boxplot of Tea Sales"))) +
  theme(plot.title = element_text(hjust = 0.5))
third<- quantile(sales_data$Sales,0.75)
maxval<- third+1.5*(sd(sales_data$Sales))
maxval<- as.integer(maxval)
anoms2<-sales_data[(sales_data$Sales>maxval)==TRUE,3:4]
anoms2$Weeknum<- NULL
anoms2

# Z score Method
outliersZ<-function(data, zCutOff = 1.96, replace = NA, values = FALSE, digits = 2) {
  stdev <- sqrt(sum((data - mean(data, na.rm = T))^2, na.rm = T) / sum(!is.na(data)))
  absZ <- abs(data - mean(data, na.rm = T)) / stdev
  data[absZ > zCutOff] <- replace 
  if (values == TRUE) {
    return(round(absZ, digits)) 
  } else {
    return(round(data, digits)) 
  }
}
wthoutlier<-outliersZ(sales_data$Sales)
wthoutlier<-as.data.frame(wthoutlier)
sum(is.na(wthoutlier))
index_outlierNA=which(is.na(wthoutlier))
index_outlierNA=array(index_outlierNA)
outlier_sales=sales_data$Sales[c(index_outlierNA)]
outlier_sales=array(outlier_sales)
anoms3<-cbind(index_outlierNA,outlier_sales)
anoms3<-as.data.frame(anoms3)
anoms3

# MAD Method
outliersMAD <- function(data, MADCutOff = 2.5, replace = NA, values = FALSE, bConstant = 1.4826, digits = 2) {
  absMADAway <- abs((data - median(data, na.rm = T))/mad(data, constant = bConstant, na.rm = T))
  data[absMADAway > MADCutOff] <- replace
  
  if (values == TRUE) { 
    return(round(absMADAway, digits)) #if values == TRUE, return number of mads for each value
  } else {
    return(round(data, digits)) #otherwise, return values with outliers replaced
  }
}
wthoutliers<-outliersMAD(sales_data$Sales)
wthoutliers<-as.data.frame(wthoutliers)
sum(is.na(wthoutliers))
index_outliersNA=which(is.na(wthoutliers))
index_outliersNA=array(index_outliersNA)
outlier_data=sales_data$Sales[c(index_outliersNA)]
outlier_data=array(outlier_data)
anoms4<-cbind(index_outliersNA,outlier_data)
anoms4<-as.data.frame(anoms4)
anoms4