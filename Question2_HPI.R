#Read about the Happy Planet Index on Wikipedia:
#https://en.wikipedia.org/wiki/Happy_Planet_Index Be sure to read the methodology
#behind the index and its criticism. Download the data for 2012 from
#http://www.happyplanetindex.org/data/. 
#Examine the distribution of the HPI
#variable graphically. What would be appropriate measures of center and spread of
#this distribution -­‐-­‐-­‐ (mean, SD) or (median, IQR). Justify your answers.
#Make scatterplots of HPI against each of the three variables on which the
#index is best. Comment on what you see. Will it be appropriate to use
#correlation to summarize the relationship of HPI with the other three variables?
#If yes, provide

library(ggplot2)

#Read all the columns from CSV HPI(Happy Planet Index)file
data_hpi<- read.csv(file.choose(), header = T) 
#Fetch Summary data i.e max, min, 1st quartile, mean, median and 3rd quartile of HPI
summary(data_hpi[,8]) #HPI data present in 8th column of the data file
#Find interquartile range of HPI
hpi_iqr=IQR(data_hpi[,8])
#print interquartile range of HPI
print(hpi_iqr)
#Find standard deviation of HPI
hpi_sd=sd(data_hpi[,8])
#Print standard deviation of HPI
print(hpi_sd)
#Draw boxplot of HPI data
boxplot(data_hpi[,8],main=toupper("2012 Happy Planet Index"),
        ylab="Happy Planet Index",col='darkgrey')

p<-ggplot(data_hpi,aes(x=data_hpi[,8]))+ 
 geom_histogram(aes(y =..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(data_hpi[,8], na.rm=T)),   # Ignore NA values for mean
            color="darkblue", linetype="dashed", size=1)+
  geom_density(alpha=.2,fill="#FF6666")+  # Overlay with transparent density plot
  xlab("HPI")+ylab("Density")

ggsave(p,file="Underlying distribution histogram file.pdf")
 
#Draw scatterplot of HPI Vs Experienced Life Expectancy
plot(data_hpi[,8],data_hpi[,4],
     main="Scatterplot of HPI Vs Experienced Life Expectancy",
     xlab ="HPI",ylab = "Experienced Life Expectancy",col = "dark red")
#Draw scatterplot of HPI Vs Well Being
plot(data_hpi[,8],data_hpi[,5],
     main="Scatterplot of HPI Vs Well Being",
     xlab ="HPI",ylab = "Well Being",col = "dark red")
#Draw scatterplot of HPI Vs Ecological Foot Print
plot(data_hpi[,8],data_hpi[,7],
     main="Scatterplot of HPI Vs Ecological Foot Print",
     xlab ="HPI",ylab = "Ecological Foot Print",col = "dark red")
#Find Correlation HPI and Experienced Life Expectancy
print(cor(data_hpi[,8],data_hpi[,4]))
#Find Correlation HPI and Well Being
print(cor(data_hpi[,8],data_hpi[,5]))
#Find Correlation HPI and Ecological Foot Print
print(cor(data_hpi[,8],data_hpi[,7]))
  