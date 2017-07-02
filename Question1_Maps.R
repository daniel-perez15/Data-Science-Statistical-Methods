#Use R to make three maps of the states in the USA. The first map should plot
#state level income share of the top 1% of income earners in 2012. The second map
#should plot the same variable but for 1999. The third map should plot the
#2012-­‐1999 difference of the variable. Visit the website:
#http://www.shsu.edu/eco_mwf/inequality.html to download the data and learn about
#the variable of interest. Read the handout about map making in R for a prototype
#example. Simply replace “India” with “USA” in the getData function in the
#handout to get a shape file of the states in the US.

library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # for legends
library(maps)  # for importing 


usa.df <- map_data("state") # provides data frame for states of US
str(usa.df) # to read the structure for the data frame created above
colnames(usa.df) [5] <- "state" #renaming the column name in data frame from region to state
usa.df$state <- as.factor(usa.df$state) # coerces state to a factor
str(usa.df) # displays state argument as factor with levels now

USA.dat <- read.csv(file.choose(), header = T, sep = ",") #reading data from a csv file
USA.dat$State <- tolower(USA.dat$State)
USA.year2012 <- subset(USA.dat,USA.dat$Year==2012,select=c(State,Top1_adj))
USA.year1999 <- subset(USA.dat,USA.dat$Year==1999,select=c(State,Top1_adj))
colnames(USA.year2012)[2]<-"Top1_2012"
colnames(USA.year2012)[1]<-"state"
colnames(USA.year1999)[2]<-"Top1_1999"
colnames(USA.year1999)[1]<-"state"


difference_data <- USA.year2012$Top1_2012 - USA.year1999$Top1_1999
diff_df <- data.frame("state" = USA.year1999$state, "difference"=difference_data)
usa.df1<- join(usa.df,USA.year2012,by = "state",type= "inner")
usa.df2 <- join(usa.df,USA.year1999,by="state",type="inner")
usa.df3<-join(usa.df,diff_df,by="state",type="inner")
brks <- c(5,10,15,20,25,30) 

# remove alaska and hawaii
state_abr = state.abb[state.abb!="AK" & state.abb!="HI"]
state_center_long = state.center$x[-c(2,11)]
state_center_lat = state.center$y[-c(2,11)]

p <- ggplot() +
  
  geom_polygon(data = usa.df1, aes(x = long, y = lat, group = group, fill = Top1_2012), 
               color = "black", size = 0.25) + 
  geom_text(aes(x=state_center_long,y=state_center_lat,label=state_abr)) +
  scale_fill_distiller(palette = "Reds", breaks = brks, trans = "reverse") +
  theme_nothing(legend = TRUE) +
  labs(title = "Top  1% Earners in states of US in  2012", fill = "") #plots a map of US with state names and color codes
#each state name with the value of top 1% earners in 2012

q <- ggplot() +
  
  geom_polygon(data = usa.df2, aes(x = long, y = lat, group = group, fill = Top1_1999), 
               color = "black", size = 0.25) + 
  geom_text(aes(x=state_center_long,y=state_center_lat,label=state_abr)) +
  scale_fill_distiller(palette = "Reds", breaks = brks, trans = "reverse") +
  theme_nothing(legend = TRUE) +
  labs(title = "Top  1% Earners in states of US in 1999", fill = "") #plots a map of US with state names and color codes

r <- ggplot() +
  
  geom_polygon(data = usa.df3, aes(x = long, y = lat, group = group, fill = difference), 
               color = "black", size = 0.25) + 
  geom_text(aes(x=state_center_long,y=state_center_lat,label=state_abr)) +
  scale_fill_distiller(palette = "Reds", breaks = brks, trans = "reverse") +
  theme_nothing(legend = TRUE) +  
  labs(title = "Difference between 2012 and 1999 top 1% earners", fill = "") #plots a map of US with state names and color codes

ggsave(p,filename = "2012_auto_code.pdf")
ggsave(q,filename="1999_auto_code.pdf")
ggsave(r,filename = "Difference_auto_code.pdf")
