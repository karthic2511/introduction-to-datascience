#load the necessary tidyr and dplyr packages
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)


#read the dataset and replace any blank values in all columns with NA
Womensclothingreviewsdata = read.csv("Womens Clothing E-Commerce Reviews_clean.csv", header=TRUE)


#constructing a ggplot to check the recommendations across multiple departments
#in "General" Division
ggplot(subset(Womensclothingreviewsdata, Division.Name %in% c("General")),
       aes(x=Recommendation,
           y=Clothing.ID,
           color=Recommendation))+
  geom_point() +
  geom_jitter() +
  facet_grid(. ~ Department.Name)

#Comparing Age Vs Clothes purchased
ggplot(Womensclothingreviewsdata,
       aes(y = Clothing.ID, x = Age)) +
  geom_point()



#Comparing Age Vs Department
ggplot(Womensclothingreviewsdata,
       aes(y = Department.Name, x = Age)) +
  geom_point()

#Comparing Age Vs Rating
ggplot(Womensclothingreviewsdata,
       aes(y = Age, x = Rating)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = F)


#Predicting Ratings against Age using Linear Regression
Womensclothingreviewsdata$pred.Rating <- predict(lm(Rating ~ Age, data = Womensclothingreviewsdata))

ggplot(Womensclothingreviewsdata,
       aes(y = Age, x = pred.Rating)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = F)

##Comparing Division Vs Rating
ggplot(Womensclothingreviewsdata,
       aes(x = Division.Name, y = Rating)) +
  geom_jitter() +
  stat_smooth(method = "lm", se = F) 
  #facet_grid(.~Department.Name)

  #geom_line()