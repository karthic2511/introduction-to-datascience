#load the necessary tidyr and dplyr packages
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)

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
  #geom_point()
  geom_jitter(alpha = 0.2, shape = 1)

#Comparing Age Vs Clothes purchased - Histogram plot
ggplot(Womensclothingreviewsdata, aes(x = Age)) +
  geom_histogram(stat = "bin", binwidth = 1, aes(x = Age, y = ..density..), fill = "#377EB8")

ggplot(Womensclothingreviewsdata, aes(x = Age, fill = Clothing.ID)) +
  geom_bar(position = "dodge")

posn_d <- position_dodge(width = 0.8)

ggplot(Womensclothingreviewsdata, aes(x = Age, fill = Clothing.ID)) +
  geom_bar(position = posn_d, alpha = 0.6)

#Comparing Age Vs Clothing.ID - Histogram plot
ggplot(Womensclothingreviewsdata, aes(x = Age, fill = Clothing.ID)) +
  geom_histogram(binwidth = 1, alpha = 0.8, position = "dodge", stat = "bin")


#Comparing Age Vs Recommendation - Histogram plot
ggplot(Womensclothingreviewsdata, aes(x = Age, fill = Recommendation)) +
  geom_bar(position = posn_d, alpha = 1)


ggplot(Womensclothingreviewsdata, aes(Age, fill = Recommendation)) +
  geom_histogram(binwidth = 1, alpha = 0.8, position = "stack", stat = "bin")

#Comparing Rating Vs Department.Name - Histogram plot
ggplot(Womensclothingreviewsdata, aes(Rating, fill = Department.Name)) +
  geom_histogram(binwidth = 1, alpha = 0.8, position = posn_d, stat = "bin")

# Definition of a set of blue colors
blues <- brewer.pal(11, "Blues")

# Make a color range using colorRampPalette() and the set of blues
blue_range <- colorRampPalette(blues)

# Use blue_range to adjust the color of the bars, use scale_fill_manual()
ggplot(Womensclothingreviewsdata, aes(x = Age, fill = Rating)) +
  geom_bar(position = "dodge") +
  #scale_fill_manual(values = blue_range(11))
  scale_fill_brewer()

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
Age.model <- predict(lm(Rating ~ Age, data = Womensclothingreviewsdata))

summary(Age.model)
#coef(summary(Age.model))
#anova(Age.model)

plot(Age.model)


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
