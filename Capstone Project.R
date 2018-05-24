#load the necessary tidyr and dplyr packages
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)

#read the dataset and replace any blank values in all columns with NA
Womensclothingreviewsdata = read.csv("Womens Clothing E-Commerce Reviews_clean.csv", header=TRUE)

#see the structure of the data
str(Womensclothingreviewsdata)

#Age distribution - Histogram plot
#majority of the age groups are between 25 to 50
hist(Womensclothingreviewsdata$Age, breaks=25, col="blue", freq = TRUE, xlab = "Age", main = "Age Distribution", labels = TRUE)

#Rating distribution - Histogram plot
#Majority of the consumers gave good rating - 4 or 5
hist(Womensclothingreviewsdata$Rating, breaks=12, col="red", xlab = "Age", main = "Rating Distribution")

#Department distribution
# Bar Plot - To count the Departments
department_counts <- table(Womensclothingreviewsdata$Department.Name)

barplot(department_counts, main="Dept Distribution", 
        xlab="Departments", ylab="Count", col=c("darkblue","red", "green", "brown", "black", "pink"),
        legend = rownames(counts), axes = TRUE, beside = TRUE, legend.text = TRUE, 
        args.legend = list(x = "topright", bty = "n", inset=c(-0.15, 0)))

#Division distribution
# Bar Plot - To count the Division
# Bar Plot - To compare the Recommendations across Divisions
#division_counts <- table(Womensclothingreviewsdata$Division.Name, Womensclothingreviewsdata$Recommendation)
division_counts <- table(Womensclothingreviewsdata$Division.Name)

barplot(division_counts, main="Division Distribution", 
        xlab="Division", ylab="Count", col=c("darkblue","red", "green"),
        legend = rownames(counts), axes = TRUE, beside = TRUE, legend.text = TRUE)

# Bar Plot - To compare the Recommendations across Divisions
division_recommend_counts <- table(Womensclothingreviewsdata$Division.Name, Womensclothingreviewsdata$Recommendation)

barplot(division_recommend_counts, main="Division Distribution", 
        xlab="Recommendation", ylab="Count", col=c("darkblue","red", "green"),
        legend = rownames(counts), axes = TRUE, beside = TRUE, legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", inset=c(-0.25, 0)))


# Bar Plot - To compare the Recommendations across Departments
department_recommend_counts <- table(Womensclothingreviewsdata$Department.Name, Womensclothingreviewsdata$Recommendation)

barplot(department_recommend_counts, main="Dept Distribution", 
        xlab="Recommendation", ylab="Count", col=c("darkblue","red", "green", "brown", "black", "pink"),
        legend = rownames(counts), axes = TRUE, beside = TRUE, legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", inset=c(0.10, -0.15)))


# Kernel Density Plot
d <- density(Womensclothingreviewsdata$Age) # returns the density data 
plot(d, main="Kernel Density of Age")
polygon(d, col="red", border="blue")

# Kernel Density Plot
d <- density(Womensclothingreviewsdata$Rating) # returns the density data 
plot(d, main="Kernel Density of Ratings")
polygon(d, col="red", border="blue")


#scatter plot - Age vs clothing id
#Majority of clothing IDs bought were between 800 and 1200 
attach(Womensclothingreviewsdata)
plot(Age, Clothing.ID, main="Scatterplot", 
     xlab="Age", ylab="Clothes bought", type = "p")


#constructing a ggplot to check the recommendations across multiple departments
#in "General" Division
ggplot(subset(Womensclothingreviewsdata, Division.Name %in% c("General")),
       aes(x=Recommendation,
           y=Clothing.ID,
           color=Recommendation))+
  geom_point() +
  geom_jitter() +
  facet_grid(. ~ Department.Name)

#in "General Petite" Division
ggplot(subset(Womensclothingreviewsdata, Division.Name %in% c("General Petite")),
       aes(x=Recommendation,
           y=Clothing.ID,
           color=Recommendation))+
  geom_point() +
  geom_jitter() +
  facet_grid(. ~ Department.Name)

#in "Initmates" Division
ggplot(subset(Womensclothingreviewsdata, Division.Name %in% c("Initmates")),
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

#Comparing Age Vs Rating purchased
ggplot(Womensclothingreviewsdata,
       aes(y = Rating, x = Age)) +
  #geom_point()
  geom_jitter(alpha = 0.2, shape = 1)

#Comparing Age Vs Recommendation purchased
ggplot(Womensclothingreviewsdata,
       aes(y = Recommendation, x = Age)) +
  #geom_point()
  geom_jitter(alpha = 0.2, shape = 1)

#Comparing Division Vs Department purchased
ggplot(Womensclothingreviewsdata,
       aes(y = Department.Name, x = Division.Name)) +
  #geom_point()
  geom_jitter(alpha = 0.2, shape = 1)

#axis(side = 2, at = seq(0,1500, by=100))
#box()


# Boxplot of Age vs Rating
boxplot(Age~Rating, data = Womensclothingreviewsdata)

#Boxplot of Clothing ID vs Rating
boxplot(Clothing.ID~Rating, data = Womensclothingreviewsdata)



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

####################################
#Constructing a box plot for Ratings Vs Review Text length
ReviewTextLength <- c(Womensclothingreviewsdata$Review.Text)
ReviewTextLength

boxplot(ReviewTextLength~Rating, data = Womensclothingreviewsdata, col=c("blue", "green", "red", "pink", "brown"))



ggplot(Womensclothingreviewsdata, aes(x = Age)) +
  geom_histogram(stat = "bin", binwidth = 1, aes(x = Age, y = ..density..), fill = "#377EB8")

ggplot(Womensclothingreviewsdata, aes(x = Age, fill = Clothing.ID)) +
  geom_bar(position = "dodge")


  #geom_line()