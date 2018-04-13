#load the necessary tidyr and dplyr packages
library(tidyr)
library(dplyr)
library(stringr)

#read the dataset and replace any blank values in all columns with NA
Womensclothingreviewsdata = read.csv("Womens Clothing E-Commerce Reviews.csv", header = TRUE, na.strings=c(""," ","NA"))

#replace missing age with mean age
mean(Womensclothingreviewsdata$Age)
Womensclothingreviewsdata$Age = ifelse(is.na(Womensclothingreviewsdata$Age), mean(Womensclothingreviewsdata$Age, na.rm = TRUE), Womensclothingreviewsdata$Age)

#change the column "Recommended IND" to "Recommendation"
colnames(Womensclothingreviewsdata)[6] <- "Recommendation"
colnames(Womensclothingreviewsdata)[6]

#replace recommended ind from 1 or 0 as YES or NO
Womensclothingreviewsdata$Recommendation <- replace(x = c("0", "1"), values = c("NO", "YES"))

#write the cleaned data into a new .csv file
write.csv(Womensclothingreviewsdata, file = "Womens Clothing E-Commerce Reviews_clean.csv")

