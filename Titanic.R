#load the tidyr and dplyr packages
library(tidyr)
library(dplyr)
library(stringr)

remove(titanic_original)

#replace missing values with string "S"
titanic_original$embarked <- str_replace_na(titanic_original$embarked, replacement = "S")

#replace missing age with mean age
mean(titanic_original$age)
titanic_original$age = ifelse(is.na(titanic_original$age), mean(titanic_original$age, na.rm = TRUE), titanic_original$age)

#creating a new column has_cabin_number
titanic_original <- titanic_original %>% 
  mutate("has_cabin_number" = ifelse(str_detect(titanic_original$cabin, 'NA'), 0, 1))

titanic_original

#export the cleaned up data to a csv file
library(xlsx)
write.csv(titanic_original, file = "titanic_clean.csv")
