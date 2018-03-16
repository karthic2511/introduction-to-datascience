#load the tidyr and dplyr packages
library(tidyr)
library(dplyr)
library(stringr)

remove(refine_original)

# print the data frame refine_original from the imported dataset
refine_original

# output first 6 rows of all columns of the DF
head(refine_original)

#standardizing the case to all lowercase 
refine_original <- mutate_all(refine_original, funs(tolower))

#separate product code and product number into 2 columns
refine_original <- separate(refine_original, `Product code / number`, c("Product code", "Product number"), sep = "-")

#add product categories
refine_original <-  refine_original %>% 
                    mutate(`Product category` = str_replace_all(`Product code`, c("p" = "Smartphone", "v" = "TV", "x" = "laptop", "q" = "tablet"))) %>%
                        select(company, `Product code`, `Product number`, `Product category`, address, city, country, name ) 

#concatenates the 3 address fields - address, city and country into full_address
refine_original <- unite(refine_original, "full_address", address, city, country, sep = ",")

#creating the dummy binary variables for company
refine_original <- refine_original %>%
  mutate("company_philips" = ifelse(str_detect(refine_original$company, 'phil|phl'), 1, 0), 
         "company_azko" = ifelse(str_detect(refine_original$company, 'ak'), 1, 0),
         "company_van_houten" = ifelse(str_detect(refine_original$company, 'van'), 1, 0),
         "company_unilever" = ifelse(str_detect(refine_original$company, 'uni'), 1, 0))

#print refine_original
refine_original

#export the cleaned up data to a csv file
library(xlsx)
write.csv(refine_original, file = "refine_clean.csv")
#write.xlsx(refine_original, "Users/kvenk5/Downloads/refine_clean.xlsx")
