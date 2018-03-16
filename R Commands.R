# The social data has been created for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
facebook <- c(17, 7, 5, 16, 8, 13, 14)

views_vector <- c(linkedin, facebook)

names(views_vector) <- c("linkedin", "facebook")

views <- matrix(c(linkedin, facebook), nrow = 2, byrow = TRUE)



linkedin <- c(16, 9, 13, 5, 2, 17, 14)
last <- tail(linkedin, 1) #last element of the vector linkedin - last day's number of linkedin views

#paste command and for loop
# define the double for loop

#for matrix we use nrow and ncol.. for lists and vector, we use 1:length()

i <- 1
j <- 1

for (i in 1:nrow(ttt)) {
  for (j in 1:ncol(ttt)) {
    x <- ttt[i,j]
    print(paste("On row", i, "and column", j, "the board contains", x))
    j <- j + 1
  }
  i <- i + 1
}


#print and paste
speed <- 31
print(paste("Your speed is", speed))

Rseek.org
Quick-R subsetting data
RStudio
