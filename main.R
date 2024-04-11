install.packages("tidyverse")
library(tidyverse)

#install the packages and open the data
Student_Behaviour <- read.csv("C:/Projects/R-Project/Student_Behaviour.csv")
str(Student_Behaviour)    

#we wanted to check the corelation between the student mark in the 10th and college marks
cor(Student_Behaviour$X10th.Mark , Student_Behaviour$college.mark)

# B:

#create an empty vector
random1 <- rep(NA,14000)
#calculate how many is 28% of the rows,and use "round" to get a whole number of rows
samplerow <- (28/100)*235
samplerows1 <- round(samplerow)
#set an opening number for the sample algorithm 
set.seed(1)
#using sample_n function for sample from a data frame
for(i in 1:14000){
  samplestudent <-sample_n(Student_Behaviour,samplerows1,replace = FALSE)
  random1[i] <- cor(samplestudent$X10th.Mark , samplestudent$college.mark)
}
#
a <- mean(random1)
b <- sd(random1)
plot(random1)


#calculate the mean of on factor and then make a categorial factor based on it
mean10th <- mean(samplestudent$X10th.Mark)
samplestudent$categorial_10marks <- ifelse(test = samplestudent$X10th.Mark<mean10th,
                                           yes = "low",
                                           no = "high")
view(samplestudent)
#download ggplot2
library(ggplot2)
#make a ggplot of the 3 categories
ggplot(samplestudent, aes( x = college.mark, y = X10th.Mark,  color = categorial_10marks))+
  geom_point()

#
table(samplestudent$categorial_10marks)
(sum(samplestudent$categorial_10marks =="low")/length(samplestudent$categorial_10marks))*100

#
meancollege <- mean(samplestudent$college.mark)
samplestudent$categorial_collegemarks <- ifelse(test = samplestudent$college.mark<meancollege,
                                                yes = "low",
                                                no = "high")
view(samplestudent)
frequency <- table(samplestudent$categorial_collegemarks, samplestudent$categorial_10marks)
colnames(frequency)[1] <- "college marks"
view(frequency)
