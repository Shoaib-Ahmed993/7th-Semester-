 data(cars)
 str(cars)
 head(cars)
 plot(cars)


 x11() # new graphical window
 hist(cars$speed)

# Vectors and factors
# creating vectors using combine function
 gpa <- c(3.45, 2.79, 3.22, 2.98)

 # checking structure of gpa vector
 str(gpa)

 # accessing individual elements (like 3rd element)
 gpa[3]

 # accessing range of elements (1st to 3rd )
 gpa[1:3]

 # creating character vectors using combine
 fst_name <- c("ajmal" ,"zafar", "Saem" , "Jea")

 # checking structure of gpa vector
 str(fst_name)

 # accessing range of elements (1st to 3rd )
 gpa[1:3]

 
 # creating character vectors using combine
 fst_name <- c("ajmal","zafar","Saem","Jea")

 # checking structure of fst_name vector
 str(fst_name)

 # creating boolean vectors using combine
 pass <- c(TRUE, FALSE, TRUE, TRUE)

 # checking structure of pass vector
 str(pass)

 # Creating factor vector

 # creating gender
 gender <- c("M", "M" , "M" , "F" )

 # checking structure of pass vector
 str(gender)

 # gender is a character to convert it into factor
 gender <- factor(gender)

 # it overwrite the gender into factor

 # Creating factor vector

 # defining levels into a factor
 gender <- factor(gender, levels=c("M","F"))
 gender <- factor(gender, 
                  levels=c("M","F"),
                  labels = c("Male","Female"))
 
 # checking structure of gendere vector
 str(gender)

 # Creating a dataframe

 # to check how many vectors are defined in

 # memory using list objects
 ls()

 # creating a data frame
 df1 <- data.frame(fst_name, gender, gpa, pass)
 str(df1)

# Creating a dataframe
# to access individual elements of a data frame
# df1[row, col]

 # following command will show 2nd row 4th col
 df1[2,4]

 # following command will show entire 2nd row
 df1[2,]

 
 df1 <- data.frame(fst_name, gender, gpa, pass, stringsAsFactors = FALSE)
 str(df1)
 
 
 # Exploring Data  V9
 # find the current directory and copy your csv file into this folder
 # load the csv file using read.csv command
 
 df3 <- read.csv("usedcars.csv")
 
 #showing structure of usedcars.csv data
 str(df3)
 
 # showing first 6 rows of usedcars.csv data
 head(df3)
 
 # showing last 6 rows of usedcars.csv data
 tail(df3)
 
 # again read this file with one option
 df4 <- read.csv("usedcars.csv", stringsAsFactors = FALSE)
 
 # now check the strucure of this file and compare it with df3
 str(df4)
 
 # we can see that now model color and transmission variables are not factors now they are characters 
 # change transmission variable from chr to factor
 
 df4$transmission <- factor(df4$transmission)
 str(df4) 

 # transmission variable becomes factor
 # obtaining a subset of data frame
 # suppose we are interested in only the yellow cars
 # then use subset command as given below
 yellow_cars <- subset(df4, color %in% "Yellow")
 # same version yellow_cars <- subset(df4, color == "Yellow") 
 # we can see that only three yellow cars
 # we have to consider only those cars whose mileage is more than 100000
 
 high_mileage <- subset(df4, mileage > 100000)
 # there are only 7 such cars
 automatic_cars <- subset(df4, transmission %in% "AUTO")
 manual_cars <- subset(df4, transmission %in% "MANUAL")
 
head(df4$color)
df4$color[3] 
df4$color[3:5] 


table(df4$color)

# shows all rows and all colums it is similar as df4
df4[ , ]


# shows first rows and all colums it is similar as df4
df4[1, ]

# shows only price of first, third and  fifth car
df4[c(1,3,5) , 3 ]

# 3rd column name is price we can use column name instead of its serial number
df4[c(1,3,5) , "price" ]


# shows price and color columns  of first, third and  fifth car
df4[c(1,3,5) , c(3,5) ]

# shows price and color columns  of first, third and  fifth car
df4[c(1,3,5) , c("price", "color" )]

# shows except 4rd and 5th columns of first, third and  fifth car
df4[c(1,3,5) , -c(3,5) ]

# how many cars having automatic and manual transmission
table(df4$transmission)

# if we are willing to find proportion of each type of car
table(df4$transmission)/length(df4$transmission)

# making pie chart of transmission variable
pie(table(df4$transmission))


#bar chart of transmission variable
barplot(table(df4$transmission))

#unvariate sumaries
#mean of milage variable

hist((1/df4$mileage)     )
lgmil <- log(df4$mileage)


boxplot((df4$mileage))
boxplot(df4$mileage,horizontal = 
          TRUE)


plot(mileage ~ transmission, data = df4)
tapply(df4$mileage, df4$transmission, mean)
tapply(df4$mileage, df4$transmission, sd)
hist(df4$price)
boxplot(df4$price)
boxplot(df4$price,horizontal = TRUE)
plot(df4$mileage, df4$price)


#pch is the ploting character on the graph
#using triangles and circles for ploting depending transmission
plot(price ~ mileage, data=df4,  pch=as.integer(transmission))


#using color function in plot 
plot(price ~ mileage, data=df4,  pch=as.integer(transmission),col=as.integer(transmission)+2)
legend("topright",legend = c("AUTO","MANUAL"),pch=c(1,2),col=c(3,4))


# V-13 MULTIVARIATE SUMMARIES
# ploting scatter matrix
pairs(df4)
# this will shows error can anyone explain why?


pairs(df4[, -c(2,5,6)])






cor(df4[, -c(2,5,6)])

#finding mean price of cars seperately by transmission
aggregate(price ~ transmission, data=df4, FUN=mean)


#the same thing can be done by using tapply function
tapply(df4$price, df4$transmission, FUN=mean)

# aggregate can be used transmission and color of car basis 
aggregate(price ~ transmission + color, data=df4, FUN=mean)

#creating contingency table using transmission and color
# it is two way crosstabulation b/w two categorical variables
table(df4$transmission, df4$color)


# proportions can be obtained as below
table(df4$transmission, df4$color)/length(df4$transmission)


# proportions can be obtained as below in one decimal place using round function
round(table(df4$transmission, df4$color)/length(df4$transmission),1)


#creating box plot of prices using different colors
plot(price ~ color, data= df4)


#this command will not work b/c color variable is not factor 
# now we convert color variable into factor
df4$color <- factor(df4$color)
plot(price ~ color, data= df4)


## ordering a data frame by a column
df4[order(df4$color),]


