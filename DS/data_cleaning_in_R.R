# data cleaning in R
# firstly we are handling NA's 
# the data airquality data set is used for this purpose 
# airquality data set is found in Base R

df <- airquality
df
str(df)

# this data contains 153 observations of 6 variables
is.na(df)

# Now we are creating some NA delibrately in this data set
# change all values of last column to NA
sum(!is.na(df))
df[,7] <- c(NA) 

df[154,] <- c(NA)
str(df)
any(is.na(df))

is.na(df)

# removing column number 7 because it is full of NA's
df <- df[,-7]

str(df)

df <- df[-154,]
str(df)

any(is.na(df))

# how many total NA's are there
sum(is.na(df))


# now we check in which column we have how many na's
sum(is.na(df$Solar.R))


#instead of checking one by ony columumn for missing cases 
# we can use colSums function whcih give us sum in all columns
colSums(is.na(df))

#this shows that majority of missisng cases are in first column which is 37
# there are 7 missing cases in column 2 
# rest of the columns are full
# we can use na.omit function to remove all missing cases

df.clean <- na.omit(df)
sum(is.na(df.clean))
# na.omit and complete.cases are identical functions
# as most of the na's were in first colum which were 37
# if this column does not plays an important role in data analysis 
# then we can omit this column first then we will remove na
# this will enhance our sample size

df.clean2 <- na.omit(df[,-1])
nrow(df.clean2)

#df.clean contains 111 rows
# df.clean2 contain 146 rows


# we can implement a rule of keeping all columns in which na are less than 10
df.clean3 <- df[, colSums(is.na(df))<10]
nrow(df.clean3)

# if we are computing mean, median and standard deviation of variable having missin number 
# then the result will also be NA
mean(airquality$Solar.R)
median(airquality$Solar.R)
sd(airquality$Solar.R)

# All three results are NA's
mean(!is.na(airquality$Solar.R))
sd(!is.na(airquality$Solar.R))


## IMPUTATION BY MEAN AND MEDIAN
# instead of deleting missing rows we can impute them by mean or by median
df.meanImputed <- df
df.medianImputed <- df


df.meanImputed$Solar.R[is.na(df.meanImputed$Solar.R)] <-  mean(!is.na(df.meanImputed$Solar.R))

df.medianImputed$Solar.R[is.na(df.medianImputed$Solar.R)] <-  median(!is.na(df.medianImputed$Solar.R))
# now we check is there any na in solar.r of the two data frames
any(is.na(df.meanImputed$Solar.R))
any(is.na(df.medianImputed$Solar.R))


# Removing outliers from the data
str(df.clean2)
boxplot(df.clean2$Temp)


# No outlier in Temp variable
boxplot(df.clean2$Wind)


# There are three outliers in the Wind variable
summary(df.clean2$Wind)
Q1=quantile(df.clean2$Wind,0.25)
Q3=quantile(df.clean2$Wind,0.75)
IQR_wind=Q3-Q1 

# there is a direct function of IQR 
# IQR(variablename)
upFenceWind <- Q3 + 1.5 * IQR_wind
df.clean4 <- subset(df.clean2,Wind<=upFenceWind)

#Now we can check the box plot of Wind variable in clean4
boxplot(df.clean4)

# box plot of clean4 shows no outlier in any of the variable
boxplot(df.clean4$Wind)
# no other variable have outliers
# checking for duplicates 
str(df.clean4)

str(unique(df.clean4))
# we duplicate the 130 row at the 145 position 
df.clean4[145,]<- df.clean4[130,]
str(df.clean4)
df.clean4[c(130,145),] 

#Now using unique function we eliminate this row
df.clean4Distinct <- unique(df.clean4)
str(df.clean4Distinct)
hist(df.meanImputed$Wind)
hist(df.meanImputed$Temp)
# this histogram is showing slightly left skewed
# we can use a transformation to make it normal


# spliting data into training and validation sets
# 80% training and 20%testing data split
sample_data <- sample(2,nrow(df.clean4),replace = TRUE,prob = c(0.8,0.2))
test_data <- df.clean4[sample_data==1,]
train_data <- df.clean4[sample_data==2,]
head(test_data)
head(train_data)
str(test_data)
str(train_data)


# direct output to a file 
# sink("myfile", append=FALSE, split=FALSE)
# return output to the terminal 
# sink()
sink("myfile", append=FALSE, split=FALSE)
# proportions can be obtained as below
str(test_data)   # output routed to myfile
str(train_data)  # output routed to myfile  
sink()           # return output to screen

# we can store graphical output to any seperate file
# pdf("mygraph.pdf")	pdf file 
# win.metafile("mygraph.wmf")	windows metafile 
# png("mygraph.png")	png file 
# jpeg("mygraph.jpg")	jpeg file 
# bmp("mygraph.bmp")	bmp file 
# postscript("mygraph.ps")	postscript file
# to close the output use dev.off() function
# Saving output to pdf
pdf("myplot.pdf")
hist(df.meanImputed$Wind)
hist(df.meanImputed$Temp)
dev.off()


