ds <- read.csv("F:/7th Semester/DS/R/titanic_data.csv") #external csv file ko import karny ke liye read.csv ke andar path dengy as an argument

is.na(ds)
any(is.na(ds))
colSums(is.na(ds))
na.omit(ds)
complete.cases(ds)
df <- airquality
str(df)
df
is.na(df)
View(ds)
