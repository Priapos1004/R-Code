Titanic <- read.csv("data/Titanic.csv",na.strings=".")
## Define Sex as factor (0 = Female, 1 = Male)
Titanic$Sex <- factor(Titanic$Sex,levels=c(0,1),labels=c("Female","Male"))

# sex distribution absolutes
table(Titanic$Sex,Titanic$Survived)

# sex distribution percentage
table(Titanic$Sex,Titanic$Survived)/nrow(Titanic)

# proportion of survivants
table(Titanic$Survived)/nrow(Titanic)

tableTitanic <- table(Titanic$Sex,Titanic$Survived)/nrow(Titanic)
addmargins(tableTitanic) # add sum row/column

# get percentage from absolutes
prop.table(table(Titanic$Sex,Titanic$Survived))

summary(table(Titanic$Sex,Titanic$Survived))

# mosaic
plot(tableTitanic,main="Titanic",ylab="Survived",xlab="Sex",col=rainbow(2))

a <- as.table(HairEyeColor[,,"Male"]) # Extract table for Male category
plot(a,main="Hair and eye colour (Male)",ylab="Eye",xlab="Hair",col=rainbow(4))

b <- as.table(HairEyeColor[,,"Female"]) # Extract table for Female category
plot(b,main="Hair and eye colour (Female)",ylab="Eye",xlab="Hair",col=rainbow(4))


# bar plot
barplot(t(a),main="Hair and eye colour (Male)",col=rainbow(4),beside=T,xlab="Hair color",legend=T,args.legend=list(x="topleft",bty="n"))

# get absolutes for all combinations
ftable(HairEyeColor)

# load personal.csv
personal_df <- read.csv("data/Personal.csv",na.strings=".")
# flat the table
ftable(personal_df, col.vars = "Sex")

# load airquality dataset (with missing data)
air=airquality[,1:4]
colMeans(air) # if missing data -> whole column NA
colMeans(air,na.rm=T)
apply(air, 2, var, na.rm=T) # calculate variance
cor_mat <- cor(air, use = "pairwise.complete.obs")
cor(air, use="na.or.complete") - cor(na.omit(air)) # same way (better to use)
cov_mat <- cov(air, use = "na.or.complete")

# scatter plot
plot(air$Temp,air$Wind,ylab="Wind",xlab="Temperature (°F)")

# scatter plot plus Loess curve
scatter.smooth(air$Temp,air$Wind,ylab="Wind",xlab="Temperature (°F)",lpars=list(col="red"))

# plot all
plot(air) #same as pairs()

# install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(air$Temp,air$Wind,air$Solar.R,color="blue")

plot(iris)
scatterplot3d(iris$Sepal.Length,iris$Sepal.Width,iris$Petal.Length,color="blue")


