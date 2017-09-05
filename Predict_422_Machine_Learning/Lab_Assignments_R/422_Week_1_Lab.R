library(forecast)
library(datasets)
library(MASS)
library(ISLR)
library(ggplot2)
library(devtools)

x<-rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)

set.seed(1303)
rnorm(50)

set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",
       main="Plot of X vs Y")

jpeg("Figure.jpeg")
plot(x,y,col="green")
dev.off()

x=seq(1,10)
x

x=1:10
x

x=seq(-pi,pi,length=50)
x

#3-Dimensional/Heatmaps
y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour(x,y,f,nlevels=45,add=T)

fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)

image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20) 
persp(x,y,fa,theta=30,phi=70) 
persp(x,y,fa,theta=30,phi=40)


#Indexing Data
A=matrix(1:16,4,4)
A
A[2,3]
#combine values at intersection of row 1 and row 3, column 2 and 4
A[c(1,3),c(2,4)]
A[1:3,2:4]
#combine first two rows
A[1:2,]
#combine first two columns
A[,1:2]
A[1,]
#drop rows 1 and 3
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]

#Visualizations
plot(Auto$cylinders,Auto$mpg)
pairs(~Auto$mpg+Auto$displacement+Auto$horsepower+Auto$weight+Auto$acceleration)

plot(Auto$horsepower,Auto$mpg)
identify(Auto$horsepower,Auto$mpg,Auto$name)

##################
#Exercise 2.4
##################
#8)
write_csv(College,'college.csv')
college<- data.frame(read.csv('college.csv'))
summary(college)
pairs(college[,1:10])

boxplot(college$Outstate,college$Private)

Elite<- c(college$Top10perc)
Elite=rep("No",nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite<- as.factor(Elite)
college=data.frame(college,Elite)
summary(college)

boxplot(college$Outstate,college$Elite)

par(mfrow=c(2,2))
hist(college$Outstate)
hist(college$Room.Board)
hist(college$Expend)
hist(college$Grad.Rate)

#9)
#a
str(Auto)

#b
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)

#c
summary(Auto)

#d
Auto[-c(10:85),]
summary(Auto[-c(10:85),])

#e
pairs(Auto)


