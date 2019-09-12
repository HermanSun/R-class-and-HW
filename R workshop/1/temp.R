a=1; b=2 # briefly scalers
a+b
a/b
a-b
a*b
c(100,150,130,145,120,160) #create vector
sales_2019_firsthalf<-c(100,150,130,145,120,160) # save the vector as a name

sales_2019_firsthalf<-c(Jan=100,Feb=150,Mar=130,Apr=145,May=120,June=160) # save the vector as a name

sum(sales_2019_firsthalf)

seq(from = 2, to = 11, by = 3)
seq(2,11,3)
tem<-seq(1,15000,3)
tem[100]
##########################
rep(0,10)
seq(4,20,by=2)
xSeq <- 1:5
xSeq

1:5*2 
(1:5)*2
1:(5*2)
a=c(1,2)
b=c(2,3)
a+b
a/b

##################################
##################################
s1=sample(10)
s1
s2=sample(10,replace=TRUE)
s2
s3=sample(c(1,0),10,replace=TRUE)
s3

s1[1]
s1[1:5]

s2_1=which(s3==1)
s2_1
s2_max=which.max(s2)
s2_max

###############################
###############################
x1<-100
is.numeric(x1)
is.logical(x1)
is.character(x1)
x_logic1 <-(x1==1) 
x_logic100 <- (x1==100)
is.logical(x_logic1)
x_char=c("Arizona","state","university")
is.character(x_char)
x_list=list(x1,x_char)
x_list

x=c(1,2,3)
xx <-c(1,2,3)
x
xx
(xx==x)
is.matrix(xx)
is.matrix(x)
dim(xx)
length(xx)
y<-c(4,5,6)
z<-c(7,8,9)
A<-cbind(x,y,z)
A
dim(A)
is.matrix(A)

X<-as.matrix(x)
dim(X)

B=matrix(rnorm(9,0,1),3,3)
B

A[1,]
A[,1]
A[,1:2]
CAB=cbind(A,B)
colnames(CAB)<-c("x","y","z","a","b","c")
CAB
RAB=rbind(A,B)
RAB
##################################################
##################################################
A+B
A*B
A%*%B
solve(B)
B%*%solve(B)

my.arry1 <- array(1:24, dim=c(3,4,2))
my.arry2 <- array(1:48, dim=c(3,4,2,2))
mat1 <- matrix(1:9,3,3)
mat2<-matrix(10:18,3,3)
my.arry1
my.arry2
mat1
mat2

z<-array(c(mat1,mat2),dim=c(3,3,2))
z
library(abind)
newarray <- abind(mat1,mat2,along=1)#1 append row, 2 append column,3 append new matrix
newarray

x7=1:10
x7

for (i in 1:10){
x7[i]=x7[i]+1
}#
x7

x8=0
for (i in 1:10){
x8=x8+i
}# 
x8

###################################
x<- runif(1,min=-1,max=1)# draw a random number from random uniform distribution
print(x)
if(x>0){
print("youwin!")
}else{
print("you lose")}
####################################
x9<-list(a=1:5, b=rnorm(10))
x9
lapply(x9,mean) ## lapply

x10<-1:4
x10
lapply(x10,runif)

x11 <-matrix(rnorm(200),20,10)
x11
apply(x11,2,mean)#2nd dimension column
apply(x11,1,mean)# 1st dimension row

system.time()
x12<-matrix(rnorm(2000000),200000,10)
n1 <-dim(x12)[1]
rowmean=rep(0,n1)
system.time(
for (i in 1:n1){rowmean[i]=mean(x12[i,])})

system.time(apply(x12,1,mean))

summing=function(x){
for (i in 1:10){
x=x+i
}
print(x)
}

summing(0)


f.sum <-function(x,y){
r <-x+y
r}

f.sum(5,10)

setwd("D:/MSBA-term 2/R workshop/1")
source("summing.R")
summing(0)





















