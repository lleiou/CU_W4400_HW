#The followings is the procedure to cross validate in each iteration
#and get the average error rate for both training set and the testing set.


X<-read.table("uspsdata.txt")
y<-read.table("uspscl.txt")
y<-as.vector(unlist(y))

#note here that y is a Xframe instead of a vector
#use as.vector to transform it, so that it will be a lot more easier...
B<-10
  
n<-length(y)
dim<-length(X[1,])
  

#randomly setting all the data into 5 equal parts, select 1 of them as the validation set every time, 
#calculate the error for every aggregate classifier from 1 to B  
#so altogether we will get 5 sets of errors and we average them all and plot the error~b plot
sample<-sample(1:200,200)
rand<-list(rand1=sample[1:40],rand2=sample[41:80],rand3=sample[81:120],rand4=sample[121:160],rand5=sample[161:200])


#this is the date frame we will use to store the error for every fold and every iteration
errors_train<-data.frame(cv1=rep(0,B),cv2=rep(0,B),cv3=rep(0,B),cv4=rep(0,B),cv5=rep(0,B))
errors_test<-data.frame(cv1=rep(0,B),cv2=rep(0,B),cv3=rep(0,B),cv4=rep(0,B),cv5=rep(0,B))
#cv stands for cross validation

#The following loops calculate the error for every fold of the cross validation
#Remember, for every fold, everytime you iterate for one time, you get a new AGGREGATED classifier
#(from one to the current b), and calculate both testing error and training error on this 
#aggregated classifier 

#####################################
# The key point to understand this cross validation:
# It is used to get the optimal B which generates the least errors
# So what we really care about is the 
####################################

#It took me really long time to get this, with the help of students and TAs


for(k in 1:5){
  
  xtest<-X[unlist(rand[k]),]
  xtrain<-X[-unlist(rand[k]),]
  ytest<-y[unlist(rand[k])]
  ytrain<-y[-unlist(rand[k])]
 
 print(k)
  
  for (b in 1:B){
  print(b)
  adaboost<-AdaBoost(xtrain,ytrain,b)
  alpha<-adaboost$alpha
  allPars<-adaboost$allPars
  errors_train[b,k]<-sum(1*(ytrain!=agg_class(xtrain,alpha,allPars)))/length(xtrain[,1])
  errors_test[b,k]<-sum(1*(ytest!=agg_class(xtest,alpha,allPars)))/length(xtest[,1])
  #minor errors always happen here such as the parenthese are not the same from left and right
  #what's more, you write xtrain but you write y instead of xtrain's real counterpart ytrain
  }
  
}


#the following procedure is to calculate the average for every iteration and plot the error~b plot


plot(rowMeans(errors_train)~c(1:B),col="red",type="l",lty=1,ylim<-c(0:0.2),axes=FALSE,ylab="",xlab="") 
par(new=TRUE)
#the par command lets us draw two lines in exactly the same plot
#but the axis may overlap and look weiered, so we reduce the 
#first plot to only the line itself, without any axis and label
#but i have to say that the method is not that great...
plot(rowMeans(errors_test)~c(1:B),col="green",type="l",lty=46,ylim<-c(0:0.2),xlab="B",ylab="Error")
title("The Testing Error and the Cross-Validated Test Error")
legend(6,0.16, c("Training Error","Testing Error"),lty=c(1,46),col=c("red", "green"))
#I started to wonder how to draw the two lines in one plot......

