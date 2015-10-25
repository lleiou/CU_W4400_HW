###########
#W4400 HW3 Q1
#AdaBoost
#seems a little bit difficult.
#I think the most difficult part of this method is the way
#we generate the new classifier after assigning the new weight to the 
#n data points
#what's more, the decision stump part statement on the hw sheet also is misleading me...
#At first I don't know why we should find a unique j and theta when the final classifier 
#is the aggression of various classifiers...
#but now i know that we are just looking for a optimal j, 
#theta and m for every iteration..


#Actually, I read a whole bunch of materials about the adaboost, 
#for most of them, the equations have a similar different frim the one on our own slides, 
#that is they will divide a z when calculating the new weights...
#I don't know wheather this will cause a differece in the final result.

#Although I understand this part, 
#I still find it confusing how we conduct a cross validation to the data
#should we code all the original process 
#or just use the package which will do cross validation?

#one more question, what is the iteration time B? 
#i.e. How do we decide when it is time to stop iteration any more?

# OK, here comes my code...


library(e1071)
data<-read.table("uspsdata.txt")
y<-read.table("uspscl.txt")

#note here that y is a dataframe instead of a vector
#use as.vector to transform it, so that it will be a lot more easier...
y<-as.vector(unlist(y))
###################
n<-length(y)
dim<-length(data[1,])
#use the par to store the the (j, theta, m) 
#which specify the decision stump that we get in every iteration...
pars<-data.frame(j=rep(0, dim),theta=rep(0, dim),m=rep(0, dim))

#use the dataframe "final" to store the optimal value,..
#the orientation of the classier, and so on in every axis j
final<-data.frame(j=c(1:dim),cum=rep(0,dim),theta=rep(0,dim),m=rep(0,dim))
alpha<-rep(0,n)
w<-rep(1/n,n)

for(b in 1:2){
  
  for (j in 1:dim) {
    
    x.order<-data[,j][order(data[,j])]
    y.order<-y[order(data[,j])]
    
    
    #use dataframe to help calculate the sum of the first several 
    #which is the method introduce bt the TA (which I still think is 
    #not the way to find the parameters which generates the least mieclassify error...)
    dataframe<-data.frame(x=x.order, y=y.order, product=rep(0,n), m=rep(0,n))
    
    
    for (i in 1:n) {
      
      mframe<-data.frame(indicator=c(0,0),m=c(-1,1))
      mframe$indicator[1]<-sum(w[which(x.order!=x.order[i])]*(1*(y.order[which(x.order!=x.order[i])]!=-1*sign(x.order[which(x.order!=x.order[i])]-x.order[i]))))
      mframe$indicator[2]<-sum(w[which(x.order!=x.order[i])]*(1*(y.order[which(x.order!=x.order[i])]!=sign(x.order[which(x.order!=x.order[i])]-x.order[i]))))
      
      #regard the right hand side of the line is negative, whereas the left hand side is positive
      dataframe$product[i]<-min(mframe$indicator)
      
      dataframe$m[i]<-mframe$m[which.min(mframe$indicator)]
      
      
    }
    
    
 
      final$theta[j]<-dataframe$x[which.min(dataframe$product)]
      final$m[j]<-dataframe$m[which.min(dataframe$product)]
      final$cum[j]<-min(dataframe$product)
      
      }
  
  
  pars$j[b]<-final$j[which.min(abs(final$cum))]
  pars$theta[b]<-final$theta[which.min(abs(final$cum))]
  pars$m[b]<-final$m[which.min(abs(final$cum))]
  
  
  indicator<-1*(y!=sign(pars$m[b]*(pars$theta[b]-data[,pars$j[b]])))
  indicator[which(sign(pars$m[b]*(pars$theta[b]-data[,pars$j[b]]))==0)]<-0
  error<-sum(w*indicator)
  
  alpha[b]<-log((1-error)/error)https://www.evernote.com/shard/s200/nl/2147483647/fb7d54e8-5626-4672-a5d1-5c92a0d3ff62/
  
  for(k in 1:n){
    
    w[k]<-w[k]*exp(alpha[b]*indicator[k])
    
  }
  
  #indicator function: I(yi=c(xi))
  #1*(y!=-sign(data[,j]-threshold))
  
  
}





#If we wanna seperate the data into training part and testing part:
#rand<-sample(1:200, 40)

#data.train<-data[-rand,]
#y.train<-y[-rand]

#data.test<-data[rand,]
#y.test<-y[rand]
####################

#
#The following is a minor data set in order to test the codes...
#data<-data.frame(x1=c(1,3,5,7,2,4,6,8),x2=c(1,1,2,2,2,2,1,1))
#y<-c(1,1,1,1,-1,-1,-1,-1)
