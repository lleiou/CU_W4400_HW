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
alpha<-rep(0,2)
w<-rep(1/n,n)

for(b in 1:2){
  
  for (j in 1:dim) {
    
    x<-rep(0,n)
    
    for (g in 1:n){
     x[g]<- min(data[,j])+g*(max(data[,j])-min(data[,j]))/(n+1)
    }
    
    
    
    #use dataframe to help calculate the sum of the first several 
    #which is the method introduce bt the TA (which I still think is 
    #not the way to find the parameters which generates the least mieclassify error...)
    dataframe<-data.frame(x=x, y=y, product=rep(0,n), m=rep(0,n))
    
    
    for (i in 1:n) {
      
      mframe<-data.frame(indicator=c(0,0),m=c(-1,1))
      mframe$indicator[1]<-sum(w*(1*(y!=-1*sign(data[,j]-x[i]))))
      mframe$indicator[2]<-sum(w*(1*(y!=sign(data[,j]-x[i]))))
  
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
  
  alpha[b]<-log((1-error)/error)
  for(k in 1:n){
    
    w[k]<-w[k]*exp(alpha[b]*indicator[k])
    
  }
  
  #indicator function: I(yi=c(xi))
  #1*(y!=-sign(data[,j]-threshold))
  
  
}



#now it is the time to aggreate all the b weak classifier into one:
allpars<-pars[1:2,]
TheResult<-sign(sum(alpha*(sign(allpars$m*(x[allpars[,1]]-allpars$theta)))))



sample<-sample(1:200,200)
rand<-list(rand1=sample[1:40],rand2=sample[41:80],rand3=sample[81:120],rand4=sample[121:160],rand5=sample[161:200])


for(k in 1:5){
  xtest<-data[unlist(rand[k]),]
  xtrain<-data[-unlist(rand[k]),]
  ytest<-y[unlist(rand[k])]
  ytrain<-y[-unlist(rand[k])]
  
}

for(p in 1:40){
  print(ytest[p]-sign(sum(alpha*(sign(allpars$m*(xtest[p,][allpars[,1]]-allpars$theta))))))
}

mean(ytrain!=sign(sum(alpha*(sign(allpars$m*(xtrain[allpars[,1]]-allpars$theta))))))


    
}




#If we wanna seperate the data into training part and testing part:
#rand<-sample(1:200, 40)

#data.train<-data[-rand,]
#y.train<-y[-rand]

#data.test<-data[rand,]
#y.test<-y[rand]
####################


