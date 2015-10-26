library(e1071)
data<-read.table("uspsdata.txt")
y<-read.table("uspscl.txt")

#note here that y is a dataframe instead of a vector
#use as.vector to transform it, so that it will be a lot more easier...

y<-as.vector(unlist(y))
###################
########CESHI 

###################
n<-length(y)
dim<-length(data[1,])
#use the par to store the the (j, theta, m) 
#which specify the decision stump that we get in every iteration...
pars<-data.frame(j=rep(0, n),theta=rep(0, n),m=rep(0, n))

#use the dataframe "final" to store the optimal value,..
#the orientation of the classier, and so on in every axis j
final<-data.frame(j=c(1:dim),cum=rep(0,dim),theta=rep(0,dim),m=rep(0,dim))
alpha<-rep(0,10)
w<-rep(1/n,n)

for(b in 1:10){
  
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
      
      #mframe temporarily stores the value for both m=+1and-1, which is used to find the best m under theta
      mframe<-data.frame(indicator=c(0,0),m=c(-1,1))
      #the indicator is the error when we select theta=x[i] and m
      mframe$indicator[1]<-sum(w*(1*(y!=-1*sign(data[,j]-x[i]))))/sum(w)
      mframe$indicator[2]<-sum(w*(1*(y!=sign(data[,j]-x[i]))))/sum(w)
      
      #m=-1: regard the right hand side of the line is negative, whereas the left hand side is positive
      #m=+1: regard the left hand side of the line is negative, whereas the right hand side is positive
      
      #dataframe gives the best theta for this j
      dataframe$product[i]<-min(mframe$indicator)
      dataframe$m[i]<-mframe$m[which.min(mframe$indicator)]
      
      
    }
    
    
    #final is a dataframe under b, which stores the best theta for every j
    final$theta[j]<-dataframe$x[which.min(dataframe$product)]
    final$m[j]<-dataframe$m[which.min(dataframe$product)]
    final$cum[j]<-min(dataframe$product)
    
  }
  
  pars$j[b]<-final$j[which.min(abs(final$cum))]
  pars$theta[b]<-final$theta[which.min(abs(final$cum))]
  pars$m[b]<-final$m[which.min(abs(final$cum))]
  
  
  #to see whether every point is well classified under the j and theta we get:
  indicator<-1*(y!=sign(pars$m[b]*(data[,pars$j[b]]-pars$theta[b])))
  #indicator[which(sign(pars$m[b]*(data[,pars$j[b]]-pars$theta[b]))==0)]<-0
  error<-sum(w*indicator)/sum(w)
  
  ############
  ############
  # PLEAZ FOLLOW ALL THE INSTRUCTIONS ON THE SHEET, 
  # AS IT IS WRITTEN ON THE SHEET,YOU HAVE TO HAVE DENOMINATOR OFTHE ERROR!!!!!!!!!!!!!!
  ##########
  ########## The source of all the despair and time waste!!!!!!!!!!!!!!!!!!
  ########### Written at 23:43, OCT 25TH, 2015, Butler library
  ###################################################
  #### PLUS , THE PROBLEM IT CAUSE IS THAT THE PARS RESULT WILL REMAIN UNCHANGED AFTER SEVERAL LOOP
  ##########################################
  
  alpha[b]<-log((1-error)/error)
  
  for(k in 1:n){
    
    w[k]<-w[k]*exp(alpha[b]*indicator[k])
    
  }
  
  #indicator function: I(yi=c(xi))
  #1*(y!=-sign(data[,j]-threshold))
  
  
}



