perceptrain<-function(S,y){
	
	Z_history<-matrix(, nrow<-0, ncol<-d+1)
	plot(S[,2]~S[,1], col=(y+2), pch=(y+2))
	
	
for(k in 1: 10000){
	
#####################################################
###### first calculate the gradient of the cost function  #######
#####################################################
#create a vector grad to store the sum of every single value used to calculate the gradient
	grad<-0
	
	for(i in 1:n)
	{
		if(y[i]==classify(S,z)[i]){
			grad<-grad+0
		}
		
		else{
			grad<-grad+sign(z%*%S[i,])*S[i,]
		}
	}
	
		

#######################################################
#######################################################
#######################################################
	
	
	z<-z-(1/k)*grad
	
	##########
	#this space is left blank for adding the plot function for the each of the history of z.
	
	
	abline(a=-z[3]/z[2],b=-z[1]/z[2])
	
	
	
	##########
	
	
	Z_history<-rbind(Z_history, z)

#test if the cost function equals 0 or not, if so, terminate the function.

	if (sum(abs(y-classify(S,z)))==0){break}

}
	
	
	return(list(z=z, Z_history=Z_history))
	
	
}
