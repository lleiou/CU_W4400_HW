
x<-seq(0,4,0.01)
plot(exp(1)^(-1*x)~x,type="l", xlim=c(0,4))


#g
y<-seq(0,4,0.01)
x<-rexp(256,1)

n<-4
plot(exp(1)^(log(y)*(1+n)+(2+n)*log(sum(x[1:n])+0.2)-(sum(x[1:n])+0.2)*y-lgamma(2+n))~y,par(new=TRUE),xlim=c(0,4),ylim=c(0,7),type="l",ylab=" ", lty=1)

n=8
plot(exp(1)^(log(y)*(1+n)+(2+n)*log(sum(x[1:n])+0.2)-(sum(x[1:n])+0.2)*y-lgamma(2+n))~y,par(new=TRUE),xlim=c(0,4),ylim=c(0,7),type="l",ylab=" ", lty=2)

n=16
plot(exp(1)^(log(y)*(1+n)+(2+n)*log(sum(x[1:n])+0.2)-(sum(x[1:n])+0.2)*y-lgamma(2+n))~y,par(new=TRUE),xlim=c(0,4),ylim=c(0,7),type="l",ylab=" ",lty=3)

n=256
plot(exp(1)^(log(y)*(1+n)+(2+n)*log(sum(x[1:n])+0.2)-(sum(x[1:n])+0.2)*y-lgamma(2+n))~y,par(new=TRUE),xlim=c(0,4),ylim=c(0,7),type="l",ylab=" ",lty=4)


exp(1)^(exp(1)^((2+n)*log(0.2+sum(x[1:4]))-(0.2+sum(x[1:4]))-lgamma(2+n)))






