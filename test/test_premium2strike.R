

EC <- function(S,X,t,r,v)
{
  d1 <- (log(S/X)+(r+0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  S*pnorm(d1)-X*exp(-r*t)*pnorm(d2)
}


EP  <- function(S,X,t,r,v)
{
  d1 <- (log(S/X)+(r+0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  X*exp(-r*t)*pnorm(-d2)-S*pnorm(-d1)
}

F=100
K=100
v=0.1
t=1

d1 <- function(F,K,v,t)(log(F/K)+(0.5*v^2)*t)/(v*sqrt(t))
d2 <- function(F,K,v,t)d1(F,K,v,t)-v*sqrt(t)


# call premium to strike
C2K<-function(K,C,F,v,t,n=3){
  for(i in 1:n){
    d1<-(log(F/K)+(0.5*v^2)*t)/(v*sqrt(t))
    d2<-d1-v*sqrt(t)
    Nd1<-pnorm(d1)
    Nd2<-pnorm(d2)
    K<-F*Nd1/Nd2-C/Nd2
  }
  K
}

# put premium to strike
P2K<-function(K,P,F,v,t,n=3){
  for(i in 1:n){
    d1<-(log(F/K)+(0.5*v^2)*t)/(v*sqrt(t))
    d2<-d1-v*sqrt(t)
    Nmd1<-pnorm(-d1)
    Nmd2<-pnorm(-d2)
    K<-F*Nmd1/Nmd2+P/Nmd2
  }
  K
}



K<-100
for(i in 1:3)K<-C2K(K,2,100,0.1,1)

K<-100
for(i in 1:3)K<-P2K(K,2,100,0.1,1)



