##Hello Kitty decisio analysis
collect.I=function(npurchased){
   costI=0
   cost.r=5
   cost.d=25
   cardsbought=sample(1:101,
                      size=npurchased,
                      replace=TRUE)
   nhit=length(unique(cardsbought))
   nmissed=101-nhit
   costI=cost.r*npurchased+cost.d*nmissed
   return(costI)
}
collect.I(0)
#S:Number of simulation runs
S=2000
costs.I=replicate(S,collect.I(200))
summary(costs.I)
sum(costs.I<=2525)/S

#seed 起點 亂設
set.seed(5566)

#儲存saving 平均
savings.I=c()
for(s in 1:505){
    savings.I[s]=
    mean(2525-replicate(S,collect.I(s)))
    if(s%%50==0)print(s)
}

x11(width=8,height=5)
par(mar=c(4,4,2,1))
plot(1:505,savings.I,type='l',ylim=c(0,1600),
     lwd=3,ylab="E[Savings]",xlab="Cards to Buy",
     xaxt="n")
axis(1,seq(0,505,50))
points(which.max(savings.I),
       savings.I[which.max(savings.I)],
       col='black',cex=1.5)
which.max(savings.I)

#rbinom(100, 10, 0.5)

#Exchange allowed
prob.exchange.yes=0.3
collect.II=function(npurchased){
  costII=0
  cost.r=5
  cost.d=25
  cardsbought=sample(1:101,
                     size=npurchased,
                     replace=TRUE)
  nhit=length(unique(cardsbought))
  nleft=length(cardsbought)-nhit
  #
  if(nleft==0){
     nexchange.yes=0
     nmissed=101-nhit
     }else{
     nexchagne.yes=rbinom(1,min(101-nhit,nleft),
                          prob.exchange.yes)
     nmissed=101-nhit-nexchagne.yes
  }
  costII=cost.r*npurchased+cost.d*nmissed
  return(costII)
}
collect.II(0)

savings.II=c()
for(s in 1:505){
  savings.II[s]=
    mean(2525-replicate(S,collect.II(s)))
  if(s%%50==0)print(s)
}

lines(1:505,savings.II,col='blue',lwd=3,lty=2)
points(which.max(savings.II),
       savings.II[which.max(savings.II)],
       col='blue',cex=1.5)
which.max(savings.II)


#Exchange & resell allowed
#Assuming resell price=dealer price
prob.exchange.yes=0.3
prob.resell.yes=0.1
collect.III=function(npurchased){
  costIII=0
  cost.r=5
  cost.d=25
  cardsbought=sample(1:101,
                     size=npurchased,
                     replace=TRUE)
  nhit=length(unique(cardsbought))
  nleft=length(cardsbought)-nhit
  #
  if(nleft==0){
    nexchagne.yes=0
    nmissed=101-nhit
    }else{
    nexchagne.yes=rbinom(1,min(101-nhit,nleft),
                         prob.exchange.yes)
    nmissed=101-nhit-nexchagne.yes
  }
  nleft=nleft-nexchagne.yes
  if(nleft>0){
     nresell.ok=rbinom(1,nleft,prob.resell.yes)
  }else{
     nresell.ok=0
  }
  costIII=cost.r*npurchased+cost.d*nmissed-
          cost.d*nresell.ok
  return(costIII)
}
collect.III(0)

savings.III=c()
for(s in 1:505){
  savings.III[s]=
    mean(2525-replicate(S,collect.III(s)))
  if(s%%50==0)print(s)
}

lines(1:505,savings.III,col='green',lwd=3,lty=3)
points(which.max(savings.III),
       savings.III[which.max(savings.III)],
       col='green',cex=1.5)
which.max(savings.III)


#Exchange & resell allowed
#Assuming resell price<dealer price & both random
prob.exchange.yes=0.3
prob.resell.yes=0.1
collect.IV=function(npurchased){
  costIV=0
  cost.r=5
  #cost.d=25
  cardsbought=sample(1:101,
                     size=npurchased,
                     replace=TRUE)
  nhit=length(unique(cardsbought))
  nleft=length(cardsbought)-nhit
  #
  if(nleft==0){
    nexchagne.yes=0
    nmissed=101-nhit
  }else{
    nexchagne.yes=rbinom(1,min(101-nhit,nleft),
                         prob.exchange.yes)
    nmissed=101-nhit-nexchagne.yes
  }
  nleft=nleft-nexchagne.yes
  if(nleft>0){
    nresell.ok=rbinom(1,nleft,prob.resell.yes)
  }else{
    nresell.ok=0
  }
  if(nmissed==0 & nresell.ok==0){
     costIV=cost.r*npurchased
  }
  if(nmissed==0 & nresell.ok>0){
    cost.d.sell=
      sample(seq(5,15),nresell.ok,replace=TRUE)
    costIV=cost.r*npurchased-
           sum(cost.d.sell)
  }
  if(nmissed>0 & nresell.ok==0){
     cost.d.buy=
       sample(seq(20,30),nmissed,replace=TRUE)
     costIV=cost.r*npurchased+
            sum(cost.d.buy)
  }
  if(nmissed>0 & nresell.ok>0){
    cost.d.buy=
      sample(seq(20,30),nmissed,replace=TRUE)
    cost.d.sell=
      sample(seq(5,15),nresell.ok,replace=TRUE)
    costIV=cost.r*npurchased+
           sum(cost.d.buy)-
           sum(cost.d.sell)
  }

  return(costIV)
}
collect.IV(0)
costs.IV=replicate(S,collect.IV(500))
summary(costs.IV)
sum(costs.IV<=2525)/S

savings.IV=c()
for(s in 1:505){
  savings.IV[s]=
    mean(2525-replicate(S,collect.IV(s)))
  if(s%%50==0)print(s)
}

lines(1:505,savings.IV,col='red',lwd=3,lty=4)
points(which.max(savings.IV),
       savings.IV[which.max(savings.IV)],
       col='red',cex=1.5)
which.max(savings.IV)

legend(90,700,c("M1-Base","M2-Exchange","M3-Exchange & Resell",
                  "M4-Random Price"),
       lty=c(1,2,3,4),lwd=c(3,3,3,3),
       bty="n",cex=1.25,col=c('black','blue','green','red'))

which.max(savings.I)
which.max(savings.II)
which.max(savings.III)
which.max(savings.IV)

