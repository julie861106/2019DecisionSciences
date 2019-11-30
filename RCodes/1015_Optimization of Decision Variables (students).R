
#Below is a function that simulates random samples from 
#the Myerson distribution in section 4.4 of Roger Myerson's 2005 book
#This is a generalized version of normal & lognormal distribution
rMyerson <- function(n,q1,q2,q3,lower=-Inf, upper=Inf,tl=0.5){
  #n: the number of random samples
  #q1: xx percentile, xx<50 & usually xx=25
  #q2: 50 percentile
  #q3: xx percentile, xx>50 & usually xx=75
  #lower: minimum possible value
  #upper: maximum possible value
  #tl: tail probability = P(X<q1)+P(X>q3)
  ######################################
  x=runif(n)
  x[which(x>0.999999)]=0.999999
  x[which(x<0.000001)]= 0.000001
  #Above avoids sampling values that are too extreme
  norml=qnorm(x)/qnorm(1-tl/2)
  br=(q3-q2)/(q2-q1)
  if(br==1){
    res=q2+(q3-q2)*norml
  } else {
    res=q2+(q3-q2)*(br^norml-1)/(br-1)
  }
  pmin(pmax(res, lower), upper)
}


##Case: Scotia Snowboards
#P(weather is cold)
p.cold=1/3
#Demand parameters
q1.normal=60000
q2.normal=75000
q3.normal=90000

q1.cold=80000
q2.cold=100000
q3.cold=125000

#Cost parameters
unitcost=20
unitprice=48
salvage=8


#x as the order quantity. Try different ordering decisions
x.val=seq(50000,150000,1000)

#Number of simulation runs
S=15000

#Simulate random demand
#1: cold; 0:weather
#模擬各個需求的可能
sim.weather=sample(c(1,0),S,replace=TRUE,
                   prob=c(p.cold,1-p.cold))

sim.demand=rep(0,S)
for(s in 1:S){
  #1: cold; 0:weather
  if(sim.weather[s]==1){
    sim.demand[s]=rMyerson(1,q1.cold,q2.cold,q3.cold,
                           lower=0)
  }else{
    sim.demand[s]=rMyerson(1,q1.normal,q2.normal,q3.normal,
                           lower=0)
  }
  sim.demand[s]=round(sim.demand[s],0)
}
sim.demand

#算利潤
profit=function(x=80000,d){
  #d: demand realizations
  profit.val=c()
  for(i in 1:length(d)){
    profit.val[i]=(unitprice-unitcost)*min(x, d[i])+
      (salvage-unitcost)*max(x-d[i], 0)
  }
  profit.val
}


sim.profit=matrix(0,nrow=S,ncol=length(x.val))
avg.profit=c()
sd.profit=c()

start.time <- Sys.time()

for(i in 1:length(x.val)){
  sim.profit[,i]=profit(x.val[i],d=sim.demand)
  avg.profit[i]=mean(sim.profit[,i])
  sd.profit[i]=sd(sim.profit[,i])
  cat("production quantity:", x.val[i], "\n")
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

x11(width=12,height=5)
par(mfrow=c(1,2))
plot(x.val,avg.profit,type='l',xlab="production quantity",lwd=3)
plot(x.val[which(avg.profit>1800000)],
     avg.profit[which(avg.profit>1800000)],
     type='l',xlab="production quantity",lwd=3)


x.val[which.max(avg.profit)]
max(avg.profit)



##Assess the value of perfect information
cu=unitprice-unitcost
co=unitcost-salvage
cu
co
#Calculate the critical fractile
frac=cu/(cu+co)
frac

#Compute Q* for cold weather
#冬天可能的需求
dcold=rMyerson(S,q1.cold,q2.cold,q3.cold,lower=0)
hist(dcold)
x.cold=quantile(dcold,frac,names=FALSE)
x.cold=round(x.cold,0)
x.cold


#Compute Q* for normal weather
dnormal=rMyerson(S,q1.normal,q2.normal,q3.normal,lower=0)
hist(dnormal)
x.normal=quantile(dnormal,frac,names=FALSE)
x.normal=round(x.normal,0)
x.normal

profit.x.cold=profit(x.cold,dcold)
profit.x.normal=profit(x.normal,dnormal)
mean(profit.x.cold)
mean(profit.x.normal)

frac
#知道是否偏冷
profit.perfectinfo=c()

for(s in 1:S){
  #1: cold; 0:weather
  if(sim.weather[s]==1){
    x=x.cold
  }else{
    x=x.normal
  }
  demand=sim.demand[s]
  profit.perfectinfo[s]=
    (unitprice-unitcost)*min(x, demand)+
    (salvage-unitcost)*max(x-demand, 0)
}

mean(profit.perfectinfo)

max(avg.profit)

mean(profit.perfectinfo)-max(avg.profit)



##A Simple Biding Problem
#cost parameters
cost.q1=86
cost.q2=100
cost.q3=120

#parameters of opponents' bids
oppbid.q1=120
oppbid.q2=140
oppbid.q3=180

#Number of simulation runs
S=20000

opponents=sample(c(1,2,3,4,5),S,prob=c(0.2,0.3,0.3,0.1,0.1),
                 replace=TRUE)
oppbids=list()
for(s in 1:S){
  oppbids.s=rMyerson(opponents[s],oppbid.q1,oppbid.q2,oppbid.q3,
                     lower=0)
  oppbids[[s]]=round(oppbids.s,0)
}
sim.cost=rMyerson(S,cost.q1,cost.q2,cost.q3,lower=0)


#b: the bidding value 我出的價格
bidding=function(b,oppb,c){
  win=all(oppb>b)
  profit=win*(b-c)
  c(win,profit)
}

#Try different bidding values
b.val=seq(100,200,5)

sim.win=matrix(0,nrow=S,ncol=length(b.val))
sim.profit=matrix(0,nrow=S,ncol=length(b.val))
avg.profit=c()
sd.profit=c()
wins=c()

start.time <- Sys.time()

for(i in 1:length(b.val)){
  for(s in 1:S){
    temp=bidding(b=b.val[i],opp=oppbids[[s]],c=sim.cost[s])
    sim.win[s,i]=temp[1]
    sim.profit[s,i]=temp[2]
  }
  wins[i]=sum(sim.win[,i])
  avg.profit[i]=mean(sim.profit[,i])
  sd.profit[i]=sd(sim.profit[,i])
  #
  cat("bidding value:", b.val[i], "\n")
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

x11(width=8,height=5)
plot(b.val,avg.profit,type='l',xlab="bidding value",lwd=3)
abline(h=0,lty=2,col='red',lwd=2)

b.val[which.max(avg.profit)]
max(avg.profit)

wins[which.max(avg.profit)]/S



##The Winners' Curse
#Capture stochastic dependencies between C and A 
#corrleation coefficient
#corr假設0.4
corr.opp.cost=0.4

#Below simulates bivariate standard normal variates
#both have standard deviation of 1
cov.opp.cost=corr.opp.cost*1*1
#variance-covariance matrix
covmat.opp.cost=matrix(c(1,cov.opp.cost,
                         cov.opp.cost,1),nrow=2)
covmat.opp.cost
library(MASS)
binorm=mvrnorm(S,c(0,0),covmat.opp.cost)
#check the correlation of simulated values
cor(binorm[,1],binorm[,2])

#Transform the simulated standard normal variates
#into uniform (0, 1) variates
birand1=pnorm(binorm[,1])
birand2=pnorm(binorm[,2])
birand=cbind(birand1,birand2)
#check correlation between two uniform(0, 1) variates
cor(birand[,1],birand[,2])

apply(birand,2,summary)
x11(width=12, height=5)
par(mfrow=c(1,2))
hist(birand[,1])
hist(birand[,2])

##Pre-draw indepedent oppbids & costs
sim.oppbids.ind=rep(0,S)
sim.cost.ind=rMyerson(S,cost.q1,cost.q2,cost.q3,lower=0)
for(s in 1:S){
  oppbids=rMyerson(opponents[s],oppbid.q1,oppbid.q2,oppbid.q3,lower=0)
  sim.oppbids.ind[s]=round(min(oppbids),0)
}

start.time <- Sys.time()

##Pre-draw correlated oppbids & costs
#有相關性的 藍線
sim.oppbids.corr=rep(0,S)
sim.cost.corr=rep(0,S)
for(s in 1:S){
  sim.oppbids.corr[s]=round(quantile(sim.oppbids.ind,birand[s,1]),0)
  sim.cost.corr[s]=quantile(sim.cost.ind,birand[s,2])
}

cor(sim.oppbids.corr,sim.cost.corr)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

sim.win.corr=matrix(0,nrow=S,ncol=length(b.val))
sim.profit.corr=matrix(0,nrow=S,ncol=length(b.val))
avg.profit.corr=rep(0,length(b.val))
sd.profit.corr=rep(0,length(b.val))
wins.corr=rep(0,length(b.val))


for(i in 1:length(b.val)){
  oppbids.i=sim.oppbids.corr
  if(any(b.val[i]<oppbids.i)){
    winnings=which(b.val[i]<oppbids.i)
    sim.win.corr[winnings,i]=1
    wins.corr[i]=sum(sim.win.corr[,i])
    #
    cost.i=sim.cost.corr
    sim.profit.corr[winnings,i]=b.val[i]-cost.i[winnings]
    avg.profit.corr[i]=mean(sim.profit.corr[,i])
    sd.profit.corr[i]=sd(sim.profit.corr[,i])
  }
  #
  cat("bidding value:", b.val[i], "\n")
}



y.max=max(c(avg.profit,avg.profit.corr))
y.min=min(c(avg.profit,avg.profit.corr))

x11(width=8,height=5)
plot(b.val,avg.profit,type='l',lwd=3,
     xlab="bidding value",
     ylim=c(y.min,y.max))
abline(h=0,lty=2,col='red',lwd=2)
lines(b.val,avg.profit.corr,lty=3,col='blue',lwd=3)

b.val[which.max(avg.profit.corr)]
max(avg.profit.corr)

wins.corr[which.max(avg.profit.corr)]/S

b.val[which.max(avg.profit)]
max(avg.profit)

wins[which.max(avg.profit)]/S
