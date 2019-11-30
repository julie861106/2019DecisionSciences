##Case: Operations at Conley Fisheries
#The quantity of fish caught each day
fish.Q = 3500

##S: The number of simulation runs
S=10000

#expected price at Rock port
mu.PR = 3.65
#the standard devaition of the price at Rock port
sigma.PR = 0.2

#Simulated price at Rock port
sim.PR=rnorm(S,mu.PR,sigma.PR)

#Simulated demand at Rock port 
sim.D = sample(c(0,1000,2000,3000,4000,5000,6000),S,
               prob = c(0.02,0.03,0.05,0.08,0.33,0.29,0.2),
               replace = TRUE)

#The operation cost of the boat
oper.cost = 10000


F=c()
#F is the stochastic revenue when selling all the fish at Rockport
for(s in 1:S){
  F[s] = sim.PR[s]*min(fish.Q, sim.D[s]) - oper.cost
}


summary(F)
hist(F, breaks=200)

#打敗G港
sum(F>1375)/S
#虧錢
sum(F<0)/S

quantile(F,0.975)
quantile(F,0.025)

lowestq5=F[which(F<=quantile(F,0.05))]
CVaRq5=mean(lowestq5)
CVaRq5



##Complications of Operations at Conley Fisheries
S=1000
PR.Rock=rnorm(S,3.65,0.25)
PR.Glou=rnorm(S,3.5,0.5)
summary(PR.Rock)
summary(PR.Glou)

x11(width=12,height=5)
par(mfrow=c(1,2))
hist(PR.Rock,breaks=50)
hist(PR.Glou,breaks=50)


install.packages("EnvStats")
#Install the package above if needed
library(EnvStats)
D.Glou=round(rtri(S,2000,6000,5000),0)
D.Rock=sample(c(0,1000,2000,3000,4000,5000,6000),S,
              prob = c(0.02,0.03,0.05,0.08,0.33,0.29,0.2),
              replace = TRUE)

x11(width=12,height=5)
par(mfrow=c(1,2))
hist(D.Rock)
hist(D.Glou)


##S: The number of simulation runs
S=10000

#The quantity of fish caught each day
fullload = 3500
frac=runif(S,0.7,1)

fish.Q=round(fullload*frac,0)


#expected price at Rock port
mu.PRR = 3.65
#the standard devaition of the price at Rock port
sigma.PRR = 0.2
#expected price at Glou
mu.PRG = 3.5
#the standard devaition of the price at Glou
sigma.PRG = 0.5

#Simulated prices
sim.PRR=rnorm(S,mu.PRR,sigma.PRR)
sim.PRG=rnorm(S,mu.PRG,sigma.PRG)

#Simulated demand 
sim.DR = sample(c(0,1000,2000,3000,4000,5000,6000),S,
               prob = c(0.02,0.03,0.05,0.08,0.33,0.29,0.2),
               replace = TRUE)

sim.DG = round(rtri(S,2000,6000,5000),0)

#The operation cost of the boat
oper.cost = 10000


F=c()
G=c()
#F is the stochastic revenue when selling all the fish at Rockport
for(s in 1:S){
  F[s] = sim.PRR[s]*min(fish.Q[s], sim.DR[s]) - oper.cost
  G[s] = sim.PRG[s]*min(fish.Q[s], sim.DG[s]) - oper.cost
}


summary(G)
summary(F)
x11(width=12,height=5)
par(mfrow=c(1,2))
hist(G, breaks=200)
hist(F, breaks=200)

sum(G>1375)/S
sum(G<0)/S

sum(F>1375)/S
sum(F<0)/S

quantile(G,0.975)
quantile(G,0.025)

quantile(F,0.975)
quantile(F,0.025)

#最壞的5%
lowestq5G=G[which(G<=quantile(G,0.05))]
CVaRq5G=mean(lowestq5G)
CVaRq5G

lowestq5F=F[which(F<=quantile(F,0.05))]
CVaRq5F=mean(lowestq5F)
CVaRq5F



##Modeling exponentially distributed time
#一個小時處理幾通電話
S=10000
calls=c()
for(s in 1:S){
    k=0
    totaltime=0
    while(totaltime<=60){
      totaltime=totaltime+rexp(1,1/10)
      k=k+1
      cat("totaltime=",totaltime,"; k=",k,"\n")
    }
    calls[s]=k-1
    if(s%%1000==0)print(s)
}

plot(table(calls)/S)
#理論值
lines(min(calls):max(calls),
      dpois(min(calls):max(calls),6),col='red',lty=2,lwd=3)


##Verify Memoryless
Tsamples=rexp(S,1/10)
sum(Tsamples>5)/S
sum(Tsamples>15)/sum(Tsamples>10)


##Verify gamma distribution
S=10000
time.five=c()
for(s in 1:S){
  time.five[s]=sum(rexp(5,1/10))
}

min.x=round(min(time.five),2)
max.x=round(max(time.five),2)
x=seq(min.x, max.x, 0.01)
shape.est=5
scale.est=10
hist(time.five,breaks=50,freq=FALSE)
#理論值
lines(x,dgamma(x,shape=shape.est,scale=scale.est),col='red',lwd=3)


x=seq(1,30,0.1)
plot(x,dexp(x,0.2),type='l',lwd=2)
lines(x,dgamma(x,shape=1,scale=1/0.2),col='red',lty=2)
lines(x,dgamma(x,shape=2,scale=1/0.2),col='blue')
lines(x,dgamma(x,shape=5,scale=1/0.2),col='green')






##Project duration Simulation
taskt.mean=c(20,50,60,15,65,35,30,10)
taskt.stdev=c(7,10,12,3,30,15,5,3)
#Assuming task time as a RV X ~ gamma(shape, scale)
#E[X]=shape*scale & Var[X]=shape*scale^2
shape.est=c()
scale.est=c()
scale.est=(taskt.stdev)^2/taskt.mean
scale.est

shape.est=taskt.mean/scale.est
shape.est
#Need to check both parameters>0 or not

#Define BT:Begin Time & FT: Finish Time

S=10000
#遲一天罰金
penaltyperday=100000
B.reduced=1
simDays.temp=c()
simPenalty.temp=c()
for(i in 1:S){
#i:index for the ith simulation
    taskt.i=c()
    for(j in 1:length(taskt.mean)){
    #j:activity index
        shape.j=shape.est[j]
        scale.j=scale.est[j]
        taskt.i.j=rgamma(1,shape=shape.j,
                         scale=scale.j)
        taskt.i[j]=round(taskt.i.j,0)
    }
    ##Assuming NO reduction in task B time
    #BT beginning time, ET end time
    BT.A=BT.C=BT.E=0
    ET.A=BT.A+taskt.i[1]
    ET.C=BT.C+taskt.i[3]
    ET.E=BT.E+taskt.i[5]
    #
    BT.B=ET.A
    #不花錢找外包
    if(B.reduced==0){
       ET.B=BT.B+taskt.i[2]
    }
    ##花錢找外包
    if(B.reduced==1){
      ET.B=BT.B+round(taskt.i[2]*0.8,0)
    }
    #
    BT.D=max(ET.B,ET.C)
    ET.D=BT.D+taskt.i[4]
    #
    BT.F=ET.E
    ET.F=BT.F+taskt.i[6]
    #
    BT.G=ET.D
    ET.G=ET.D+taskt.i[7]
    #
    BT.H=ET.G
    ET.H=BT.H+taskt.i[8]
    #H or F結束
    simDays.temp[i]=max(ET.H, ET.F)
    #算有沒有delay
    delay.i=max(simDays.temp[i]-130,0)
    simPenalty.temp[i]=delay.i*penaltyperday
}

simDays.base=simDays.temp
simPenalty.base=simPenalty.temp
#
summary(simDays.base)
sd(simDays.base)
sum(simDays.base<=130)/S
#
summary(simPenalty.base)


simDays.reduced=simDays.temp
simPenalty.reduced=simPenalty.temp
summary(simDays.reduced)
sd(simDays.reduced)
sum(simDays.reduced<=130)/S
#
summary(simPenalty.reduced)

