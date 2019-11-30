#John's payoff is deterministic
John_pay    = 12000

#The probability of getting the job from Vanessa
prob_interval    = seq(0.1,0.9,0.1)
#算各種offer拿到的機會

#Simulation times
simulation_times = 1:5000


#Earning if Vanessa gives an offer
#results1：reject John, if Vanessa = 1, take it, else School
results1  = matrix(NA, nrow=length(prob_interval),
                       ncol=length(simulation_times))
rownames(results1) = prob_interval 
colnames(results1) = simulation_times
#Earning if Vanessa does NOT give an offer
#建一個各種offer的矩陣
#results2：reject John, skip Vanessa, try School
results2  = matrix(NA,  nrow=length(prob_interval),
                        ncol=length(simulation_times))
rownames(results2) = prob_interval 
colnames(results2) = simulation_times

#外圈0.1-0.9, 內圈：給定一個機率的情況下, 做第一次到5000次的模擬
for(p in prob_interval){
  for(times in simulation_times){
    
    #The payoff from Vanessa is deterministic
    Vanessa_pay = 14000
    #The chance to get the job from Vanessa
    #模擬是否得到V的offer 得出0or1
    Vanessa_offer = sample(c(1,0),1,
                           prob = c(p,1-p))
    
    Vanessa_pay = Vanessa_offer*Vanessa_pay
    
    
    #The payoff from school is uncertain
    #模擬學校pay 機率會對應
    School_pay = sample(c(21600,16800,12000,6000,0),1,
                        prob = c(0.05,0.25,0.4,0.25,0.05))
    
    if(Vanessa_offer==1){
      results1[which(p==prob_interval),times] = Vanessa_pay
    }else{
      results1[which(p==prob_interval),times] = School_pay
    }
    #直接去學校
    results2[which(p==prob_interval),times] = School_pay
    
    
  }
  results1
  cat("P(Vanessa Offer=1)=",p, "\n")
}



#Earnings when Vanessa gives the offer 
#pctile 機率的範圍
pctile.range = c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)
sim.pctile1 = matrix(NA, ncol=length(pctile.range), 
                         nrow= length(prob_interval))
colnames(sim.pctile1)=pctile.range
rownames(sim.pctile1)=prob_interval
  
#apply(results1,1,summary)

for(i in 1:nrow(results1)){
  sim.pctile1[i,] = 
    quantile(results1[i,],probs =pctile.range)
  #quantile 算百分位數
}

sim.pctile1

#Earnings from accepting the school offer
sim.pctile2 = matrix(NA, ncol=length(pctile.range), 
                     nrow= length(prob_interval))
colnames(sim.pctile2)=pctile.range
rownames(sim.pctile2)=prob_interval

#apply(results2,1,summary)

for(i in 1:nrow(results2)){
  sim.pctile2[i,] = quantile(results2[i,],probs = pctile.range)
}

sim.pctile2


#大於John需要至少多少獲得Vanessa工作的機會
#比John好的機率
Pbetter1=c()
#i = 1～9
for(i in 1:nrow(results1)){
    Pbetter1[i]=
      sum(results1[i,]>=John_pay )/ncol(results1)
    #比John好的機率 sum(pay>=$12000)/5000次
}

x11(width=8,height=5)
plot(prob_interval,Pbetter1,type='l',xaxt='n',lwd=3,
     xlab="P(Vanessa offer=1)",ylab="P(Earning>=John offer)")
axis(1,seq(0.1,0.9,0.1))
#大於John的信心 = 0.95(討厭風險), V>0.8才會
abline(h=0.95,col='red',lty=2,lwd=2)
abline(h=0.9,col='green',lty=3,lwd=2)


