##Dynamic Simulation: A Simple Inventory Model
#Poisson demand parameter: 6 TVs per day
lambda.est=6

#Holding cost per item per day
h.val=0.6

unit.price=400
unit.cost=250
#Profit margin per TV sold
unit.profit=unit.price-unit.cost

#Parameters of (s, S) inventory policy
s=30
S=130

#cost per delivery from supplier to the dealer
shipping.cost=500

#The number of simulation runs
M=1000
#The number of days
T=360

#The experiment will be repeated for 
# m in 1:M repetitions
# t in 1:T duration

#Vector initialization
#SL: service level
SL=rep(NA,M)
daily.profit=rep(NA,M)
set.seed(5566)

#計算程式跑多久
start.time <- Sys.time()
for(m in 1:M){
  #Initialization: Set inventory on-hand=S in day 0
  inv.onhand=c(S,rep(NA,T-1))
  sales=rep(NA,T)
  orderQ=rep(0,T)
  #Keep track of inventory on-order but not arrived yet
  inv.onorder=c()
  #時間戳記
  inv.onorder.stamp=c()
  #The number of deliveries from the supplier
  delivery=0
  #The total amount of lost sales 
  loss=0
  #The number of stockouts in a repetition
  stockout=0
  #Simulate Poisson demand for T periods
  #360天的需求
  d=rpois(T,lambda.est)
  #
  for(t in 1:T){
     if(t==1){
       #Selling process
       sales[t]=min(inv.onhand[t],d[t])
       #Check stockout
       if(d[t]>sales[t]){
         stockout=stockout+1
         #loss缺貨
         loss=loss+(d[t]-sales[t])
       }
       #Compute inventory position
       inv.position=inv.onhand[1]
       #Ordering mechanism of (s, S)
       if(inv.position<=s){
         #Compute order quantity
         Q=S-inv.position
         #Update inventory on-order
         #追蹤貨物運送過程
         inv.onorder=c(inv.onorder,Q)
         #Simulate stochastic lead time
         #到的時間 時間戳記
         time.to.arrive=t+sample(c(4,5),1,prob=c(0.5,0.5))+1
         #Update time stamp for invenory on-order
         inv.onorder.stamp=c(inv.onorder.stamp,time.to.arrive)
         #Record order quantity
         orderQ[t]=Q
       }
       #Update inventory on-hand in the end of each day
       inv.onhand[t+1]=inv.onhand[t]-sales[t]
     }
     #
     if(t>1){
        #Check if any inventory on-order should arrive
        if(any(inv.onorder.stamp==t)){
           #Update the number of deliveries
           delivery=delivery+1
           #Compute the total of arrived inventories
           index=which(inv.onorder.stamp==t)
           arrival=sum(inv.onorder[index])
           #Update inventory on-hand before starting the day
           inv.onhand[t]=inv.onhand[t]+arrival
           #Removed those just arrived from inventory on-order
           inv.onorder=inv.onorder[-index]
           inv.onorder.stamp=inv.onorder.stamp[-index]
           #cat("arrival time:",t)
        } #end t==1
       #Record sales
       sales[t]=min(inv.onhand[t],d[t])
       #Check if any stockout takes place
       if(d[t]>sales[t]){
         stockout=stockout+1
         loss=loss+d[t]-sales[t]
       }
       #Update inventory position
       inv.position=inv.onhand[t]+sum(inv.onorder)
       #Ordering mechanism of (s, S) inventory policy
       if(inv.position<=s){
         Q=S-inv.position
         inv.onorder=c(inv.onorder,Q)
         time.to.arrive=t+sample(c(4,5),1,prob=c(0.5,0.5))+1
         inv.onorder.stamp=c(inv.onorder.stamp,time.to.arrive)
         orderQ[t]=Q
       }
       #Update inventory on-hand in the end of the day
       inv.onhand[t+1]=inv.onhand[t]-sales[t]
     } #end t>1
     #cat("day:",t,";sales:",sales[t],";onhand:",inv.onhand[t],";onorder:",
     #     inv.onorder,";order:",orderQ[t],"\n") 
  } #end for t
  
  #服務水準
  SL[m]=(T-stockout)/T  #Calculate the service rate; 
    
  #Calculate the average daily profit
  daily.profit[m]=(unit.profit*sum(sales)-sum(h.val*inv.onhand)-
                   shipping.cost*delivery-unit.profit*loss)/T
  #Simulation progress marker
  if(m%%100==0){cat("Repetitions:",m,"\n")} 

} #end for m

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#最大化利潤跟服務水平
summary(daily.profit)
summary(SL)



#搜尋 最佳化(s,S) 
##Search for optimal (s*, S*)
s.val=seq(0,60,5)
S.val=seq(90,360,10)

search.val=as.matrix(expand.grid(s.val,S.val))
dim(search.val)

#i = 1~364
#The (s, S) model as a function for simulation
InvModel=function(i){
  s=search.val[i,1]
  S=search.val[i,2]
  #Vector initialization
  SL=rep(NA,M)
  daily.profit=rep(NA,M)
  for(m in 1:M){
    #Initialization: Set inventory on-hand=S in day 0
    inv.onhand=c(S,rep(NA,T-1))
    sales=rep(NA,T)
    orderQ=rep(0,T)
    #Keep track of inventory on-order but not arrived yet
    inv.onorder=c()
    inv.onorder.stamp=c()
    #The number of deliveries from the supplier
    delivery=0
    #The total amount of lost sales 
    loss=0
    #The number of stockouts in a repetition
    stockout=0
    #Simulate Poisson demand for T periods
    d=rpois(T,lambda.est)
    #
    for(t in 1:T){
      if(t==1){
        #Selling process
        sales[t]=min(inv.onhand[t],d[t])
        #Check stockout
        if(d[t]>sales[t]){
          stockout=stockout+1
          loss=loss+(d[t]-sales[t])
        }
        #Compute inventory position
        inv.position=inv.onhand[1]
        #Ordering mechanism of (s, S)
        if(inv.position<=s){
          #Compute order quantity
          Q=S-inv.position
          #Update inventory on-order
          inv.onorder=c(inv.onorder,Q)
          #Simulate stochastic lead time
          time.to.arrive=t+sample(c(4,5),1,prob=c(0.5,0.5))+1
          #Update time stamp for invenory on-order
          inv.onorder.stamp=c(inv.onorder.stamp,time.to.arrive)
          #Record order quantity
          orderQ[t]=Q
        }
        #Update inventory on-hand in the end of each day
        inv.onhand[t+1]=inv.onhand[t]-sales[t]
      }
      #
      if(t>1){
        #Check if any inventory on-order should arrive
        if(any(inv.onorder.stamp==t)){
          #Update the number of deliveries
          delivery=delivery+1
          #Compute the total of arrived inventories
          index=which(inv.onorder.stamp==t)
          arrival=sum(inv.onorder[index])
          #Update inventory on-hand before starting the day
          inv.onhand[t]=inv.onhand[t]+arrival
          #Removed those just arrived from inventory on-order
          inv.onorder=inv.onorder[-index]
          inv.onorder.stamp=inv.onorder.stamp[-index]
          #cat("arrival time:",t)
        } #end t==1
        #Record sales
        sales[t]=min(inv.onhand[t],d[t])
        #Check if any stockout takes place
        if(d[t]>sales[t]){
          stockout=stockout+1
          loss=loss+d[t]-sales[t]
        }
        #Update inventory position
        inv.position=inv.onhand[t]+sum(inv.onorder)
        #Ordering mechanism of (s, S) inventory policy
        if(inv.position<=s){
          Q=S-inv.position
          inv.onorder=c(inv.onorder,Q)
          time.to.arrive=t+sample(c(4,5),1,prob=c(0.5,0.5))+1
          inv.onorder.stamp=c(inv.onorder.stamp,time.to.arrive)
          orderQ[t]=Q
        }
        #Update inventory on-hand in the end of the day
        inv.onhand[t+1]=inv.onhand[t]-sales[t]
      } #end t>1
      #cat("day:",t,";sales:",sales[t],";onhand:",inv.onhand[t],";onorder:",
      #     inv.onorder,";order:",orderQ[t],"\n") 
    } #end for t
    
    SL[m]=(T-stockout)/T  #Calculate the service rate; 
    
    #Calculate the average daily profit
    daily.profit[m]=(unit.profit*sum(sales)-sum(h.val*inv.onhand)-
                       shipping.cost*delivery-unit.profit*loss)/T
    
  } #end for m
  c(mean(SL),mean(daily.profit),sd(daily.profit))
}


InvModel(1)

#平行運算
#用套件使用CPU六個核心
##Use parallel CPU computing for optimal search
library(foreach)
library(doParallel)
registerDoParallel(cores=6)

getDoParWorkers()

start.time <- Sys.time()
sim.results=foreach(i=1:nrow(search.val),
             .combine=rbind,.verbose=F) %dopar% InvModel(i)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#sim.results
summary(sim.results[,1])
summary(sim.results[,2])
#找最佳化組合
which.max(sim.results[,2])
search.val[which.max(sim.results[,2]),]

#Use non-parallel computing for optimal search
start.time <- Sys.time()
sim.resultsII=matrix(NA,ncol=3,nrow=nrow(search.val))
for(i in 1:nrow(search.val)){
   sim.resultsII[i,]=InvModel(i)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#sim.resultsII
summary(sim.resultsII[,1])
summary(sim.resultsII[,2])
which.max(sim.resultsII[,2])
search.val[which.max(sim.resultsII[,2]),]