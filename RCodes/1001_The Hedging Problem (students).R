#DM put options
putDM=matrix(c(c(0.66,0.65,0.64,0.63,0.62,0.61,0.60,0.59,0.55),
               c(0.085855,0.032191,0.020795,0.017001,0.013711,
                 0.010851,0.008388,0.006291,0.001401)),ncol=2)
             
colnames(putDM)=c("kDM","cDM")

#BP put options
putBP=matrix(c(c(1.3,1.25,1.20,1.15,1.1,1.05,1,0.95,0.9),
               c(0.137213,0.082645,0.0450460,0.028348,0.016146,
                 0.007860,0.003277,0.001134,0.000245)),ncol=2)

colnames(putBP)=c("kBP","cBP")

#81種 因為各有9個可能，9*9=81
put.grid=expand.grid(putDM[,1],putBP[,1])

put.grid=cbind(put.grid[,1],rep(NA,nrow(put.grid)),
                put.grid[,2],rep(NA,nrow(put.grid)))

colnames(put.grid)=c(colnames(putDM),colnames(putBP))


for(i in 1:nrow(put.grid)){
   put.grid[i,2]=putDM[which(putDM[,1]==put.grid[i,1]),2]
   put.grid[i,4]=putBP[which(putBP[,1]==put.grid[i,3]),2]
}


#算HedgeRev
HedgeRevDM=function(DMfcst=645,currentDM=0.6513,nDM=500,
                    kDM,cDM,deltaDM){
     DMfcst*currentDM*(1+deltaDM/100)+
     nDM*(max(kDM-currentDM*(1+deltaDM/100),0)-cDM)
}


HedgeRevBP=function(BPfcst=272,currentBP=1.234,nBP=500,
                    kBP,cBP,deltaBP){
     BPfcst*currentBP*(1+deltaBP/100)+
     nBP*(max(kBP-currentBP*(1+deltaBP/100),0)-cBP)
}


sigmaDM=9
sigmaBP=11
corrDMxBP=0.675
covDMxBP=sigmaDM*sigmaBP*corrDMxBP
#mu
mu.est=c(0,0)
#矩陣
covMatrix=matrix(c(sigmaDM^2,covDMxBP,covDMxBP,sigmaBP^2),nrow=2)
covMatrix

#轉回corr
cov2cor(covMatrix)

S=1000

library(MASS)
set.seed(9527)

ExRate=mvrnorm(S,mu.est,covMatrix)
#約等於0.675
cor(ExRate[,1],ExRate[,2])

CVARq5=rep(0,nrow(put.grid))
#平均收益
muRev=rep(0,nrow(put.grid))
sigRev=rep(0,nrow(put.grid))
#希望大於706
bottomlineRev=706
probtol=rep(0,nrow(put.grid))

#1~81種選擇
#S=1000
for(i in 1:nrow(put.grid)){
    revUS.temp=rep(0,S)
    for(s in 1:S){
       DMtoUS.s=HedgeRevDM(nDM=500,
                  kDM=put.grid[i,1],cDM=put.grid[i,2],
                  deltaDM=ExRate[s,1])
       BPtoUS.s=HedgeRevBP(nBP=500,
                  kBP=put.grid[i,3],cBP=put.grid[i,4],
                  deltaBP=ExRate[s,2])
       revUS.temp[s]=DMtoUS.s+BPtoUS.s
    }
    revUS.temp.q5=quantile(revUS.temp,0.05)
    CVARq5[i]=mean(revUS.temp[which(revUS.temp<revUS.temp.q5)])
    muRev[i]=mean(revUS.temp)
    sigRev[i]=sd(revUS.temp)
    probtol[i]=sum(revUS.temp<bottomlineRev)/S
    print(i)
}

which.max(CVARq5)
which.max(muRev)
which.min(sigRev)
#81個組合裡，各個小於706的機率(越小越好)
which.min(probtol)

plot(probtol,CVARq5)
which(probtol<0.1 & CVARq5>690)
muRev[which(probtol<0.1 & CVARq5>690)]
summary(muRev)

put.grid[which(probtol<0.1 & CVARq5>690),]



##Each option bought for 55.55556 (i.e., 500/9)
##9個都買一樣多
HedgeRevDM.base=function(DMfcst=645,currentDM=0.6513,nDM=500,
                         kDM,cDM,deltaDM){
  DMfcst*currentDM*(1+deltaDM/100)
}
#
HedgeRevDM.opt=function(DMfcst=645,currentDM=0.6513,nDM=500,
                        kDM,cDM,deltaDM){
  nDM*(max(kDM-currentDM*(1+deltaDM/100),0)-cDM)
}
##
HedgeRevBP.base=function(BPfcst=272,currentBP=1.234,nBP=500,
                         kBP,cBP,deltaBP){
  BPfcst*currentBP*(1+deltaBP/100)
}
#
HedgeRevBP.opt=function(BPfcst=272,currentBP=1.234,nBP=500,
                        kBP,cBP,deltaBP){
  nBP*(max(kBP-currentBP*(1+deltaBP/100),0)-cBP)
}
###
revUS.base=rep(0,S)
revUS.opt=matrix(0,nrow=S,ncol=nrow(putDM))
for(s in 1:S){
  DMtoUS.base.s=HedgeRevDM.base(nDM=500/nrow(putDM),
                              kDM=putDM[i,1],cDM=putDM[i,2],
                              deltaDM=ExRate[s,1])
  BPtoUS.base.s=HedgeRevBP.base(nBP=500/nrow(putBP),
                              kBP=putBP[i,1],cBP=putBP[i,2],
                              deltaBP=ExRate[s,2])
  revUS.base[s]=DMtoUS.base.s+BPtoUS.base.s
}
#
for(i in 1:nrow(putDM)){
  revUS.opt.temp=rep(0,S)
  for(s in 1:S){
    DMtoUS.opt.s=HedgeRevDM.opt(nDM=500/nrow(putDM),
                        kDM=putDM[i,1],cDM=putDM[i,2],
                        deltaDM=ExRate[s,1])
    BPtoUS.opt.s=HedgeRevBP.opt(nBP=500/nrow(putBP),
                        kBP=putBP[i,1],cBP=putBP[i,2],
                        deltaBP=ExRate[s,2])
    revUS.opt.temp[s]=DMtoUS.opt.s+BPtoUS.opt.s
  }
  revUS.opt[,i]=revUS.opt.temp
  print(i)
}

revUS.equal=apply(revUS.opt,1,sum)+revUS.base
length(revUS.equal)

temp.q5=quantile(revUS.equal,0.05)
mean(revUS.equal[which(revUS.equal<temp.q5)])
mean(revUS.equal)
#懶人選擇較不好 低於706的機會變高 變成21.4%
sum(revUS.equal<bottomlineRev)/S



##Expand put options
nDM.opt=c(100L,300L,500L)
nBP.opt=c(100L,300L,500L)

n.opt=expand.grid(nDM.opt,nBP.opt)
colnames(n.opt)=c("nDM","nBP")

#1-729(9*81)
put.grid.ii=c()
for(j in 1:nrow(n.opt)){
    put.grid.temp=cbind(put.grid,
                        rep(n.opt[j,1],nrow(put.grid)),
                        rep(n.opt[j,2],nrow(put.grid)))
    put.grid.ii=rbind(put.grid.ii,put.grid.temp)
}


colnames(put.grid.ii)=c(colnames(put.grid),colnames(n.opt))
head(put.grid.ii)

CVARq5=rep(0,nrow(put.grid.ii))
muRev=rep(0,nrow(put.grid.ii))
sigRev=rep(0,nrow(put.grid.ii))
bottomlineRev=706
probtol=rep(0,nrow(put.grid.ii))

for(i in 1:nrow(put.grid.ii)){
  revUS.temp=rep(0,S)
  for(s in 1:S){
    DMtoUS.s=HedgeRevDM(nDM=put.grid.ii[i,5],
                        kDM=put.grid.ii[i,1],
                        cDM=put.grid.ii[i,2],
                        deltaDM=ExRate[s,1])
    #
    BPtoUS.s=HedgeRevBP(nBP=put.grid.ii[i,6],
                        kBP=put.grid.ii[i,3],
                        cBP=put.grid.ii[i,4],
                        deltaBP=ExRate[s,2])
    revUS.temp[s]=DMtoUS.s+BPtoUS.s
  }
  revUS.temp.q5=quantile(revUS.temp,0.05)
  CVARq5[i]=mean(revUS.temp[which(revUS.temp<revUS.temp.q5)])
  muRev[i]=mean(revUS.temp)
  sigRev[i]=sd(revUS.temp)
  probtol[i]=sum(revUS.temp<bottomlineRev)/S
  print(i)
}


which.max(CVARq5)
which.max(muRev)
which.min(sigRev)
which.min(probtol)

options(scipen = 999)
put.grid.ii[417,]
put.grid.ii[81,]


plot(probtol,CVARq5)
which(probtol<0.05 & CVARq5>700)
muRev[which(probtol<0.05 & CVARq5>700)]
summary(muRev)

#最好的買法
put.grid.ii[which(probtol<0.05 & CVARq5>700),]


