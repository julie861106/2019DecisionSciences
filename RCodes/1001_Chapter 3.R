#抽樣
x = rbinom(10000, 50, 1/50)
x
table(x)
sum(x<=3)/10000

pbinom(3,50,1/50)

##S: The number of simulation runs
S=10000

set.seed(5566)

#Parameters of time for underwriting
mu1=150
sig1=30
#Parameters of time for rating
mu2=75
sig2=25

Time1=rnorm(S,mu1,sig1)
Time2=rnorm(S,mu2,sig2)

TotTime=Time1+Time2
hist(TotTime)
var(TotTime)

sum(TotTime<=180)/S
#What is the theoretical probability

quantile(TotTime,0.95)
#What is the theoretical percentile value?

S=10000

#ch3 p6
#Random processing time of cases 1-3 for underwriting
Time11=rnorm(S,mu1,sig1)
Time12=rnorm(S,mu1,sig1)
Time13=rnorm(S,mu1,sig1)
#Random processing time of cases 1-3 for rating
Time21=rnorm(S,mu2,sig2)
Time22=rnorm(S,mu2,sig2)
Time23=rnorm(S,mu2,sig2)

##Assuming rating must wait for underwriting!
#Beginning time for the 2nd case of rating
#假設要等第一個人做完兩件事or自己完成第一件事的時間
#第二個人處理第二個case的時間
BeginTime22=c()
for(s in 1:S){
    BeginTime22[s]=max(Time11[s]+Time21[s],
                       Time11[s]+Time12[s])
}

#Beginning time for the 3rd case of rating
BeginTime23=c()
for(s in 1:S){
  BeginTime23[s]=max(Time11[s]+Time12[s]+Time13[s],
                     BeginTime22[s]+Time22[s])
}

#Ending time for the 3rd case of rating
EndTime=BeginTime23+Time23

summary(EndTime)
sum(EndTime<=480)/S




#製作矩陣
##Assuming X & Y are NOT independent of each other
corrXY=0.37
varcovMatrix=matrix(c(sig1^2,sig1*sig2*corrXY,sig1*sig2*corrXY,sig2^2),
                    nrow=2,ncol=2)
varcovMatrix

library(MASS)
mvrnorm(10,mu=c(mu1,mu2),Sigma=varcovMatrix)
xy=mvrnorm(10000,mu=c(mu1,mu2),Sigma=varcovMatrix)
cor(xy[,1],xy[,2])

S=10000

Time1.corr=mvrnorm(S,mu=c(mu1,mu2),Sigma=varcovMatrix)
Time2.corr=mvrnorm(S,mu=c(mu1,mu2),Sigma=varcovMatrix)
Time3.corr=mvrnorm(S,mu=c(mu1,mu2),Sigma=varcovMatrix)
#Random processing time of cases 1-3 for underwriting
Time11.corr=Time1.corr[,1]
Time12.corr=Time2.corr[,1]
Time13.corr=Time3.corr[,1]
#Random processing time of cases 1-3 for rating
Time21.corr=Time1.corr[,2]
Time22.corr=Time2.corr[,2]
Time23.corr=Time3.corr[,2]

#Beginning time for the 2nd case of rating
BeginTime22.corr=c()
for(s in 1:S){
  BeginTime22.corr[s]=max(Time11.corr[s]+Time21.corr[s],
                          Time11.corr[s]+Time12.corr[s])
}

#Beginning time for the 3rd case of rating
BeginTime23.corr=c()
for(s in 1:S){
  BeginTime23.corr[s]=max(Time11.corr[s]+Time12.corr[s]+Time13.corr[s],
                          BeginTime22.corr[s]+Time22.corr[s])
}

#Ending time for the 3rd case of rating
EndTime.corr=BeginTime23.corr+Time23.corr

summary(EndTime.corr)

FinishTime1.corr=Time11.corr+Time21.corr
sum(FinishTime1.corr<=180)/S

quantile(FinishTime1.corr,0.95)

sum(EndTime.corr<=480)/S





##Estimating correlations/covariances from data
Fund1=c(65, 79, 85, 78, 107, 108, 124, 156, 195, 181, 216)
Fund2=c(47, 61, 73, 60, 89, 86, 104, 120, 140, 134, 175)
Fund3=c(38, 37, 39, 40, 47, 46, 57, 71, 74, 72, 87)
Fund4=c(61, 64, 74, 72, 95, 89, 114, 147, 146, 127, 152)

AnnualGR=matrix(0,nrow=(length(Fund1)-1),ncol=4)

AnnualGR

for(i in 2:length(Fund1)){
    AnnualGR[i-1,1]=Fund1[i]/Fund1[i-1]
    AnnualGR[i-1,2]=Fund2[i]/Fund2[i-1]
    AnnualGR[i-1,3]=Fund3[i]/Fund3[i-1]
    AnnualGR[i-1,4]=Fund4[i]/Fund4[i-1]
}

AnnualGR
#cov共變異 做矩陣
Sigma.est=cov(AnnualGR)
Sigma.est

#矩陣相關係數corr
cov2cor(Sigma.est)

mu.est=c(mean(AnnualGR[,1]),mean(AnnualGR[,2]),
         mean(AnnualGR[,3]),mean(AnnualGR[,4]))
mu.est

#import MASS
library(MASS)
MVN.AGR=mvrnorm(50,mu.est,Sigma.est)

Fund1.MVN=MVN.AGR[,1]
Fund2.MVN=MVN.AGR[,2]

#Ignore dependencies
Fund1.N=rnorm(50,mean(AnnualGR[,1]),sd(AnnualGR[,1]))
Fund2.N=rnorm(50,mean(AnnualGR[,2]),sd(AnnualGR[,2]))


x11(width=18,height=5)
par(mfrow=c(1,3))
plot(AnnualGR[,1],AnnualGR[,2],type='p',pch=1,lwd=5)
#有相關
plot(Fund1.MVN,Fund2.MVN,type='p',pch=2,lwd=4,col='red')
#自己抽自己的
plot(Fund1.N,Fund2.N,type='p',pch=3,lwd=4,col='green')





