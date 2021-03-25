library(bayesm)
data(margarine)
choiceprice<-data.frame(margarine$choicePrice)
demos<-data.frame(margarine$demos)

#================================
# codes for multinomial choices 
#================================

######Exercise 1
###basic clean
alt<-data.frame(colnames(choiceprice)[3:12])
alt$id<-1:10
colnames(alt)[1]<-"mode"
choiceprice<-merge(choiceprice,alt,by.x="choice",by.y="id")
colnames(choiceprice)[3:12] <- paste("price", colnames(choiceprice)[3:12], sep = ".")
all_data<-merge(choiceprice,demos,by="hhid")

####get mean and sd
A2_e1_1<-t(sapply(choiceprice[,3:12], function(x) c( "Stand dev" = sd(x,na.rm=TRUE), "Mean"= mean(x,na.rm=TRUE))))
quant<-t(data.frame(apply(choiceprice[,3:12] , 2 , quantile, na.rm = TRUE )))
A2_e1_1<-merge(A2_e1_1,quant,by="row.names")
kable(A2_e1_1, format = "latex", caption = "mean and dispertion")

####market share
A2_e1_2<-ddply(choiceprice,~mode,summarise,count=length(mode))
A2_e1_2$share<-A2_e1_2$count/sum(A2_e1_2$count)*100
kable(A2_e1_2, format = "latex", caption = "market share")

choiceprice2<-data.frame(margarine$choicePrice)
choiceprice2<-merge(choiceprice2,alt,by.x="choice",by.y="id")
A2_e1_mean<-data.frame(apply(choiceprice2[,3:12],2, mean , na.rm = TRUE ))
colnames(A2_e1_mean)<-"price_mean"
A2_e1_mean$mode<-rownames(A2_e1_mean)
choiceprice2<-merge(choiceprice2,A2_e1_mean, by="mode")
for (i in 1:nrow(choiceprice2)){
  if (choiceprice2[i,choiceprice2$choice[i]+3]<=choiceprice2[i,"price_mean"]){
    choiceprice2$class[i]="below or equal to average"
  }
  else{
    choiceprice2$class[i]="over average"
  }
}
A2_e1_3<-ddply(choiceprice2,.(mode,class),summarise,count=length(mode))
A2_e1_3$share<-A2_e1_3$count/sum(A2_e1_2$count)*100
A2_e1_mean_2<-round(data.frame(A2_e1_mean[,"price_mean"]),4)
rownames(A2_e1_mean_2)<-rownames(A2_e1_mean)
colnames(A2_e1_mean_2)[1]<-"average"
kable(t(A2_e1_mean_2), format = "latex", caption = "Average price by choice")
kable(A2_e1_3, format = "latex", caption = "Market Share by Product Characteristics")

###mapping between the observed attributes and choices
####get quantile of income
library(gtools)
all_data$Income_quant<-quantcut(all_data$Income, q=3, na.rm=TRUE)  
map_income <- data.frame(ddply(all_data,.(mode,Income_quant),summarise,count=length(mode)))
library(ggplot2)
ggplot(data=map_income, aes(x=Income_quant, y=count, fill=mode)) +
  geom_bar(stat="identity", position=position_dodge())+ theme(legend.position="top")+xlab("income quantile")

for (i in 1:nrow(all_data)){
  if (all_data$Fs3_4[i]==1) {
    all_data$fam_s[i]<-"median"
  }
  if (all_data$Fs5.[i]==1) {
    all_data$fam_s[i]<-"large"
  }
  if (all_data$Fs5.[i]==0 & all_data$Fs3_4[i]==0) {
    all_data$fam_s[i]<-"small"
  }
  
}
map_family <- data.frame(ddply(all_data,.(mode,fam_s),summarise,count=length(mode)))
colnames(map_family)[2]<-"family_size"
ggplot(data = map_family, aes(x=family_size, y=mode, fill=count)) + 
  geom_tile()
ggplot(data=map_family, aes(x=family_size, y=count, fill=mode)) +
  geom_bar(stat="identity", position=position_dodge())+ theme(legend.position="top")+xlab("family size")


map_coll <- data.frame(ddply(all_data,.(mode,college),summarise,count=length(mode)))
ggplot(data=map_coll, aes(x=college, y=count, fill=mode)) +
  geom_bar(stat="identity", position=position_dodge())+ theme(legend.position="top")+xlab("college")
kable(map_coll, format = "latex", caption = "transactions by choice and eduaction status")

map_wcollar <- data.frame(ddply(all_data,.(mode,whtcollar),summarise,count=length(mode)))
ggplot(data=map_wcollar, aes(x=whtcollar, y=count, fill=mode)) +
  geom_bar(stat="identity", position=position_dodge())+ theme(legend.position="top")+xlab("white collar")
kable(map_wcollar, format = "latex", caption = "transactions by choice and job status")

map_retired <- data.frame(ddply(all_data,.(mode,retired),summarise,count=length(mode)))
ggplot(data=map_retired, aes(x=retired, y=count, fill=mode)) +
  geom_bar(stat="identity", position=position_dodge())+ theme(legend.position="top")+xlab("retired")
kable(map_retired, format = "latex", caption = "transactions by choice and retirement status")

######Exercise 2
####obtain results using mlogit function
price<-mlogit.data(choiceprice, varying = 3:12, shape = "wide", choice = "mode")
A2_check<-mlogit(mode ~ price , data = price)
A2_c_coe<-A2_check$coefficients
summary(A2_check)
####likelihood and optimization
like_fun = function(param,choiceprice)
{
  ni = nrow(choiceprice)
  nj = length(unique(choiceprice$choice))
  probij = mat.or.vec(ni,nj)
  probij[,2]=param[nj]*choiceprice[,4]
  probij[,1]=param[1]+param[nj]*choiceprice[,3]
  for (j in c(3:nj)){
    probij[,j]=param[j-1]+param[nj]*choiceprice[,j+2]
  }
  
  prob   = exp(probij)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choiceprice$choice[i]]
  }
  
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}
npar=10
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar) 
A2_re     = nloptr(start,eval_f=like_fun,lb=lower,ub=upper,
                 opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
                 choiceprice=choiceprice)  
coeff_A2<-data.frame(A2_re$solution)
A2_price <- paste("(Intercept)", alt$mode[c(1,3:10)], sep = ":")
rownames(coeff_A2)<-rbind(as.matrix(A2_price),"price")
coeff_A2$name<-rownames(coeff_A2)
c_a2<-data.frame(summary(A2_check)$coefficients)
c_a2$name<-rownames(c_a2)
coeff_A2<-merge(coeff_A2,c_a2,by="name")
colnames(coeff_A2)<-c("variable_name","own:","mlogit:")
kable(coeff_A2, format = "latex", caption = "Conditional Logit Model Results")
####marginal effect
mlogit_eff<-effects(A2_check,covariate = "price")

prob_fun = function(param,choiceprice){
  ni = nrow(choiceprice)
  nj = length(unique(choiceprice$choice))
  probij = mat.or.vec(ni,nj)
  probij[,2]=param[nj]*choiceprice[,4]
  probij[,1]=param[1]+param[nj]*choiceprice[,3]
  for (j in c(3:nj)){
    probij[,j]=param[j-1]+param[nj]*choiceprice[,j+2]
  }
  
  prob   = exp(probij)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
    return(prob)
}
prob_A2<-prob_fun(A2_re$solution,choiceprice)
  
me <- array(0, dim = c(ncol(prob_A2), ncol(prob_A2), nrow(prob_A2)))
theta=array(0, dim = c(ncol(prob_A2), ncol(prob_A2), nrow(prob_A2)))
for (i in 1: nrow(prob_A2)){
  for (j in 1:ncol(prob_A2)){
    for (k in 1:ncol(prob_A2)){
      if (j==k){
        theta[j,k,i]=1
      }
      else
        theta[j,k,i]=0
      
    }
  }
}
beta_A2<-A2_re$solution[10]
for (i in 1: nrow(prob_A2)){
  for (j in 1:ncol(prob_A2)){
    for (k in 1:ncol(prob_A2)){
      me[j,k,i]=prob_A2[i,j]*(theta[j,k,i]-prob_A2[i,k])*beta_A2
    }
  }
}
dimnames(me)<-list(alt$mode,alt$mode,c(1:nrow(prob_A2)))
me_mean<-round(apply(me,c(1,2),mean),4)
kable(me_mean, format = "latex", caption = "Marginal Effects for Model 1 - Conditional Logit")


######Exercise 3
all<-mlogit.data(all_data, varying = 3:12, shape = "wide", choice = "mode")
A3_check<-mlogit(mode ~ 0 | Income, data = all)
summary(mlogit(mode ~ 0 | Income, data = all))

#####likelihood and optimization
like_fun2 = function(param,choiceprice)
{
  ni = nrow(choiceprice)
  nj = length(unique(choiceprice$choice))
  probij = mat.or.vec(ni,nj)
  in_p<-param[nj:18]
  probij[,2]=0
  probij[,1]=param[1]+in_p[1]*choiceprice[,"Income"]
  for (j in c(3:nj)){
    probij[,j]=param[j-1]+in_p[j-1]*choiceprice[,"Income"]
  }
  
  prob   = exp(probij)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    
    probc[i] = prob[i,choiceprice$choice[i]]
  }
  
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}
npar=18
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = rep (0,npar)
A3_re     = nloptr(start,eval_f=like_fun2,lb=lower,ub=upper,
                   opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
                   choiceprice=all_data)  

coeff_A3<-data.frame(A3_re$solution)
A3_income <- paste("(Intercept)", alt$mode[c(1,3:10)], sep = ":")
A3_income2<-paste("Income", alt$mode[c(1,3:10)], sep = ":")
rownames(coeff_A3)<-rbind(as.matrix(A3_income),as.matrix(A3_income2))
coeff_A3$name<-rownames(coeff_A3)
c_a3<-data.frame(summary(A3_check)$coefficients)
c_a3$name<-rownames(c_a3)
coeff_A3<-merge(coeff_A3,c_a3,by="name")
colnames(coeff_A3)<-c("variable_name","own:","mlogit:")
kable(coeff_A3, format = "latex", caption = "Multinomial Logit Model Results")

#####marginal effect——get predicted probability
prob_fun2 = function(param,choiceprice){
  ni = nrow(choiceprice)
  nj = length(unique(choiceprice$choice))
  probij = mat.or.vec(ni,nj)
  in_p<-param[nj:18]
  probij[,2]=0
  probij[,1]=param[1]+in_p[1]*choiceprice[,"Income"]
  for (j in c(3:nj)){
    probij[,j]=param[j-1]+in_p[j-1]*choiceprice[,"Income"]
  }
  
  prob   = exp(probij)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  return(prob)
}
prob_A3<-prob_fun2(A3_re$solution,all_data)


####marginal effect
####compute bata_hat
beta_A3<-rep(0,10)
beta_A3[1]<-A3_re$solution[10]
beta_A3[2]<-0
beta_A3[3:10]<-A3_re$solution[11:18]
beta_A3<-as.matrix(beta_A3)
beta_hat<-prob_A3%*%beta_A3
me_A3<-matrix(0,nrow(prob_A3),ncol(prob_A3))
for (i in 1:nrow(prob_A3)){
  for (j in 1:ncol(prob_A3)){
    me_A3[i,j]<-prob_A3[i,j]*(beta_A3[j]-beta_hat[i])
  }
}
me_mean_A3<-data.frame(apply(me_A3,2,mean))
me_mean_A3$name<-alt$mode
effects_mlogit<-data.frame(effects(A3_check,covariate = "Income"))
effects_mlogit$name<-rownames(effects_mlogit)
me_mean_A3<-merge(me_mean_A3,effects_mlogit,by="name")
colnames(me_mean_A3)[2:3]<-c("own:","mlogit:")
kable(me_mean_A3, format = "latex", caption = "Marginal Effects for Model 2 - Multinomial Logit")

#####the mixed logit setting
A5_check<-mlogit(mode ~ price | Income, data = all)
summary(mlogit(mode ~ price | Income, data = all))

#####likelihood and optimization
like_fun3 = function(param,choiceprice,drop)
{
  ni = as.numeric(nrow(choiceprice))
  nj = as.numeric(length(unique(choiceprice$choice)))
  probij = mat.or.vec(ni,nj)
  nj2<-2*(nj-1)
  in_p<-param[nj:nj2]
  probij[,2]=param[nj2+1]*choiceprice[,4]
  probij[,1]=param[1]+in_p[1]*choiceprice[,"Income"]+param[nj2+1]*choiceprice[,3]
  for (j in c(3:nj)){
    probij[,j]=param[j-1]+in_p[j-1]*choiceprice[,"Income"]+param[nj2+1]*choiceprice[,j+2]
  }
  prob   = exp(probij)         # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,as.numeric(choiceprice$choice[i])-drop]
  }
  
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}
npar=19
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = rep (0,npar)
A5_re     = nloptr(start,eval_f=like_fun3,lb=lower,ub=upper,
                   opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
                   choiceprice=all_data,drop=0)  
rm(coeff_A5)
coeff_A5<-data.frame(A5_re$solution)
A5_income <- paste("(Intercept)", alt$mode[c(1,3:10)], sep = ":")
A5_income2<-paste("Income", alt$mode[c(1,3:10)], sep = ":")
rownames(coeff_A5)<-rbind(as.matrix(A5_income),as.matrix(A5_income2),"price")
coeff_A5$name<-rownames(coeff_A5)
c_a5<-data.frame(summary(A5_check)$coefficients)
c_a5$name<-rownames(c_a5)
coeff_A5<-merge(coeff_A5,c_a5,by="name")
colnames(coeff_A5)<-c("variable_name","own:","mlogit:")
kable(coeff_A5, format = "latex", caption = "Mixed Logit Model Results")

#####the mixed logit setting _ drop one choice from data
alt2<-alt[-1,]
A5_check2<-mlogit(mode ~ price | Income, data = all, alt.subset =alt2)
hmftest(A5_check,A5_check2)

like_fun4 = function(param,choiceprice,drop)
{
  ni = as.numeric(nrow(choiceprice))
  nj = as.numeric(length(unique(choiceprice$choice)))
  probij = mat.or.vec(ni,nj)
  nj2<-2*(nj-1)
  in_p<-param[nj:nj2]
  probij[,1]=param[nj2+1]*choiceprice[,4]
  for (j in c(2:nj)){
    probij[,j]=param[j-1]+in_p[j-1]*choiceprice[,"Income"]+param[nj2+1]*choiceprice[,j+3]
  }
  prob   = exp(probij)         # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,as.numeric(choiceprice$choice[i])-drop]
  }
  
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

all_data2<-all_data[!all_data$choice==1,]

npar=17
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = rep(0,npar)
A5_re2 = nloptr(start,eval_f=like_fun4,lb=lower,ub=upper,
                   opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
                   choiceprice=all_data2,drop=1) 

coeff_A52<-data.frame(A5_re2$solution)
A5_income2 <- paste("(Intercept)", alt2$mode[c(2:9)], sep = ":")
A5_income22<-paste("Income", alt2$mode[c(2:9)], sep = ":")
rownames(coeff_A52)<-rbind(as.matrix(A5_income2),as.matrix(A5_income22),"price")
coeff_A52$name<-rownames(coeff_A52)
c_a52<-data.frame(summary(A5_check2)$coefficients)
c_a52$name<-rownames(c_a52)
coeff_A52<-merge(coeff_A52,c_a52,by="name")
colnames(coeff_A52)<-c("variable_name","own:","mlogit:")
kable(coeff_A52, format = "latex", caption = "Mixed Logit Model Results - drop PPk_Stk")

####compute MTT
A5_s<-A5_re$solution[c(-1,-10)]
A5_Lr<-like_fun4(A5_s,all_data2,1)
loglikelihood<-data.frame(-A5_Lr,-A5_re2$objective)
colnames(loglikelihood)<-c("beta1","beta2")
rownames(loglikelihood)<-"log likelihood"
kable(loglikelihood, format = "latex", caption = "Log Likelihood")

MTT<--2*(-A5_Lr+A5_re2$objective)
pchisq(MTT, df=17, lower.tail=FALSE)
qchisq(.05, df=17, lower.tail=FALSE)


