path<- "~/simulations/"
# path<- "C:/Users/Charles Cain/Dropbox/School/Dissertation/Code/"
# path<- "C:/Users/Charles/Dropbox/School/Dissertation/Code/"

.libPaths("~/RPackages")

library('mvtnorm', lib = "~/RPackages")
library('glmnet', lib = "~/RPackages")
library('doParallel', lib = "~/RPackages")
library('MASS', lib = "~/RPackages")
library('randomForest', lib="~/RPackages")

source(paste0(path, "SimFunction_v4.R"))

#modifiable variables ----
M<-10000
vy<- 1
d<- 0.3
dims<- c(1,5,20)
rt<- 0.5
v<- 0.16
rho<- c(0, 0.8)
type<- 1:3
type.fun<- function(x){
  if(x==1){
    "single"
  }else if(x==2){
    "even"
  }else if(x==3){
    "diminishing"
  }
}
title<- "sit2_b_"

# vars<- expand.grid(dims,rt,v,vy,d, rho, type)
# names(vars)<- c("dim", "rt", "nu", "vy", "delta", "rho", "type")
# vars<- vars[-1*which(vars$dim==1 & vars$type!=1),]
# 
# write.table(vars, paste0(path, "SituationTwo/inputs_b.txt"))

vars<- read.table(paste0(path, "SituationTwo/inputs_b.txt"))

vars1<- vars[vars$dim==1,]
vars5<- vars[vars$dim==5,]
vars20<- vars[vars$dim==20,]

#finding n/setup ----
# n<- power.tailor(power=0.95, R_t = 0.5, nu=0.16, V_y = 1, delta=0.3, seed = 12345)
n<- 196
b.fun<- function(vect){
  b<- betas.fun(R_t = vect[2], nu = vect[3], V_y = vect[4], delta = vect[5], p=vect[1], sig.p=vect[6]^abs(outer(1:vect[1], 1:vect[1], "-")),
                betas.type = type.fun(vect[7]))
  return(c(b))
}
bs1<- apply(vars1, 1, b.fun)
bs5<- apply(vars5, 1, b.fun)
bs20<- apply(vars20, 1, b.fun)

cr<- 24
cl<- makeCluster(cr)
clusterEvalQ(cl, .libPaths("~/RPackages"))
registerDoParallel(cl)

# set.seed(7358514)
# dim<- max(dims)
# x.valids<- rmvnorm(10000, sigma=diag(dim))
# x.valids_cor<- rmvnorm(10000, sigma=0.8^abs(outer(1:dim, 1:dim, "-")))
# 
# write.table(x.valids, paste0(path, "SituationTwo/x_valids_b.txt"), row.names = F)
# write.table(x.valids_cor, paste0(path, "SituationTwo/x_valids_b_cor.txt"), row.names = F)

x.valids<-do.call(cbind, read.table(paste0(path, "SituationTwo/x_valids_b.txt"), header = T))
x.valids_cor<- do.call(cbind, read.table(paste0(path, "SituationTwo/x_valids_b_cor.txt"), header = T)) 

#simulation ----
mod.selects<- c("none", "forward", "lasso", "modified", "elastic", "rf")

# dim 1
set.seed(12345)
res<- list()
sim<-foreach(1:M, .packages = c('mvtnorm', 'glmnet', 'MASS', 'randomForest')) %dopar% {
  for(i in 1:length(bs1)){
    if(i%in%which(vars1$rho!=0)){
      x.v<- x.valids_cor
    }else{
      x.v<- x.valids
    }
    bs.temp<- bs1[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.none<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[1])
    l.forward<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[2]) 
    l.lasso<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[3]) 
    l.mod<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[4]) 
    l.elastic<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[5])
    l.rf<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[6]) 
    res[[paste0("dim", vars1[i,"dim"], "rho", vars1[i,"rho"], "type", type.fun(vars1[i,"type"]))]]<- c(l.none, l.forward, l.lasso, l.mod, l.elastic, l.rf)
  }
  res
}

result<- do.call(rbind, sim)
by.dim.result<- list()
for(i in colnames(result)){
  new<- do.call(rbind, result[,i])
  colnames(new)<- mod.selects
  by.dim.result[[i]]<- new
}

for(i in names(by.dim.result)){
  write.table(by.dim.result[[i]], paste0(path, "SavedSimulations/", title, i, ".txt"), row.names = F)
}

# dim 5
res<- list()
sim<-foreach(1:M, .packages = c('mvtnorm', 'glmnet', 'MASS', 'randomForest')) %dopar% {
  for(i in 1:length(bs5)){
    if(i%in%which(vars5$rho!=0)){
      x.v<- x.valids_cor
    }else{
      x.v<- x.valids
    }
    bs.temp<- bs5[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.none<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[1])
    l.forward<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[2]) 
    l.lasso<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[3]) 
    l.mod<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[4]) 
    l.elastic<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[5])
    l.rf<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[6]) 
    res[[paste0("dim", vars5[i,"dim"], "rho", vars5[i,"rho"], "type", type.fun(vars5[i,"type"]))]]<- c(l.none, l.forward, l.lasso, l.mod, l.elastic, l.rf)
  }
  res
}

result<- do.call(rbind, sim)
by.dim.result<- list()
for(i in colnames(result)){
  new<- do.call(rbind, result[,i])
  colnames(new)<- mod.selects
  by.dim.result[[i]]<- new
}

for(i in names(by.dim.result)){
  write.table(by.dim.result[[i]], paste0(path, "SavedSimulations/", title, i, ".txt"), row.names = F)
}

# dim 20
res<- list()
sim<-foreach(1:M, .packages = c('mvtnorm', 'glmnet', 'MASS', 'randomForest')) %dopar% {
  for(i in 1:length(bs20)){
    if(i%in%which(vars20$rho!=0)){
      x.v<- x.valids_cor
    }else{
      x.v<- x.valids
    }
    bs.temp<- bs20[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.none<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[1])
    l.forward<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[2]) 
    l.lasso<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[3]) 
    l.mod<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[4]) 
    l.elastic<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[5])
    l.rf<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.v[,1:length(beta3)]), model.selection = mod.selects[6]) 
    res[[paste0("dim", vars20[i,"dim"], "rho", vars20[i,"rho"], "type", type.fun(vars20[i,"type"]))]]<- c(l.none, l.forward, l.lasso, l.mod, l.elastic, l.rf)
  }
  res
}

result<- do.call(rbind, sim)
by.dim.result<- list()
for(i in colnames(result)){
  new<- do.call(rbind, result[,i])
  colnames(new)<- mod.selects
  by.dim.result[[i]]<- new
}

for(i in names(by.dim.result)){
  write.table(by.dim.result[[i]], paste0(path, "SavedSimulations/", title, i, ".txt"), row.names = F)
}

#Results----
# #Simulation b ----
inputs<- read.table(paste0(path, "Code/SituationTwo/inputs_b.txt"), header = T)
x.valids<- read.table(paste0(path, "Code/SituationTwo/x_valids_b.txt"), header = T)
x.valids_cor<- read.table(paste0(path, "Code/SituationTwo/x_valids_b_cor.txt"), header = T)

type.fun<- function(x){
  if(x==1){
    "single"
  }else if(x==2){
    "even"
  }else if(x==3){
    "diminishing"
  }
}

b.fun<- function(vect){
  b<- betas.fun(R_c = vect[2], nu = vect[3], V_y = vect[4], delta = vect[5], p=vect[1], sig.p=vect[6]^abs(outer(1:vect[1], 1:vect[1], "-")),
                betas.type = type.fun(vect[7]))
  return(c(b))
}
bs<- apply(inputs, 1, b.fun)

alltrt<-NULL
for(i in 1:length(bs)){
  x<- bs[[i]]
  beta2<- x$beta2; beta3<- x$beta3; p<- length(beta3)
  if(inputs[i, "rho"]==0){
    x.v<- as.matrix(x.valids[,1:p])
  }else if(inputs[i, "rho"]==0.8){
    x.v<- as.matrix(x.valids_cor[,1:p])
  }
  vest<- (beta2 + x.v%*%beta3)
  alltrt[i]<- mean(vest)
}

#adding in powers
tab<- cbind(inputs, 0,0,0,0,0,0)
colnames(tab)<- c(colnames(inputs), "None", "Forward", "LASSO", "Modified", "Elastic", "Random Forest")
diff.tab<- cbind(inputs, 0,0,0,0,0,0)
colnames(tab)<- c(colnames(inputs), "None", "Forward", "LASSO", "Modified", "Elastic", "Random Forest")
for(i in 1:dim(inputs)[1]){
  res<- read.table(paste0(path, "SavedSimulations/sit2_b_dim", inputs[i,"dim"],
                          "rho", inputs[i,"rho"], "type", type.fun(inputs[i,"type"]), ".txt"), header = T)
  #benefit
  none.temp.benefit<- round(res$none,5)> round(alltrt[i],5)
  forward.temp.benefit<- round(res$forward,5)> round(alltrt[i],5)
  lasso.temp.benefit<- round(res$lasso,5)> round(alltrt[i],5)
  modified.temp.benefit<- round(res$modified,5)> round(alltrt[i],5)
  elastic.temp.benefit<- round(res$elastic,5) > round(alltrt[i],5)
  rf.temp.benefit<- round(res$rf,5) > round(alltrt[i],5)
  #harm
  none.temp.harm<- round(res$none,5)< round(alltrt[i],5)
  forward.temp.harm<- round(res$forward,5)< round(alltrt[i],5)
  lasso.temp.harm<- round(res$lasso,5)< round(alltrt[i],5)
  modified.temp.harm<- round(res$modified,5) < round(alltrt[i],5)
  elastic.temp.harm<- round(res$elastic,5) < round(alltrt[i],5)
  rf.temp.harm<- round(res$rf,5) < round(alltrt[i],5)
  #combining the two
  none.temp<- paste0(format(round(mean(none.temp.benefit),2),nsmall=2), "/", format(round(mean(none.temp.harm),2),nsmall=2))
  forward.temp<- paste0(format(round(mean(forward.temp.benefit),2),nsmall=2), "/", format(round(mean(forward.temp.harm),2),nsmall=2))
  lasso.temp<- paste0(format(round(mean(lasso.temp.benefit),2),nsmall=2), "/", format(round(mean(lasso.temp.harm),2),nsmall=2))
  modified.temp<- paste0(format(round(mean(modified.temp.benefit),2),nsmall=2), "/", format(round(mean(modified.temp.harm),2),nsmall=2))
  elastic.temp<- paste0(format(round(mean(elastic.temp.benefit),2),nsmall=2), "/", format(round(mean(elastic.temp.harm),2),nsmall=2))
  rf.temp<- paste0(format(round(mean(rf.temp.benefit),2),nsmall=2), "/", format(round(mean(rf.temp.harm),2),nsmall=2))
  #mean
  tab[i,"None"]<- none.temp
  tab[i, "Forward"]<- forward.temp
  tab[i, "LASSO"]<- lasso.temp
  tab[i, "Modified"]<- modified.temp
  tab[i, "Elastic"]<- elastic.temp
  tab[i, "Random Forest"]<- rf.temp
  #difference between v(d) - d(w)
  none.temp.diff<- res$none - alltrt[i]
  forward.temp.diff<- res$forward - alltrt[i]
  lasso.temp.diff<- res$lasso - alltrt[i]
  modified.temp.diff<- res$modified - alltrt[i]
  elastic.temp.diff<- res$elastic  - alltrt[i]
  rf.temp.diff<- res$rf  - alltrt[i]
  diff.tab[i,"None"]<- round(mean(none.temp.diff),3)
  diff.tab[i, "Forward"]<- round(mean(forward.temp.diff),3)
  diff.tab[i, "LASSO"]<- round(mean(lasso.temp.diff),3)
  diff.tab[i, "Modified"]<- round(mean(modified.temp.diff),3)
  diff.tab[i, "Elastic"]<- round(mean(elastic.temp.diff),3)
  diff.tab[i, "Random Forest"]<- round(mean(rf.temp.diff),3)
}

finaltable<- tab[,c("type", "rho", "dim", "None", "Forward", "LASSO", "Modified", "Elastic", "Random Forest")]

finaltable<- rbind(c("Single predictor with non-zero coefficient", rep("", dim(finaltable)[2]-2)), finaltable[which(finaltable[,"type"]%in%1),-1],
                   c("All predictor with non-zero and equal coefficients", rep("", dim(finaltable)[2]-2)), finaltable[which(finaltable[,"type"]%in%2),-1],
                   c("All predictor with non-zero and diminishing coefficients", rep("", dim(finaltable)[2]-2)), finaltable[which(finaltable[,"type"]%in%3),-1])

rownames(finaltable)<- NULL

print(xtable(finaltable, type="latex", align = c(rep("c", dim(finaltable)[2]+1))), file = paste0(path, "Paper 1/situation2_power_b_v4.tex"),
      include.rownames=FALSE)

finaltable<- diff.tab[,c("type", "rho", "dim", "None", "Forward", "LASSO", "Modified", "Elastic", "Random Forest")]
rownames(finaltable)<- NULL
finaltable$type<- sapply(finaltable$type, type.fun)

print(xtable(finaltable, type="latex", align = c(rep("c", dim(finaltable)[2]+1))), file = paste0(path, "Paper 1/situation2_power_b_diff.tex"),
      include.rownames=FALSE)

#for presentation
print(xtable(finaltable[,c(1:3,5,7)], type="latex", align = c(rep("c", 6))), file = paste0(path, "Paper 1/situation2_power_b_v3_pres.tex"),
      include.rownames=FALSE)
