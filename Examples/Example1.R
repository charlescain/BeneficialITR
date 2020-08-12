path<- "~/simulations/"
# path<- "C:/Users/Charles Cain/Dropbox/School/Dissertation/Code/"
# path<- "C:/Users/Charles/Dropbox/School/Dissertation/Code/"

.libPaths("~/RPackages")

library('mvtnorm', lib = "~/RPackages")
library('glmnet', lib = "~/RPackages")
library('doParallel', lib = "~/RPackages")
library('MASS', lib = "~/RPackages")
library('randomForest', lib="~/RPackages")

source(paste0(path, "SimFunction_v6.R"))

#modifiable variables ----
M<-10000
vy<- 1
d<- 0.3
dims<- c(1,5,20)
rc<- c(0.2, 0.7)
v<- c(0, 0.02,0.16,0.3)
title<- "site2_a_"

vars<- expand.grid(dims,rc,v,vy,d)
names(vars)<- c("dim", "rc", "nu", "vy", "delta")

vars1<- vars[vars$dim==1,]
vars5<- vars[vars$dim==5,]
vars20<- vars[vars$dim==20,]

write.table(vars, paste0(path, "SituationTwo/inputs_a.txt"))

#finding n/setup ----
# n<- power.tailor(power=0.95, R_c = 0.2, nu=0.16, V_y = 1, delta=0.3, seed = 12345)
n<- 280

b.fun<- function(vect){
  b<- betas.fun(R_c = vect[2], nu = vect[3], V_y = vect[4], delta = vect[5], p=vect[1])
  return(c(b))
}
bs1<- apply(vars1, 1, b.fun)
bs5<- apply(vars5, 1, b.fun)
bs20<- apply(vars20, 1, b.fun)

cr<- 24
cl<- makeCluster(cr)
clusterEvalQ(cl, .libPaths("~/RPackages"))
registerDoParallel(cl)

set.seed(7358514)
dim<- max(dims)
x.valids<- rmvnorm(10000, sigma=diag(dim))

write.table(x.valids, paste0(path, "SituationTwo/x_valids_a.txt"))

#simulation ----
mod.selects<- c("none", "forward", "lasso", "modified", "elastic", "rf")

# Dim 1
res<- list()
sim<-foreach(1:M, .packages = c('mvtnorm', 'glmnet', 'MASS', 'randomForest')) %dopar% {
  for(i in 1:length(bs1)){
    bs.temp<- bs1[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.none<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[1])
    l.forward<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[2]) 
    l.lasso<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[3]) 
    l.mod<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[4]) 
    l.elastic<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[5])
    l.rf<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[6]) 
    res[[paste0("dim", vars1[i,1], "rt", vars1[i,2], "nu", vars1[i,3])]]<- c(l.none, l.forward, l.lasso, l.mod, l.elastic, l.rf)
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

# Dim 5
res<- list()
sim<-foreach(1:M, .packages = c('mvtnorm', 'glmnet', 'MASS', 'randomForest')) %dopar% {
  for(i in 1:length(bs5)){
    bs.temp<- bs5[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.none<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[1])
    l.forward<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[2]) 
    l.lasso<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[3]) 
    l.mod<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[4]) 
    l.elastic<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[5])
    l.rf<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[6]) 
    res[[paste0("dim", vars5[i,1], "rt", vars5[i,2], "nu", vars5[i,3])]]<- c(l.none, l.forward, l.lasso, l.mod, l.elastic, l.rf)
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

# Dim 20
res<- list()
sim<-foreach(1:M, .packages = c('mvtnorm', 'glmnet', 'MASS', 'randomForest')) %dopar% {
  for(i in 1:length(bs20)){
    bs.temp<- bs20[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.none<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[1])
    l.forward<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[2]) 
    l.lasso<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[3]) 
    l.mod<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[4]) 
    l.elastic<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[5])
    l.rf<- one.value(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[6]) 
    res[[paste0("dim", vars20[i,1], "rt", vars20[i,2], "nu", vars20[i,3])]]<- c(l.none, l.forward, l.lasso, l.mod, l.elastic, l.rf)
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
# path<- "C:/Users/Charles Cain/Dropbox/School/Dissertation/"
path<- "C:/Users/Charles/Dropbox/School/Dissertation/"

library(mvtnorm)
library(xtable)

source(paste0(path, "Code/SimFunction_v6.R"))

inputs<- read.table(paste0(path, "Code/SituationTwo/inputs_a.txt"), header = T)
x.valids<- read.table(paste0(path, "Code/SituationTwo/x_valids_a.txt"), header = T)

#changing inputs matrix for updated table
inputs<- inputs[inputs$rt%in%0.2,]
inputs<- inputs[order(inputs$dim),]

n<-280

b.fun<- function(vect){
  b<- betas.fun(R_c = vect[2], nu = vect[3], V_y = vect[4], delta = vect[5], p=vect[1])
  return(c(b))
}
bs<- apply(inputs, 1, b.fun)

alltrt<- lapply(bs, function(x){
  beta2<- x$beta2; beta3<- x$beta3; p<- length(beta3)
  vest<- (beta2 + as.matrix(x.valids[,1:p])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)

#adding in powers
nms<- c("None", "Forward", "LASSO", "Modified", "Elastic", "Random Forest")
tab<- cbind(inputs, 0,0,0,0,0,0)
diff.tab<- cbind(inputs, 0,0,0,0,0,0)
colnames(tab)<- c(colnames(inputs), nms)
colnames(diff.tab)<- c(colnames(inputs), nms)
for(i in 1:dim(inputs)[1]){
  res<- read.table(paste0(path, "SavedSimulations/site2_a_dim", inputs[i,"dim"],
                          "rt", inputs[i,"rt"], "nu", inputs[i,"nu"], ".txt"), header = T)
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

finaltable<- tab[,c("dim", "nu", nms)]

finaltable<- rbind(c("p=1", rep("", dim(finaltable)[2]-2)),finaltable[which(finaltable[,"dim"]%in%1),-1],
                   c("p=5", rep("", dim(finaltable)[2]-2)),finaltable[which(finaltable[,"dim"]%in%5),-1],
                   c("p=20", rep("", dim(finaltable)[2]-2)),finaltable[which(finaltable[,"dim"]%in%20),-1])
rownames(finaltable)<- NULL

print(xtable(finaltable, type="latex", align = c(rep("c", dim(finaltable)[2]+1))), file = paste0(path, "Paper 1/situation2_power_a_v5.tex"),
      include.rownames=FALSE)

diff.table<- diff.tab[,c("dim", "nu", nms)]

diff.table<- rbind(c("p=1", rep("", dim(diff.table)[2]-2)),diff.table[which(diff.table[,"dim"]%in%1),-1],
                   c("p=5", rep("", dim(diff.table)[2]-2)),diff.table[which(diff.table[,"dim"]%in%5),-1],
                   c("p=20", rep("", dim(diff.table)[2]-2)),diff.table[which(diff.table[,"dim"]%in%20),-1])
rownames(diff.table)<- NULL

print(xtable(diff.table, type="latex", align = c(rep("c", dim(diff.table)[2]+1))),
      file = paste0(path, "Paper 1/situation2_dist_a_v4.tex"), include.rownames=FALSE)