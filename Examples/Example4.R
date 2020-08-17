#set up ----
path<- "~/simulations/"
# path<- "C:/Users/Charles Cain/Dropbox/School/Dissertation/Code/"
# path<- "C:/Users/Charles/Dropbox/School/Dissertation/Code/"

.libPaths("~/RPackages")

library('mvtnorm', lib = "~/RPackages")
library('glmnet', lib = "~/RPackages")
library('doParallel', lib = "~/RPackages")

source(paste0(path, "BeneficialITR.R"))

cr<- 24
cl<- makeCluster(cr)
clusterEvalQ(cl, .libPaths("~/RPackages"))
registerDoParallel(cl)

#modifiable variables----
M<- 10000
v_y<-1
delta<- 0.3
r_c<- 0.1
nu<- 0.16

# Achieve Main ----
title<- "sit1_achievemain_lasso_"

# Running Simulation ----
alpha<- 0.05
power<- 0.9
dims<- c(1,2, seq(5,20, by=5))
bs<- lapply(dims, function(x){betas.fun(delta = delta, V_y = v_y, R_c = r_c, nu=nu, p=x)})
v<- 4*v_y
n<- ceiling((qnorm(1-alpha/2, 0, 1) + qnorm(power, 0,1))^2 * v/(delta)^2 /2)*2
dim<- tail(dims, n=1)
H<- abs(outer(1:dim, 1:dim, "-"))
V<- 0^H

set.seed(72818)
x.valids<- do.call(cbind, read.table(paste0(path, "SituationOne/x.valids.txt")))

res<- list()
sim<- foreach(1:M, .packages = c('mvtnorm', 'glmnet')) %dopar%{
  for(i in 1:length(bs)){
    bs.temp<- bs[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.min<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "lasso") 
    l.1se<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "lasso", lasso.penalty = "1se") 
    l.new<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "modified") 
    res[[paste0("dim", dims[i])]]<- c(l.min, l.1se, l.new)
  }
  res
}

result<- do.call(rbind, sim)
by.dim.result<- list()
for(i in colnames(result)){
  new<- do.call(rbind, result[,i])
  colnames(new)<- c("min", "se1", "new")
  by.dim.result[[i]]<- new
}

for(i in names(by.dim.result)){
  write.table(by.dim.result[[i]], paste0(path, "SavedSimulations/", title, i, ".txt"), row.names = F)
}

# Under Achieve Main ----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- bs[[i]]$beta2/2
}
title<- "sit1_undermain_lasso_"

res<- list()
sim<- foreach(1:M, .packages = c('mvtnorm', 'glmnet')) %dopar%{
  for(i in 1:length(bs)){
    bs.temp<- bs[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.min<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "lasso") 
    l.1se<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "lasso", lasso.penalty = "1se") 
    l.new<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "modified") 
    res[[paste0("dim", dims[i])]]<- c(l.min, l.1se, l.new)
  }
  res
}

result<- do.call(rbind, sim)
by.dim.result<- list()
for(i in colnames(result)){
  new<- do.call(rbind, result[,i])
  colnames(new)<- c("min", "se1", "new")
  by.dim.result[[i]]<- new
}

for(i in names(by.dim.result)){
  write.table(by.dim.result[[i]], paste0(path, "SavedSimulations/", title, i, ".txt"), row.names = F)
}

# No Main ----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- 0
}
title<- "sit1_nomain_lasso_"

res<- list()
sim<- foreach(1:M, .packages = c('mvtnorm', 'glmnet')) %dopar%{
  for(i in 1:length(bs)){
    bs.temp<- bs[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.min<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "lasso") 
    l.1se<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "lasso", lasso.penalty = "1se") 
    l.new<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "modified") 
    res[[paste0("dim", dims[i])]]<- c(l.min, l.1se, l.new)
  }
  res
}

result<- do.call(rbind, sim)
by.dim.result<- list()
for(i in colnames(result)){
  new<- do.call(rbind, result[,i])
  colnames(new)<- c("min", "se1", "new")
  by.dim.result[[i]]<- new
}

for(i in names(by.dim.result)){
  write.table(by.dim.result[[i]], paste0(path, "SavedSimulations/", title, i, ".txt"), row.names = F)
}

# Null result new ----
delta
nu<- 0
bs<- lapply(dims, function(x){betas.fun(delta = delta, V_y = v_y, R_c = r_c, nu=nu, p=x)})

title<- "sit1_null_lasso_"

res<- list()
sim<- foreach(1:M, .packages = c('mvtnorm', 'glmnet')) %dopar%{
  for(i in 1:length(bs)){
    bs.temp<- bs[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.min<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "lasso") 
    l.1se<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "lasso", lasso.penalty = "1se") 
    l.new<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = "modified") 
    res[[paste0("dim", dims[i])]]<- c(l.min, l.1se, l.new)
  }
  res
}

result<- do.call(rbind, sim)
by.dim.result<- list()
for(i in colnames(result)){
  new<- do.call(rbind, result[,i])
  colnames(new)<- c("min", "se1", "new")
  by.dim.result[[i]]<- new
}

for(i in names(by.dim.result)){
  write.table(by.dim.result[[i]], paste0(path, "SavedSimulations/", title, i, ".txt"), row.names = F)
}

#Results----
path<- "C:/Users/Charles/Dropbox/School/Dissertation/"
# path<- "C:/Users/Charles Cain/Dropbox/School/Dissertation/"
# path<- "~/simulations/"

library(MASS)
library(ggplot2)
library(gridExtra)
library(grid)
library(mvtnorm)
library(tidyr)
library(glmnet)
library(ggpubr)
library(doParallel)
library(randomForest)

source(paste0(path, "Code/SimFunction_v4.R"))

# Setting Crucial Values (uncorrelated) ----
M<- 10000
v_y<-1
delta<- 0.3
r_c<- 0.1
nu<- 0.16

dims<- c(1,2, seq(5,20, by=5))
bs<- lapply(dims, function(x){betas.fun(delta = delta, V_y = v_y, R_c = r_c, nu=nu, p=x)})
dim<- tail(dims, n=1)
H<- abs(outer(1:dim, 1:dim, "-"))
V<- 0^H
set.seed(72818)
x.valids<- do.call(cbind, read.table(paste0(path, "Code/SituationOne/x.valids.txt")))

#reading data----
null<- list()
nomain<- list()
under<- list()
achieve<- list()
for(i in dims){
  null[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_null_lasso_dim", i, ".txt"), header = T)
  nomain[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_nomain_lasso_dim", i, ".txt"), header = T)
  under[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_undermain_lasso_dim", i, ".txt"), header = T)
  achieve[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_achievemain_lasso_dim", i, ".txt"), header = T)
}

#achieve.main----
alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
names(alltrt)<- paste0("dim", dims)

tab<- NULL
diff.tab<- NULL
for(i in dims){
  temp.tab<- achieve[[paste0("dim",i)]]
  power.new<- sapply(1:dim(temp.tab)[2], function(x){mean(round(temp.tab[,x],5)>round(alltrt[paste0("dim",i)],5))})
  power.new<- cbind(i, power.new, colnames(temp.tab))
  diff.new<- sapply(1:dim(temp.tab)[2], function(x){mean(temp.tab[,x]-alltrt[paste0("dim",i)])})
  diff.new<- cbind(i, diff.new, colnames(temp.tab))
  tab<- rbind(tab, power.new)
  diff.tab<- rbind(diff.tab, diff.new)
}

achieve.power<- data.frame(cbind(tab, "achievemain"))
names(achieve.power)<- c("dims", "power", "sim", "type")

achieve.diff<- data.frame(cbind(diff.tab, "achievemain"))
names(achieve.diff)<- c("dims", "diff", "sim", "type")

#underachieve.main----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- bs[[i]]$beta2/2
}

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
names(alltrt)<- paste0("dim", dims)

tab<- NULL
diff.tab<- NULL
for(i in dims){
  temp.tab<- under[[paste0("dim",i)]]
  power.new<- sapply(1:dim(temp.tab)[2], function(x){mean(round(temp.tab[,x],5)>round(alltrt[paste0("dim",i)],5))})
  power.new<- cbind(i, power.new, colnames(temp.tab))
  diff.new<- sapply(1:dim(temp.tab)[2], function(x){mean(temp.tab[,x]-alltrt[paste0("dim",i)])})
  diff.new<- cbind(i, diff.new, colnames(temp.tab))
  tab<- rbind(tab, power.new)
  diff.tab<- rbind(diff.tab, diff.new)
}

under.power<- data.frame(cbind(tab, "undermain"))
names(under.power)<- c("dims", "power", "sim", "type")

under.diff<- data.frame(cbind(diff.tab, "undermain"))
names(under.diff)<- c("dims", "diff", "sim", "type")

#no.main----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- 0
}

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
names(alltrt)<- paste0("dim", dims)

tab<- NULL
diff.tab<- NULL
for(i in dims){
  temp.tab<- nomain[[paste0("dim",i)]]
  power.new<- sapply(1:dim(temp.tab)[2], function(x){mean(round(temp.tab[,x],5)>round(alltrt[paste0("dim",i)],5))})
  power.new<- cbind(i, power.new, colnames(temp.tab))
  diff.new<- sapply(1:dim(temp.tab)[2], function(x){mean(temp.tab[,x]-alltrt[paste0("dim",i)])})
  diff.new<- cbind(i, diff.new, colnames(temp.tab))
  tab<- rbind(tab, power.new)
  diff.tab<- rbind(diff.tab, diff.new)
}
nomain.power<- data.frame(cbind(tab, "nomain"))
names(nomain.power)<- c("dims", "power", "sim", "type")

nomain.diff<- data.frame(cbind(diff.tab, "nomain"))
names(nomain.diff)<- c("dims", "diff", "sim", "type")

#Null----
bs<- lapply(dims, function(x){betas.fun(delta = delta, V_y = v_y, R_c = r_c, nu=nu, p=x)})
bs<- lapply(bs, function(x){
  out<- x
  out$beta3<- ifelse(out$beta3!=0, 0, out$beta3)
  return(out)
})

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
names(alltrt)<- paste0("dim", dims)

tab<- NULL
diff.tab<- NULL
for(i in dims){
  temp.tab<- null[[paste0("dim",i)]]
  power.new<- sapply(1:dim(temp.tab)[2], function(x){mean(round(temp.tab[,x],5)>round(alltrt[paste0("dim",i)],5))})
  power.new<- cbind(i, power.new, colnames(temp.tab))
  diff.new<- sapply(1:dim(temp.tab)[2], function(x){mean(temp.tab[,x]-alltrt[paste0("dim",i)])})
  diff.new<- cbind(i, diff.new, colnames(temp.tab))
  tab<- rbind(tab, power.new)
  diff.tab<- rbind(diff.tab, diff.new)
}
null.power<- data.frame(cbind(tab, "null"))
names(null.power)<- c("dims", "power", "sim", "type")

null.diff<- data.frame(cbind(diff.tab, "null"))
names(null.diff)<- c("dims", "diff", "sim", "type")

#making graph ----
library(RColorBrewer)
overall.power<- rbind(nomain.power, under.power, achieve.power, null.power)

overall.power$dims<- as.numeric(as.character(overall.power$dims))
overall.power$power<- as.numeric(as.character(overall.power$power))
overall.power$type<- factor(overall.power$type, levels = c("nomain", "undermain", "achievemain", "null"),
                            labels = c("\u0394 = 0", "\u0394 = 0.15", "\u0394 = 0.3",
                                       "\u0394 = 0.3, \u03BD = 0"))
overall.power$sim<- factor(overall.power$sim, levels = c("min", "se1", "new"))

g<- ggplot(overall.power, aes(x=dims, y=power, color=sim, shape=sim))
p<- g+geom_line(size=1)+ geom_point()+
  facet_wrap(~type, ncol=1, scales = 'free') +
  labs(x="Number of Predictors", y=expression("P"[B]),
       color="Penalty Parameter", shape="Penalty Parameter")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        strip.text.x = element_text(size = 16),
        legend.position = "top")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_color_manual(values=c(brewer.pal(7, "Set1")[3], brewer.pal(7, "Set1")[7], brewer.pal(7, "Set1")[4]),
                     labels = c(expression(lambda[min]), expression(lambda["1se"]), expression(lambda[0.8]))) +
  scale_shape_discrete(labels = c(expression(lambda[min]), expression(lambda["1se"]), expression(lambda[0.8])))

png(paste0(path, "Results/LASSO_different_penalty_v2.png"), width = 900, height = 600)
print(p)
dev.off()

benefit<- p

#harm ----
null<- list()
nomain<- list()
under<- list()
achieve<- list()
for(i in dims){
  null[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_null_lasso_dim", i, ".txt"), header = T)
  nomain[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_nomain_lasso_dim", i, ".txt"), header = T)
  under[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_undermain_lasso_dim", i, ".txt"), header = T)
  achieve[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_achievemain_lasso_dim", i, ".txt"), header = T)
}

#achieve.main----
alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
names(alltrt)<- paste0("dim", dims)

tab<- NULL
diff.tab<- NULL
for(i in dims){
  temp.tab<- achieve[[paste0("dim",i)]]
  power.new<- sapply(1:dim(temp.tab)[2], function(x){mean(round(temp.tab[,x],5)<round(alltrt[paste0("dim",i)],5))})
  power.new<- cbind(i, power.new, colnames(temp.tab))
  diff.new<- sapply(1:dim(temp.tab)[2], function(x){mean(temp.tab[,x]-alltrt[paste0("dim",i)])})
  diff.new<- cbind(i, diff.new, colnames(temp.tab))
  tab<- rbind(tab, power.new)
  diff.tab<- rbind(diff.tab, diff.new)
}

achieve.power<- data.frame(cbind(tab, "achievemain"))
names(achieve.power)<- c("dims", "power", "sim", "type")

achieve.diff<- data.frame(cbind(diff.tab, "achievemain"))
names(achieve.diff)<- c("dims", "diff", "sim", "type")

#underachieve.main----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- bs[[i]]$beta2/2
}

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
names(alltrt)<- paste0("dim", dims)

tab<- NULL
diff.tab<- NULL
for(i in dims){
  temp.tab<- under[[paste0("dim",i)]]
  power.new<- sapply(1:dim(temp.tab)[2], function(x){mean(round(temp.tab[,x],5)<round(alltrt[paste0("dim",i)],5))})
  power.new<- cbind(i, power.new, colnames(temp.tab))
  diff.new<- sapply(1:dim(temp.tab)[2], function(x){mean(temp.tab[,x]-alltrt[paste0("dim",i)])})
  diff.new<- cbind(i, diff.new, colnames(temp.tab))
  tab<- rbind(tab, power.new)
  diff.tab<- rbind(diff.tab, diff.new)
}

under.power<- data.frame(cbind(tab, "undermain"))
names(under.power)<- c("dims", "power", "sim", "type")

under.diff<- data.frame(cbind(diff.tab, "undermain"))
names(under.diff)<- c("dims", "diff", "sim", "type")

#no.main----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- 0
}

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
names(alltrt)<- paste0("dim", dims)

tab<- NULL
diff.tab<- NULL
for(i in dims){
  temp.tab<- nomain[[paste0("dim",i)]]
  power.new<- sapply(1:dim(temp.tab)[2], function(x){mean(round(temp.tab[,x],5)<round(alltrt[paste0("dim",i)],5))})
  power.new<- cbind(i, power.new, colnames(temp.tab))
  diff.new<- sapply(1:dim(temp.tab)[2], function(x){mean(temp.tab[,x]-alltrt[paste0("dim",i)])})
  diff.new<- cbind(i, diff.new, colnames(temp.tab))
  tab<- rbind(tab, power.new)
  diff.tab<- rbind(diff.tab, diff.new)
}
nomain.power<- data.frame(cbind(tab, "nomain"))
names(nomain.power)<- c("dims", "power", "sim", "type")

nomain.diff<- data.frame(cbind(diff.tab, "nomain"))
names(nomain.diff)<- c("dims", "diff", "sim", "type")

#Null----
bs<- lapply(dims, function(x){betas.fun(delta = delta, V_y = v_y, R_c = r_c, nu=nu, p=x)})
bs<- lapply(bs, function(x){
  out<- x
  out$beta3<- ifelse(out$beta3!=0, 0, out$beta3)
  return(out)
})

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
names(alltrt)<- paste0("dim", dims)

tab<- NULL
diff.tab<- NULL
for(i in dims){
  temp.tab<- null[[paste0("dim",i)]]
  power.new<- sapply(1:dim(temp.tab)[2], function(x){mean(round(temp.tab[,x],5)<round(alltrt[paste0("dim",i)],5))})
  power.new<- cbind(i, power.new, colnames(temp.tab))
  diff.new<- sapply(1:dim(temp.tab)[2], function(x){mean(temp.tab[,x]-alltrt[paste0("dim",i)])})
  diff.new<- cbind(i, diff.new, colnames(temp.tab))
  tab<- rbind(tab, power.new)
  diff.tab<- rbind(diff.tab, diff.new)
}
null.power<- data.frame(cbind(tab, "null"))
names(null.power)<- c("dims", "power", "sim", "type")

null.diff<- data.frame(cbind(diff.tab, "null"))
names(null.diff)<- c("dims", "diff", "sim", "type")

#making graph ----
library(RColorBrewer)
overall.power<- rbind(nomain.power, under.power, achieve.power, null.power)

overall.power$dims<- as.numeric(as.character(overall.power$dims))
overall.power$power<- as.numeric(as.character(overall.power$power))
overall.power$type<- factor(overall.power$type, levels = c("nomain", "undermain", "achievemain", "null"),
                            labels = c("\u0394 = 0", "\u0394 = 0.15", "\u0394 = 0.3",
                                       "\u0394 = 0.3, \u03BD = 0"))
overall.power$sim<- factor(overall.power$sim, levels = c("min", "se1", "new"))

g<- ggplot(overall.power, aes(x=dims, y=power, color=sim, shape=sim))
p<- g+geom_line(size=1)+ geom_point()+
  facet_wrap(~type, ncol=1, scales = 'free') +
  labs(x="Number of Predictors", y=expression("P"[H]),
       color="Penalty Parameter", shape="Penalty Parameter")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        strip.text.x = element_text(size = 16),
        legend.position = "top")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_color_manual(values=c(brewer.pal(7, "Set1")[3], brewer.pal(7, "Set1")[7], brewer.pal(7, "Set1")[4]),
                     labels = c(expression(lambda[min]), expression(lambda["1se"]), expression(lambda[0.8]))) +
  scale_shape_discrete(labels = c(expression(lambda[min]), expression(lambda["1se"]), expression(lambda[0.8])))

harm<- p

png(paste0(path, "Results/LASSO_different_penalty_benefit_harm.png"), width = 800, height = 800)
ggarrange(benefit, harm, ncol = 2, common.legend = T)
dev.off()

#diff graph
overall.diff<- rbind(nomain.diff, under.diff, achieve.diff, null.diff)

overall.diff$dims<- as.numeric(as.character(overall.diff$dims))
overall.diff$diff<- as.numeric(as.character(overall.diff$diff))
overall.diff$type<- factor(overall.diff$type, levels = c("nomain", "undermain", "achievemain", "null"),
                           labels = c("\u0394 = 0", "\u0394 = 0.15", "\u0394 = 0.3",
                                      "\u0394 = 0.3, \u03BD = 0"))
overall.diff$sim<- factor(overall.diff$sim, levels = c("min", "se1", "new"))

g<- ggplot(overall.diff, aes(x=dims, y=diff, color=sim, shape=sim))
p<- g+geom_line(size=1)+ geom_point()+
  facet_wrap(~type, ncol=2, scales = 'free') +
  labs(x="Number of Predictors", y=expression("V("*widehat(d)*")-V("*w^opt*")"),
       color="Penalty Parameter", shape="Penalty Parameter")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        strip.text.x = element_text(size = 16),
        legend.position = "top")+
  scale_color_manual(values=c(brewer.pal(7, "Set1")[3], brewer.pal(7, "Set1")[7], brewer.pal(7, "Set1")[4]),
                     labels = c(expression(lambda[min]), expression(lambda["1se"]), expression(lambda[0.8]))) +
  scale_shape_discrete(labels = c(expression(lambda[min]), expression(lambda["1se"]), expression(lambda[0.8])))

png(paste0(path, "Results/LASSO_different_penalty_diff.png"), width = 900, height = 600)
print(p)
dev.off()

