#set up ----
path<- "~/simulations/"
# path<- "C:/Users/Charles Cain/Dropbox/School/Dissertation/Code/"

.libPaths("~/RPackages")

library('mvtnorm', lib = "~/RPackages")
library('glmnet', lib = "~/RPackages")
library('doParallel', lib = "~/RPackages")
library('MASS', lib = "~/RPackages")
library('randomForest', lib = "~/RPackages")

source(paste0(path, "SimFunction_v4.R"))

cr<- 24
cl<- makeCluster(cr)
clusterEvalQ(cl, .libPaths("~/RPackages"))
registerDoParallel(cl)

#modifiable variables----
M<- 10000
v_y<-1
delta<- 0.3
r_t<- 0.3
nu<- 0.16

# Achieve Main ----
title<- "sit1_achievemain_"

# Running Simulation ----
alpha<- 0.05
power<- 0.9
dims<- c(1,2, seq(5,20, by=5))
bs<- lapply(dims, function(x){betas.fun(delta = delta, V_y = v_y, R_t = r_t, nu=nu, p=x)})
v<- 4*v_y
n<- ceiling((qnorm(1-alpha/2, 0, 1) + qnorm(power, 0,1))^2 * v/(delta)^2 /2)*2
dim<- tail(dims, n=1)
H<- abs(outer(1:dim, 1:dim, "-"))
V<- 0^H

set.seed(72818)
x.valids<- do.call(cbind, read.table(paste0(path, "SituationOne/x.valids.txt")))

mod.selects<- c("none", "forward", "lasso", "elastic", "rf")

res<- list()
sim<- foreach(1:M, .packages = c('mvtnorm', 'glmnet', 'MASS', 'randomForest')) %dopar%{
  for(i in 1:length(bs)){
    bs.temp<- bs[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.none<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[1])
    l.forward<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[2]) 
    l.lasso<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[3]) 
    l.elastic<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[4])
    l.rf<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[5]) 
    res[[paste0("dim", dims[i])]]<- c(l.none, l.forward, l.lasso, l.elastic, l.rf)
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

# Under Achieve Main ----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- bs[[i]]$beta2/2
}
title<- "sit1_undermain_"

# Running Simulation 
res<- list()
sim<- foreach(1:M, .packages = c('mvtnorm', 'glmnet', 'MASS', 'randomForest')) %dopar%{
  for(i in 1:length(bs)){
    bs.temp<- bs[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.none<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[1])
    l.forward<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[2]) 
    l.lasso<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[3]) 
    l.elastic<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[4])
    l.rf<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[5]) 
    res[[paste0("dim", dims[i])]]<- c(l.none, l.forward, l.lasso, l.elastic, l.rf)
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

# No Main ----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- 0
}
title<- "sit1_nomain_"

# Running Simulation
res<- list()
sim<- foreach(1:M, .packages = c('mvtnorm', 'glmnet', 'MASS', 'randomForest')) %dopar%{
  for(i in 1:length(bs)){
    bs.temp<- bs[[i]]
    beta1<- bs.temp$beta1
    beta2<- bs.temp$beta2
    beta3<- bs.temp$beta3
    sigma<- bs.temp$sigma2
    l.none<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[1])
    l.forward<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[2]) 
    l.lasso<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[3]) 
    l.elastic<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[4])
    l.rf<- one.loss(n=n, beta1 = beta1, beta2=beta2, beta3=beta3, sigma = sigma, x.valids = as.matrix(x.valids[,1:length(beta1)]), model.selection = mod.selects[5]) 
    res[[paste0("dim", dims[i])]]<- c(l.none, l.forward, l.lasso, l.elastic, l.rf)
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
r_t<- 0.3
nu<- 0.16

dims<- c(1,2, seq(5,20, by=5))
bs<- lapply(dims, function(x){betas.fun(delta = delta, V_y = v_y, R_t = r_t, nu=nu, p=x)})
dim<- tail(dims, n=1)
H<- abs(outer(1:dim, 1:dim, "-"))
V<- 0^H
set.seed(72818)
x.valids<- do.call(cbind, read.table(paste0(path, "Code/SituationOne/x.valids.txt")))

#read in results ----
nomain<- list()
under<- list()
achieve<- list()
for(i in dims){
  nomain[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_nomain_dim", i, ".txt"), header = T)
  under[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_undermain_dim", i, ".txt"), header = T)
  achieve[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_achievemain_dim", i, ".txt"), header = T)
}

# Achieved main effect (uncorrelated) ----
achieve<- do.call(cbind, achieve)

results.no.lasso<- achieve[, grepl("none", names(achieve))]
results.forward<- achieve[, grepl("forward", names(achieve))]
results.lasso<- achieve[, grepl("lasso", names(achieve))]
results.mod<- achieve[, grepl("modified", names(achieve))]
results.elastic<- achieve[, grepl("elastic", names(achieve))]
results.rf<- achieve[, grepl("rf", names(achieve))]

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
alltrt.achieve<- alltrt[1]

power.tab.no.lasso<-sapply(1:dim(results.no.lasso)[2], function(x){mean(round(results.no.lasso[,x],5)>round(alltrt[x],5))})
power.tab.no.lasso<- cbind(dims, power.tab.no.lasso)
power.tab.no.lasso<- data.frame(power.tab.no.lasso)
names(power.tab.no.lasso)<- c("dims", "power_achieve")

power.tab.forward<-sapply(1:dim(results.forward)[2], function(x){mean(round(results.forward[,x],5)>round(alltrt[x],5))})
power.tab.forward<- cbind(dims, power.tab.forward)
power.tab.forward<- data.frame(power.tab.forward)
names(power.tab.forward)<- c("dims", "power_achieve")

power.tab.lasso<-sapply(1:dim(results.lasso)[2], function(x){mean(round(results.lasso[,x],5)>round(alltrt[x],5))})
power.tab.lasso<- cbind(dims, power.tab.lasso)
power.tab.lasso<- data.frame(power.tab.lasso)
names(power.tab.lasso)<- c("dims", "power_achieve")

power.tab.mod<-sapply(1:dim(results.mod)[2], function(x){mean(round(results.mod[,x],5)>round(alltrt[x],5))})
power.tab.mod<- cbind(dims, power.tab.mod)
power.tab.mod<- data.frame(power.tab.mod)
names(power.tab.mod)<- c("dims", "power_achieve")

power.tab.elastic<-sapply(1:dim(results.elastic)[2], function(x){mean(round(results.elastic[,x],5)>round(alltrt[x],5))})
power.tab.elastic<- cbind(dims, power.tab.elastic)
power.tab.elastic<- data.frame(power.tab.elastic)
names(power.tab.elastic)<- c("dims", "power_achieve")

power.tab.rf<-sapply(1:dim(results.rf)[2], function(x){mean(round(results.rf[,x],5)>round(alltrt[x],5))})
power.tab.rf<- cbind(dims, power.tab.rf)
power.tab.rf<- data.frame(power.tab.rf)
names(power.tab.rf)<- c("dims", "power_achieve")

# Underachieved main effect (uncorrelated) ----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- bs[[i]]$beta2/2
}
under<- do.call(cbind, under)

results.no.lasso<- under[, grepl("none", names(under))]
results.forward<- under[, grepl("forward", names(under))]
results.lasso<- under[, grepl("lasso", names(under))]
results.mod<- under[, grepl("modified", names(under))]
results.elastic<- under[, grepl("elastic", names(under))]
results.rf<- under[, grepl("rf", names(under))]

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
alltrt.under<- alltrt[1]

power.tab.no.lasso$power_under<-sapply(1:dim(results.no.lasso)[2], function(x){mean(round(results.no.lasso[,x],5)>round(alltrt[x],5))})
power.tab.forward$power_under<-sapply(1:dim(results.forward)[2], function(x){mean(round(results.forward[,x],5)>round(alltrt[x],5))})
power.tab.lasso$power_under<-sapply(1:dim(results.lasso)[2], function(x){mean(round(results.lasso[,x],5)>round(alltrt[x],5))})
power.tab.mod$power_under<-sapply(1:dim(results.mod)[2], function(x){mean(round(results.mod[,x],5)>round(alltrt[x],5))})
power.tab.elastic$power_under<-sapply(1:dim(results.elastic)[2], function(x){mean(round(results.elastic[,x],5)>round(alltrt[x],5))})
power.tab.rf$power_under<-sapply(1:dim(results.rf)[2], function(x){mean(round(results.rf[,x],5)>round(alltrt[x],5))})

# No main effect ----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- 0
}
nomain<- do.call(cbind, nomain)

results.no.lasso<- nomain[, grepl("none", names(nomain))]
results.forward<- nomain[, grepl("forward", names(nomain))]
results.lasso<- nomain[, grepl("lasso", names(nomain))]
results.mod<- nomain[, grepl("modified", names(nomain))]
results.elastic<- nomain[, grepl("elastic", names(nomain))]
results.rf<- nomain[, grepl("rf", names(nomain))]

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
alltrt.nomain<- alltrt[1]

power.tab.no.lasso$power_nomain<-sapply(1:dim(results.no.lasso)[2], function(x){mean(round(results.no.lasso[,x],5)>round(alltrt[x],5))})
power.tab.forward$power_nomain<-sapply(1:dim(results.forward)[2], function(x){mean(round(results.forward[,x],5)>round(alltrt[x],5))})
power.tab.lasso$power_nomain<-sapply(1:dim(results.lasso)[2], function(x){mean(round(results.lasso[,x],5)>round(alltrt[x],5))})
power.tab.mod$power_nomain<-sapply(1:dim(results.mod)[2], function(x){mean(round(results.mod[,x],5)>round(alltrt[x],5))})
power.tab.elastic$power_nomain<-sapply(1:dim(results.elastic)[2], function(x){mean(round(results.elastic[,x],5)>round(alltrt[x],5))})
power.tab.rf$power_nomain<-sapply(1:dim(results.rf)[2], function(x){mean(round(results.rf[,x],5)>round(alltrt[x],5))})

#making graph----
power.df.no.lasso<- gather(power.tab.no.lasso, "line", "val", c("power_nomain", "power_under", "power_achieve"))
power.df.forward<- gather(power.tab.forward, "line", "val", c("power_nomain", "power_under", "power_achieve"))
power.df.lasso<- gather(power.tab.lasso, "line", "val", c("power_nomain", "power_under", "power_achieve"))
power.df.mod<- gather(power.tab.mod, "line", "val", c("power_nomain", "power_under", "power_achieve"))
power.df.elastic<- gather(power.tab.elastic, "line", "val", c("power_nomain", "power_under", "power_achieve"))
power.df.rf<- gather(power.tab.rf, "line", "val", c("power_nomain", "power_under", "power_achieve"))

power.df.no.lasso$mod.select<- "None"
power.df.forward$mod.select<- "Forward"
power.df.lasso$mod.select<- "LASSO"
power.df.mod$mod.select<- "Modified"
power.df.elastic$mod.select<- "Elastic"
power.df.rf$mod.select<- "Random Forest"

power.df<- rbind(power.df.no.lasso, power.df.forward, power.df.lasso, power.df.mod, power.df.elastic, power.df.rf)

power.df$line<- factor(power.df$line, levels = c("power_nomain", "power_under", "power_achieve"),
                       labels = c("\u0394 = 0", "\u0394 = 0.15", "\u0394 = 0.3"))
power.df$mod.select<- factor(power.df$mod.select, levels = c("None", "Forward", "LASSO", "Modified", "Elastic", "Random Forest"))

library(viridis)
library(RColorBrewer)

#Making PNG file for uncorrelated ----
png(paste0(path, "Results/Situation_one_v7.png"), width = 1200, height = 900)

g.lines<- ggplot(data = power.df, aes(x=dims, y=val*100, color = mod.select, shape= mod.select))
gs<- g.lines + geom_line(size=1) + geom_point() +
  facet_wrap(line ~ ., scales = 'free', nrow = 3)+
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  labs(y=expression("P"[B]), x="p",
       color="ITR Identification", shape="ITR Identification") +
  theme(panel.background = element_rect(fill="white"),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size=20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        strip.text.x = element_text(size = 16)) +
  scale_x_continuous(limits = c(0,20)) +
  scale_color_manual(values=brewer.pal(6, "Set1"), labels = c("None", "Forward Selection", expression(lambda[min]*"-LASSO"), expression(lambda[0.8]*"-LASSO"),
                                                              "Elastic Net", "Random Forest")) +
  scale_shape_discrete(labels = c("None", "Forward Selection", expression(lambda[min]*"-LASSO"), expression(lambda[0.8]*"-LASSO"),
                                  "Elastic Net", "Random Forest"))

print(gs)

dev.off()

benefit.g<-gs

# probablity of harm ----
bs<- lapply(dims, function(x){betas.fun(delta = delta, V_y = v_y, R_t = r_t, nu=nu, p=x)})

nomain<- list()
under<- list()
achieve<- list()
for(i in dims){
  nomain[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_nomain_dim", i, ".txt"), header = T)
  under[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_undermain_dim", i, ".txt"), header = T)
  achieve[[paste0("dim", i)]]<- read.table(paste0(path, "SavedSimulations/sit1_achievemain_dim", i, ".txt"), header = T)
}

# Achieved main effect (uncorrelated) ----
achieve<- do.call(cbind, achieve)

results.no.lasso<- achieve[, grepl("none", names(achieve))]
results.forward<- achieve[, grepl("forward", names(achieve))]
results.lasso<- achieve[, grepl("lasso", names(achieve))]
results.mod<- achieve[, grepl("modified", names(achieve))]
results.elastic<- achieve[, grepl("elastic", names(achieve))]
results.rf<- achieve[, grepl("rf", names(achieve))]

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
alltrt.achieve<- alltrt[1]

power.tab.no.lasso<-sapply(1:dim(results.no.lasso)[2], function(x){mean(round(results.no.lasso[,x],5)<round(alltrt[x],5))})
power.tab.no.lasso<- cbind(dims, power.tab.no.lasso)
power.tab.no.lasso<- data.frame(power.tab.no.lasso)
names(power.tab.no.lasso)<- c("dims", "power_achieve")

power.tab.forward<-sapply(1:dim(results.forward)[2], function(x){mean(round(results.forward[,x],5)<round(alltrt[x],5))})
power.tab.forward<- cbind(dims, power.tab.forward)
power.tab.forward<- data.frame(power.tab.forward)
names(power.tab.forward)<- c("dims", "power_achieve")

power.tab.lasso<-sapply(1:dim(results.lasso)[2], function(x){mean(round(results.lasso[,x],5)<round(alltrt[x],5))})
power.tab.lasso<- cbind(dims, power.tab.lasso)
power.tab.lasso<- data.frame(power.tab.lasso)
names(power.tab.lasso)<- c("dims", "power_achieve")

power.tab.mod<-sapply(1:dim(results.mod)[2], function(x){mean(round(results.mod[,x],5)<round(alltrt[x],5))})
power.tab.mod<- cbind(dims, power.tab.mod)
power.tab.mod<- data.frame(power.tab.mod)
names(power.tab.mod)<- c("dims", "power_achieve")

power.tab.elastic<-sapply(1:dim(results.elastic)[2], function(x){mean(round(results.elastic[,x],5)<round(alltrt[x],5))})
power.tab.elastic<- cbind(dims, power.tab.elastic)
power.tab.elastic<- data.frame(power.tab.elastic)
names(power.tab.elastic)<- c("dims", "power_achieve")

power.tab.rf<-sapply(1:dim(results.rf)[2], function(x){mean(round(results.rf[,x],5)<round(alltrt[x],5))})
power.tab.rf<- cbind(dims, power.tab.rf)
power.tab.rf<- data.frame(power.tab.rf)
names(power.tab.rf)<- c("dims", "power_achieve")

# Underachieved main effect (uncorrelated) ----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- bs[[i]]$beta2/2
}
under<- do.call(cbind, under)

results.no.lasso<- under[, grepl("none", names(under))]
results.forward<- under[, grepl("forward", names(under))]
results.lasso<- under[, grepl("lasso", names(under))]
results.mod<- under[, grepl("modified", names(under))]
results.elastic<- under[, grepl("elastic", names(under))]
results.rf<- under[, grepl("rf", names(under))]

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
alltrt.under<- alltrt[1]

power.tab.no.lasso$power_under<-sapply(1:dim(results.no.lasso)[2], function(x){mean(round(results.no.lasso[,x],5)<round(alltrt[x],5))})
power.tab.forward$power_under<-sapply(1:dim(results.forward)[2], function(x){mean(round(results.forward[,x],5)<round(alltrt[x],5))})
power.tab.lasso$power_under<-sapply(1:dim(results.lasso)[2], function(x){mean(round(results.lasso[,x],5)<round(alltrt[x],5))})
power.tab.mod$power_under<-sapply(1:dim(results.mod)[2], function(x){mean(round(results.mod[,x],5)<round(alltrt[x],5))})
power.tab.elastic$power_under<-sapply(1:dim(results.elastic)[2], function(x){mean(round(results.elastic[,x],5)<round(alltrt[x],5))})
power.tab.rf$power_under<-sapply(1:dim(results.rf)[2], function(x){mean(round(results.rf[,x],5)<round(alltrt[x],5))})

# No main effect ----
for(i in 1:length(bs)){
  bs[[i]]$beta2<- 0
}
nomain<- do.call(cbind, nomain)

results.no.lasso<- nomain[, grepl("none", names(nomain))]
results.forward<- nomain[, grepl("forward", names(nomain))]
results.lasso<- nomain[, grepl("lasso", names(nomain))]
results.mod<- nomain[, grepl("modified", names(nomain))]
results.elastic<- nomain[, grepl("elastic", names(nomain))]
results.rf<- nomain[, grepl("rf", names(nomain))]

alltrt<- lapply(dims, function(x){
  beta2<- bs[[which(x==dims)]]$beta2; beta3<- bs[[which(x==dims)]]$beta3
  vest<- (beta2 + as.matrix(x.valids[,1:x])%*%beta3)
  mean(vest)
})
alltrt<- unlist(alltrt)
alltrt.nomain<- alltrt[1]

power.tab.no.lasso$power_nomain<-sapply(1:dim(results.no.lasso)[2], function(x){mean(round(results.no.lasso[,x],5)<round(alltrt[x],5))})
power.tab.forward$power_nomain<-sapply(1:dim(results.forward)[2], function(x){mean(round(results.forward[,x],5)<round(alltrt[x],5))})
power.tab.lasso$power_nomain<-sapply(1:dim(results.lasso)[2], function(x){mean(round(results.lasso[,x],5)<round(alltrt[x],5))})
power.tab.mod$power_nomain<-sapply(1:dim(results.mod)[2], function(x){mean(round(results.mod[,x],5)<round(alltrt[x],5))})
power.tab.elastic$power_nomain<-sapply(1:dim(results.elastic)[2], function(x){mean(round(results.elastic[,x],5)<round(alltrt[x],5))})
power.tab.rf$power_nomain<-sapply(1:dim(results.rf)[2], function(x){mean(round(results.rf[,x],5)<round(alltrt[x],5))})

#making graph----
power.df.no.lasso<- gather(power.tab.no.lasso, "line", "val", c("power_nomain", "power_under", "power_achieve"))
power.df.forward<- gather(power.tab.forward, "line", "val", c("power_nomain", "power_under", "power_achieve"))
power.df.lasso<- gather(power.tab.lasso, "line", "val", c("power_nomain", "power_under", "power_achieve"))
power.df.mod<- gather(power.tab.mod, "line", "val", c("power_nomain", "power_under", "power_achieve"))
power.df.elastic<- gather(power.tab.elastic, "line", "val", c("power_nomain", "power_under", "power_achieve"))
power.df.rf<- gather(power.tab.rf, "line", "val", c("power_nomain", "power_under", "power_achieve"))

power.df.no.lasso$mod.select<- "None"
power.df.forward$mod.select<- "Forward"
power.df.lasso$mod.select<- "LASSO"
power.df.mod$mod.select<- "Modified"
power.df.elastic$mod.select<- "Elastic"
power.df.rf$mod.select<- "Random Forest"

power.df<- rbind(power.df.no.lasso, power.df.forward, power.df.lasso, power.df.mod, power.df.elastic, power.df.rf)

power.df$line<- factor(power.df$line, levels = c("power_nomain", "power_under", "power_achieve"),
                       labels = c("\u0394 = 0", "\u0394 = 0.15", "\u0394 = 0.3"))
power.df$mod.select<- factor(power.df$mod.select, levels = c("None", "Forward", "LASSO", "Modified", "Elastic", "Random Forest"))

png(paste0(path, "Results/Situation_one_harm.png"), width = 1200, height = 900)

g.lines<- ggplot(data = power.df, aes(x=dims, y=val*100, color = mod.select, shape= mod.select))
gs<- g.lines + geom_line(size=1) + geom_point() +
  facet_wrap(line ~ ., scales = 'free', nrow = 3)+
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  labs(y=expression("P"[H]), x="p",
       color="ITR Identification", shape="ITR Identification") +
  theme(panel.background = element_rect(fill="white"),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size=20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        strip.text.x = element_text(size = 16)) +
  scale_x_continuous(limits = c(0,20)) +
  scale_color_manual(values=brewer.pal(6, "Set1"), labels = c("None", "Forward Selection", expression(lambda[min]*"-LASSO"), expression(lambda[0.8]*"-LASSO"),
                                                              "Elastic Net", "Random Forest")) +
  scale_shape_discrete(labels = c("None", "Forward Selection", expression(lambda[min]*"-LASSO"), expression(lambda[0.8]*"-LASSO"),
                                  "Elastic Net", "Random Forest"))

print(gs)

dev.off()

harm.g<- gs

#putting into single graph
png(paste0(path, "Results/Situation_one_benefit_harm.png"), width = 800, height = 600)

ggarrange(benefit.g, harm.g, nrow = 1, common.legend = T)

dev.off()

# Looking at Ratios as well ----
bs<- lapply(dims, function(x){betas.fun(delta = delta, V_y = v_y, R_t = r_t, nu=nu, p=x)})

nomain2<- nomain[,grepl("dim2.", names(nomain)) & !grepl("dim20.", names(nomain))]
under2<- under[,grepl("dim2.", names(under)) & !grepl("dim20.", names(under))]
achieve2<- achieve[,grepl("dim2.", names(achieve)) & !grepl("dim20.", names(achieve))]

nomaintrue2<- mean(abs(0 + as.matrix(x.valids)[,1:2]%*%bs[[2]]$beta3))
undertrue2<- mean(abs(0.075 + as.matrix(x.valids)[,1:2]%*%bs[[2]]$beta3))
achievetrue2<- mean(abs(0.15 + as.matrix(x.valids)[,1:2]%*%bs[[2]]$beta3))

nomainworst<- mean(-1*sign(0 + as.matrix(x.valids)[,1:2]%*%bs[[2]]$beta3)*(0 + as.matrix(x.valids)[,1:2]%*%bs[[2]]$beta3))
underworst<- mean(-1*sign(0.075 + as.matrix(x.valids)[,1:2]%*%bs[[2]]$beta3)*(0.075 + as.matrix(x.valids)[,1:2]%*%bs[[2]]$beta3))
achieveworst<- mean(-1*sign(0.15 + as.matrix(x.valids)[,1:2]%*%bs[[2]]$beta3)*(0.15 + as.matrix(x.valids)[,1:2]%*%bs[[2]]$beta3))

nomain2<- nomain2-alltrt.nomain
under2<- under2-alltrt.under
achieve2<- achieve2-alltrt.achieve

nomain10<- nomain[,grepl("dim10.", names(nomain)) ]
under10<- under[,grepl("dim10.", names(under))]
achieve10<- achieve[,grepl("dim10.", names(achieve))]

nomaintrue10<- mean(abs(0 + as.matrix(x.valids)[,1:10]%*%bs[[4]]$beta3))
undertrue10<- mean(abs(0.075 + as.matrix(x.valids)[,1:10]%*%bs[[4]]$beta3))
achievetrue10<- mean(abs(0.15 + as.matrix(x.valids)[,1:10]%*%bs[[4]]$beta3))

nomain10<- nomain10-alltrt.nomain
under10<- under10-alltrt.under
achieve10<- achieve10-alltrt.achieve

achieve.best.diff<- achievetrue2-alltrt.achieve
under.best.diff<- undertrue2-alltrt.under
nomain.best.diff<- nomaintrue2-alltrt.nomain

# dim 2
achieve<- achieve2
under<- under2
nomain<- nomain2

#achieved main effect
achieve.cdf<- list()
for(j in names(achieve)){
  new.cdf<- sapply(seq(achieveworst, achieve.best.diff, length.out = 1000), function(x){sum(achieve[[j]]<=x)/10000})
  new.cdf<- cbind(j, new.cdf, seq(achieveworst, achieve.best.diff, length.out = 1000))
  achieve.cdf[[j]]<- new.cdf
}

achieve.cdf<- data.frame(do.call(rbind, achieve.cdf), stringsAsFactors = F)
names(achieve.cdf)<- c("model", "cdf", "x")
achieve.cdf$cdf<- as.numeric(achieve.cdf$cdf)
achieve.cdf$x<- as.numeric(achieve.cdf$x)
achieve.cdf$model<- factor(achieve.cdf$model, levels = paste0("dim2.", c("none", "forward", "lasso", "modified", "elastic", "rf")),
                           labels = c("None", "Forward", "LASSO", "Modified LASSO", "Elastic Net", "Random Forest"))

achieve.g2<- ggplot(achieve.cdf, aes(x=x, y=cdf, color = model)) + geom_line(size=1) +
  labs(y=expression("F"[n]*"{V("*widehat(d)*")-V("*w^opt*")"*"}"), x=expression("V("*widehat(d)*")-V("*w^opt*")"),
       title = "\u0394 = 0.3, p = 2",
       color = "ITR Identification") +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA, size = 0.5),
        legend.key.size = unit(1.25, "cm"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size=20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        legend.text.align = 0) +
  geom_hline(yintercept = 1, size=0.25, linetype="dashed", alpha=0.5) +
  geom_vline(xintercept = 0, size=0.25, linetype="dashed", alpha=0.5) +
  scale_color_manual(values=brewer.pal(6, "Set1"), labels = c("None", "Forward Selection", expression(lambda[min]*"-LASSO"), expression(lambda[0.8]*"-LASSO"),
                                                              "Elastic Net", "Random Forest"))

# Underachieved main effect (uncorrelated)
under.cdf<- list()
for(j in names(under)){
  new.cdf<- sapply(seq(underworst, under.best.diff, length.out = 1000), function(x){sum(under[[j]]<=x)/10000})
  new.cdf<- cbind(j, new.cdf, seq(underworst, under.best.diff, length.out = 1000))
  under.cdf[[j]]<- new.cdf
}

under.cdf<- data.frame(do.call(rbind, under.cdf), stringsAsFactors = F)
names(under.cdf)<- c("model", "cdf", "x")
under.cdf$cdf<- as.numeric(under.cdf$cdf)
under.cdf$x<- as.numeric(under.cdf$x)
under.cdf$model<- factor(under.cdf$model, levels = paste0("dim2.", c("none", "forward", "lasso", "modified", "elastic", "rf")),
                         labels = c("None", "Forward", "LASSO", "Modified LASSO", "Elastic Net", "Random Forest"))

under.g2<- ggplot(under.cdf, aes(x=x, y=cdf, color = model)) + geom_line(size=1) +
  labs(y=expression("F"[n]*"{V("*widehat(d)*")-V("*w^opt*")"*"}"), x=expression("V("*widehat(d)*")-V("*w^opt*")"),
       title = "\u0394 = 0.15, p = 2",
       color = "ITR Identification") +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA, size = 0.5),
        legend.key.size = unit(1.25, "cm"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size=20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        legend.text.align = 0) +
  geom_hline(yintercept = 1, size=0.25, linetype="dashed", alpha=0.5)+
  geom_vline(xintercept = 0, size=0.25, linetype="dashed", alpha=0.5) +
  scale_color_manual(values=brewer.pal(6, "Set1"), labels = c("None", "Forward Selection", expression(lambda[min]*"-LASSO"), expression(lambda[0.8]*"-LASSO"),
                                                              "Elastic Net", "Random Forest"))

# No main effect
nomain.cdf<- list()
for(j in names(nomain)){
  new.cdf<- sapply(seq(nomainworst, nomain.best.diff, length.out = 1000), function(x){sum(nomain[[j]]<=x)/10000})
  new.cdf<- cbind(j, new.cdf, seq(nomainworst, nomain.best.diff, length.out = 1000))
  nomain.cdf[[j]]<- new.cdf
}

nomain.cdf<- data.frame(do.call(rbind, nomain.cdf), stringsAsFactors = F)
names(nomain.cdf)<- c("model", "cdf", "x")
nomain.cdf$cdf<- as.numeric(nomain.cdf$cdf)
nomain.cdf$x<- as.numeric(nomain.cdf$x)
nomain.cdf$model<- factor(nomain.cdf$model, levels = paste0("dim2.", c("none", "forward", "lasso", "modified", "elastic", "rf")),
                          labels = c("None", "Forward", "LASSO", "Modified LASSO", "Elastic Net", "Random Forest"))

nomain.g2<- ggplot(nomain.cdf, aes(x=x, y=cdf, color = model)) + geom_line(size=1) +
  labs(y=expression("F"[n]*"{V("*widehat(d)*")-V("*w^opt*")"*"}"), x=expression("V("*widehat(d)*")-V("*w^opt*")"),
       title = "\u0394 = 0, p = 2",
       color = "ITR Identification") +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA, size = 0.5),
        legend.key.size = unit(1.25, "cm"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size=20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        legend.text.align = 0) +
  geom_hline(yintercept = 1, size=0.25, linetype="dashed", alpha=0.5)+
  geom_vline(xintercept = 0, size=0.25, linetype="dashed", alpha=0.5) +
  scale_color_manual(values=brewer.pal(6, "Set1"), labels = c("None", "Forward Selection", expression(lambda[min]*"-LASSO"), expression(lambda[0.8]*"-LASSO"),
                                                              "Elastic Net", "Random Forest"))

# dim 10
achieve<- achieve10
under<- under10
nomain<- nomain10

#achieved main effect
achieve.cdf<- list()
for(j in names(achieve)){
  new.cdf<- sapply(seq(achieveworst, achieve.best.diff, length.out = 1000), function(x){sum(achieve[[j]]<=x)/10000})
  new.cdf<- cbind(j, new.cdf, seq(achieveworst, achieve.best.diff, length.out = 1000))
  achieve.cdf[[j]]<- new.cdf
}


achieve.cdf<- data.frame(do.call(rbind, achieve.cdf), stringsAsFactors = F)
names(achieve.cdf)<- c("model", "cdf", "x")
achieve.cdf$cdf<- as.numeric(achieve.cdf$cdf)
achieve.cdf$x<- as.numeric(achieve.cdf$x)
achieve.cdf$model<- factor(achieve.cdf$model, levels = paste0("dim10.", c("none", "forward", "lasso", "modified", "elastic", "rf")),
                           labels = c("None", "Forward", "LASSO", "Modified LASSO", "Elastic Net", "Random Forest"))

achieve.g10<- ggplot(achieve.cdf, aes(x=x, y=cdf, color = model)) + geom_line(size=1) +
  labs(y=expression("F"[n]*"{V("*widehat(d)*")-V("*w^opt*")"*"}"), x=expression("V("*widehat(d)*")-V("*w^opt*")"),
       title = "\u0394 = 0.3, p = 10",
       color = "ITR Identification") +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA, size = 0.5),
        legend.key.size = unit(1.25, "cm"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size=20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        legend.text.align = 0) +
  geom_hline(yintercept = 1, size=0.25, linetype="dashed", alpha=0.5) +
  geom_vline(xintercept = 0, size=0.25, linetype="dashed", alpha=0.5) +
  scale_color_manual(values=brewer.pal(6, "Set1"), labels = c("None", "Forward Selection", expression(lambda[min]*"-LASSO"), expression(lambda[0.8]*"-LASSO"),
                                                              "Elastic Net", "Random Forest"))

# Underachieved main effect (uncorrelated)
under.cdf<- list()
for(j in names(under)){
  new.cdf<- sapply(seq(underworst, under.best.diff, length.out = 1000), function(x){sum(under[[j]]<=x)/10000})
  new.cdf<- cbind(j, new.cdf, seq(underworst, under.best.diff, length.out = 1000))
  under.cdf[[j]]<- new.cdf
}


under.cdf<- data.frame(do.call(rbind, under.cdf), stringsAsFactors = F)
names(under.cdf)<- c("model", "cdf", "x")
under.cdf$cdf<- as.numeric(under.cdf$cdf)
under.cdf$x<- as.numeric(under.cdf$x)
under.cdf$model<- factor(under.cdf$model, levels = paste0("dim10.", c("none", "forward", "lasso", "modified", "elastic", "rf")),
                         labels = c("None", "Forward", "LASSO", "Modified LASSO", "Elastic Net", "Random Forest"))

under.g10<- ggplot(under.cdf, aes(x=x, y=cdf, color = model)) + geom_line(size=1) +
  labs(y=expression("F"[n]*"{V("*widehat(d)*")-V("*w^opt*")"*"}"), x=expression("V("*widehat(d)*")-V("*w^opt*")"),
       title = "\u0394 = 0.15, p = 10",
       color = "ITR Identification") +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA, size = 0.5),
        legend.key.size = unit(1.25, "cm"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size=20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        legend.text.align = 0) +
  geom_hline(yintercept = 1, size=0.25, linetype="dashed", alpha=0.5)+
  geom_vline(xintercept = 0, size=0.25, linetype="dashed", alpha=0.5) +
  scale_color_manual(values=brewer.pal(6, "Set1"), labels = c("None", "Forward Selection", expression(lambda[min]*"-LASSO"), expression(lambda[0.8]*"-LASSO"),
                                                              "Elastic Net", "Random Forest"))

# No main effect
nomain.cdf<- list()
for(j in names(nomain)){
  new.cdf<- sapply(seq(nomainworst, nomain.best.diff, length.out = 1000), function(x){sum(nomain[[j]]<=x)/10000})
  new.cdf<- cbind(j, new.cdf, seq(nomainworst, nomain.best.diff, length.out = 1000))
  nomain.cdf[[j]]<- new.cdf
}

nomain.cdf<- data.frame(do.call(rbind, nomain.cdf), stringsAsFactors = F)
names(nomain.cdf)<- c("model", "cdf", "x")
nomain.cdf$cdf<- as.numeric(nomain.cdf$cdf)
nomain.cdf$x<- as.numeric(nomain.cdf$x)
nomain.cdf$model<- factor(nomain.cdf$model, levels = paste0("dim10.", c("none", "forward", "lasso", "modified", "elastic", "rf")),
                          labels = c("None", "Forward", "LASSO", "Modified LASSO", "Elastic Net", "Random Forest"))

nomain.g10<- ggplot(nomain.cdf, aes(x=x, y=cdf, color = model)) + geom_line(size=1) +
  labs(y=expression("F"[n]*"{V("*widehat(d)*")-V("*w^opt*")"*"}"), x=expression("V("*widehat(d)*")-V("*w^opt*")"),
       title = "\u0394 = 0, p = 10",
       color = "ITR Identification") +
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA, size = 0.5),
        legend.key.size = unit(1.25, "cm"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size=20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size=18),
        legend.text.align = 0) +
  geom_hline(yintercept = 1, size=0.25, linetype="dashed", alpha=0.5)+
  geom_vline(xintercept = 0, size=0.25, linetype="dashed", alpha=0.5) +
  scale_color_manual(values=brewer.pal(6, "Set1"), labels = c("None", "Forward Selection", expression(lambda[min]*"-LASSO"), expression(lambda[0.8]*"-LASSO"),
                                                              "Elastic Net", "Random Forest"))

png(paste0(path, "Results/Situation_one_ratio_dim2_10_v4.png"), width = 1000, height = 800)

ggarrange(nomain.g2, under.g2, achieve.g2, nomain.g10, under.g10, achieve.g10, ncol = 2, nrow = 3, common.legend = TRUE)

dev.off()
