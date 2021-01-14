#packages needed:
library(mvtnorm)
library(rootSolve)
library(doParallel)
library(MASS)
library(glmnet)
library(foreach)
library(randomForest)

one.value<- function(n, beta1, beta2, beta3, sigma, x.valids, model.selection="none", lasso.penalty="min", g.model.type="parameteric"){
  p<- dim(x.valids)[2]
  if(g.model.type=="non-parameteric"){
    x<- x.valids[sample(1:dim(x.valids)[1], n, replace = T), ]
  }else{
    x<- rmvnorm(n, sigma = cov(x.valids))
  }
  x<- rmvnorm(n, sigma = cov(x.valids))
  a<- sample(c(rep(-1,ceiling(n/2)), rep(1, ceiling(n/2))))[1:n]
  X<- cbind(x, a, a*x)
  betas<- c(beta1, beta2, beta3)
  y<- rnorm(n, X%*%betas, sqrt(sigma))
  dat<- data.frame(cbind(y=y, a, x))
  x.nms<- paste0("x", 1:p)
  colnames(X)<- c(x.nms, "a", paste0("a:", x.nms))
  names(dat)<- c("y", "a", x.nms)
  if(model.selection=="none"){
    model<- lm(y~a*x, data = dat)
    bs<- coefficients(model)
    bs<- bs[grepl("a", names(bs))]
  }else if(model.selection=="forward"){
    model.null<- lm(y~a, data = dat)
    model.forward<- stepAIC(model.null, 
                            scope = list(lower=~a, upper=as.formula(paste0("~a*(", paste(paste0("x", 1:p), collapse = "+"), ")"))),
                            direction = "forward", trace = F)
    bs<- coefficients(model.forward)
    bs<- bs[grepl("a", names(bs))]
    bs<- c(bs, rep(0, p-length(bs)+1))
    names(bs)[names(bs)==""]<- paste0("a:", x.nms)[!paste0("a:", x.nms)%in%names(bs)]
    bs<- bs[c("a", paste0("a:", x.nms))]
  }else if(model.selection=="lasso"){
    lasso.cv<- cv.glmnet(X, y, family = "gaussian", nfolds = 10, parallel = T)
    if(lasso.penalty=="min"){
      lambda.select<- lasso.cv$lambda.min
    }else if(lasso.penalty=="1se"){
      lambda.select<- lasso.cv$lambda.1se
    }
    lasso.model<- glmnet(X, y, family = "gaussian", lambda = lambda.select, intercept = T,
                         penalty.factor = c(rep(1,p),0,rep(1,p)))
    lasso.bs<- coefficients(lasso.model)[,1]
    lasso.bs<- lasso.bs[-1]
    are0<- lasso.bs[which(lasso.bs==0)]
    are0<- are0[grepl("a", names(are0))]
    not0<- names(lasso.bs[which(lasso.bs!=0)])
    if(length(not0)==0){
      not0<- "1"
    }
    model<- lm(as.formula(paste0("y ~", paste(not0, collapse = "+"))), data = dat)
    bs<- coefficients(model)
    bs<- bs[grepl("a", names(bs))]
    if(any(grepl(":a", names(bs)))){
      names(bs)[grepl(":a", names(bs))]<- paste0(sub("x.*:", "", names(bs)[grepl(":a", names(bs))]),
                                                 ":", sub(":.*a", "", names(bs)[grepl(":a", names(bs))]))
    }
    bs<- c(bs, are0)
    bs<- bs[c("a", paste0("a:", x.nms))]
  }else if(model.selection=="modified"){
    delta<- mean(subset(dat, dat$a==1)$y) - mean(subset(dat, dat$a==-1)$y)
    psuedoy<- ifelse(a== 1, y-delta, y)
    lambdas<- 1/exp(1:80/10)
    perc<- sapply(1:1000, function(l){
      psuedodat<- dat
      psuedodat$a<- sample(psuedodat$a)
      psuedoX<- model.matrix(y~x*a, psuedodat)[,-1]
      lasso<- glmnet(psuedoX, psuedoy, family = "gaussian", lambda = lambdas,
                     penalty.factor = c(rep(1,p),0,rep(1,p)))
      betahats<- coef(lasso)
      tab<- betahats[grepl(":a", row.names(betahats)),]
      if(p<2){
        has.inter<- tab==0
      }else{
        has.inter<- colSums(tab==0)==p
      }
      return(has.inter)
    })
    tab<- rowSums(perc)/1000
    lambda.best<- lambdas[tail(which(tab>0.8),1)]
    lasso.model<- glmnet(X, y, family = "gaussian", lambda = lambda.best, intercept = T,
                         penalty.factor = c(rep(1,p),0,rep(1,p)))
    lasso.bs<- coefficients(lasso.model)[,1]
    lasso.bs<- lasso.bs[-1]
    are0<- lasso.bs[which(lasso.bs==0)]
    are0<- are0[grepl("a", names(are0))]
    not0<- names(lasso.bs[which(lasso.bs!=0)])
    if(length(not0)==0){
      not0<- "1"
    }
    model<- lm(as.formula(paste0("y ~", paste(not0, collapse = "+"))), data = dat)
    bs<- coefficients(model)
    bs<- bs[grepl("a", names(bs))]
    if(any(grepl(":a", names(bs)))){
      names(bs)[grepl(":a", names(bs))]<- paste0(sub("x.*:", "", names(bs)[grepl(":a", names(bs))]),
                                                 ":", sub(":.*a", "", names(bs)[grepl(":a", names(bs))]))
    }
    bs<- c(bs, are0)
    bs<- bs[c("a", paste0("a:", x.nms))]
  }else if(model.selection=="elastic"){
    a <- seq(0.1, 0.9, 0.05)
    search <- foreach(i = a, .combine = rbind, .packages = 'glmnet') %dopar% {
      cv <- cv.glmnet(X, y, family = "gaussian", nfold = 10, parallel = TRUE, alpha = i)
      data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.min], lambda.min = cv$lambda.min, alpha = i, parallel=T)
    }
    cv3 <- search[search$cvm == min(search$cvm), ]
    elastic.model<- glmnet(X, y, family = "gaussian", lambda = cv3$lambda.min, intercept = T,
                           alpha = cv3$alpha, penalty.factor = c(rep(1,p),0,rep(1,p)))
    elastic.bs<- coefficients(elastic.model)[,1]
    elastic.bs<- elastic.bs[-1]
    are0<- elastic.bs[which(elastic.bs==0)]
    are0<- are0[grepl("a", names(are0))]
    not0<- names(elastic.bs[which(elastic.bs!=0)])
    if(length(not0)==0){
      not0<- "1"
    }
    model<- lm(as.formula(paste0("y ~", paste(not0, collapse = "+"))), data = dat)
    bs<- coefficients(model)
    bs<- bs[grepl("a", names(bs))]
    if(any(grepl(":a", names(bs)))){
      names(bs)[grepl(":a", names(bs))]<- paste0(sub("x.*:", "", names(bs)[grepl(":a", names(bs))]),
                                                 ":", sub(":.*a", "", names(bs)[grepl(":a", names(bs))]))
    }
    bs<- c(bs, are0)
    bs<- bs[c("a", paste0("a:", x.nms))]
  }else if(model.selection=="rf"){
    rf<- randomForest(y~., data = dat, importance = TRUE)
    
    x.valid<- as.data.frame(cbind(1, x.valids))
    names(x.valid)<- c("a", x.nms)
    pred1<- predict(rf, x.valid)
    x.valid<- as.data.frame(cbind(-1, x.valids))
    names(x.valid)<- c("a", x.nms)
    pred0<- predict(rf, x.valid)
    
    trt.rule<- sign(pred1 - pred0)
    trt.rule<- ifelse(trt.rule==0, 1, trt.rule)
  }else{
    stop("Must us model selection of 'none', 'forward', 'lasso', 'modified', 'elastic', or 'rf'.")
  }
  x.valid<- cbind(1, x.valids)
  psi<-x.valid%*%c(beta2, beta3)
  if(model.selection=="rf"){
    psihat<- trt.rule
  }else{
    psihat<- x.valid%*%bs
  }
  vect<- sign(psihat)*psi
  
  static<- lm(y~a, data=dat)
  static.rule<- sign(coefficients(static)["a"])
  static.vect<- static.rule*psi
  
  return(c(mean(vect), mean(static.vect)))
}

beneficial.lasso<- function(y, x, a, q){
  dat<- data.frame(cbind(y=y, a, x))
  x.nms<- paste0("x", 1:p)
  colnames(X)<- c(x.nms, "a", paste0("a:", x.nms))
  names(dat)<- c("y", "a", x.nms)
  psuedoy<- ifelse(a== 1, y-2*beta2, y)
  lambdas<- 1/exp(1:80/10)
  perc<- sapply(1:1000, function(l){
    psuedodat<- dat
    psuedodat$a<- sample(psuedodat$a)
    psuedoX<- model.matrix(y~x*a, psuedodat)[,-1]
    lasso<- glmnet(psuedoX, psuedoy, family = "gaussian", lambda = lambdas,
                   penalty.factor = c(rep(1,p),0,rep(1,p)))
    betahats<- coef(lasso)
    tab<- betahats[grepl(":a", row.names(betahats)),]
    if(p<2){
      has.inter<- tab==0
    }else{
      has.inter<- colSums(tab==0)==p
    }
    return(has.inter)
  })
  tab<- rowSums(perc)/1000
  lambda.best<- lambdas[tail(which(tab>q),1)]
  lasso.model<- glmnet(X, y, family = "gaussian", lambda = lambda.best, intercept = T,
                       penalty.factor = c(rep(1,p),0,rep(1,p)))
  lasso.bs<- coefficients(lasso.model)[,1]
  lasso.bs<- lasso.bs[-1]
  are0<- lasso.bs[which(lasso.bs==0)]
  are0<- are0[grepl("a", names(are0))]
  not0<- names(lasso.bs[which(lasso.bs!=0)])
  if(length(not0)==0){
    not0<- "1"
  }
  model<- lm(as.formula(paste0("y ~", paste(not0, collapse = "+"))), data = dat)
  return(model)
}

betas.fun<- function(delta, nu, V_y, R_c, p=1, betas.type="single", sig.p=diag(p)){
  if(!betas.type%in%c("single", "even", "diminishing")){
    stop("betas.type must be 'single', 'even', or 'diminishing'")
  }
  solve.fun<- function(betas.sig){
    if(betas.type%in%"single"){
      beta1<- c(betas.sig[1], rep(0,p-1))
      beta3<- c(betas.sig[3], rep(0,p-1))
    }
    if(betas.type%in%"even"){
      beta1<- rep(betas.sig[1],p)
      beta3<- rep(betas.sig[3],p)
    }
    if(betas.type%in%"diminishing"){
      beta1<- rep(betas.sig[1],p)/1:p
      beta3<- rep(betas.sig[3],p)/1:p
    }
    beta2<- betas.sig[2]
    sigma<- betas.sig[4]
    d<- delta - 2*beta2
    n<- t(beta3)%*%sig.p%*%beta3 - ( -beta2/qnorm(nu))^2
    v<- sigma + t(beta1 - beta3)%*%sig.p%*%(beta1-beta3) - V_y
    rc<- (t(beta1 - beta3)%*%sig.p%*%(beta1-beta3))/(sigma + t(beta1 - beta3)%*%sig.p%*%(beta1-beta3)) - R_c
    return(c(Delta = d, NU=n, VY=v, RC = rc))
  }
  b2<- delta/2
  b3<- -b2/qnorm(nu)
  b1<- b3 + sqrt(R_c*V_y)
  s<- V_y - (b1-b3)^2
  out<- multiroot(f= solve.fun, start = c(b1, b2, b3, s))
  out<- out$root
  if(betas.type%in%"single"){
    put<- list(beta1=c(out[1], rep(0,p-1)), beta2=out[2], beta3=c(out[3], rep(0,p-1)), sigma2=out[4])
  }
  if(betas.type%in%"even"){
    put<- list(beta1=rep(out[1],p), beta2=out[2], beta3=rep(out[3],p), sigma2=out[4])
  }
  if(betas.type%in%"diminishing"){
    put<- list(beta1=rep(out[1],p)/1:p, beta2=out[2], beta3=rep(out[3],p)/1:p, sigma2=out[4])
  }
  return(put)
}

power.fun<- function(n, beta1, beta2, beta3, sigma, sig.p=diag(p), x.valids=NULL, model.selection="none", seed=NULL){
  M<-1000
  if(!is.null(seed)){
    set.seed(seed)
  }
  if(is.null(x.valids)){
    x.valids<- rmvnorm(10000, sigma = sig.p)
  }
  ld0<- mean(beta2 + x.valids%*%as.matrix(beta3))
  res<-NULL
  time1<- Sys.time()
  sim<- foreach(i=1:M, .packages = c('mvtnorm', 'glmnet', 'MASS', 'doParallel', 'randomForest'),
                .export = "one.loss") %dopar% {if(!is.null(seed)){set.seed(seed+i)}
                  one.value(n=n, beta1=beta1, beta2=beta2, beta3=beta3,
                            sigma = sigma, model.selection = model.selection,
                            x.valids = x.valids)}
  res<- unlist(sim)
  put<- sum(res > ld0, na.rm = T)/M
  return(put)
}

power.tailor<- function(n = NULL, power=NULL, delta, nu, V_y, R_c, p=1, betas.type="single", sig.p=diag(p),
                        x.valids=NULL, model.selection="none", seed=NULL, interval=c(10,10000)){
  betas<- betas.fun(delta = delta, nu=nu, V_y = V_y, R_c = R_c, p=p, betas.type = betas.type, sig.p = sig.p)
  beta1<- betas$beta1
  beta2<- betas$beta2
  beta3<- betas$beta3
  sigma<- betas$sigma2
  if(is.null(power) & !is.null(n)){
    #if n is defined, power is not
    res<- power.fun(n=n, beta1 = beta1, beta2 = beta2, beta3 = beta3, sigma = sigma, sig.p=sig.p, x.valids=x.valids, model.selection = model.selection, seed = seed)
  }else if(!is.null(power) & is.null(n)){
    #if n is not defined, power is defined
    res<- uniroot(function(x){power.fun(n=round(x), beta1 = beta1, beta2 = beta2, beta3 = beta3, sigma = sigma, sig.p=sig.p,
                                        x.valids = x.valids, model.selection = model.selection, seed = seed) - power},
                  interval = interval)
    res<- ceiling(res$root/2)*2
  }else{
    stop("Must specify either n or power")
  }
  return(res)
}

