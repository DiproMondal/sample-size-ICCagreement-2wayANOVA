
rm(list=ls())
library(pscl)
library(parallel)
nclus <- max(1,detectCores()-2)

## Data Generation Process
data_gen<-function(n,k,rho, R){
  
  sd.2.rater <- R
  sd.2.subject <- (R+1)/(1/rho-1)
  
  
  e_ij <- matrix(rnorm(n*k, sd = 1), nrow=n, ncol=k)            ## errors
  r_j <- matrix(rep(rnorm(k, sd=sqrt(sd.2.rater)),n), nrow=n, byrow =TRUE) ## rater effects
  s_i <- matrix(rep(rnorm(n, sd=sqrt(sd.2.subject)),k), nrow=n) ## subject effects
  return(s_i + r_j + e_ij)
}



## Estinmation of ICC
ICC_estimate<-function(data,                  ## A 2d matrix with columns representing raters and rows representing subjects
                       type=c("agreement"),   ## Mention which type of ICC - "agreement" (default) or "consistency"
                       verbose=FALSE){        ## Optional argument. verbose=TRUE makes output both the ICCs as well as the mean squares
  data[is.na(data)]<-0
  
  k <- ncol(data)
  n <- nrow(data)
  
  MS_between_subjects <- 1/(n-1) * sum(k*(rowMeans(data)-mean(data))^2)
  MS_between_raters <- 1/(k-1) * sum(n*(colMeans(data)-mean(data))^2)
  MS_errors <- 1/((n-1)*(k-1)) * sum(sapply(1:n, function(i)
    sum(sapply(1:k, function(j)
      (data[i,j] - mean(data[i,]) - mean(data[,j]) + mean(data))^2))))
  
  coeff_consistency <- (MS_between_subjects - MS_errors)/
    (MS_between_subjects + (k-1)*MS_errors)
  coeff_agreement <- (MS_between_subjects - MS_errors)/
    (MS_between_subjects + (k/n)*MS_between_raters + (k-1-k/n)*MS_errors)
  
  if (verbose==TRUE){
    return(list("ICC.agreement" = coeff_agreement,
                "ICC.consistency"=coeff_consistency,
                "MS.subjects"=MS_between_subjects,
                "MS.raters" = MS_between_raters,
                "MS.errors" = MS_errors))
  }else if(type=="consistency"){
    return(coeff_consistency)
  }else if(type=="agreement"){
    return(coeff_agreement)
  }else if(type=="both"){
    return(list("ICC.agreement"=coeff_agreement,
                "ICC.consistency"=coeff_consistency))
  }else{
    return(
      print("Please specificy type of reliability : agreement or consistency")
    )
  }
  
}

### Confidence Intervals based on log transformed ICC
ci.delta.log <-function(data,    ## A 2d matrix with columns representing raters and rows representing subjects
                        alpha=0.05){
  start = Sys.time()
  est <- ICC_estimate(data, verbose = TRUE)
  k <- ncol(data)
  n <- nrow(data)
  V.MSS <- 2*est$MS.subjects^2/(n-1)
  V.MSR <- 2*est$MS.raters^2/(k-1)
  V.MSE <- 2*est$MS.errors^2/((n-1)*(k-1))
  d2 <- k/n
  d3 <- k-1-k/n
  elem1 <- (V.MSS+V.MSE)/(est$MS.subjects-est$MS.errors)^2
  elem2 <- (V.MSS+d2^2*V.MSR+d3^2*V.MSE)/(est$MS.subjects+d2*est$MS.raters+d3*est$MS.errors)^2
  elem3 <- 2*(V.MSS-d3*V.MSE)/((est$MS.subjects-est$MS.errors)*(est$MS.subjects+d2*est$MS.raters+d3*est$MS.errors))
  
  V.Log.ICC <- elem1+elem2-elem3
  
  est$ICC.agreement <- ifelse(est$ICC.agreement<1e-6,1e-6,est$ICC.agreement)
  L = log(est$ICC.agreement) - qnorm(1-alpha/2)*sqrt(V.Log.ICC)
  U = log(est$ICC.agreement) + qnorm(1-alpha/2)*sqrt(V.Log.ICC)
  return(c("Lower"=exp(L),            
           "Upper"=exp(U),            
           "time"=Sys.time()-start))
}

### Confidence Intervals based on matrix formulation
ci.delta.mat <- function(data,      ## A 2d matrix with columns representing raters and rows representing subjects
                         alpha=0.05
){
  start = Sys.time()
  k <- ncol(data)
  n <- nrow(data)
  icc_agree<- ICC_estimate(data)
  vec1 <- integer(k)+1
  vmat <- cov(data)
  num<-t(vec1)%*%vmat%*%vec1-sum(diag(vmat))
  den <- 1/n*t(vec1)%*%vmat%*%vec1+(n*k-n-k)/n*sum(diag(vmat))+
    k*sum((colMeans(data)-mean(data))^2)
  icc.agree <- num/den
  
  
  elem1 <- (1+(k-1-k/n)*icc.agree)^2*sum(diag(vmat%*%vmat))
  elem2 <- (1-icc.agree/n)^2*(t(vec1)%*%vmat%*%vec1)^2
  elem3 <- 2*(1+(k-1-k/n)*icc.agree)*(1-icc.agree/n)*
    (t(vec1)%*%(vmat%*%vmat)%*%vec1)
  elem4 <- 4*k*icc.agree^2*(sum((colMeans(data)-mean(data))^2)^2)*(n+1)
  elem5 <- (n+1)*(t(vec1)%*%vmat%*%vec1-sum(diag(vmat)))^2
  
  SE.2.ICC <- 2*icc.agree^2*(elem1+elem2-elem3+elem4)/elem5
  L = icc.agree-qt(p=1-alpha/2,n+1)*sqrt(SE.2.ICC)
  U = icc.agree+qt(p=1-alpha/2,n+1)*sqrt(SE.2.ICC)
  return(c("Lower"=L,            
           "Upper"=U,            
           "time"=Sys.time()-start))
  
}


## Confidence interval with Variance partitioning method
ci.variance.partition <- function(data,        ## A 2d matrix with columns representing raters and rows representing subjects
                                  method="F",  ## The confidence interval for variance partitioning approach can be based on F-distribution ("F")
                                  ## or Beta-distribution ("Beta") which must be specified. Default is "F"
                                  alpha=0.05
){
  start = Sys.time()
  est <- ICC_estimate(data, verbose = TRUE)
  k <- ncol(data)
  n <- nrow(data)
  
  sig.2.G <- (est$MS.subjects-est$MS.errors)/k
  sig.2.E <- (est$MS.raters+(n-1)*est$MS.errors)/n
  th.0 <- est$MS.errors
  th.11<- est$MS.subjects
  th.12<- est$MS.raters
  th.4 <- est$MS.subjects+est$MS.raters-est$MS.errors
  phi  <- (n-1)*(k-1)/est$MS.errors^2+(n-1)/est$MS.subjects^2+
    (k-1)/th.12^2+1/th.4^2
  FI.mat <- 1/2*matrix(c(phi, k*((n-1)/th.11^2+1/th.4^2),
                         n*((k-1)/th.12^2+1/th.4^2),
                         n*k/th.4^2, k^2*((n-1)/th.11^2+1/th.4^2), n*k/th.4^2,
                         n*((k-1)/th.12^2+1/th.4^2),k*((n-1)/th.11^2+1/th.4^2),
                         n^2*((k-1)/th.12^2+1/th.4^2)), ncol=3, nrow = 3)
  
  
  vmat<- tryCatch({
    solve(FI.mat)
    },error = function(e){
      solve(FI.mat+diag(3)*1e-2)
                          })
  diag(vmat) <- replace(diag(vmat),diag(vmat)<0, 1e-8)
  if(method=="F"){
    vmat = diag(diag(vmat))
  }
  
  
  tau.2.G <- vmat[2,2]
  tau.2.E <- vmat[1,1]+vmat[3,3]+vmat[1,3]
  
  tau.2.ICC <- sig.2.E^2/(sig.2.E+sig.2.G)^4*tau.2.G+
    sig.2.G^2/(sig.2.E+sig.2.G)^4*tau.2.E-
    2*sig.2.E*sig.2.G/(sig.2.E+sig.2.G)^4*(vmat[2,3]+vmat[1,3])
  
  ########F-approach#######
  if (method=="F"){
    df.G <- max(1,2*sig.2.G^2/tau.2.G)
    df.E <- 2*sig.2.E^2/tau.2.E
    
    L <- sig.2.G*qf(p=alpha/2, df1=df.G, df2=df.E)/
      (sig.2.G*qf(p=alpha/2, df1=df.G, df2=df.E) + sig.2.E)
    U <- sig.2.G*qf(p=1-alpha/2, df1=df.G, df2=df.E)/
      (sig.2.G*qf(p=1-alpha/2, df1=df.G, df2=df.E) + sig.2.E)
    return(c("Lower"=L,            
             "Upper"=U,            
             "time"=Sys.time()-start))
  }else if (method=="Beta"){
    ######Beta-approach######
    mu <- est$ICC.agreement
    sig.2 <- tau.2.ICC
    a = mu*(mu*(1-mu)-sig.2)/sig.2
    b = (1-mu)*(mu*(1-mu)-sig.2)/sig.2
    
    if(a<1 & b <1){
      if(est$ICC.agreement<0.5){
        b=1
      }else{
        a=1
      }
    }
    if(a<0){
      b = 1
      a = est$ICC.agreement/(1-est$ICC.agreement)
    }else if(b<0){
      a = 1
      b = est$ICC.agreement/(1-est$ICC.agreement)
    }
    a <- max(0.01, a)
    b <- max(0.01, b)
    L = qbeta(p=0.05/2, shape1 = a, shape2=b)
    U = qbeta(p=1-0.05/2, shape1 = a, shape2=b)
    return(c("Lower"=L,            
             "Upper"=U,            
             "time"=Sys.time()-start))
  }else{
    cat("Please choose a method between 'F' or 'Beta'")
  }
  
  
}
ci.VPB <- function(data, alpha=0.05){
  return(ci.variance.partition(data = data, method = "Beta", alpha=alpha))
}
ci.VPF <- function(data, alpha=0.05){
  return(ci.variance.partition(data = data, method = "F", alpha=alpha))
}

ci.Wlog <- function(data, alpha=0.05){
  return(ci.delta.log(data = data, alpha=alpha))
}

ci.Wmat <- function(data, alpha=0.05){
  return(ci.delta.mat(data = data, alpha=alpha))
}

## Confidence interval with MLS_A
ci.MLSA <- function(data, alpha = 0.05) {
  start = Sys.time()
  est <- ICC_estimate(data, verbose = TRUE)
  k <- ncol(data)
  n <- nrow(data)
  
  d2 <- k/n
  d3 <- k-1-k/n
  
  F1 <- est$MS.subjects/est$MS.errors
  F2 <- est$MS.raters/est$MS.errors
  
  
  F.n <- function(alpha){qf(alpha, n-1, Inf)}
  F.nk <- function(alpha){qf(alpha, n-1, k-1)}
  F.nnk <- function(alpha){qf(alpha, n-1, (n-1)*(k-1))}
  
  A<- function(alpha){
    (-1+1/F.n(alpha)*F1 + (1-1/F.n(alpha)*F.nnk(alpha))* F.nnk(alpha)/F1)/(n-1 + 1/F.n(alpha)*F.nk(alpha)*F2)
  }
  
  L <- n*max(0,A(1-alpha/2))/(k+n*max(0,A(1-alpha/2)))
  U <- n*max(0,A(alpha/2))/(k+n*max(0,A(alpha/2)))
  
  return(c("Lower"=L,            
           "Upper"=U,            
           "time"=Sys.time()-start))
}

## Confidence interval with GCI
ci.GCI <- function(data,
                   MC=1e5,
                   alpha=0.05) {
  start = Sys.time()
  est <- ICC_estimate(data, verbose = TRUE)
  k <- ncol(data)
  n <- nrow(data)
  
  W11 <- rchisq(MC,df=k-1);
  W21 <- rchisq(MC,df=n-1);
  W31 <- rchisq(MC,df=(n-1)*(k-1));
  
  PivQsosq <- rep(NA,MC);
  PivQgamy <- rep(NA,MC);
  for (m in 1:MC) {
    PivQsosq[m] <- max(c(0,(n-1)*est$MS.subjects/(k*W21[m])-(k*n-k-n+1)*est$MS.errors/(k*W31[m])));
    PivQgamy[m] <-
      (k-1)*est$MS.raters/(n*W11[m]) +
      (n-1)*est$MS.subjects/(k*W21[m]) +
      (n*k-n-k)*(n*k-n-k+1)*est$MS.errors/(n*k*W31[m]) ;
  }
  PivQICC <- PivQsosq/PivQgamy;
  
  L <- unname(quantile(PivQICC,alpha/2, na.rm=TRUE))
  U <- unname(quantile(PivQICC,1-alpha/2,na.rm=TRUE))
  
  return(c("Lower"=L,
           "Upper"=U,
           "time"=Sys.time()-start))
  
}

## Confidence interval with MLS_G
ci.MLSG <- function(data, alpha = 0.05) {
  start = Sys.time()
  est <- ICC_estimate(data, verbose = TRUE)
  k <- ncol(data)
  n <- nrow(data)
  
  d2 <- k/n
  d3 <- k-1-k/n
  
  
  # calculate quantities in the Appendix of Cappelleri & Ting
  H1 <- (1/qf(alpha/2, n-1, Inf))-1
  H2 <- (1/qf(alpha/2, k-1, Inf))-1
  H3 <- (1/qf(alpha/2, (n-1)*(k-1), Inf))-1
  G1 <- 1-(1/qf(1-alpha/2, n-1, Inf))
  G2 <- 1-(1/qf(1-alpha/2, k-1, Inf))
  G3 <- 1-(1/qf(1-alpha/2, (n-1)*(k-1), Inf))
  H12 <- ((1-qf(alpha/2, n-1, k-1))^2 - (H1*qf(alpha/2, n-1, k-1))^2 - G2^2)/(qf(alpha/2, n-1, k-1))
  H13 <- ((1-qf(alpha/2, n-1, (n-1)*(k-1)))^2 - (H1*qf(alpha/2, n-1, (n-1)*(k-1)))^2 - G3^2)/(qf(alpha/2, n-1, (n-1)*(k-1)))
  G12 <- ((qf(1-alpha/2, n-1, k-1)-1)^2 - (G1*qf(1-alpha/2, n-1, k-1))^2 - H2^2)/(qf(1-alpha/2, n-1, k-1))
  G13 <- ((qf(1-alpha/2, n-1, (n-1)*(k-1))-1)^2 - (G1*qf(1-alpha/2, n-1, (n-1)*(k-1)))^2 - H3^2)/(qf(1-alpha/2, n-1, (n-1)*(k-1)))
  
  # 100(1-alpha) per cent upper confidence limit U
  Au <- (1-H1^2)*(est$MS.subjects^2) + (1-G2^2)*(d2^2)*(est$MS.raters^2) + (1-G3^2)*(d3^2)*(est$MS.errors^2) + (2+H12)*d2*est$MS.subjects*est$MS.raters + (2+H13)*d3*est$MS.subjects*est$MS.errors + 2*d2*d3*est$MS.raters*est$MS.errors
  Bu <- (-2)*(1-H1^2)*(est$MS.subjects^2) + 2*(1-G3^2)*d3*(est$MS.errors^2) - (2+H12)*d2*est$MS.subjects*est$MS.raters - (2+H13)*(d3-1)*est$MS.subjects*est$MS.errors + 2*d2*est$MS.raters*est$MS.errors
  Cu <- (1-H1^2)*(est$MS.subjects^2) + (1-G3^2)*(est$MS.errors^2) - (2+H13)*est$MS.subjects*est$MS.errors
  Q1 <- max(0, (Bu^2-4*Au*Cu))
  U <- (-Bu + sqrt(Q1))/(2*Au)
  
  # 100(1-alpha) per cent lower confidence limit L
  Al <- (1-G1^2)*(est$MS.subjects^2) + (1-H2^2)*(d2^2)*(est$MS.raters^2) + (1-H3^2)*(d3^2)*(est$MS.errors^2) + (2+G12)*d2*est$MS.subjects*est$MS.raters + (2+G13)*d3*est$MS.subjects*est$MS.errors + 2*d2*d3*est$MS.raters*est$MS.errors
  Bl <- (-2)*(1-G1^2)*(est$MS.subjects^2) + 2*(1-H3^2)*d3*(est$MS.errors^2) - (2+G12)*d2*est$MS.subjects*est$MS.raters - (2+G13)*(d3-1)*est$MS.subjects*est$MS.errors + 2*d2*est$MS.raters*est$MS.errors
  Cl <- (1-G1^2)*(est$MS.subjects^2) + (1-H3^2)*(est$MS.errors^2) - (2+G13)*est$MS.subjects*est$MS.errors
  Q2 <- max(0, (Bl^2-4*Al*Cl))
  L <- (-Bl - sqrt(Q2))/(2*Al)
  
  return(c("Lower"=L,
           "Upper"=U,
           "time"=Sys.time()-start))
  
}

k_for_nk <-function(N, R, rho){
  phi <- R * (1-rho)/(1+R)
  b = -(N+1)
  d = -phi^2-2*(N-1)*phi^2/(N*R)-(N-1)*phi^2/(N*R^2)
  e = 2*phi^2 - 2*phi^3/(rho*R)-2*(2*N-1)*phi^3/(N*rho*R^2)-2*(N-1)*phi^3/(N*rho*R^3)
  f = -N*phi^2 + 2*phi^3/(rho*R) + 2*phi^3/(rho*R^2)-(1/rho)^2*phi^4/R^2 -
    2*phi^4/(rho^2*R^3) - phi^4/(rho^2*R^4)
  
  A = (-d-e-f)/(N-1)
  B = (N^2*d+N*e+f)/(N-1)
  
  k = (N*A+B)/(A+B)-sqrt((N*A+B)^2-(A+B)*(N^2*A+B))/(A+B)
  
  return(c('k'=k,'n'=N/k))
}

nk_combs <- function(N,
                     rho,
                     R){
  nk <- k_for_nk(N=N, rho=rho, R= R)
  n <- as.integer(nk[2])
  k <- as.integer(nk[1])
  k_rng <- seq(k, sqrt(N))
  n_rng <- as.integer(N/k_rng)
  
  nk <- cbind.data.frame("n"=n_rng, "k"=k_rng)[which(n_rng*k_rng==N),]
  if(nrow(nk)==0){
    return(NULL)
  }else{
    return(cbind.data.frame(nk,'N'=N))
  }
  
}
samplesize.saito<- function(rho,
                            R,
                            target=0.3,
                            nsims = 1e3,
                            method = "MLSG",
                            alpha =0.05,
                            N_max=1e3,
                            N_min=12,
                            tol = 1e-6,
                            seed.start = 0,
                            verbose = FALSE){
  
  St <- Sys.time()
  
  Ncombs <- do.call(rbind.data.frame,
                    lapply(N_min:N_max,
                           function(N)
                             nk_combs(N=N,
                                      rho=rho,
                                      R=R)))
  
  ci.func <- eval(parse(text=paste0("ci.",method)))
  width<- function(n,k,rho,R,alpha=0.05){
    cl <- makeCluster(nclus, outfile="hoopa.txt")
    on.exit(stopCluster(cl), add = TRUE)
    clusterExport(cl, list("n",
                           "k",
                           "rho",
                           "R",
                           "nsims",
                           "ci.func",
                           "alpha",
                           "data_gen",
                           "ICC_estimate",
                           "target",
                           "nclus",
                           "seed.start"),
                  envir=environment())
    ciw <- parSapply(cl, 1:nclus, function(i) {
      fail_count = 0
      wd = c()
      st_seed = i*nsims+seed.start
      while(length(wd)<nsims/nclus){
        set.seed(st_seed)
        data <- data_gen(n = n, k =k, rho=rho, R=R)
        cis <- try(ci.func(data, alpha= alpha))
        if(inherits(cis,"try-error")){
          fail_count <- fail_count + 1
        }
        if(fail_count>round(nsims/nclus)){
          return(Inf)
        }else{
          wd <- c(wd, cis[['Upper']]-cis[['Lower']])
        }
      }
      
      return(mean(wd))
    })
    return('width'=mean(ciw, na.rm=T))
  }
  
  
  
  w <- function(N){
    nk <- Ncombs[Ncombs[['N']]==N,]
    wd <- sapply(1:nrow(nk), function(x) width(n=nk[x,1],
                                               k=nk[x,2],
                                               rho=rho,
                                               R=R,
                                               alpha=alpha))
    return(target-min(wd, na.rm=TRUE))
  }
  
  N.a <- N_min
  N.b <- N_max
  Nf <- N_max
  #N.mid <- Ncombs[which(Ncombs[['N']]>=(N.a+N.b)/2),][1,3]
  N.search <- c()
  #wd.mid <- w(N.mid)
  w.search <- c()
  
  N_maxa <-  Ncombs[which(Ncombs[['N']]==max(Ncombs[['N']])),][1,3]
  if(w(N_maxa)>target){
    if(verbose==FALSE){
      return("Sample.Size" = Inf)
    }else{
      return(list("Sample.Size" = Inf,
                  "Search" = cbind.data.frame("N"=Inf,
                                              "Min.width"=NA),
                  "All.combinations" = Ncombs,
                  "Time"=Sys.time()-St))
    }
  }
  
  for (i in 1:length(unique(Ncombs[['N']]))){
    N.mid <- Ncombs[which(Ncombs[['N']]>=(N.a+N.b)/2),][1,3]
    wd.mid <- w(N.mid)
    N.search <- c(N.search, N.mid)
    w.search <- c(w.search, wd.mid)
    cat("N:", tail(N.search,1),"min width:",target - tail(w.search,1), "\n")
    
    if (wd.mid>=0 & wd.mid<=tol){
      Nf <- N.mid
      break
    }else if(length(N.search)>2 & tail(N.search,2)[1]==tail(N.search,1)){
      Nf <- tail(N.search,1)
      break
    }
    if(sign(wd.mid)==sign(w(N.a))){
      N.a <- N.mid
    }else{
      N.b <- N.mid
    }
    
    
    
    N.mid <- Ncombs[which(Ncombs[['N']]>=(N.a+N.b)/2),][1,3]
  }
  
  nf <- Ncombs[Ncombs[['N']]==Nf,]
  if(nrow(nf)==1){
    wd.f <- cbind.data.frame(nf, "Width"=wd.mid)
  }else{
    wd.f <- cbind.data.frame(nf, "Width"=
                               sapply(1:nrow(nf), function(x) target-width(n=nf[x,1],
                                                                           k=nf[x,2],
                                                                           rho=rho,
                                                                           R=R)))
  }
  
  wd.f[["Width"]] = target-wd.f$Width
  minwd<- wd.f[which.min(wd.f$Width),]
  
  if(verbose==FALSE){
    return("Sample.Size" = minwd)
  }else{
    return(list("Sample.Size" = minwd,
                "Search" = cbind.data.frame("N"=N.search,
                                            "Min.width"=target-w.search),
                "All.combinations" = Ncombs,
                "Time"=Sys.time()-St))
  }
  
}


mybisection <- function(f,lower,upper,tol=1e-4) {
  # THIS FUNCTION RETURNS A TWO-ELEMENT VECTOR:
  #  ELEMENT 1 IS THE ESTIMATED VALUE AT WHICH THE FUNCTION IS ZERO
  #  ELEMENT 2 IS THE NUMBER OF EVALUATIONS TO GET TO THE VALUE (SEARCH STEPS)
  # For an arbitrary function "f", this function applies a bisection algorithm
  # to find a zero between "lower" and "upper", assuming they are of different signs.
  newpoint = 0
  flow <- f(lower);
  fupper <- f(upper);
  diff <- upper - lower;
  feval <- 2;
  
  if (flow*fupper>0) {stop("Interval does not contain zero.\n"); }
  
  while ( abs(diff)>1 & feval < 100) {
    newpoint = round( (lower+upper)/2 );
    newf <- f(newpoint);
    if (abs(newf)<= tol) break;
    if (newpoint==lower) break;
    if (newpoint==upper) break;
    if (flow*newf < 0) {upper <- newpoint; }
    if (fupper*newf < 0) {lower <- newpoint; }
    diff <- upper-lower;
    feval <- feval+1;
  }
  return(c(newpoint,feval))
}

myMLSvolfun <- function(alpha,b0,l0,r0,sigmabsq,sigmalsq,sigmaesq,MCruns=100000,randseed=5) {
  
  set.seed(randseed);
  bigreps <- MCruns;
  bigdf <- 10000000;
  
  # Calculate vectors of observed mean squares
  mysbsqs <- ((sigmaesq+l0*r0*sigmabsq)/(b0-1))*rchisq(bigreps,b0-1);
  myslsqs <- ((sigmaesq+b0*r0*sigmalsq)/(l0-1))*rchisq(bigreps,l0-1);
  mysesqs <- (sigmaesq/(b0*l0*r0-b0-l0+1))*rchisq(bigreps,b0*l0*r0-b0-l0+1);
  
  # Constants from  Burdick book
  myG2 <- 1-qf(alpha/2,bigdf,b0-1);
  myF5 <- qf(1-alpha/2,b0-1,b0*l0*r0-b0-l0+1);
  myF4 <- qf(alpha/2,l0-1,b0-1);
  myH2 <- qf(1-alpha/2,bigdf,b0-1)-1;
  myF6 <- qf(alpha/2,b0-1,b0*l0*r0-b0-l0+1);
  myF3 <- qf(1-alpha/2,l0-1,b0-1);
  
  # Calculate the lower bounds for the bigreps intervals
  Lstarnums <- 
    b0*(1-myG2)*mysbsqs^2-b0*mysbsqs*mysesqs+b0*(myF5-(1-myG2)*myF5^2)*mysesqs^2;
  
  Lstardenoms <- 
    l0*(b0*r0-1)*mysbsqs*mysesqs+l0*(1-myG2)*mysbsqs*myslsqs/myF4;
  
  Lstars <- Lstarnums/Lstardenoms;
  for (i in 1:length(Lstars)) { Lstars[i] <- max(Lstars[i],0); }
  
  Ls <- Lstars/(1+Lstars);  
  
  # Calculate the upper bounds for the bigreps interval
  Ustarnums <- 
    b0*(1+myH2)*mysbsqs^2-b0*mysbsqs*mysesqs+b0*(myF6-(1+myH2)*myF6^2)*mysesqs^2;
  
  Ustardenoms <- 
    l0*(b0*r0-1)*mysbsqs*mysesqs+l0*(1+myH2)*mysbsqs*myslsqs/myF3;
  
  Ustars <- Ustarnums/Ustardenoms;
  for (i in 1:length(Ustars)) { Ustars[i] <- max(Ustars[i],0); }
  
  Us <- Ustars/(1+Ustars);
  # Returns the mean of the interval widths
  meandiffs <- mean(Us-Ls);
  
  myret <- meandiffs;
  myret;
}

myMLSvolfunRBization <- function(alpha,b0,l0,r0,sigmabsq,sigmalsq,sigmaesq,MCruns=100000,randseed=5) {
  
  set.seed(randseed);
  bigreps <- MCruns;
  bigdf <- 10000000;
  
  # Uses Rao-Blackwellization with RBreps on sigmalsq
  RBreps <- 100;
  myslsqs <- ((sigmaesq+b0*r0*sigmalsq)/(l0-1))*rchisq(RBreps,l0-1);
  
  
  # Calculate vectors of observed mean squares
  mysbsqs <- ((sigmaesq+l0*r0*sigmabsq)/(b0-1))*rchisq(bigreps,b0-1);
  mysesqs <- (sigmaesq/(b0*l0*r0-b0-l0+1))*rchisq(bigreps,b0*l0*r0-b0-l0+1);
  
  # Constants from  Burdick book
  myG2 <- 1-qf(alpha/2,bigdf,b0-1);
  myF5 <- qf(1-alpha/2,b0-1,b0*l0*r0-b0-l0+1);
  myF4 <- qf(alpha/2,l0-1,b0-1);
  myH2 <- qf(1-alpha/2,bigdf,b0-1)-1;
  myF6 <- qf(alpha/2,b0-1,b0*l0*r0-b0-l0+1);
  myF3 <- qf(1-alpha/2,l0-1,b0-1);
  
  mymeandiffs <- rep(NA,RBreps);
  for (RBstep in 1:RBreps) {
    # Calculate the lower bounds for the bigreps intervals
    Lstarnums <- 
      b0*(1-myG2)*mysbsqs^2-b0*mysbsqs*mysesqs+b0*(myF5-(1-myG2)*myF5^2)*mysesqs^2;
    
    Lstardenoms <- 
      l0*(b0*r0-1)*mysbsqs*mysesqs+l0*(1-myG2)*mysbsqs*myslsqs[RBstep]/myF4;
    
    Lstars <- Lstarnums/Lstardenoms;
    for (i in 1:length(Lstars)) { Lstars[i] <- max(Lstars[i],0); }
    
    Ls <- Lstars/(1+Lstars);  
    
    # Calculate the upper bounds for the bigreps interval
    Ustarnums <- 
      b0*(1+myH2)*mysbsqs^2-b0*mysbsqs*mysesqs+b0*(myF6-(1+myH2)*myF6^2)*mysesqs^2;
    
    Ustardenoms <- 
      l0*(b0*r0-1)*mysbsqs*mysesqs+l0*(1+myH2)*mysbsqs*myslsqs[RBstep]/myF3;
    
    Ustars <- Ustarnums/Ustardenoms;
    for (i in 1:length(Ustars)) { Ustars[i] <- max(Ustars[i],0); }
    
    Us <- Ustars/(1+Ustars);
    # Returns the mean of the interval widths
    mymeandiffs[RBstep] <- mean(Us-Ls);
  }
  myret <- mean(mymeandiffs);
  myret;
}
MLSb0fun <- function(alpha=0.05,initial=4,stepsize,target,L,R,sigmabsq,sigmalsq,sigmaesq,MCreps=100000,randseed=5) {
  # Initial is the starting value for number of biological replicates (smallest possible)
  # Steps is the size of the steps to increase by until obtaining a width below target
  # Target is the targeted mean interval width
  
  # First, check if the targeted width is achievable. 
  
  curB <- initial;
  nexB <- initial;
  myret <- initial;
  nexmu <- myMLSvolfun(alpha,nexB,L,R,sigmabsq,sigmalsq,sigmaesq,MCreps,randseed)[1];
  
  if (myMLSvolfun(alpha,curB,L,R,sigmabsq,sigmalsq,sigmaesq)[1]<target) {
    return(initial); break;
  }
  
  # Create the function to which the bisection algorithm will be applied
  tempfun <- function(x) myMLSvolfun(alpha,x,L,R,sigmabsq,sigmalsq,sigmaesq,MCreps,randseed)[1]-target;
  numsteps <- 0;
  
  while (tempfun(nexB)>0 & numsteps < 1e3) {
    nexB <- nexB + stepsize;
    numsteps <- numsteps+1;
  }
  if (numsteps==1e3) { myret <- c(Inf,Inf); }
  else {
    curB <- nexB-stepsize; 
    mybisret <- mybisection(tempfun,curB,nexB);
    myret <- c(1 + mybisret[1]);
  }
  myret;
}


MLSb0estfun <- function(alpha,initial,stepsize,target,L,R,sigmabsq,sigmalsq,sigmaesq,MCreps) {
  
  # if (target < minwidthb0varies(alpha,sigmabsq,sigmalsq,sigmaesq,L)) 
  #  { myret <- Inf; }
  # else { 
  myb0s <- rep(NA,40);
  for (i in 1:40) {
    myb0s[i] <- MLSb0fun(alpha,initial,stepsize,target,L,R,sigmabsq,sigmalsq,sigmaesq,MCreps,randseed=i)[1] 
  }
  cat(myb0s)
  myret <- c(mean(myb0s),
             sqrt(var(myb0s)/length(myb0s)),min(myb0s),max(myb0s));
  # }
  myret;
}


mypigamma <- function(myinput,alpha,beta) {
  # This fixes a problem in pigamma that it returns errors for 0 and smaller
  myret = rep(NA,length(myinput));
  
  for (i in 1:length(myinput)) {
    if (is.na(myinput[i])||myinput[i] <= 0) { 
      myret[i] = 0; 
    }else if(myinput[i] > 0) {
      myret[i] = pigamma(myinput[i], alpha,beta);
    }
  }
  myret;
}


mycircrb <- function(g0,M1,M2,M3,a,b,c,d,l0) {
  
  input23 = rep(NA,length(M1));
  for (i in 1:length(M1)) {
    input23[i] <- (max(M2[i]*a-M3[i]*b,0)- M2[i]*a*g0 - M3[i]*d*g0)/(c*g0) ;
  }
  
  f23 <- #1- pinvchisq(input23, nu = (l0-1))
    1-mypigamma( input23
                 ,alpha=(l0-1)/2,beta=1/2);
  myret <- f23;
  myret <- mean(myret);
  myret;
}

quantilecircrb <- function(myquant,M1,M2,M3,a,b,c,d,l0) {
  
  myfun <- function(x) { mycircrb(x,M1,M2,M3,a,b,c,d,l0)-myquant; }
  if (myfun(0.0000001)>= 0) { myres = 0; }
  else {
    rootfind <- uniroot(myfun,lower=0.0000001,upper=0.999999);
    myres <- rootfind$root;
  }
  myres;
  
}


####
##  Ending of code cut-and-pasted from RB file
####


####
##  Beginning of code cut-and-pasted from simplemcwithrbvscvwithrb.txt file
####


CtrlVoverMS = function(alpha,sigmabsq,sigmalsq,sigmaesq,b0,l0,r0,randomseed=2,mcrunsMS=100, mcrunsW=100) {
  
  set.seed(randomseed);
  
  iccb = sigmabsq/10;
  
  lambda1sq <- sigmaesq+b0*r0*sigmalsq;
  lambda2sq <- sigmaesq+l0*r0*sigmabsq;
  
  #Generate ws, using same set of w's for each mean square
  
  sbsq = (sigmaesq+sigmabsq*l0*r0)*rchisq(mcrunsMS,b0-1)/(b0-1);
  slsq = (sigmaesq+sigmalsq*b0*r0)*rchisq(mcrunsMS,l0-1)/(l0-1);
  sesq = sigmaesq*rchisq(mcrunsMS,b0*l0*r0-b0-l0+1)/(b0*l0*r0-b0-l0+1);
  a = (b0-1)*sbsq/(l0*r0);
  b = (b0*l0*r0-b0-l0+1)*sesq/(l0*r0);
  c = (l0-1)*slsq/(b0*r0);
  d = b*(b0*l0*r0-b0-l0)/b0;
  theselowers = rep(NA,mcrunsMS);
  theseuppers = rep(NA,mcrunsMS);
  
  retwidths = rep(NA,mcrunsMS);
  # Generate mean squares and CI's
  for (i in 1:mcrunsMS) {
    W1 <- rchisq(mcrunsW,l0-1);
    W2 <- rchisq(mcrunsW,b0-1);
    W3 <- rchisq(mcrunsW,b0*l0*r0-b0-l0+1);
    M1 = 1/W1;
    M2 = 1/W2;
    M3 = 1/W3;
    theselowers[i] = quantilecircrb(alpha/2,M1,M2,M3,a[i],b[i],c[i],d[i],l0);
    theseuppers[i] = quantilecircrb(1-alpha/2,M1,M2,M3,a[i],b[i],c[i],d[i],l0);
  }
  myrats = slsq/sbsq;
  myFvar = (lambda1sq/lambda2sq)^2*2*(b0-1)^2*(b0-1+l0-1-2)/((l0-1)*(b0-1-2)^2*(b0-1-4));
  mycovLower = cov(theselowers,myrats);
  CtrlVLower = theselowers - (mycovLower/myFvar)*((slsq/sbsq) - (lambda1sq/lambda2sq)*(b0-1)/(b0-1-2));
  mycovUpper = cov(theseuppers,myrats);
  CtrlVUpper = theseuppers - (mycovUpper/myFvar)*((slsq/sbsq) - (lambda1sq/lambda2sq)*(b0-1)/(b0-1-2));
  retwidths = CtrlVUpper - CtrlVLower;
  
  for (i in 1:mcrunsMS) { retwidths[i] = max(0,retwidths[i]); }
  
  retwidths;
}


####
##  Ending of code cut-and-pasted from simplemcwithrbvscvwithrb.txt file
####





myGCIwidthfun = function(alpha=0.05, sigmabsq,sigmalsq,sigmaesq,b0,l0,r0,mcrunsMS=100, mcrunsW=100,randomseed=2) {
  
  widthvec = CtrlVoverMS(alpha, sigmabsq,sigmalsq,sigmaesq,b0,l0,r0,mcrunsMS, mcrunsW, randomseed);
  
  myret = mean(widthvec);
  
  myret;
  
  
}


b0estfun<- function(alpha=0.05,initial=3,biggest=1000,target,sigmabsq,sigmalsq,sigmaesq,l0,r0,mcrunsMS=100, mcrunsW=100, randomseed=2) {
  
  myinitial = initial;
  
  tempfun = function(x) myGCIwidthfun(alpha, sigmabsq,sigmalsq,sigmaesq,x,l0,r0,mcrunsMS, mcrunsW, randomseed) - target;
  
  while(is.na(tempfun(myinitial))){
    cat("Adjust minimum", myinitial,":", tempfun(myinitial), "\n")
    myinitial = myinitial+1
  }
  if (tempfun(myinitial) > 0) {
    myres = mybisection(tempfun,myinitial,biggest)
  }
  else {
    myres = myinitial;
  }
  myres;
  
}



bisection <- function(f, a, b,
                      tol=1e-5,
                      max.iter=1e3,
                      integer_vals =TRUE,
                      verbose=TRUE){
  
  if(f(a) * f(b) > 0){
    #cat(f(a), f(b), "\n")
    stop("The supplied number of participants is inadequate to obtain a sample.")
  }
  
  vals.search <- c()
  fvals.search <- c()
  
  for(i in 1:max.iter){
    d <- ifelse(integer_vals == TRUE, as.integer((a + b)/2), (a + b)/2)
    val.it <- f(d)
    
    if(verbose==TRUE){
      cat("Values: n=", d, "f(n)=", val.it, "bisection", "\n")
    }
    
    vals.search <- c(vals.search, d)
    fvals.search <- c(fvals.search, val.it)
    
    if(length(vals.search) > 2 && tail(vals.search, 1) == tail(vals.search, 2)[1]){
      if(tail(fvals.search, 1) > 0){
        op <- tail(vals.search, 1)
      }else{
        dvals <- vals.search - tail(vals.search, 1)
        op <- min(dvals[dvals > 0])
      }
      break
    }
    
    if(integer_vals == TRUE){
      if (abs(b - a) < 1) {
        op <- d
        break
      }
    }
    
    if (val.it >= 0 && val.it < tol) {
      op <- d
      break
    }
    
    if (f(a) * f(d) < 0) {
      b <- d
    } else {
      a <- d
    }
  }
  
  if(f(max(a, b)) > 0){
    op <- max(a, b)
  }else{
    op <- Inf
  }
  
  if(verbose == TRUE){
    return(list("Search" = vals.search,
                "Search.vals" = fvals.search,
                "final" = op,
                "final.val" = f(op)))
  }else{
    return(op)
  }
}

samplesize.doros<- function(rho,
                            R,
                            k,
                            target=0.3,
                            nsims = 1e3,
                            method = "MLSG",
                            alpha =0.05,
                            n_max=1e3,
                            n_min=4,
                            tol = 1e-6,
                            seed.start = 0,
                            verbose = FALSE){
  
  St <- Sys.time()
  
  
  ci.func <- eval(parse(text=paste0("ci.",method)))
  width<- function(n,k,rho,R,alpha=0.05){
    cl <- makeCluster(10, outfile="hoopa.txt")
    on.exit(stopCluster(cl), add = TRUE)
    clusterExport(cl, list("n",
                           "k",
                           "rho",
                           "R",
                           "nsims",
                           "ci.func",
                           "alpha",
                           "data_gen",
                           "ICC_estimate",
                           "target",
                           "nclus",
                           "seed.start"),
                  envir=environment())
    ciw <- parSapply(cl, 1:nclus, function(i) {
      fail_count = 0
      wd = c()
      st_seed = i*nsims+seed.start
      while(length(wd)<nsims/nclus){
        set.seed(st_seed)
        data <- data_gen(n = n, k =k, rho=rho, R=R)
        cis <- try(ci.func(data, alpha= alpha))
        if(inherits(cis,"try-error")){
          fail_count <- fail_count + 1
        }
        
        if(fail_count>round(nsims/10)){
          return(Inf)
        }else{
          wd <- c(wd, cis[['Upper']]-cis[['Lower']])
        }
      }
      
      return(mean(wd))
    })
    return('width'=mean(ciw, na.rm=T))
  }
  
  
  
  w <- function(n){
    wd <- width(n=n,k=k,rho=rho, R=R,alpha=alpha)
    return(target-min(wd, na.rm=TRUE))
  }
  
  n.a <- n_min
  n.b <- n_max
  nf <- n_max
  n.search <- c()
  w.search <- c()
  
  
  if(w(n_max)<0){
    if(verbose==FALSE){
      return("Sample.Size" = Inf)
    }else{
      return(list("Sample.Size" = Inf,
                  "Search" = cbind.data.frame("n"=Inf,
                                              "Min.width"=NA),
                  "Time"=Sys.time()-St))
    }
  }
  
  bis = bisection(f = function(x) w(x),
                  a = n_min,
                  b = n_max,
                  integer_vals = TRUE,
                  verbose = TRUE)
  if(verbose==FALSE){
    return("Sample.Size" = bis[["final"]])
  }else{
    return(list("Sample.Size" = bis[["final"]],
                "Final.Val"   = target-bis[["final.val"]],
                "Search.Vals" = target-bis[['Search.vals']],
                "Time"=Sys.time()-St))
  }
}


samplesize.dobbin <- function(rho, R, k, target, max_n=1e3, min_n=4, seed=2, method="GCI", alpha= 0.05, reps = 1e2, reps_VC = 1e2){
  
  st <- Sys.time()
  
  cat("Start*************", "k", k, "R", R, "rho", rho, "target",target, "method", method, "Starting\n")
  opt = list()
  opt[['k']] = k
  opt[['rho']] = rho
  opt[['R']] = R
  opt[['target']] = target
  
  width_fun <- if(method=="GCI"){
    function(x){
      target-myGCIwidthfun(sigmabsq = (R+1)/(1/rho-1), 
                           sigmalsq = R, 
                           sigmaesq = 1,
                           b0=x,
                           l0 = k,
                           r0=1,
                           mcrunsMS = reps, 
                           mcrunsW = reps_VC,
                           randomseed = seed)
    }
  }else if(method=="MLSG"){
    function(x){
      target-myMLSvolfun(alpha=alpha,
                         b0=x,
                         l0 = k,
                         r0=1,
                         sigmabsq = (R+1)/(1/rho-1), 
                         sigmalsq = R, 
                         sigmaesq = 1,
                         MCruns = reps,
                         randseed = seed
      )
    }
  }
  
  if(width_fun(min_n)>0){
    opt[['final']] = min_n
    opt[['final.val']] = width_fun(min_n)
    return(opt)
  }
  if(width_fun(max_n)<0){
        opt[['final']] = Inf
        opt[['final.val']] = target-width_fun(max_n)
        return(opt)
    }
  
  
  
  
  bis <- bisection(f=width_fun,
                   a = min_n,
                   b = max_n,
                   integer_vals = TRUE,
                   verbose = TRUE
  )
  cat("k", k, "R", R, "rho", rho, "target",target, "time", Sys.time()-st, "Complete*************\n")
  opt[['time']] = Sys.time()-st
  opt[['final']] = bis[['final']]
  opt[['Search.vals']]=target-bis[['Search.vals']]
  opt[['final.val']] = target-bis[['final.val']]
  return(opt)
  
}

