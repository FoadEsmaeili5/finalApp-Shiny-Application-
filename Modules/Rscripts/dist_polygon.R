
dist_polygon=function(dens,params,xxmax,xxmin=NULL,xcol="red",...){
  if(is.null(xxmin)){
  x=seq(ifelse(is.null(params$xmin),xxmax-20,params$xmin),xxmax,length.out = 1e5)
    }else{
  x=seq(xxmin,xxmax,length.out = 1e5)
    }
  y=NULL
  fdensity=function(dens,params)
    switch(dens,
           "beta" = dbeta(x,params$par1,params$par2),
           "cauchy"=dcauchy(x,params$par1,params$par2),
           "chisq"=dchisq(x,params$par1),
           "exp"=dexp(x,params$par1),
           "f"=df(x,params$par1,params$par2),
           "gamma"=dgamma(x,params$par1,params$par2),
           "lnorm"=dlnorm(x,params$par1,params$par2),
           "norm"=dnorm(x,params$par1,params$par2),
           "t"=dt(x,params$par1),
           "unif"=dunif(x,params$par1,params$par2),
           "weibull"=dweibull(x,params$par1,params$par2)
    )
  if(params$discrete){
    if(is.null(xxmin)){
        x=params$xmin:params$xmax
       xcol=ifelse(x<=xxmax,"red","black")
      dist_Curve(dens,params,ylab="",xlab="x",col=xcol,...)
    }else{
      x=params$xmin:params$xmax
      xcol=ifelse(x>xxmax | x<xxmin,"black","red")
      dist_Curve(dens,params,ylab="",xlab="x",col=xcol,...)
    }
  }else{
    polygon(c(min(x),x,max(x)),c(0,fdensity(dens,params),0),col=xcol,...)
  }
  
}
