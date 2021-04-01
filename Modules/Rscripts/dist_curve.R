dist_Curve=function(dens,params,pdf=T,...){
  
  # pdf plot ----------------------------------------------------------------
  #if(pdf){
    
    # betaDens=function(params){
    #   curve(dbeta(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # cauchyDens=function(params){
    #   curve(dcauchy(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # chisqDens=function(params){
    #   curve(dchisq(x,params$par1),params$xmin,params$xmax,...)
    # }
    # expDens=function(params){
    #   curve(dexp(x,params$par1),params$xmin,params$xmax,...)
    # }
    # fDens=function(params){
    #   curve(df(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # gammaDens=function(params){
    #   curve(df(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # lnormDens=function(params){
    #   curve(dlnorm(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # normDens=function(params){
    #   curve(dnorm(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # tDens=function(params){
    #   curve(dt(x,params$par1),params$xmin,params$xmax,...)
    # }
    # unifDens=function(params){
    #   curve(dunif(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # weibullDens=function(params){
    #   curve(dweibull(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # switch (dens,
    #         "dbeta" = betaDens(params),
    #         "dcauchy"=cauchyDens(params),
    #         "dchisq"=chisqDens(params),
    #         "dexp"=expDens(params),
    #         "df"=fDens(params),
    #         "dgamma"=gammaDens(params),
    #         "dlnorm"=lnormDens(params),
    #         "dnorm"=normDens(params),
    #         "dt"=tDens(params),
    #         "dunif"=unifDens(params),
    #         "dweibull"=weibullDens(params)
    # )
    #}
  par(lwd=3)
  denss=ifelse(pdf,paste("d",dens,sep=""),paste("p",dens,sep=""))
  if(params$discrete){
    fdistPlot1=function(dens,par1,xmin,xmax,...){
      x<-xmin:xmax
      plot(x,eval(call(dens,x,par1)),type=ifelse(str_starts(dens,"p"),"s","h"),...)
    }
    
    fdistPlot2=function(dens,par1,par2,xmin,xmax,...){
      x<-xmin:xmax
    plot(x,eval(call(dens,x,par1,par2)),type=ifelse(str_starts(dens,"p"),"s","h"),...)
    }
    fdistPlot3=function(dens,par1,par2,par3,xmin,xmax,...){
      x<-xmin:xmax
      plot(x,eval(call(dens,x,par1,par2,par3)),type=ifelse(str_starts(dens,"p"),"s","h"),...)
    }
    
    if(is.null(params$par2)){
      fdistPlot1(denss,params$par1,params$xmin,params$xmax,...)
    }else{
      if(is.null(params$par3)){
      fdistPlot2(denss,params$par1,params$par2,params$xmin,params$xmax,...)
      }else{
      fdistPlot3(denss,params$par1,params$par2,params$par3,params$xmin,params$xmax,...)
    }
    }
    
  }else{
    fdist1Curve=function(dens,par1,xmin,xmax,...){
        curve(eval(call(dens,x,par1)),xmin,xmax,...)
      }
    fdist2Curve=function(dens,par1,par2,xmin,xmax,...){
        curve(eval(call(dens,x,par1,par2)),xmin,xmax,...)
      }
    if(is.null(params$par2)){
      fdist1Curve(denss,params$par1,params$xmin,params$xmax,...)
    }else{
      fdist2Curve(denss,params$par1,params$par2,params$xmin,params$xmax,...)
    }
  }
    
    # cdf plot ----------------------------------------------------------------
    
  #}else{
    
    
    # betap=function(params){
    #   curve(pbeta(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # cauchyp=function(params){
    #   curve(pcauchy(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # chisqp=function(params){
    #   curve(pchisq(x,params$par1),params$xmin,params$xmax,...)
    # }
    # expp=function(params){
    #   curve(pexp(x,params$par1),params$xmin,params$xmax,...)
    # }
    # fp=function(params){
    #   curve(pcauchy(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # gammap=function(params){
    #   curve(pgamma(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # lnormp=function(params){
    #   curve(plnorm(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # normp=function(params){
    #   curve(pnorm(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # tp=function(params){
    #   curve(pt(x,params$par1),params$xmin,params$xmax,...)
    # }
    # unifp=function(params){
    #   curve(punif(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # weibullp=function(params){
    #   curve(pcauchy(x,params$par1,params$par2),params$xmin,params$xmax,...)
    # }
    # switch (dens,
    #         "dbeta" = betap(params),
    #         "dcauchy"=cauchyp(params),
    #         "dchisq"=chisqp(params),
    #         "dexp"=expp(params),
    #         "df"=fp(params),
    #         "dgamma"=gammap(params),
    #         "dlnorm"=lnormp(params),
    #         "dnorm"=normp(params),
    #         "dt"=tp(params),
    #         "dunif"=unifp(params),
    #         "dweibull"=weibullp(params)
    # )
}

#dist_Curve("beta",params = list(xmin=0,xmax=1,par1=2,par2=4,par3=NULL,discrete=F),ylab="foad")
# x=0:40
 #plot(x,eval(call("dexp",x,20)),type=ifelse(str_starts("dbinom",pattern = "d"),"h","s"),ylab="foad")
