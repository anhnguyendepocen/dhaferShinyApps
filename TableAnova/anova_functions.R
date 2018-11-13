TableAnova2=function(X,alpha){
  library(agricolae)
  library(plyr)
  X=as.data.frame(X)
  X=na.omit(X)
  X[,1]=factor(X[,1])
  X[,2]=factor(X[,2])
  cond1=levels(X[,1])
  cond2=levels(X[,2])
  p=length(cond1)
  Y=X[,-c(1,2)]
  q=ncol(Y)
  res1=list()
  for(i in 1:q){
    res=list()
    for(j in 1:length(cond1)){
      res[[j]]=rep(NA,length(cond2))
      names(res[[j]])=cond2
      x=Y[X[,1]==cond1[j],i]
      a=X[X[,1]==cond1[j],2]
      a=droplevels(a)
      l=length(levels(a))
      ll=length(unique(x))
      if(ll>1){
      if(l>1){
        m1=aov(x~a) 
        z=HSD.test(m1,"a", group=TRUE,console = F,alpha = alpha)
        zsd=tapply(x,a,sd)
        xx=paste(round(z$groups$x,3),"(",round(zsd,3),")",z$groups$groups,sep="")
        names(xx)=rownames(z$groups)
        ii=match(levels(a),names(xx))
        xx=xx[ii]
        res[[j]][names(res[[j]])%in%names(xx)]=xx
      }
      }
    }
    names(res)=cond1
    res1[[i]]=ldply(res)
    colnames(res1[[i]])[1]=colnames(X)[1]
  }
  names(res1)=colnames(Y)
  res2=ldply(res1)
  colnames(res2)[1]="Variable"
  return(res2)
}


TableAnova1=function(X,alpha){
  library(agricolae)
  library(plyr)
  X=as.data.frame(X)
  X=na.omit(X)
  
  cond1=levels(X[,1])
  
  p=length(cond1)
  Y=X[,-1]
  q=ncol(Y)
  res1=list()
  for(i in 1:q){
    x=Y[,i]
    ll=length(unique(x))
    a=X[,1]
    a=droplevels(a)
    l=length(unique(levels(a)))
    if(ll>1){
      if(l>1){
    m1=aov(x~a) 
    z=HSD.test(m1,"a", group=TRUE,console = F,alpha = alpha)
    zsd=tapply(x,a,sd)
    xx=paste(round(z$groups$x,3),"(",round(zsd,3),")",z$groups$groups,sep="")
    names(xx)=rownames(z$groups)
    ii=match(levels(a),names(xx))
    xx=xx[ii]
    res1[[i]]=xx 
      }
    }
  }
  
  names(res1)=colnames(Y)
  res2=ldply(res1)
  colnames(res2)[1]="Variable"
  return(res2)
}