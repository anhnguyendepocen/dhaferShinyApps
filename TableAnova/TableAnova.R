TableAnova2=function(X){
  library(agricolae)
  library(plyr)
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
      l=length(unique(a))
      if(l>1){
      m1=aov(x~a) 
      z=HSD.test(m1,"a", group=TRUE,console = F)
      zsd=tapply(x,a,sd)
      x=paste(round(z$groups$means,3),"(",round(zsd,3),")",z$groups$M,sep="")
      names(x)=levels(a)
      res[[j]][names(res[[j]])%in%names(x)]=x
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



TableAnova1=function(X){
  library(agricolae)
  library(plyr)
  cond1=levels(X[,1])
  
  p=length(cond1)
  Y=X[,-1]
  q=ncol(Y)
  res1=list()
  for(i in 1:q){
      x=Y[,i]
      a=X[,1]
      a=droplevels(a)
      m1=aov(x~a) 
      z=HSD.test(m1,"a", group=TRUE,console = F)
      zsd=tapply(x,a,sd)
      x=paste(round(z$groups$means,3),"(",round(zsd,3),")",z$groups$M,sep="")
      names(x)=levels(a)
      res1[[i]]=x 
    }

  names(res1)=colnames(Y)
  res2=ldply(res1)
  colnames(res2)[1]="Variable"
  return(res2)
}