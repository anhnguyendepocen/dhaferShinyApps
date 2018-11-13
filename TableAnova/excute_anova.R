### Example Nesrine

X=Sminja_Table_1[,c(2,4,6:12)]
X=na.omit(X)
X=as.data.frame(X)
X$Variety=factor(X$Variety)
X$MI=factor(X$MI)
x1=TableAnova2(X,alpha = 0.05)


library(dplyr)
X%>%group_by(MI,Variety)%>%summarise(x1=mean(Acidity))

### Execute 
#1 
X=Sminja_Table_1[,c(2,4,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/sminja_anova.csv")

#2
X=Gafsa_Table_1[,c(2,4,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/gafsa_anova.csv")

#3 
X=Fahes_Table_1[,c(2,4,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/fahs_anova.csv")


#4 
X=Chetoui_Table_1[,c(3,4,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/chetoui_anova.csv")

#5 

X=Arbosana_Table_1[,c(3,4,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/arbosana_anova.csv")


#6 

X=Arbequina_Table_1[,c(3,4,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/arbequina_anova.csv")

## The opposite


#1 
X=Sminja_Table_1[,c(4,2,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/sminja_anova2.csv")

#2
X=Gafsa_Table_1[,c(4,2,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/gafsa_anova2.csv")

#3 
X=Fahes_Table_1[,c(4,2,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/fahs_anova2.csv")


#4 
X=Chetoui_Table_1[,c(4,3,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/chetoui_anova2.csv")

#5 

X=Arbosana_Table_1[,c(4,3,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/arbosana_anova2.csv")


#6 

X=Arbequina_Table_1[,c(4,3,6:12)]
x1=TableAnova2(X,alpha = 0.05)
write.csv(x1,"/Users/dhafermalouche/Documents/Research/Nesrine2018_2/arbequina_anova2.csv")


### Travail Hedi

reg=unique(base_hedi_vrac$region_1)

for(i in reg){
  j=which(base_hedi_vrac$region_1==i)
  X=base_hedi_vrac[j,c(2,4,9:29)]
  x1=TableAnova2(X,alpha = 0.05)
  p1=paste0("/Users/dhafermalouche/Documents/Research/Hedi2018/",i,"_anova.csv")
  write.csv(x1,p1)
  X=base_hedi_vrac[j,c(4,2,9:29)]
  x1=TableAnova2(X,alpha = 0.05)
  p1=paste0("/Users/dhafermalouche/Documents/Research/Hedi2018/",i,"_anova2.csv")
  write.csv(x1,p1)
}

vari=unique(base_hedi_vrac$varieté)

for(i in vari){
  j=which(base_hedi_vrac$varieté==i)
  X=base_hedi_vrac[j,c(6,4,9:29)]
  x1=TableAnova2(X,alpha = 0.05)
  p1=paste0("/Users/dhafermalouche/Documents/Research/Hedi2018/",i,"_anova.csv")
  write.csv(x1,p1)
  X=base_hedi_vrac[j,c(4,6,9:29)]
  x1=TableAnova2(X,alpha = 0.05)
  p1=paste0("/Users/dhafermalouche/Documents/Research/Hedi2018/",i,"_anova2.csv")
  write.csv(x1,p1)
}
