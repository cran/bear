prdcount<-function(i,s.split,prdcount)
{
if( prdcount==3){
   if((s.split[[i]]$drug[s.split[[i]]$prd==1])[[1]]==1){ #when prd=1, drug=?
  n1="Period#1 Ref"
   }
  else{
   n1="Period#1 Test"
   }
  if((s.split[[i]]$drug[s.split[[i]]$prd==2])[[1]]==1){ #when prd=2, drug=?
   n2="Period#2 Ref"
   }
  else{
   n2="Period#2 Test"
   }
 if((s.split[[i]]$drug[s.split[[i]]$prd==3])[[1]]==1){ #when prd=3, drug=?
  n3="Period#3 Ref"
   }
  else{
   n3="Period#3 Test"
   }
 temp <- legend("topright",legend = c(n1, n2, n3),
               text.width = strwidth("1,000,000,000"),
               pch=c(19,17,15), xjust = 1, yjust = 1,lwd=2)   ### as ggplot() pch default order. -YJ
 }

if(prdcount==4){
   if((s.split[[i]]$drug[s.split[[i]]$prd==1])[[1]]==1){ #when prd=1, drug=?
  n1="Period#1 Ref"
   }
  else{
   n1="Period#1 Test"
   }
  if((s.split[[i]]$drug[s.split[[i]]$prd==2])[[1]]==1){ #when prd=2, drug=?
   n2="Period#2 Ref"
   }
  else{
   n2="Period#2 Test"
   }
 if((s.split[[i]]$drug[s.split[[i]]$prd==3])[[1]]==1){ #when prd=3, drug=?
  n3="Period#3 Ref"
   }
  else{
   n3="Period#3 Test"
   }
 if((s.split[[i]]$drug[s.split[[i]]$prd==4])[[1]]==1){ #when prd=4, drug=?
  n4="Period#4 Ref"
   }
  else{
   n4="Period#4 Test"
   }
 temp <- legend("topright",legend = c(n1, n2, n3, n4),
               text.width = strwidth("1,000,000,000"),
               pch=c(19,17,15,3), xjust = 1, yjust = 1,lwd=2)   ### as ggplot() pch default order. -YJ
 }

if(prdcount==5){
   if((s.split[[i]]$drug[s.split[[i]]$prd==1])[[1]]==1){ #when prd=1, drug=?
  n1="Period#1 Ref"
   }
  else{
   n1="Period#1 Test"
   }
  if((s.split[[i]]$drug[s.split[[i]]$prd==2])[[1]]==1){ #when prd=2, drug=?
   n2="Period#2 Ref"
   }
  else{
   n2="Period#2 Test"
   }
 if((s.split[[i]]$drug[s.split[[i]]$prd==3])[[1]]==1){ #when prd=3, drug=?
  n3="Period#3 Ref"
   }
  else{
   n3="Period#3 Test"
   }
 if((s.split[[i]]$drug[s.split[[i]]$prd==4])[[1]]==1){ #when prd=4, drug=?
  n4="Period#4 Ref"
   }
  else{
   n4="Period#4 Test"
   }
if((s.split[[i]]$drug[s.split[[i]]$prd==5])[[1]]==1){ #when prd=4, drug=?
  n5="Period#5 Ref"
   }
  else{
   n5="Period#5 Test"
   }    
 temp <- legend("topright",legend = c(n1, n2, n3, n4, n5),
               text.width = strwidth("1,000,000,000"),
               pch=c(19,17,15,3,13), xjust = 1, yjust = 1,lwd=2)   ### as ggplot() pch default order. -YJ
 }

if(prdcount==6){
   if((s.split[[i]]$drug[s.split[[i]]$prd==1])[[1]]==1){ #when prd=1, drug=?
  n1="Period#1 Ref"
   }
  else{
   n1="Period#1 Test"
   }
  if((s.split[[i]]$drug[s.split[[i]]$prd==2])[[1]]==1){ #when prd=2, drug=?
   n2="Period#2 Ref"
   }
  else{
   n2="Period#2 Test"
   }
 if((s.split[[i]]$drug[s.split[[i]]$prd==3])[[1]]==1){ #when prd=3, drug=?
  n3="Period#3 Ref"
   }
  else{
   n3="Period#3 Test"
   }
 if((s.split[[i]]$drug[s.split[[i]]$prd==4])[[1]]==1){ #when prd=4, drug=?
  n4="Period#4 Ref"
   }
  else{
   n4="Period#4 Test"
   }
if((s.split[[i]]$drug[s.split[[i]]$prd==5])[[1]]==1){ #when prd=4, drug=?
  n5="Period#5 Ref"
   }
  else{
   n5="Period#5 Test"
   }    
 if((s.split[[i]]$drug[s.split[[i]]$prd==6])[[1]]==1){ #when prd=4, drug=?
  n6="Period#6 Ref"
   }
  else{
   n6="Period#6 Test"
   }           
temp <- legend("topright",legend = c(n1, n2, n3, n4, n5, n6),
               text.width = strwidth("1,000,000,000"),
               pch=c(19,17,15,3,13,8), xjust = 1, yjust = 1,lwd=2)   ### as ggplot() pch default order. -YJ
 }

}