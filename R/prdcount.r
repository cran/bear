prdcount<-function(i,s.split,prdcount)
{
if( prdcount==3){
   if((s.split[[i]]$drug[s.split[[i]]$prd==1])[[1]]==1){ #when prd=1, drug=?
  n1="Prd1 Ref"
   }
  else{
   n1="Prd1 Test"
   }
  if((s.split[[i]]$drug[s.split[[i]]$prd==2])[[1]]==1){ #when prd=2, drug=?
   n2="Prd2 Ref"
   }
  else{
   n2="Prd2 Test"
   }
 if((s.split[[i]]$drug[s.split[[i]]$prd==3])[[1]]==1){ #when prd=3, drug=?
  n3="Prd3 Ref"
   }
  else{
   n3="Prd3 Test"
   }
 temp <- legend("topright",legend = c(n1, n2, n3),
               text.width = strwidth("1,000,000,000"),
               pch=c(19,1,15), xjust = 1, yjust = 1)
 }

if(prdcount==4){
   if((s.split[[i]]$drug[s.split[[i]]$prd==1])[[1]]==1){ #when prd=1, drug=?
  n1="Prd1 Ref"
   }
  else{
   n1="Prd1 Test"
   }
  if((s.split[[i]]$drug[s.split[[i]]$prd==2])[[1]]==1){ #when prd=2, drug=?
   n2="Prd2 Ref"
   }
  else{
   n2="Prd2 Test"
   }
 if((s.split[[i]]$drug[s.split[[i]]$prd==3])[[1]]==1){ #when prd=3, drug=?
  n3="Prd3 Ref"
   }
  else{
   n3="Prd3 Test"
   }
 if((s.split[[i]]$drug[s.split[[i]]$prd==4])[[1]]==1){ #when prd=4, drug=?
  n4="Prd4 Ref"
   }
  else{
   n4="Prd4 Test"
   }
 temp <- legend("topright",legend = c(n1, n2, n3, n4),
               text.width = strwidth("1,000,000,000"),
               pch=c(19,1,15,22), xjust = 1, yjust = 1)
 }
}