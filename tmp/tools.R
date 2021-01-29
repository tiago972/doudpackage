ft_merge<-function(tab_1, tab_2)
{
  i = 1;
  tmp<-c()
  while (i < nrow(tab_1))
  {
    tmp2<-rbind(tab_1[i,], tab_2[i,])
    tmp<-rbind(tmp, tmp2)
    i = i + 1;
  }
  return(tmp)
}
ft_merge_tot<-function(dicho, tot)
{
  i = 1;
  j = 1;
  tmp<-c()
  while (i < nrow(dicho))
  {
    tmp2<-rbind(dicho[,c(i, i+1)], )

  }
  return(tmp)
}

