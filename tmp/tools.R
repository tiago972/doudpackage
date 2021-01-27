ft_merge<-function(tab_1, tab_2)
{
  i = 1;
  j = 1;
  tmp<-c()
  while (i < nrow(tab_1))
  {
    tmp2<-rbind(tab_1[i,], tab_2[j,])
    tmp2<-rbind(tmp2, tab_1[i+1,])
    tmp<-rbind(tmp, tmp2)
    i = i + 2;
    j = j + 1;
  }
  return(tmp)
}
