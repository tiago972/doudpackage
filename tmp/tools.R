ft_merge<-function(tab_1, tab_2)
{
  i = 1;
  tmp<-c()
  while ((i + 1) <= nrow(tab_1))
  {
    tmp2<-rbind(tab_1[i:(i+1),], tab_2[i:(i+1),])
    tmp<-rbind(tmp, tmp2)
    i = i + 2;
  }
  return(tmp)
}

ft_merge_tot<-function(tab_1, tab_2)
{
  i = 1;
  j = 1;
  tmp<-c()
  while ((i + 3) <= nrow(tab_1))
  {
    tmp2<-rbind(tab_1[i:(i+3),], tab_2[j:(j+1),])
    tmp<-rbind(tmp, tmp2)
    i = i + 4;
    j = j + 2;
  }
  return(tmp)
}
