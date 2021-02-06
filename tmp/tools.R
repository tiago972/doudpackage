ft_merge<-function(tab_1, tab_2)
{
  i = 1;
  tmp<-c()
  while (i <= nrow(tab_1))
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
  # print(tot)
  # print(dicho)
  while ((i + 1) <= nrow(dicho))
  {
    if (j+1 <= nrow(tot) && grepl(pattern = "^Missing*.", tot[j+1, "var"]))
    {
      tmp2<-rbind(dicho[i:(i+1),], tot[j:(j+1),])
      j = j + 2
    }
    else if (j <= nrow(tot))
    {
      tmp2<-rbind(dicho[i:(i+1),], tot[j,])
      j = j + 1;
    }
    else
      tmp2<-rbind(dicho[i:(i+1),])
    tmp<-rbind(tmp, tmp2)
    i = i + 2;
  }
  return(tmp)
}

