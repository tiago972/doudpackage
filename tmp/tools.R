### Remove all corresponding NA lines if there are no NA in total group
ft_remove_null_na<-function(data)
{
  i = 1;
  tmp<-c();
  while (i <= nrow(data))
  {
    if ((i + 5) <= nrow(data) && grepl(pattern = "0(0)", data[(i+5), "Total"], fixed = T))
      tmp2<-rbind(data[i,], data[(i+2),], data[(i+4),])
    else
      tmp2<-rbind(data[i:(i+5),])
    i = i + 6
    tmp<-rbind(tmp, tmp2)
  }
  return(tmp)
}

#### Fonction pour le filtrage des elements selon les options choisies ####
ft_parse_quanti_opt<-function(data, min.max, na.print, p.value)
{
  i = 1;
  while (i <= nrow(data))
  {
    data[i,1]<-paste(data[i,1], ", mean(SD)", sep = "")
    i = i + 2;
  }
  if (!isTRUE(min.max))
    data<-data[,!names(data) %in% "Min-Max"]
  if (!isTRUE(na.print))
    data<-data[!grepl("Missing values, n(%)", data[,"var"], fixed = T),]
  else
    data<-ft_remove_null_na(data)
  if (!isTRUE(p.value))
    data<-data[,!names(data) %in% "p"]
  return(data)
}

## iterates rbind to conserve row order for merging
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

ft_error<-function(data, group, complete, quanti, quali)
{
  if (!is.null(group) && (!is.factor(data[,group]) || nlevels(data[,group]) > 3))
  {
    write("Grouping error dude, check if the variable is a binary factor", stderr())
    return (-1)
  }
  if (isFALSE(complete) && isFALSE(quanti) && isFALSE(quali))
  {
    write("Error, if complete is FALSE, quanti or quali must be TRUE", stderr())
    return(-1)
  }
  return(0)
}
