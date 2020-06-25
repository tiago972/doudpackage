### parser le nom du niveau avec le nom de la variable
## Mettre le nombre de Nas au niveau du titre de groupe
### Mettre le n total de chaque groupe dans le titre

ft_count_row<-function(data)
{
  count<-0
  for (i in 1:ncol(data))
  {
    if (is.factor(data[,i]))
      count<-count + nlevels(data[,i])
  }
  return (count)
}

ft_parse_quali_col<-function(tmp, opt, v_group=NULL)
{
  if (opt == 1)
  {
    tmp[,"n(%)"]<-paste(tmp$n, "(", tmp[,"%"], ")", sep="") 
    tmp[,"NAs(%)"]<-paste(tmp[,"NAs"], "(", tmp[,"%_NAs"], ")", sep = "")
    tmp<-tmp%>%select(-n, -'%', -NAs, -'%_NAs')
  }
  else if (opt == 2)
  {
    for (j in 1:length(v_group))
    {
      n_tmp<-paste("n(%)_", v_group[j], sep="")
      na_tmp<- paste("NAs(%)_", v_group[j], sep="")
      for (k in 1:nrow(tmp))
        tmp[k,n_tmp]<-paste(tmp[k,n_tmp], tmp[k,na_tmp],sep=";")
      tmp<-tmp%>%select(-all_of(na_tmp))
      colnames(tmp)[colnames(tmp)==n_tmp]<-v_group[j]
    }
  }
  else if (opt == 3)
  {
    tmp$Total<-paste(tmp[,"n(%)"], tmp[,"NAs(%)"], sep = ";")
    tmp<-tmp[,!names(tmp) %in% c("n(%)", "NAs(%)")]
  }
  return (tmp)
}

ft_parse_quali_fin<-function(data, res, group){
  res<-pivot_wider(res, names_from = Group, values_from = c("n(%)", "NAs(%)"))
  v_group<-c(levels(data[,group]))
  res<-ft_parse_quali_col(res, 2, v_group)
  res_tot<-ft_parse_quali_col(ft_quali.group_false(data), 3)
  res<-merge(res, res_tot, all=TRUE)
  p<-ft_ana_biv(bdd, group) %>% select(-signi, -test)
  p$p<-ifelse(as.numeric(p$p) < 0.001, '<.001', round(as.numeric(p$p), digits=3))
  res<-merge(res, p, by.x="var", by.y="nom", all.x=TRUE)
  return(res)
}

ft_quali.group_true<-function(data, group, complete, digits.opt=1)
{
  for (i in 1:nlevels(data[,group]))
  {
    tmp<-data[which(data[,group]==levels(data[,group])[i]),]
    tmp<-tmp[,-which(colnames(tmp)==group)]
    tmp<-ft_quali.group_false(tmp, group=NULL, complete, digits.opt)
    tmp$Group=levels(data[,group])[i]
    assign(paste("tab",i,sep="_"), tmp)
  }
  for (i in 1:nlevels(data[,group]))
  {
    tmp<-as.data.frame(mget(ls(pattern = paste("tab", i, sep = "_"))))
    colnames(tmp)<-c("var", "level", "n(%)", "NAs(%)", "Group")
    if (i == 1)
       res<-tmp
     else
       res<-merge(res, tmp, all=TRUE)
  }
  return (ft_parse_quali_fin(data, res, group))
}

ft_quali.group_false<-function(data, group=NULL, complete=TRUE, digits.opt=1){
  if (is.null(group))
  {
    j <- 0;
    tmp<-as.data.frame(matrix(NA, ft_count_row(data), 6))
    colnames(tmp)<-c("var", "level", "n", "%", "NAs", "%_NAs")
    for (i in 1:ncol(data))
    {
      if (!is.factor(data[,i]) || nlevels(data[,i]==1))
        next;
        for (k in 1:nlevels(data[,i]))
        {
            tmp[j + k, "var"]<-colnames(data)[i]
            tmp[j + k, "level"]<-levels(data[,i])[k]
            tmp[j + k, "n"]<-table(data[,i])[k]
            tmp[j + k, "%"]<-round(prop.table(table(data[,i], useNA = "always"))[k] * 100, digits = digits.opt)
            tmp[j + k, "NAs"]<-table(data[,i], useNA = "always")[nlevels(data[,i]) + 1]
            tmp[j + k, "%_NAs"]<-round(prop.table(table(data[,i], useNA = "always"))[nlevels(data[,i]) + 1] * 100, digits = digits.opt)
              if (k == nlevels(data[,i]))
                j = j + k
        }
    }
      tmp = ft_parse_quali_col(tmp, 1, group)
      return (tmp)
  }
    else
      return (ft_quali.group_true(data, group, complete, digits.opt))
}