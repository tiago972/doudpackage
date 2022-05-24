#### Fonction principale pour les differents elements de l analyse univariee ####
ft_tab_quanti<-function(data, i, group=NULL, group_level=NULL, digits.opt)
{
  if (!is.null(group))
    subset<-data[which(data[,group]==group_level),]
  else
    subset<-data
  "Min"<-min(subset[,i], na.rm=TRUE)
  "Max"<-max(subset[,i], na.rm=TRUE)
  "mean"<-round(mean(subset[,i], na.rm = T), digits = digits.opt)
  "sd"<-round(sd(subset[,i], na.rm = T), digits = digits.opt)
  "prop_NAs"<-ifelse(is.na(table(is.na(subset[,i]))[2]), 0, round(prop.table(table(is.na(subset[,i])))[2] * 100, digits = digits.opt))
  "NNAs"<-ifelse(is.na(table(is.na(subset[,i]))[2]), 0,table(is.na(subset[,i]))[2])
  tmp_mat<-c(var = colnames(data)[i],
             "Total"=gsub(" ", "", paste(mean, "(",  sd,")")), "Min-Max"=paste(Min, Max, sep="-"))
  tmp_NA <-c(paste(colnames(data)[i], "Missing values, n(%)", sep = "."), gsub(" ", "", paste(NNAs, "(", prop_NAs, ")")),
             gsub(" ", "", paste(NNAs, "(", prop_NAs, ")")))
    tmp_mat<-rbind(tmp_mat, tmp_NA)
    rownames(tmp_mat)<-NULL
  return(tmp_mat)
}

#### si pvalue est true #####
#' @import plyr
ft_univ_quanti_p.value<-function(data, group, min.max, na.print,tab_tmp, digits.opt)
{
  total<-ft_quanti(data, NULL, NULL, min.max, na.print, digits.opt)
  biv<-ft_ana_biv(data, group)
  total$Group <- "Total"
  total<-merge(tab_tmp, total, all = T)
  biv<-biv[biv$var %in% total$var,]
  biv<-biv[,!names(biv) %in% c("test", "signi")]
  total<-plyr::join(total, biv, by = "var")
  return(total)
}

### simple fonction coupee ####
ft_univ_quanti_2<-function(data, group, p.value, min.max, na.print, digits.opt){
  tab_1<-data.frame("var"=NA, "Total"=NA, "Min-Max"=NA)
  colnames(tab_1)=c("var", "Total", "Min-Max")
  tab_2<-data.frame("var"=NA, "Total"=NA, "Min-Max"=NA)
  colnames(tab_2)=c("var", "Total", "Min-Max")
  j = 1;
  tab_1<-c()
  tab_2<-c()
  for (i in 1:ncol(data))
  {
    if (colnames(data)[i]==group || !is.numeric(data[,i]))
      next ;
    tmp_1<-ft_tab_quanti(data, i, group, levels(data[,group])[1], digits.opt)
    tmp_2<-ft_tab_quanti(data, i, group, levels(data[,group])[2], digits.opt)
    tab_1<-as.data.frame(rbind(tab_1, tmp_1))
    tab_2<-as.data.frame(rbind(tab_2, tmp_2))
  }
  tab_1$Group=levels(data[,group])[1]
  tab_2$Group=levels(data[,group])[2]
  tmp<-merge(tab_1, tab_2, all = T)
  return (ft_univ_quanti_p.value(data, group, min.max, na.print,tmp, digits.opt))
}

ft_quanti<-function(data, group=NULL, p.value, min.max, na.print, digits.opt){
  if (is.null(group))
  {
    tab<-data.frame("var"=NA, "Total"=NA, "Min-Max"=NA)
    colnames(tab)=c("var", "Total", "Min-Max")
    j = 1
    for (i in 1:ncol(data))
    {
      if (!is.numeric(data[,i]))
        next;
      tmp<-ft_tab_quanti(data,i, NULL, NULL, digits.opt)
      for (k in 1:3)
      {
        tab[j,k]<-tmp[1,k]
        tab[j+1,k]<-tmp[2,k]
      }
      j = j + 2
    }
    return(tab)
  }
  else
    return(ft_univ_quanti_2(data, group, p.value, min.max, na.print, digits.opt))
}
