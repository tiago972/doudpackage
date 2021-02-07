ft_parse_p_quali<-function(biv, total)
{
  i = 0;
  tmp<-c()
  rownames(total)<-1:nrow(total)

  while (i < nrow(biv))
  {
    i = i + 1;
    while (i <= nrow(biv) && length(grep(pattern = paste(biv[i, "var"], ".*", sep = ""), total$var)) == 0)
      i = i + 1;
    patt = paste("^", biv[i,"var"], ".*",sep = "")
    k = table(grepl(pattern = patt, total$var))[2]
    k = k + 2
    # if (i == 9)
    # {
    #   grep_tmp<-grep(pattern = patt, total$var)
    #   print(k)
    #   print(paste("k = ", k))
    # }
    tmp2<-data.frame("p" = rep(biv[i, "p"], k), "var" = rep(biv[i, "var"], k))
    tmp<-rbind(tmp, tmp2, NA)
    print(paste("nrow tmp ", nrow(tmp)))
  }
  if (nrow(tmp) > nrow(total))
    print(paste("MAX ", i))
  assign("total", total, envir = globalenv())
  return(tmp)
}

ft_quali.pvalue<-function(data, res, group, na.print, digits.opt){
  res_tot<-ft_quali(data, NULL, NULL, na.print, digits.opt)
  res_tot$Group="Total"
  remove_group = min(grep(pattern = paste(group, ".*", sep = ""), res_tot$var))
  res_tot<-res_tot[-c(remove_group:(remove_group + nlevels(data[,group]))),]
  total<-ft_merge(res, res_tot)
  biv<-ft_ana_biv(data, group)
  total<-ft_parse_p_quali(biv, total)
  p<-p[,!names(p) %in% c("signi", "test")]
  p$p<-ifelse(as.numeric(p$p) < 0.001, '<.001', round(as.numeric(p$p), digits=3))
  res<-merge(res, p, all.x=TRUE)
  return(res)
}

#' @import tidyr
ft_quali_grouped<-function(data, group, p.value, na.print, digits.opt)
{
  my_env<-environment()
  for (i in 1:nlevels(data[,group]))
  {
    tmp<-data[which(data[,group]==levels(data[,group])[i]),]
    tmp<-tmp[,-which(colnames(tmp)==group)]
    tmp<-ft_quali(tmp, group=NULL, p.value, na.print, digits.opt)
    tmp$Group=levels(data[,group])[i]
    nametab<-paste("tmp", i,sep="_")
    assign(nametab,tmp, envir = my_env)
  }
  res<-ft_merge(tmp_1, tmp_2)
  # res<-tidyr::pivot_wider(res, names_from = "Group", values_from =  "Total")
  if (isFALSE(p.value))
    return(res)
  else
    return (ft_quali.pvalue(data, res, group, na.print, digits.opt))
}

ft_quali<-function(data, group=NULL, p.value, na.print, digits.opt){
  if (is.null(group))
  {
    j <- 0;
    tmp<-data.frame("var" = NA, "Total" = NA)
    for (i in 1:ncol(data))
    {
      if (!is.factor(data[,i]) || nlevels(data[,i]==1))
        next;
      for (k in 1:nlevels(data[,i]))
      {
          tmp[(j + k), "var"]<-paste(colnames(data)[i], levels(data[,i])[k], sep = ", ")
          tmp[(j + k), "Total"]<-paste(table(data[,i])[k], "(",
                                     round(prop.table(table(data[,i], useNA = "always"))[k] * 100, digits = digits.opt),
                                     ")", sep ="")
          if (k == nlevels(data[,i]))
            j = j + k
      }
      j = j + 1
      # tmp[j, "var"]<-paste("Missing values, n(%)", colnames(data)[i], sep = ":")
      tmp[j, "var"]<-"Missing values, n(%)"
      tmp[j, "Total"]<-paste(table(data[,i], useNA = "always")[nlevels(data[,i]) + 1], "(",
                        round(prop.table(table(data[,i], useNA = "always"))[nlevels(data[,i]) + 1] * 100, digits = digits.opt),
                        ")", sep = "")
    }
    # tmp = ft_parse_quali_opt(tmp, 1, group, na.print)
    return (tmp)
  }
    else
      return (ft_quali_grouped(data, group, p.value, na.print, digits.opt))
}
