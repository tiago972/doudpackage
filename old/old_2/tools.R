### Parse NA to remove Null NA rows and to get rid of the temp label
ft_parse_na<-function(data)
{
  col<-intersect(grep(pattern = ".*.Missing values, n\\(\\%\\)", data$var), grep("0\\(0\\)", data$Total))
  if (length(col) != 0)
    data<-data[-c(col),]
  data$var<-gsub(pattern = ".*.Missing values, n\\(%\\)", "Missing values, n(%)", data$var)
  return(data)
}

#### Fuction to select what to print according to options (quanti) ####
#' @import tidyr
ft_parse_quanti_opt<-function(data, min.max, na.print, p.value, group)
{
  i = 1;
  if(isFALSE(min.max)) ### A Changer +++ il faudra en faite les mettre à la ligne de mean, sd
    data<-data[,!names(data) %in% "Min-Max"]
  if (!is.null(group))
    data<-tidyr::pivot_wider(data, names_from = "Group", values_from = c("Total"))
  while (i <= nrow(data))
  {
    data[i,1]<-paste(data[i,1], ", mean(SD)", sep = "")
    i = i + 2;
  }
  if (!isTRUE(na.print))
    data<-data[!grepl(".*.Missing values, n\\(\\%\\)", data$var),]
  else
    data<-ft_parse_na(data)
  if (!isTRUE(p.value))
    data<-data[,!names(data) %in% "p"]
  return(data)
}

#### Fuction to select what to print according to options (quali) ####
#' @import tidyr
ft_parse_quali_opt<-function(data, na.print, p.value, group)
{
  if (!is.null(group))
    data<-tidyr::pivot_wider(data, names_from = "Group", values_from =  "Total")
  if (!isTRUE(na.print))
    data<-data[!grepl(".*.Missing values, n\\(\\%\\)", data$var),]
  else
    data<-ft_parse_na(data)
  if (!isTRUE(p.value))
    data<-data[,!names(data) %in% "p"]
  return(data)
}

### Error to be checked at the begening of the function (need to be completed)
ft_error<-function(data, group, complete, quanti, quali)
{
  if (!is.null(group) && (!is.factor(data[,group]) || nlevels(data[,group]) > 2))
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
