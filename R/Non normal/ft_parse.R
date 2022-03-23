## Function to parse lines if binary variable
ft_delete_rows<-function(res, data){
  names.tmp<-sapply(data, class)
  names.tmp<-names(names.tmp[names.tmp %in% "factor"])
for (i in 1:length(names.tmp)){
  if (nlevels(data[,names.tmp[i]]) == 2){
    tmp<-grep(pattern = paste(names.tmp[i], ".*", "0", sep = ""), res$var)
     if (length(tmp) != 0)
      res<-res[-c(tmp),]
  }
}
  return(res)
}
## Function using kableExtra for final output
ft_kable<-function(res, group.name){
  ident<-grep("Missing values.*", res$var)
  colnames(res)[which(colnames(res) == "var")]<-""
  res<-kableExtra::kable(res) %>%
    kableExtra::kable_classic() %>%
    kableExtra::add_header_above(c(" ", "Total" = 1, setNames(1, group.name[1]),
                                   setNames(1, group.name[2]), " ")) %>%
    kableExtra::add_indent(ident)
  return(res)
}

## Function to rename columns according to the counts of each sub-group
ft_name_col<-function(res, data, group, digits.opt)
{
  
  tmp_factor<-levels(data[,group])
  tmp_table<-table(data[,group], useNA = "always")
  tmp_prop.table<-round(prop.table(table(data[,group], useNA = "always")) * 100, digits = digits.opt)
  for (i in 1:nlevels(data[,group]))
    colnames(res)[which(colnames(res) == levels(data[,group])[i])] <- paste("n = ", tmp_table[i], "(", tmp_prop.table[i],
                                                                          "%)", sep = "")
  colnames(res)[which(colnames(res) == "Total")]<-paste("n = ", nrow(data), sep = "")
  return(res)
}

### En fait à terme il faut créer un objet d'une nouvelle classe
# avec toutes les données nécessaires au parsing

#' Parsing function for Html output
#'
#' Function that allows you to create a table ready to be used in article.
#'  Output is generated in HTML by KableExtra
#' @param res Outpul of ft_desc_tab
#' @param data A database, dataframe object
#' @param col.order A vector containing levels of group according to the desired order
#' @param group The name of the group used in ft_desc_tab
#' @param group.name Vector containing subgroup names
#' @param digits.opt How many numbers to display after the ".". Should be the same as provided in ft_desc_tab, default is 0
#' @import kableExtra
#' @return Return a html KableExtra object.
#' @export
ft_parse<-function(res, data, group, col.order = NULL, group.name = NULL, digits.opt = 1)
{
  ## rajouter gestion des erreurs: si col.order ne correspond à aucun level de group
data<-data[!is.na(data[,group]),]
if (!is.null(col.order)) # ligne ci dessous a changer +++ en fonction de col.order & group.name 
  res<-res[,c(which(colnames(res) == "var"), which(colnames(res) =="Total"), which(colnames(res) == col.order[1]), which(colnames(res) == col.order[2]), which(colnames(res) =="p"))]
else
  res<-res[,c("var","Total",levels(data[,group])[1], levels(data[,group])[2],"p")]
if (!is.null(group.name))
{
  colnames(res[,3])<-group.name[1]
  colnames(res[,4])<-group.name[2]
}
 else if (is.null(group.name) && !is.null(col.order))
   group.name <- col.order
 else
   group.name <- levels(data[,group])
res<-ft_name_col(res, data, group, digits.opt)
res<-ft_delete_rows(res, data)
res<-ft_kable(res, group.name)
 return(res)
}

