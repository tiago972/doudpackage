## Function to rename columns according to the effectives of each sub-group
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
#' @param col.order A vector containing column indexes or names to use for column order
#' @param c.name A list containing column names to use. Need to be given in the same order as col_order
#' @param group The name of the group used in ft_desc_tab
#' @param digits.opt How many numbers to display after the ".". Should be the same as provided in ft_desc_tab, default is 0
#' @import KableExtra
#' @import plyr
#' @return Return a html KableExtra object.
#' @export
ft_parse<-function(res, data, group, col.order = NULL, group.name = NULL, digits.opt = 0)
{
 if (!is.null(col.order))
   res<-res[,col.order]
res<-ft_name_col(res, data, group, digits.opt)
ident<-grep("Missing values.*", res$var)
colnames(res)[which(colnames(res) == "var")]<-""
group.1<-group.name[1]
group.2<-group.name[2]
res<-kable(res) %>%
  kable_classic() %>%
  add_header_above(c(" ", "Total" = 1, group.1 = 1, group.2 = 1, " ")) %>%
  add_indent(ident)
 return(res)
}


