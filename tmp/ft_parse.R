### En fait à terme il faut créer un objet d'une nouvelle classe
# avec toutes les données nécessaires au parsing

#' Parsing function for Html output
#'
#' Function that allows you to create a table ready to be used in article.
#'  Output is generated in HTML by KableExtra
#' @param data A database, dataframe object
#' @param col.order A vector containing column indexes or names to use for column order
#' @param c.name A list containing column names to use. Need to be given in the same order as col_order
#' @param group The name of the group used in ft_desc_tab
#' @import KableExtra
#' @import plyr
#' @return Return a html KableExtra object.
#' @export
ft_parse<-function(res, data, col.order = NULL, group.name = NULL)
{
 if (!is.null(col.order))
   res<-res[,col.order]
 if (!is.null(group.name))
   colnames(res)<-group.name
 n_0<-data %>%
   summarise()

 return(res)
}


