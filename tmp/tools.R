library(stringi)
makeTable<-function(quali.Univ_list.Global, group){
  ## Makes df out of the list
  df<-bind_rows(lapply(quali.Univ_list.Global, function(x){
    
    tmp_df<-tibble(var = c(x@parsed_name, x@missing.value.name), 
                   group_var = c(x@value, x@missing.value), pvalue=c(x@pvalue, NA))
    
    names(tmp_df)[names(tmp_df) == "group_var"]<-x@group_var
    return(tmp_df)
  }))

  df<-filter(df, var != "")
  
  ## Keep only full columns and merge
  for (n in colnames(df)){
    if (n == "var" || n == "pvalue")
      next
    tmp<-df[!is.na(df[, n]), ] %>% select_if(~ !any(is.na(.)))
    if(!exists("tmp_df", inherits = FALSE))
      tmp_df<-tmp
    else
      tmp_df<-merge(tmp_df, tmp, by = "var")
  }
  bind_cols(tmp_df, df[1:nrow(tmp_df), "pvalue"])
  df<-tmp_df
  df$var<-stri_replace_all(str = df$var, replacement = "", regex  = '.*(?=Missing values)')
  return (df)
}


