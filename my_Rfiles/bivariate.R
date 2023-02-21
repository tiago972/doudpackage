
################ Quantitative #######################
### Function to calculate pvalue if group levels == 2
quantiTwoLevelsFun<-function(x, data, group){
  my_env<-environment()
  if (x@normal == TRUE){
    tryCatch({
      my_env$p<-t.test(data[,x@name]~data[,group])$p.value
    },
    error=function(e){
      warning(paste(e), x@name,  "in quantiTwoLevelsFun")
    })
  }
  else if (x@normal == FALSE){
    tryCatch({
      my_env$p<-wilcox.test(data[,x@name]~data[,group])$p.value
    },
    warning=function(w){
      my_env$p<-wilcox.test(data[,i]~data[,group], exact = F)$p.value
    },
    error=function(e){
      warning(paste(e), x@name, " in quantiTwoLevelsFun")
    })
  }
  else
    stop(sprintf("Unknown normality %s for %s", x@normal, x@name))
  return(my_env$p)
}

quantiMoreLevelsFun<-function(x, data, group){
  my_env<-environment()
  if (x@normal == TRUE){
    tryCatch({
      aov_res<-unlist(summary(aov(data[,x@name]~data[,group])))
      my_env$p<-aov_res["Pr(>F)1"]
    },
    warning=function(w){
      warning(paste(w), x@name, " in quantiMoreLevelsFun")
    },
    error=function(e){
      warning(paste(e), x@name, " in quantiMoreLevelsFun")
    })
  }
  else if (x@normal == FALSE){
    tryCatch({
      my_env$p<-kruskal.test(x@name~group, data = data)$p.value
    },
    warning=function(w){
      warning(paste(w), x@name)
    },
    error=function(e){
      warning(paste(e), x@name, " in quantiMoreLevelsFun")
    })
  }
  else
    stop(sprintf("Unknown normality %s for %s", x@normal, x@name))
  return(my_env$p)
}

## Just a hub to assess nlevels
quantiBivFun<-function(x, group, data, digits.p){
  my_env<-environment()
  if (nlevels(data[, group]) == 2)
    p = quantiTwoLevelsFun(x, data, group)
  else if (nlevels(data[,group]) > 2)
    p = quantiMoreLevelsFun(x, data, group)
  else
    stop(sprintf("Group levels must be at least two: %d for %s", nlevels(data[,group]), group))

  var.p<-VarGroup(group_var = "Total", pvalue = round(p, digits.p),
                  x = x)
  return(var.p)
}
#############################################

################ Qualitative ################
### Function to calculate pvalue for qualitative variables
qualiBivFun<-function(x, group, data, digits.p){
  my_env<-environment()
  tryCatch({
    my_env$p<-chisq.test(data[,x@name], data[,group], correct=FALSE)$p.value},
    error=function(e){
      warning(paste(e), x@name, " in qualiBivFun")
    },
    warning=function(w){
      tryCatch({
        my_env$p<-fisher.test(data[,x@name], data[,group], simulate.p.value = TRUE)$p.value},
      error=function(e){
        warning(paste(e), x@name, " in qualiBivFun")})
    })
  var.p<-VarGroup(group_var = "Total", pvalue = round(my_env$p, digits.p),
                  x = x)
  return(var.p)
}

########### Class Methods #########################
setGeneric("anaBiv", function(var, group, ...) {
  return(standardGeneric("anaBiv"))
})

setMethod("anaBiv", "listVar", function(var, group, ...){
  if (!is.null(group)){
    lst_VarGroup.Biv<-lapply(var@List, function(x, group, data, digits.p){
      if (x@type == "factor" && x@name != group)
        quali.Biv<-qualiBivFun(x, group, data, digits.p)
      else if (x@type == "numeric")
        quanti.Biv<-quantiBivFun(x, group, data, digits.p)
      else if (x@name == group)
        return(NULL)
      else
        stop(sprintf("Uknown type for %s", x@name))
    }, group, ...)
    lst_VarGroup.Biv<-purrr::compact(lst_VarGroup.Biv)
    return(lst_VarGroup.Biv)
  }
  else
    return(NULL)
})

# setMethod("anaBiv", "data.frame", function(var, group, ...) {
#
#   return()
# })
