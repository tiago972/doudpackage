## Var Class ###
setClass("Var",slots = c(name = "character", type = "character", 
           normal = "logical"))


setMethod("initialize",
          "Var",
          function(.Object, name, type, normal) {
            .Object@name <- name
            #type<-type_list[[which(sapply(names(type_list), function(y) name %in% y))]]
            
            type<-as.character(ifelse(type == "numeric" | type == "integer", "numeric", 
                                      ifelse(type == "factor", "factor", 
                                             stop(sprintf("Type unrecognised %s of %s", type, name)))))
            .Object@type <- type
            .Object@normal <- normal
            return(.Object)
          })

Var <- function(name, type = "", normal = TRUE) {
  return(new("Var", name = name, type = type, normal = normal))
}


setMethod("[", "Var", function(x, i) {
  if (i == "name")
    return(x@name)
  else if (i == "type")
    return(x@type)
  else if (i == "normal")
    return(x@normal)
})

setReplaceMethod("[", "Var", function(x, i, value) {
  if (i == "name")
    x@name <- value
  else if (i == "type")
    x@type <- value
  else if (i == "normal")
    x@normal <- value
  return(x)
})
#################

### List Var ###
setClass("listVar", representation(List = "list"))
#################

#### Var Group ###
setClass("VarGroup",contains = "Var",
         slots = c(group_var = "character", pvalue = "numeric", 
                   parsed_name = "character", value = "character", 
                   missing.value = "character", missing.value.name = "character"))

setMethod("initialize",
          "VarGroup",
          function(.Object, x, group_var, pvalue, parsed_name, 
                   value, missing.value, missing.value.name) {
            .Object@name<-x@name
            .Object@type<-x@type
            .Object@normal<-x@normal
            .Object@group_var <- group_var
            .Object@pvalue <- pvalue
            .Object@parsed_name<-parsed_name
            .Object@value <- value
            .Object@missing.value <- missing.value
            .Object@missing.value.name <- missing.value.name
            return(.Object)
          })

VarGroup <- function(x, group_var = "", pvalue = 0,  parsed_name = "", 
                     value = "", missing.value = "", missing.value.name = "") {
  return(new("VarGroup", x = x, group_var = group_var, pvalue = pvalue, 
             parsed_name = parsed_name, value = value, 
             missing.value = missing.value, missing.value.name = missing.value.name))
}


setMethod("[", "VarGroup", function(x, i) {
  if (i == "group_var")
    return(x@group_var)
  else if (i == "pvalue")
    return(x@pvalue)
  else if (i == "parsed_name")
    return(x@parsed_name)
  else if (i == "value")
    return(x@value)
  else if (i == "missing.value")
    return(x@missing.value)
  else if (i == "missing.value.name")
    return(x@missing.value.name)
})

setReplaceMethod("[", "VarGroup", function(x, i, value) {
  if (i == "group")
    x@group <- value
  else if (i == "pvalue")
    x@pvalue <- value
  else if (i == "parsed_name")
    x@parsed_name <- value
  else if (i == "value")
    x@value <- value
  else if (i == "missing.value")
    x@value <- x@missing.value
  else if (i == "missing.value.name")
    x@value <- x@missing.value.name
  return(x)
})


setAs(
  from = "Var",
  to = "VarGroup",
  def = function(from) {
    return(VarGroup(x = from, group_var = "Total"))
  })

#################