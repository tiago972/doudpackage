# setwd("C:/Users/4092931/Documents/GitHub/doudpackage/R")
setwd("/Users/tiago2/BF/doudpackage/R")

source('./ft_quali.R')
source('./ft_quanti.R')
source('./tools.R')
source('./ft_ana_biv.R')
source('./ft_quanti.R')
source('./ft_univ_tab.R')
source('./ft_parse.R')
# setwd("C:/Users/4092931/Documents/GitHub/scripts.PBR/Lea")
setwd("/Users/tiago2/BF/Judith Breth")
source('./judith.R')

a<-ft_desc_tab(bdd, group = "DC.a.6mois",complete = F, quali = T)
res_a<-ft_parse(a, bdd, group="vivant.s4", col.order = c(3,4), group.name = c("0","1"))
res_a
b<-ft_desc_tab(bdd, group = "DC.a.6mois",complete = F, quanti = T)
res_b<-ft_parse(b, bdd, group="vivant.s4", col.order = c(3,4), group.name = c("0","1"))
res_b

c<-ft_desc_tab(bdd, group = "DC.a.6mois")
c
res_c<-ft_parse(c, bdd, group="vivant.s4", col.order = c(3,4), group.name = c("0","1"))
res_c


