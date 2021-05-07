setwd("C:/Users/4092931/Documents/GitHub/doudpackage/R")
source('./ft_quali.R')
source('./ft_quanti.R')
source('./tools.R')
source('./ft_ana_biv.R')
source('./ft_quanti.R')
source('./ft_univ_tab.R')
source('./ft_parse.R')
setwd("C:/Users/4092931/Documents/GitHub/Scripts.PBR/Lea")
source('./Lea.R')
setwd("C:/Users/4092931/Documents/GitHub/doudpackage/tmp")

bdd<-droplevels(bdd[!bdd$Mortalite.a.J30 == 'ehpad les grands cedres',])
a<-ft_desc_tab(bdd, group = "Mortalite.a.J30",complete = F, quali = T)
res_a<-ft_parse(a, bdd, group="Mortalite.a.J30", col.order = c(3,4), group.name = c("0","1"))
res_a
b<-ft_desc_tab(bdd, group = "Mortalite.a.J30",complete = F, quanti = T)
res_b<-ft_parse(b, bdd, group="Mortalite.a.J30", col.order = c(3,4), group.name = c("0","1"))
res_b

c<-ft_desc_tab(bdd, group = "Mortalite.a.J30")
c
res_c<-ft_parse(c, bdd, group="Mortalite.a.J30", col.order = c(3,4), group.name = c("0","1"))
res_c


