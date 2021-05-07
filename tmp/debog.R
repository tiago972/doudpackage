setwd("C:/Users/4092931/Documents/GitHub/Scripts.PBR/Lea")
source('./Lea.R')
bdd<-droplevels(bdd[!bdd$Mortalite.a.J30 == 'ehpad les grands cedres',])
a<-ft_desc_tab(bdd, complete = F, quanti = T)
