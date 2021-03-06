### Data sample for testing #####

test_map1<-data.frame("nom" = rep(LETTERS[1:10], each=2, length.out=20),
                                      "num" = rep(1:10, each=2, length.out=20),
                      group = rep(seq(from = 0, to = 1, by = 1)))
test_map1
test_map2<-data.frame("nom" = rep(LETTERS[1:10]), "num" = rep(11:20), group = rep("0", 10))
test_map2
################ Test pour htmlTable ######
library(kableExtra)
colnames(test3)<-c("var" ,"p", "n = 34(34%)", "n = 64%", "n = 100")
total<-kable(test3[,c("var" ,"n = 100", "n = 34(34%)", "n = 64%", "p")]) %>%
  kable_classic() %>%
  add_header_above(c(" ", "Total" = 1, "Deceased" = 1, "Survivor" = 1, " ")) %>%
  add_indent(8)

total

total<-kable(test4) %>%
  kable_classic() %>%
  add_header_above(c(" ", "Total" = 1, "Deceased" = 1, "Survivor" = 1, " ")) %>%
  add_indent(c)

total

### Tests de mini fonctions
col_names<-colnames(bdd)
apply_test<-sapply(col_names, function(x){
  if (is.factor(bdd[,x]))
    return(x)
  })
apply_test<-unlist(apply_test)

# BDD pour les tests, petit echantillon de var quali
bdd_tmp<-bdd[,c(colnames(bdd)[1:3], colnames(bdd)[16:18], "vivant.s4")]
test<-ft_desc_tab(bdd_tmp, na.print = T, group = "vivant.s4")

### Test de nom de "Grands groupes"
big_groupes<-list(Demographie = c("ID", "Age", "Genre"),
              TDM = c("Atteinte.bilaterale", "ct_cat"))

### Methode pour reclasser les lignes selon l'ordre donné des groupes
res<-c()
for (i in 1:length(big_groupes))
{
  s<-lapply(big_groupes[[i]], function(x)
  {
    tmp_grep<-grep(x, test$var)
    if(!is.na(table(grepl("Missing values.*", test[(max(tmp_grep) + 1),]))["TRUE"]))
      tmp_grep<-c(tmp_grep, (max(tmp_grep) + 1))
    tmp_grep
  })
  res<-c(res, s)
}
names(res)<-unlist(big_groupes)
library(kableExtra)
test2<-rbind(test[unlist(res),], test[-unlist(res),])
res<-kableExtra::kable(test2) %>%
  kableExtra::kable_classic()

for (i in 1:length(big_groupes))
{
  res<-res %>%
  pack_rows(index = c(names(big_groupes)[i]= length(big_groupes[[i]])), indent = F, label_row_css = "border-bottom: 1px solid;
            text-indent: 4%;")
}
res
