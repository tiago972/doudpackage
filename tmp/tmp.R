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
bdd_tmp<-bdd[,c(apply_test[1:5], "vivant.s4")]
test<-ft_desc_tab(bdd_tmp, na.print = T,quali = T, complete = F,group = "vivant.s4")
res<-kableExtra::kable(test) %>%
  kableExtra::kable_classic() %>%
  kableExtra::add_header_above(c(" ", "Total" = 1, setNames(1, "group.name[1]"),
                                 setNames(1, "group.name[2]"), " ")) %>%
  pack_rows(index = c("Genre" = 2), indent = F, label_row_css = "border-bottom: 1px solid;
            text-indent: 4%;") %>%
  add_indent(1, level_of_indent = 1)

res
i = 1
while (i <= nrow(test))
{
  tmp_grepl<-grepl(test$var, )

}

