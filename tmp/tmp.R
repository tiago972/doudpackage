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

###### htmlTable
htmlTable(test3[,c("var", "Total", "1", "0", "p")], cgroup = c())

### Tests de mini fonctions
tmp_factor<-levels(bdd$vivant.s4)
tmp_table<-table(bdd$vivant.s4, useNA = "always")
tmp_prop.table<-round(prop.table(table(bdd$vivant.s4, useNA = "always")) * 100, digits = 1)

tmp_table[1]
colnames(test3)[which(colnames(test3) == "0")]<-paste("n = ", tmp_table[1], "(", tmp_prop.table[1], "%)", sep = "")

c<-grep("Missing values.*", test4[,"test"])

iris3 <- iris[c(1:2, 51:54, 101:103), ]
kable(iris3[, 1:4], booktabs = TRUE) %>%
  pack_rows(index = c("setosa" = 2, "versicolor" = 4, "virginica" = 3)
)
