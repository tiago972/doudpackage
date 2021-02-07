### Data sample for testing #####

test_map1<-data.frame("nom" = rep(LETTERS[1:10], each=2, length.out=20),
                                      "num" = rep(1:10, each=2, length.out=20),
                      group = rep(seq(from = 0, to = 1, by = 1)))
test_map1
test_map2<-data.frame("nom" = rep(LETTERS[1:10]), "num" = rep(11:20), group = rep("0", 10))
test_map2
################ Test pour htmlTable ######
output <- matrix(1:4,ncol=2,dimnames = list(list("Row 1", "Row 2"),list("Column 1", "Column 2")))

class(htmlTable(output))

