banana <- iris[1, ]
banana[1,] <- NA
iris
t1 <- seq(from=0, to=nrow(iris), by=5)
t2 <- seq(from=1, to=nrow(iris), by=5)

ref <- data.frame(from=t2, to=t1[-1])

tmp <- c()
for(i in 1:nrow(ref)){
  tmp2 <- rbind(iris[ref[i, "from"]:ref[i,"to"],], banana)
  print(tmp2)
  tmp <- rbind(tmp, tmp2)
}

test_map1<-data.frame("nom" = rep(LETTERS[1:10], each=2, length.out=20),
                                      "num" = rep(1:10, each=2, length.out=20),
                      group = rep(seq(from = 0, to = 1, by = 1)))
test_map1
test_map2<-data.frame("nom" = rep(LETTERS[1:10]), "num" = rep(11:20), group = rep("0", 10))
test_map2
test_tmp<-c()
i = 1;
j = 1;
while (i < nrow(test_map1))
{
  test_tmp2<-rbind(test_map1[i,], test_map2[j,])
  test_tmp2<-rbind(test_tmp2, test_map1[i+1,])
  test_tmp<-rbind(test_tmp, test_tmp2)
  i = i + 2;
  j = j +1;
}


