library(parallel)
library(MASS)
numCores <- detectCores() - 1

starts <- rep(100, 40)
fx <- function(nstart, ...) {
  tryCatch({
    kmeans(Boston, 4, nstart=nstart)
  },
  error=function(e){
    print(e)
  },
  warning=function(w){
    print(w)
  })
}
system.time(
  results <- lapply(starts, fx)
)

system.time(
  results <- mclapply(starts, fx, mc.cores = numCores)
)

fn_1<-function(X, FUN, ...){
  return(lapply(X, FUN, ...))
}

fn_2<-function(X, FUN, ...){
  return(lapply(X, FUN, ...))
}

test<-function(bool = FALSE, ...){
  args<-list(...)
  print(args)
  if (bool == "TRUE")
    fn<-mclapply
  else{
    args[["mc.cores"]]<-NULL
    fn<-lapply
  }
  return(list("func" = fn, "args" = args))
}

func<-test(TRUE, mc.cores = numCores)

l = list("X" = starts, "FUN" = fx, unlist(func[["args"]]))
system.time(
  r<-do.call(func[["func"]], l)
)
###
foo1 <- function (a) print(a)
foo2 <- function (b) sprintf("this is function foo2!, %s", b)

test <- function (FUN, ...) {
  if (!is.function(FUN)) stop("argument FUN is not a function!")
  FUN(...)
}
test(foo2, "a")
