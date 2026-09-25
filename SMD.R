smd_calc<-function(data, group, col, data_app){
  if ((is.factor(data[,col]) && nlevels(data[,col]) == 2)){


    m_1 <- prop.table(table(data[data[,group] == 1,col]))[2]
    m_1<-as.vector(m_1)
    m_0 <- prop.table(table(data[data[,group] == 0,col]))[2]
    m_0<-as.vector(m_0)


    s2_1 <- m_1 * (1 - m_1)
    s2_0 <- m_0 * (1 - m_0)

    z <- (s2_1 + s2_0)/2
    # Weighted

    mw_1 <- as.vector(prop.table(table(data_app[data_app[,group] == 1,col]))[2])
    mw_0 <- as.vector(prop.table(table(data_app[data_app[,group] == 0,col]))[2])

    s2w_1 <- mw_1* (1 - mw_1)
    s2w_0 <- mw_0 * (1 - mw_0)
    r<-(mw_1 - mw_0)
  }
  else if (is.numeric(data[,col])){
    m_1 <- mean(data_app[data_app[,group] == 1,col])
    m_0 <- mean(data_app[data_app[,group] == 0,col])

    s2_1 <- sd(data[data[,group] == 1,col])
    s2_0 <- sd(data[data[,group] == 0,col])
    r<-(m_1 - m_0)/s2_1
  }
  else
    print("not possible")
  return(round(r, 4))
}
smd_calc(bdd.app, "VEHIC_ON", "DT2", m.data)
