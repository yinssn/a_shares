
sh_index <- read.csv("http://quotes.money.163.com/service/chddata.html?code=0000001&start=19901219&end=20180911&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;VOTURNOVER;VATURNOVER",stringsAsFactors = FALSE) #来源：网易财经
sh_index <- sh_index[order(sh_index[[1]],decreasing= FALSE),]

merger <- function(remained){
  
  change <- sum(as.numeric(remained$涨跌额))
  changerate <- prod(as.numeric(remained$涨跌幅)/100+1)-1
  result <- data.frame(remained$日期[1],remained$日期[dim(remained)[1]],max(remained$最高价),min(remained$最低价),remained$开盘价[1],
                       remained$收盘价[dim(remained)[1]],change,changerate,sum(as.numeric(remained$成交量)),sum(as.numeric(remained$成交金额)))
  names(result) <- c("开始日","结束日","最高价","最低价","起始价","结束价","涨跌总额","涨跌总幅度","成交总量","成交总金额")
  return(result)
}

result <-data.frame()

for(i in 1:dim(sh_index)[2]){
  sh_index[,i] <- gsub("None","0",sh_index[,i])
}#平滑数据


temp <- 2

for (i in 3:dim(sh_index)[1]-1){
  if(as.numeric(sh_index$涨跌幅[i])*as.numeric(sh_index$涨跌幅[i+1])<0){
    temp_v <- merger(sh_index[temp:i,])
    result <- rbind(result,temp_v)
    temp <- i+1
  }else next
}


#rm(list = ls())

