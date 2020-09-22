#install.packages("data.table")


require(data.table)


load_data <- function(DTGame, name, sexe, age){
  #name = "Alex DLSKQ"
  #age = 21
  #sexe = 1
  
  filename = paste(name, "_log.csv", sep="")
  DT <- as.data.table(read.csv(filename, header = TRUE,sep=";"))
  DT$step = as.numeric(DT$step)
  DT = DT[order(step)]
  DT$idPlayer = name
  DT$fail = 1-DT$win
  DT <- DT[, sexe:=as.character(sexe)]
  DT$sexe = sexe
  DT$age = age

  DT$TimeNorm = as.numeric(as.POSIXct(DT$Time)) - as.numeric(as.POSIXct(DT[1]$Time))
  
  DT$variation.Model = 0
  DT$step = as.numeric(1:nrow(DT))

  
   
  variationTotale = 0
  for( i in 2:nrow(DT)){
    beta0i = DT[i]$beta0
    beta1i = DT[i]$beta1
    
    delta = 0
    nb = 0
    diffVals = seq(0,1,0.05)
    for(j in 1:3){
      if(i > j){
         beta0iprev = DT[i-j]$beta0
         beta1iprev = DT[i-j]$beta1
         for (x in diffVals)
           delta = delta + (1/(1+exp(-(beta0i+beta1i*x))) - 1/(1+exp(-(beta0iprev+beta1iprev*x))))^2
         nb = nb + 1
      }
    }
    
    if(nb > 0)
      delta = delta / nb

    DT[i]$variation.Model = sqrt(delta/length(diffVals))
    variationTotale = variationTotale + sqrt(delta/length(diffVals))
  }
  
  DT$variation.Model.Total = variationTotale / (nrow(DT)-1)
  
  #temps en premier courbe
  DT$TimeInFirstCond = nrow(DT)
  for( i in 2:nrow(DT)){
    if(DT[i]$curve != DT[i-1]$curve)
    {
      DT$TimeInFirstCond = i
      break
    }  
    
  }
  
  DT$IsInFirstCond = 1
  inFirstCond = 1;
  for( i in 2:nrow(DT)){
    if(DT[i]$curve != DT[i-1]$curve)
    {
      inFirstCond = 0;
    }  
    DT[i]$IsInFirstCond = inFirstCond
    
  }
  
  DT$FirstCond = DT[1]$curve
  
  
  
  if(ncol(DTGame) != 0)
    DT <- merge(DTGame,DT, all=TRUE)
  return(DT)
}


DTGame <- data.table()

DTGame <- load_data(DTGame,"Alex DLSKQ"   , 'M',21)
DTGame <- load_data(DTGame,"Doryan"       , 'M',26)
DTGame <- load_data(DTGame,"JaguarPygmee" , 'M',25)
DTGame <- load_data(DTGame,"Julia"        , 'F',24)
DTGame <- load_data(DTGame,"Mathieu"      , 'M',26)
DTGame <- load_data(DTGame,"Pierre"       , 'M',23)
DTGame <- load_data(DTGame,"Rambar"       , 'F',24) 
DTGame <- load_data(DTGame,"RedSPINE"     , 'M',22)
DTGame <- load_data(DTGame,"Samuel BUSSON", 'M',21) 
DTGame <- load_data(DTGame,"Théophile"    , 'M',22) 


DTGameFirstCond = DTGame[IsInFirstCond == 1]
DTGameFirstCondByPlayer = DTGameFirstCond[,.(TimeInFirstCond.Max = max(TimeInFirstCond), FirstCond = .SD[1]$FirstCond),by=idPlayer]
DTGameFirstCondByPlayer[,.(timeMean=mean(TimeInFirstCond.Max),timeSd=sd(TimeInFirstCond.Max), nb=.N),by=FirstCond]

res = wilcox.test(DTGameFirstCondByPlayer[FirstCond=="Pic"]$TimeInFirstCond.Max,
            DTGameFirstCondByPlayer[FirstCond=="Flow"]$TimeInFirstCond.Max)
print(paste("Pic/Flow",res$p.value))

res = wilcox.test(DTGameFirstCondByPlayer[FirstCond=="Low"]$TimeInFirstCond.Max,
            DTGameFirstCondByPlayer[FirstCond=="Flow"]$TimeInFirstCond.Max)
print(paste("Low/Flow",res$p.value))

res = wilcox.test(DTGameFirstCondByPlayer[FirstCond=="Pic"]$TimeInFirstCond.Max,
            DTGameFirstCondByPlayer[FirstCond=="Low"]$TimeInFirstCond.Max)
print(paste("Pic/Low",res$p.value))



