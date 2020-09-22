#install.packages("data.table")


require(data.table)


load_data <- function(DTGame, name, sexe, age){
  # name = "Antoine"
  # age = 21
  # sexe = 1
  # 
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
  # DT$TimeInFirstCond = nrow(DT)
  # for( i in 2:nrow(DT)){
  #   if(DT[i]$curve != DT[i-1]$curve)
  #   {
  #     DT$TimeInFirstCond = i
  #     break
  #   }  
  #   
  # }
  
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

DTGame <- load_data(DTGame,"Alex DLSKQ"     		 , 'M',21) 
DTGame <- load_data(DTGame,"Alexandre"      		 , 'M',25) 
#DTGame <- load_data(DTGame,"Antoine"       		 , 'M',25) 
DTGame <- load_data(DTGame,"Aurélien"       		 , 'M',31) 
DTGame <- load_data(DTGame,"Baptiste"       		 , 'M',21) 
DTGame <- load_data(DTGame,"BB"             		 , 'M',21) 
DTGame <- load_data(DTGame,"Clara Toussaint" 		 , 'F',20)  
DTGame <- load_data(DTGame,"Doryan"        	       , 'M',26) 
DTGame <- load_data(DTGame,"Guilhem"       	       , 'M',22) 
DTGame <- load_data(DTGame,"Guillaume"                 , 'M',21) 
DTGame <- load_data(DTGame,"Hugo Warion -- Saillant"   , 'M',21)
DTGame <- load_data(DTGame,"JaguarPygmee"   		 , 'M',25)
DTGame <- load_data(DTGame,"Jean-Michel Chocobanane"   , 'M',22)
DTGame <- load_data(DTGame,"Julia"          		 , 'F',24)
DTGame <- load_data(DTGame,"kiki"          		 , 'M',34)
DTGame <- load_data(DTGame,"Laurent"          		 , 'H',21)
DTGame <- load_data(DTGame,"Léo"          		 , 'H',21)
DTGame <- load_data(DTGame,"Marion"          		 , 'F',23)
DTGame <- load_data(DTGame,"Mathieu"        		 , 'M',26)
DTGame <- load_data(DTGame,"Matthieu"        		 , 'M',22)
DTGame <- load_data(DTGame,"Minna"        		 , 'F',21)
DTGame <- load_data(DTGame,"Narumik"        		 , 'M',22)
DTGame <- load_data(DTGame,"NicolasZ"        		 , 'M',21)
DTGame <- load_data(DTGame,"Pamphile"        		 , 'M',21)
DTGame <- load_data(DTGame,"Pierre"         		 , 'M',23)
DTGame <- load_data(DTGame,"Mathieu"        		 , 'M',26)
DTGame <- load_data(DTGame,"Pulu"         		 , 'M',37) 
DTGame <- load_data(DTGame,"Rambar"        		 , 'F',24)
DTGame <- load_data(DTGame,"RedSPINE"       		 , 'M',22)
DTGame <- load_data(DTGame,"Samuel BUSSON"  		 , 'M',21) 
DTGame <- load_data(DTGame,"Simon"        		 , 'M',22)
DTGame <- load_data(DTGame,"Théa"        		 , 'F',22)
DTGame <- load_data(DTGame,"Theo"        		 , 'M',20)
DTGame <- load_data(DTGame,"Théophile"      		 , 'M',22)  
DTGame <- load_data(DTGame,"Thomas"      		 , 'M',24)  
DTGame <- load_data(DTGame,"Tomskiev"      		 , 'M',29)  
DTGame <- load_data(DTGame,"ToshNobody"      		 , 'M',26)  
DTGame <- load_data(DTGame,"TOTH Benoit"      		 , 'M',22)  
DTGame <- load_data(DTGame,"Valentin"      		 , 'M',23)  
DTGame <- load_data(DTGame,"Victor"      		 , 'M',23)  


DTGameFirstCond = DTGame[IsInFirstCond == 1]

DTGameFirstCondByPlayer = DTGameFirstCond[,.(TimeInFirstCond = max(TimeNorm), FirstCond = .SD[1]$FirstCond),by=idPlayer]

#affiche
DTGameFirstCondByPlayer 
DTGameFirstCondByPlayer[,.(timeMean=mean(TimeInFirstCond),timeSd=sd(TimeInFirstCond), nb=.N),by=FirstCond]

res = wilcox.test(DTGameFirstCondByPlayer[FirstCond=="Pic"]$TimeInFirstCond,
                  DTGameFirstCondByPlayer[FirstCond=="Flow"]$TimeInFirstCond)
print(paste("Pic/Flow",res$p.value))

res = wilcox.test(DTGameFirstCondByPlayer[FirstCond=="Low"]$TimeInFirstCond,
                  DTGameFirstCondByPlayer[FirstCond=="Flow"]$TimeInFirstCond)
print(paste("Low/Flow",res$p.value))

res = wilcox.test(DTGameFirstCondByPlayer[FirstCond=="Pic"]$TimeInFirstCond,
                  DTGameFirstCondByPlayer[FirstCond=="Low"]$TimeInFirstCond)
print(paste("Pic/Low",res$p.value))



