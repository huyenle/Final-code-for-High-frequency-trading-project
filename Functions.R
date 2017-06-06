# Sharpe ratio
mySR <- function(x, # x = series of returns
                 scale)
  # scale parameter = Nt
{
  if (is.na((
        sqrt(scale) * mean(coredata(x), na.rm = T) /
          sd(coredata(x), na.rm = T)))) return(0)
    else
      return((
        sqrt(scale) * mean(coredata(x), na.rm = T) /
          sd(coredata(x), na.rm = T)))
} # end of definition


#position R
positionR <- function(signal, lower, upper, pos_flat, strategy)
{
  # lets check thevalue of the strategy parameter
  if (! strategy %in% c("mom", "mr"))
  {  print("Strategy parameter incorrect. Please use 'mom' or 'mr'!")
    stop
  }
  
  # lets first create a vector of 0s
  position <- rep(0, length(signal))
  
  for (i in 2:length(signal))
  {
    if ( pos_flat[i] == 1 ) position[i] <- 0 
    else
    { # check if values are nonmissing (otherwise calculations not possible)
      if (!is.na(signal[i-1]) & 
          !is.na(upper[i-1]) & 
          !is.na(lower[i-1]))
      { 
        # what if previous position was 0
        if (position[i-1] == 0){
          if (signal[i-1] > upper[i-1]){position[i] <- -1}
          if (signal[i-1] < lower[i-1]){position[i] <- 1}
        } else if (position[i-1]==-1){
          # what if previous position was -1
          if (signal[i-1] > lower[i-1]){position[i] <- -1}
          if (signal[i-1] < lower[i-1]){position[i] <- 1}
        } else if (position[i-1]==1){
          # what if previous position was 1
          if (signal[i-1] < upper[i-1]){position[i] <- 1}
          if (signal[i-1] > upper[i-1]){position[i] <- -1}
        }
      } else position[i] <- position[i-1]
      # if anything is missing, keep previous position
    }
  }
  # reverse the position if we use a momentum ("mom") strategy
  if(strategy == "mom") position <- (-position)
  
  # return() function clearly indicates 
  # what the function should return
  return(position)
}

#function for going flat
pos_flatR <- function(Data){
  
  pos_flat <- xts(rep(0, nrow(Data)), index(Data))
  
  indexTZ(pos_flat) <- indexTZ(Data)
  
  #going flat first 30 mins and last 20 mins 
  
  pos_flat["T09:01/T09:30"] <- 1
  
  pos_flat["T17:11/T17:30"] <-1
  
  #going flat on Sat and Sun
  
  pos_flat[as.POSIXlt(index(pos_flat))$wday %in% c(0,6)] <- 1 
  
  # going flat for the days without any price changes
  
   ends_ <- endpoints(Data, "days")
    
   daily_sd <- period.apply(Data, INDEX = ends_, 
                           FUN = function(x) sd(x, na.rm = T))
   
   pos_flat[substr(index(daily_sd[daily_sd == 0]),1,10)] <- 1
   
  #going flat on whatever is NA
  
  pos_flat[which(is.na(Data))] <- 1
           
  return(pos_flat)
}

#volatility break-out function
Vol_breakout <- function(Data, Data_name, signalEMA, slowEMA, volat.sd, m_, pos_flat)
{
  ends_ <- endpoints(Data, "days")
  index_ <- index(Data)
  #
  if (Data_name == "VW") cost = 0.6 else cost = 0.3 #assigning the cost

  
  # calculating elements of the strategy
  signalEMA.values <- EMA(coredata(na.locf(Data[,Data_name])), signalEMA)
  slowEMA.values <- EMA(coredata(na.locf(Data[,Data_name])), slowEMA)
  volat.sd.values <- runsd(coredata(na.locf(Data[,Data_name])), volat.sd, 
                           endrule = "NA", align = "right")
  
  # put missing values whenever the original price is missing
  signalEMA.values[is.na(Data[,Data_name])] <- NA
  slowEMA.values[is.na(Data[,Data_name])] <- NA
  volat.sd.values[is.na(Data[,Data_name])] <- NA
  
  # position for momentum strategy
  pos.mom <- positionCPP(signal = signalEMA.values,
                        lower = slowEMA.values - m_ * volat.sd.values,
                        upper = slowEMA.values + m_ * volat.sd.values,
                        pos_flat = coredata(pos_flat),
                        strategy = "mom" # important !!!
  )
  
  # position for mean-rev strategy is just a reverse of pos.mom
  pos.mr <- (-pos.mom)
  
  # gross pnl
  pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(coredata(Data[,Data_name]))),
                          0, pos.mom * diff.xts(coredata(Data[,Data_name])) 
  )
  pnl.gross.mr <- (-pnl.gross.mom)
  
  # nr of transactions - the same for mom and mr
  ntrans <- abs(diff.xts(pos.mom))
  ntrans[1] <- 0
  
  # net pnl
  pnl.net.mom <- pnl.gross.mom - ntrans * cost 
  pnl.net.mr <- pnl.gross.mr - ntrans * cost 
  
  # aggregate to daily
  ends_ <- endpoints(Data, "days")
  
  pnl.gross.mom.d <- period.apply(pnl.gross.mom, INDEX = ends_, 
                                  FUN = function(x) sum(x, na.rm = T))
  pnl.gross.mr.d <- period.apply(pnl.gross.mr, INDEX=ends_, 
                                 FUN = function(x) sum(x, na.rm = T))
  pnl.net.mom.d <- period.apply(pnl.net.mom, INDEX = ends_,
                                FUN = function(x) sum(x, na.rm = T))
  pnl.net.mr.d <- period.apply(pnl.net.mr, INDEX = ends_, 
                               FUN = function(x) sum(x, na.rm = T))
  ntrans.d <- period.apply(ntrans,INDEX = ends_, 
                           FUN = function(x) sum(x, na.rm = T))
  
  # calculate summary measures
  gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
  gross.SR.mr <- mySR(pnl.gross.mr.d, scale = 252)
  net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
  net.SR.mr <- mySR(pnl.net.mr.d, scale = 252)
  
  days_ <- index_[ends_]
  av.daily.ntrans <- mean(ntrans.d[!as.POSIXlt(days_)$wday %in% c(0,6)], na.rm=T) 
  
  
  # summary of a particular strategy
  summary1_ <- data.frame(Data = Data_name,
                          Strategy = "mom",
                          signalEMA = signalEMA,
                          slowMA = slowEMA,
                          volat.sd = volat.sd,
                          m = m_,
                          period = selected_quarter,
                          gross.SR = gross.SR.mom,
                          net.SR = net.SR.mom,
                          pnl.gross = sum(pnl.gross.mom, na.rm = T),
                          pnl.net = sum(pnl.net.mom, na.rm = T),
                          av.daily.ntrans,
                          stringsAsFactors = F
  )
  summary2_ <- data.frame(Data = Data_name,
                          Strategy = "mr",
                          signalEMA = signalEMA,
                          slowMA = slowEMA,
                          volat.sd = volat.sd,
                          m = m_,
                          period = selected_quarter,
                          gross.SR = gross.SR.mr,
                          net.SR = net.SR.mr,
                          pnl.gross = sum(pnl.gross.mr, na.rm = T),
                          pnl.net = sum(pnl.net.mr, na.rm = T),
                          av.daily.ntrans,
                          stringsAsFactors = F
  )
  
  return(rbind(summary1_, summary2_))
}#end of function

#double volatility break-out function
Vol_double_breakout <- function(Data, Data_name, signalEMA, slowEMA, volat.sd, m_, pos_flat)
{
 if (Data_name == "VW") cost = 0.6 else cost = 0.3 #cost of transaction
 ends_ <- endpoints(Data, "days")
 index_ <- index(Data)

        

        # print(paste("signalEMA = ", signalEMA,
        #             ", slowEMA = ", slowEMA,
        #             ", volat.sd = ", volat.sd,
        #             ", Data = ", Data_name,
        #             ", m_ = ", m_, sep = "")) 
        # 
        
        # calculating elements of the strategy
        signalEMA.values <- EMA(coredata(na.locf(Data[,Data_name])), signalEMA)
        slowEMA.values <- EMA(coredata(na.locf(Data[,Data_name])), slowEMA)
        volat.sd.values <- runsd(coredata(na.locf(Data[,Data_name])), volat.sd, 
                                 endrule = "NA", align = "right")
        
        # put missing values whenever the original price is missing
        signalEMA.values[is.na(Data[,Data_name])] <- NA
        slowEMA.values[is.na(Data[,Data_name])] <- NA
        volat.sd.values[is.na(Data[,Data_name])] <- NA
 
        
        # position for momentum strategy
        pos.mom <- position_doubleCPP(signal = signalEMA.values,
                                      lower_entry = slowEMA.values - (m_+1) * volat.sd.values,
                                      upper_entry = slowEMA.values + (m_+1) * volat.sd.values,
                                      lower_exit = slowEMA.values - m_ * volat.sd.values,
                                      upper_exit = slowEMA.values + m_ * volat.sd.values,
                        
                                      pos_flat = coredata(pos_flat),
                                      strategy = "mom" # important !!!
        )
        
        # position for mean-rev strategy is just a reverse of pos.mom
        pos.mr <- (-pos.mom)
        
        # gross pnl
        pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(coredata(Data[,Data_name]))),
                                0, pos.mom * diff.xts(coredata(Data[,Data_name])) 
        )
        pnl.gross.mr <- (-pnl.gross.mom)
        
        # nr of transactions - the same for mom and mr
        ntrans <- abs(diff.xts(pos.mom))
        ntrans[1] <- 0
        
        # net pnl
        pnl.net.mom <- pnl.gross.mom - ntrans * cost 
        pnl.net.mr <- pnl.gross.mr - ntrans * cost 
        
        # aggregate to daily
        ends_ <- endpoints(Data, "days")
        
        pnl.gross.mom.d <- period.apply(pnl.gross.mom, INDEX = ends_, 
                                        FUN = function(x) sum(x, na.rm = T))
        pnl.gross.mr.d <- period.apply(pnl.gross.mr, INDEX=ends_, 
                                       FUN = function(x) sum(x, na.rm = T))
        pnl.net.mom.d <- period.apply(pnl.net.mom, INDEX = ends_,
                                      FUN = function(x) sum(x, na.rm = T))
        pnl.net.mr.d <- period.apply(pnl.net.mr, INDEX = ends_, 
                                     FUN = function(x) sum(x, na.rm = T))
        ntrans.d <- period.apply(ntrans,INDEX = ends_, 
                                 FUN = function(x) sum(x, na.rm = T))
        
        # calculate summary measures
        gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
        gross.SR.mr <- mySR(pnl.gross.mr.d, scale = 252)
        net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
        net.SR.mr <- mySR(pnl.net.mr.d, scale = 252)
        
        days_ <- index_[ends_]
        av.daily.ntrans <- mean(ntrans.d[!as.POSIXlt(days_)$wday %in% c(0,6)], na.rm=T) 
        
        # summary of a particular strategy
        summary1_ <- data.frame(Data = Data_name,
                               Strategy = "mom",
                               signalEMA = signalEMA,
                               slowMA = slowEMA,
                               volat.sd = volat.sd,
                               m = m_,
                               period = selected_quarter,
                               gross.SR = gross.SR.mom,
                               net.SR = net.SR.mom,
                               pnl.gross = sum(pnl.gross.mom, na.rm = T),
                               pnl.net = sum(pnl.net.mom, na.rm = T),
                               av.daily.ntrans,
                               stringsAsFactors = F
        )
        summary2_ <- data.frame(Data = Data_name,
                               Strategy = "mr",
                               signalEMA = signalEMA,
                               slowMA = slowEMA,
                               volat.sd = volat.sd,
                               m = m_,
                               period = selected_quarter,
                               gross.SR = gross.SR.mr,
                               net.SR = net.SR.mr,
                               pnl.gross = sum(pnl.gross.mr, na.rm = T),
                               pnl.net = sum(pnl.net.mr, na.rm = T),
                               av.daily.ntrans,
                               stringsAsFactors = F
        )
        
        return(rbind(summary1_, summary2_))
}


Drawingpnl <- function(Data, Data_name, signalEMA, slowEMA, volat.sd, m_,pos_flat, strategy)
{ends_ <- endpoints(Data, "days")
index_ <- index(Data)
#
if (Data_name == "VW") cost = 0.6 else cost = 0.3 #assigning the cost

# calculating elements of the strategy
signalEMA.values <- EMA(coredata(na.locf(Data[,Data_name])), signalEMA)
slowEMA.values <- EMA(coredata(na.locf(Data[,Data_name])), slowEMA)
volat.sd.values <- runsd(coredata(na.locf(Data[,Data_name])), volat.sd, 
                         endrule = "NA", align = "right")

# put missing values whenever the original price is missing
signalEMA.values[is.na(Data[,Data_name])] <- NA
slowEMA.values[is.na(Data[,Data_name])] <- NA
volat.sd.values[is.na(Data[,Data_name])] <- NA

# position for momentum strategy
pos <- positionCPP(signal = signalEMA.values,
                   lower = slowEMA.values - m_ * volat.sd.values,
                   upper = slowEMA.values + m_ * volat.sd.values,
                   pos_flat = coredata(pos_flat),
                   strategy = strategy # important !!!
)


# gross pnl
pnl.gross <- ifelse(is.na(pos * diff.xts(coredata(Data[,Data_name]))),
                    0, pos * diff.xts(coredata(Data[,Data_name])) 
)

# nr of transactions - the same for mom and mr
ntrans <- abs(diff.xts(pos))
ntrans[1] <- 0

# net pnl
pnl.net <- pnl.gross - ntrans * cost 

# aggregate to daily
ends_ <- endpoints(Data, "days")

pnl.gross.d <- period.apply(pnl.gross, INDEX = ends_, 
                            FUN = function(x) sum(x, na.rm = T))
pnl.net.d <- period.apply(pnl.net, INDEX = ends_,
                          FUN = function(x) sum(x, na.rm = T))
ntrans.d <- period.apply(ntrans,INDEX = ends_, 
                         FUN = function(x) sum(x, na.rm = T))


plot(cumsum(coredata((pnl.gross.d))), type = "l", 
     main = paste(Data_name, "from", as.Date(index(Data)[1]), "to", as.Date(index(Data)[nrow(Data)]), sep = " "),
     ylim = c(min(pnl.net.d),max(pnl.gross.d)),
     ylab = "PnL")
lines(cumsum(coredata((pnl.net.d))), col = "red")
abline(h=0, lty = 2, col = "gray")
}