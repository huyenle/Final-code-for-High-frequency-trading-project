library(zoo)
library(xts)
library(urca)
library(chron)
library(TTR)
library(caTools)

setwd("...")

selected_quarter ="2015Q3"

filename_ <- paste("data1.", selected_quarter, ".RData", sep="")
load(filename_)

load(name_file)

source("Functions.R") #file with mySR and positionR functions


data1.r <- 10000*diff.xts(log(data1))

names(data1.r)[1:2] <- c("NASDAQ.return", "SP500.return")

data2 <- merge(data1[, c("NASDAQ", "SP500")],
               data1.r[, c("NASDAQ.return", "SP500.return")])

data2["T00:01/T00:15",] <- NA
data2["T13:46/T14:00",] <- NA
# No trading in the first and last 15 mins

dweek_ <- as.POSIXlt(index(data2))$wday
table (dweek_)

data2 <- data2[-which(dweek_ %in% c(0,6)),]


###########################################################
# PAIR TRADE WITH MEAN REVERTING#
###########################################################

my.endpoints <- endpoints(data2, "days")

data.av.ratio <- period.apply(data2,
                              INDEX = my.endpoints,
                              function(x) mean(0.5* x$NASDAQ/x$SP500, na.rm = T)
)

head(data.av.ratio)
names(data.av.ratio) <- "av.ratio"

index(data.av.ratio) <- index(data.av.ratio) + 60*60*10

data3 <- merge(data2, data.av.ratio)

data3$av.ratio <- na.locf(data3$av.ratio)

dweek_ <- as.POSIXlt(index(data3))$wday
table(dweek_)

data3 <- data3[-which(dweek_ %in% c(0,6)),]
# remove some unwanted data 

data3$spread <- 0.5* data3$NASDAQ - data3$av.ratio * data3$SP500

pos_flat <- xts(rep(0, nrow(data3)), index(data3), tz = 'EST')

pos_flat["T13:41/T14:00"] <- 1
# do not hold positions overnight (exit all positions 20 minutes 
# before the session end)

pos_flat["T00:01/T00:30"] <- 1
# do not trade within the first 30 minutes of quotations.


ends_ <- endpoints(data3, "days")

daily_sd <- period.apply(data3$NASDAQ, INDEX = ends_, 
                         FUN = function(x) sd(x, na.rm = T))

pos_flat[substr(index(daily_sd[daily_sd == 0]),1,10)] <- 1


    data3$spread_rollsd120 <- runsd(data3$spread, 120, 
                                    endrule = "NA", align = "right")
    
    data3$spread_rollsd120[is.na(data3$NASDAQ)] <- NA
    data3$spread_rollsd120[is.na(data3$SP500)] <- NA
    
    data3$upper <- 3 * data3$spread_rollsd120
    data3$lower <- (-3 * data3$spread_rollsd120)
    
    data3$pos_strategy <- positionR(signal = coredata(data3$spread),
                                    lower = coredata(data3$lower),
                                    upper = coredata(data3$upper),
                                    pos_flat = coredata(pos_flat),
                                    strategy = "mr" # important !!!
    )
    
    
    data3$ntrans <- abs(diff(data3$pos_strategy))
    sum_trans <- sum(data3$ntrans,na.rm=TRUE)
    
    data3$gross.pnl <- (data3$pos_strategy) *
      (diff.xts(data3$NASDAQ) * 25 -
         data3$av.ratio * diff.xts(data3$SP500)*50)
    
    data3$net.pnl <- data3$gross.pnl - 
      data3$ntrans * (4 + data3$av.ratio * 4)
    
    my.endpoints <- endpoints(data3, "days")
    
    pnl_daily <- period.apply(data3,
                              INDEX = my.endpoints,
                              function(x) colSums(x[,c("gross.pnl","net.pnl")],
                                                  na.rm = T))
    ntrans.d <- period.apply(data3,
                             INDEX = my.endpoints,
                             function(x) colSums(x[,c("ntrans")],
                                                  na.rm = T))
    
    
    ###
    index_<-index(data3)
    days_ <- index_[ends_]
    av.daily.ntrans <- mean(ntrans.d[!as.POSIXlt(days_)$wday %in% c(0,6)], na.rm=T)
    
    ###
    
    gross.SR.mr <- mySR(pnl_daily$gross.pnl, scale = 252)
    
    net.SR.mr <- mySR(pnl_daily$net.pnl, scale = 252)
    
    plot(coredata(cumsum(pnl_daily$gross.pnl)), type = "l")
    lines(coredata(cumsum(pnl_daily$net.pnl)), col = "blue")
    abline(h=0, lty = 2, col = "gray")
    
    # plotting pnl
    
    summary_1 <- data.frame(sd=120,
                            m=3,
                            gross.SR.mr,
                            net.SR.mr,
                            gross.pnl =sum(pnl_daily$gross.pnl),
                            net.pnl =sum(pnl_daily$net.pnl),
                            av.daily.ntrans,
                            stringsAsFactors = F,
                            Quarter = selected_quarter)
    
    # putting all summaries together
    if(!exists("summary_pair1")) summary_pair1 <- summary_1 else
      summary_pair1 <- rbind(summary_pair1, summary_1)
 

summary_pair1 <- summary_pair1[order(-summary_pair1$net.SR.mr),]
# - is used for the decreasing order

head(summary_pair1)

rm(summary_pair1)


