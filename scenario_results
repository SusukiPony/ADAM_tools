## Root directory and libraries

root <- 'C:/Users/AU550964/Documents/Work/Reprolac/ADAM/test'
setwd(root)

library(stringr)
library(xlsx)

## Read simulation means from .res file

ntime = 150     #Number of time steps
tsyear = 6      #Number of time steps per year


all_paths <-
  list.files(path = root,
             pattern = "geneticTrendsMeans.res$",
             recursive = T) 

### THIS PART GIVES ALL TIMESTEPS IN ALL SCENARIOS
alltimesteps <- cbind(c(0:ntime))
colnames(alltimesteps)[1] <- "time"

for (i in 1:length(all_paths))
{
  path <- all_paths[i]
  
  scenario <- lapply(strsplit(path,"/"), '[', 1)
  
  x <- read.table(file=path, header = T, nrows=ntime+1)
  x <- x[c("time","BV")]      #change if needed
  
  y <- rbind(x[c("BV")])
  colnames(y)[1] <- scenario
  
  alltimesteps <- cbind(alltimesteps, y)
  # print(scenario)
}

### THIS PART TAKES THE MEANS OF GIVEN TIME PERIODS
for (i in 1:length(all_paths))
{
  path <- all_paths[i]
  
  scenario <- lapply(strsplit(path,"/"), '[', 1)                   #Name from directory
  
  x <- read.table(file=path, header = T, nrows=ntime+1)
  x <- x[c("time","BV")]      #change if needed
  
  y15 <- subset(x,time>1*tsyear-1&time<1+5*tsyear);         x1 <- colMeans(y15);         #Mean of Year 1 to 5
  y610 <- subset(x,time>6*tsyear-1&time<1+10*tsyear);       x2 <- colMeans(y610)         #Mean of Year 6 to 10
  y1115 <- subset(x,time>11*tsyear-1&time<1+15*tsyear);     x3 <- colMeans(y1115)        #Mean of Year 11 to 15
  y1620 <- subset(x,time>16*tsyear-1&time<1+20*tsyear);     x4 <- colMeans(y1620)        #Mean of Year 16 to 20
  y2125 <- subset(x,time>21*tsyear-1);                      x5 <- colMeans(y2125)        #Mean of Year 21 to 25
  
  y <- cbind(scenario,
             year1t5=x1[c("BV")], 
             year6t10=x2[c("BV")], 
             year11t15=x3[c("BV")], 
             year16t20=x4[c("BV")], 
             year21t25=x5[c("BV")]
             )

    if ( i == 1 ) 
    {
      fiveyearmean <- y
    } 
    else 
    {
      fiveyearmean <- rbind(fiveyearmean, y)
    }
  #print(scenario)
}

## THIS PART WRITES IT ALL TO A .csv FILE
write.xlsx(alltimesteps, file="results.xlsx", sheetName="alltimesteps", row.names = FALSE)
write.xlsx(fiveyearmean, file="results.xlsx", sheetName="fiveyearmean", row.names = FALSE, append=TRUE)
