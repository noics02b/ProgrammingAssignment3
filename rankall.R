rankall <- function(outcome, num="best"){
  ## Read outcome data
  
  ## Set Working Directory
  ## setwd("/Users/totomai/Documents/R Programming - Coursera/testfunctions/ProgrammingAssignment3")
  
  ## Read Outcome Data
  outcomeOfCare <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  UniqOutcome <- c("heart attack","heart failure","pneumonia")
  dimnames(outcomeOfCare)[[2]][[11]]<-"heart.attack"  ##renaming long column name to be referenced later
  dimnames(outcomeOfCare)[[2]][[17]]<-"heart.failure"
  dimnames(outcomeOfCare)[[2]][[23]]<-"pneumonia"
  
  ## Check that state and outcome are valid
  CheckOutcome<- function(outcome){
    for (i in 1:length(UniqOutcome))
      if (outcome == UniqOutcome[i] ) { ##print(c("outcome matched!!", i))
        return (outcome)}
    else if (i == length(UniqOutcome)) return (FALSE) 
  }

  outcome<-CheckOutcome(outcome)
  if (outcome == FALSE) stop("invalid outcome")
  
  
  ## Isolate per outcome
  if (outcome=="heart attack")  {
    col.outcome<-"heart.attack"
    
  }
  
  else if (outcome=="heart failure") {
    col.outcome<-"heart.failure"
                        }
  
  else if (outcome=="pneumonia") {
    col.outcome<-"pneumonia"
  }
  
  ## Clean data according to outcome only
  cases.ha<-complete.cases(as.numeric(outcomeOfCare[,col.outcome]))
  NoNA<-outcomeOfCare$Provider.Number[cases.ha]
  newOutcome<<-outcomeOfCare[outcomeOfCare$Provider.Number %in% NoNA,]  ## no more NAs !
  dataout<-cbind(newOutcome[,"State"],newOutcome[,"Hospital.Name"],newOutcome[,col.outcome])  ## characters
  dataoutdf<-data.frame(State=dataout[,1],HospitalName= dataout[,2],Outcome=as.numeric(dataout[,3]),stringsAsFactors=FALSE) ## organized df
  
  splt<-split(dataoutdf,dataoutdf$State)  ## split according to state  c
  ranking<-tapply(dataoutdf$Outcome,dataoutdf$State,rank)   ## same as split then lapply  b
  d1 <- NULL
  for (i in names(splt)) { 
    d<-cbind(splt[[i]],Rank=ranking[[i]])
    d1<-rbind(d1,d)
    #print(c(i))
  }
  write.csv(d1,"data2.csv",row.names=FALSE)
  if (num=="best")  num <- 1
  else if (num == "worst") num <- length(output[[3]])
  else num <-num
  ##print(num)
  ## For each state, find the hospital of the given rank
  #ranking <- tapply(newOutcome[,col.outcome],newOutcome$State,function (s) order(newOutcome,newOutcome$Hospital.Name))  ## ranks all ratings for each state.  Output via list
  return (d1[d1$Rank == num,c("HospitalName","State")])
  ##return(d1)
  ## Return a data frame with the hospital names and the abbreviated state names
}