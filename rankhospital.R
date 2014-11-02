rankhospital <- function (state, outcome, num ="best") {
  ## Set Working Directory
  ## setwd("/Users/totomai/Documents/R Programming - Coursera/testfunctions/ProgrammingAssignment3")
  
   ## Read Outcome Data
  outcomeOfCare <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  UniqState <- unique(outcomeOfCare$State)
  UniqOutcome <- c("heart attack","heart failure","pneumonia")
  dimnames(outcomeOfCare)[[2]][[11]]<-"heart.attack"  ##renaming long column name to be referenced later
  dimnames(outcomeOfCare)[[2]][[17]]<-"heart.failure"
  dimnames(outcomeOfCare)[[2]][[23]]<-"pneumonia"
  
  ## Check that state and outcome are valid
  CheckState<- function(state){
    for (i in 1:length(UniqState))
      if (state == UniqState[i] ) { ##print(c("State matched!!", i))
        return (state)}
    else if (i == length(UniqState)) return(FALSE)
  }
  CheckOutcome<- function(outcome){
    for (i in 1:length(UniqOutcome))
      if (outcome == UniqOutcome[i] ) { ##print(c("outcome matched!!", i))
        return (outcome)}
    else if (i == length(UniqOutcome)) return (FALSE) 
  }
  
  state<-CheckState(state)
  outcome<-CheckOutcome(outcome)
  if (state == FALSE) stop("invalid state")
  if (outcome == FALSE) stop("invalid outcome")
  
  ##Clean data of NAs
  if (outcome=="heart attack")  {
              col.outcome<-"heart.attack"

  }
              
  else if (outcome=="heart failure") {
    col.outcome<-"heart.failure"
    #cases.ha<-complete.cases(as.numeric(outcomeOfCare$heart.failure))
    #NoNA<-outcomeOfCare$Provider.Number[cases.ha]
    #newOutcome<-outcomeOfCare[outcomeOfCare$Provider.Number %in% NoNA,]  ## no more NAs on heart.attack!
    ##write.csv(newOutcome,"data1.csv",row.names=FALSE)  ## test
    
    #ranking <- tapply(newOutcome$heart.failure,newOutcome$State,rank)  ## ranks all ratings for each state.  Output via list
    #write.csv(ranking,"data2.csv",row.names=FALSE)
    #a<- ranking[rownames=state]  ##list [define which state ]
    #b<- data.frame(newOutcome[newOutcome$State==state,"Hospital.Name"], newOutcome[newOutcome$State==state,"heart.failure"], a)
  }
  
  else if (outcome=="pneumonia") {
    col.outcome<-"pneumonia"
    #cases.ha<-complete.cases(as.numeric(outcomeOfCare$pneumonia))
    #NoNA<-outcomeOfCare$Provider.Number[cases.ha]
    #newOutcome<-outcomeOfCare[outcomeOfCare$Provider.Number %in% NoNA,]  ## no more NAs on pneumonia!
    #write.csv(newOutcome,"data1.csv",row.names=FALSE)  ## test
    
    #ranking <- tapply(newOutcome$pneumonia,newOutcome$State,rank)  ## ranks all ratings for each state.  Output via list
    
    #a<- ranking[rownames=state]  ##list [define which state ]
    #b<- data.frame(newOutcome[newOutcome$State==state,"Hospital.Name"], newOutcome[newOutcome$State==state,"pneumonia"], a)
  }
     
  cases.ha<-complete.cases(as.numeric(outcomeOfCare[,col.outcome]))
  NoNA<-outcomeOfCare$Provider.Number[cases.ha]
  newOutcome<-outcomeOfCare[outcomeOfCare$Provider.Number %in% NoNA,]  ## no more NAs on heart.attack!
  ##write.csv(newOutcome,"data1.csv",row.names=FALSE)  ## test
  
  ranking <- tapply(newOutcome[,col.outcome],newOutcome$State,rank)  ## ranks all ratings for each state.  Output via list
  
  a<- ranking[rownames=state]  ##list [define which state ]
  b<- data.frame(newOutcome[newOutcome$State==state,"Hospital.Name"], newOutcome[newOutcome$State==state,col.outcome], a)
  

  
  output<-b[with(b,order(b[3],b[1])),]
  colnames(output)<-c("HospitalName","Outcome", "Rank")
  
  if (num=="best")  num <- 1
  else if (num == "worst") num <- length(output[[3]])
  else num <-num
              
  write.csv(output,"data2.csv",row.names=FALSE)
    
  
  
  return (output)
  ## Return hospital name in that state with the given rank
  ## 30- day death rate
}