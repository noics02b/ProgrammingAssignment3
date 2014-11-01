best <- function(state,outcome) {
  ## Set Working Directory
  ## setwd("/Users/totomai/Documents/R Programming - Coursera/testfunctions/ProgrammingAssignment3")
  
  ## Read outcome data
  outcomeOfCare <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  UniqState <- unique(outcomeOfCare$State)
  ##UniqHosp <- unique(outcomeOfCare$Hospital.name)
  UniqOutcome <- c("heart attack","heart failure","pneumonia")
  dimnames(outcomeOfCare)[[2]][[11]]<-"heart.attack"  ##renaming long column name to be referenced later
  dimnames(outcomeOfCare)[[2]][[17]]<-"heart.failure"
  dimnames(outcomeOfCare)[[2]][[23]]<-"pneumonia"
  
  
  ## Check that state and outcome are valid
  CheckState<- function(state){
                for (i in 1:length(UniqState))
                  ##print (c(i,UniqState[i]))
                  if (state == UniqState[i] ) { print(c("State matched!!", i))
                                                return (state)}
                    else if (i == length(UniqState)) return(FALSE)##stop("State is not Recognized")
                }
  CheckOutcome<- function(outcome){
    for (i in 1:length(UniqOutcome))
      if (outcome == UniqOutcome[i] ) { print(c("outcome matched!!", i))
                                    return (outcome)}
    else if (i == length(UniqOutcome)) return (FALSE) ##stop("Outcome is not Recognized")
  }
   
  state<-CheckState(state)
  outcome<-CheckOutcome(outcome)
  if (state == FALSE) stop("invalid state")
  if (outcome == FALSE) stop("invalid outcome")
 
  
  ##if (state != )
  
  ## Return hospital name in that state with the lowest 30-day death rate
  if (outcome == "heart attack") minrate <- min(outcomeOfCare[outcomeOfCare$State==state,"heart.attack"]) 
  else if (outcome == "heart failure") minrate <- min(outcomeOfCare[outcomeOfCare$State==state,"heart.failure"])
  else if (outcome == "pneumonia") minrate <- min(outcomeOfCare[outcomeOfCare$State==state,"pneumonia"])
  else stop ("Check spelling of your outcome")
  print(minrate)
  output <- outcomeOfCare[outcomeOfCare$heart.attack==minrate & outcomeOfCare$State==state,2]
  print(output)
 
  
}