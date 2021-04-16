best <- function(state,outcome) {
   # load data
   getwd()
   data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",na.strings = "Not Available")
   
   # check validity of argument
   state_list <- data$State
   state <- toupper(state)
   if (!(state %in% state_list)) stop("invalid state")
   
   outcome_list <- c("heart attack", "heart failure", "pneumonia")
   outcome <- tolower(outcome)
   if (!(outcome %in% outcome_list)) stop("invalid outcome")
   
   # find the best hospital
   mortality_rate <- data[data["State"] == state,c(2,7,11,17,23)]
   colnames(mortality_rate)[3:5] <- c(outcome_list)
   
   mortality_rate_nona <- mortality_rate[!is.na(mortality_rate[outcome]),]
   
   min_pos <- which(mortality_rate_nona[outcome] == min(mortality_rate_nona[outcome]))
   sort(mortality_rate_nona$Hospital.Name[min_pos])
}


