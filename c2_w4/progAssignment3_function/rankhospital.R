rankhospital <- function(state, outcome, num = "best") {
   ## load data
   getwd()
   data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",na.strings = "Not Available")
   
   ## check validity of argument
   state_list <- data$State
   state <- toupper(state)
   if (!(state %in% state_list)) stop("invalid state")
   
   outcome_list <- c("heart attack", "heart failure", "pneumonia")
   outcome <- tolower(outcome)
   if (!(outcome %in% outcome_list)) stop("invalid outcome")
   
   ## Return hospital name in that state with the given rank
   ## 30-day death rate
   mortality_rate <- data[data["State"] == state,c(2,7,11,17,23)]
   colnames(mortality_rate)[3:5] <- c(outcome_list)
   
   # exclude hospitals without data
   mortality_rate_nona <- mortality_rate[!is.na(mortality_rate[outcome]),]
   
   mortality_rate_ordered <- mortality_rate_nona[order(mortality_rate_nona[outcome],
                                                       mortality_rate_nona["Hospital.Name"]),]
   
   if (num == "best") {
      result <- mortality_rate_ordered[1, "Hospital.Name"]
   } else if (num == "worst") {
      result <- tail(mortality_rate_ordered["Hospital.Name"],1)
   } else if (num <= nrow(mortality_rate_nona)) {
      result <- mortality_rate_ordered[num, "Hospital.Name"]
   } else {
      result <- NA
   }
   as.character(result)

}
