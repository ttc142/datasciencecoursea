rankall <- function(outcome, num = "best") {
   ## load data
   getwd()
   data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",na.strings = "Not Available")
   
   ## check validity of argument
   outcome_list <- c("heart attack", "heart failure", "pneumonia")
   outcome <- tolower(outcome)
   if (!(outcome %in% outcome_list)) stop("invalid outcome")
   
   ## find the best hospital in each state
   
   # get mortality_rate and eliminate NA values
   mortality_rate <- data[,c(2,7,11,17,23)]
   colnames(mortality_rate)[3:5] <- c(outcome_list)
   
   mortality_rate_nona <- mortality_rate[!is.na(mortality_rate[outcome]),]
   
   # create result dataframe with all states
   state_list <- unique(sort(data$State))
   result_df <- data.frame("hospital"="","state"=state_list)

   # loop through all state
   for (state in state_list) {
      
      # extract data of each state
      mortality_state <- mortality_rate_nona[mortality_rate_nona["State"]==state,]
      
      # order the outcome of each state
      mortality_rate_ordered <- mortality_state[order(mortality_state[outcome],
                                                      mortality_state["Hospital.Name"]),]
      # 
      if (num == "best") {
         result <- mortality_rate_ordered[1, "Hospital.Name"]
      } else if (num == "worst") {
         result <- result <- tail(mortality_rate_ordered["Hospital.Name"],1)
      } else if (num <= nrow(mortality_rate_nona)) {
         result <- mortality_rate_ordered[num, "Hospital.Name"]
      } else {
         result <- NA
      }
      result_df[result_df["state"] == state, "hospital"] <- result
   }
   
   result_df
}
