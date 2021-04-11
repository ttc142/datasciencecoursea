complete <- function(directory, id = 1:322){
   # set directory path from directory argument
   dir <- getwd()
   # get monitor list in directory
   file_list <- dir(directory)
   # create empty data frame to store sum and length of each monitor
   df <- data.frame(id = id, nobs = 0)
   
   # for each monitor
   for (i in seq_len(length(id))){
      # construct file path
      file_path <- file.path(directory,file_list[id[i]])
      
      # read data from monitor
      data <- read.csv(file_path)
      
      # get number of complete observation (non-NA in both columns)
      df$nobs[i] <- length(data$ID[!is.na(data$sulfate) & !is.na(data$nitrate) ])
   }
   
   df
}