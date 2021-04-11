pollutantmean <- function(directory, pollutant, id=1:332){
   # set directory path from directory argument
   dir <- getwd()
   # get monitor list in directory
   file_list <- dir(directory)
   # create empty data frame to store sum and length of each monitor
   df <- data.frame(id = id, sum = 0, len = 0)
   
   # for each monitor
   for (i in seq_len(length(id))){
      # construct file path
      file_path <- file.path(directory,file_list[id[i]])
      # read data from monitor
      data <- read.csv(file_path)
      
      # get sum and length of non-NA values of that monitor
      df$len[i] <-  length(data$ID[!is.na(data[pollutant])])
      
      # if all observations are NA, set sum and len = 0
      if (df$len[i] != 0) {
         df$sum[i] <-  sum(data[pollutant],na.rm = TRUE)
      } else {
         df$sum[i] <- 0
      }
   }
   
   # find mean across all monitors
   if (sum(df$len) != 0){
      res <- sum(df$sum)/sum(df$len)
   } else {
      # avoid divise by 0
      res <- 0
   }
   
   res

}