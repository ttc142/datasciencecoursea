corr <- function(directory, threshold = 100) {
   dir <- getwd()
   # get monitor list in directory
   file_list <- dir(directory)
   
   res <- c()
   
   for (i in seq_len(length(file_list))){

      file_path <- file.path(directory,file_list[i])

      # read data from monitor
      data <- read.csv(file_path)
      
      obs_complete <- length(data$ID[!is.na(data$sulfate) & !is.na(data$nitrate) ])

      if(obs_complete > threshold) {
         corr <- cor(data$nitrate,data$sulfate,use="complete.obs")
         res <- append(res,corr[1])
      }
   }
   res
}