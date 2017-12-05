#PREDICTION FOR SERIES TIMES

#Read files 

files <- lapply(list.files(path = "../TrainingData"), function(x) {
namefile <- x
name <- paste0("../TrainingData/", x)
file <- read.csv(name, header = TRUE)
return (file)})

  
#Put names to file
#Argument. -names. File name. eg. tread.csv
#return    -names. File name tread
name_files <- function (names)
{
  names <- gsub(".csv", "", names)
  return (names)
}

#Put the names list files
names(files) <- name_files(list.files(path = "../TrainingData"))

