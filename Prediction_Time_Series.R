#PREDICTION FOR SERIES TIMES

#Read files 

files <- lapply(list.files(pattern ="\\.csv$"), function(x) {read.csv(x, header = TRUE)})
names(files) 