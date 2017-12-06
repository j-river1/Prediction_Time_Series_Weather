#PREDICTION FOR SERIES TIMES
library(ggplot2)
library(ts)
#Read files 

files <- lapply(list.files(path = "../TrainingData"), function(x) {
namefile <- x
name <- paste0("../TrainingData/", x)
file <- read.csv(name, header = TRUE)
file <- file[rep(1:192),]
Months <- c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")
file$Month <- rep(Months,16) 
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

#Graph_series plots series
#Arguments    - table. Series Time
#Return       - Plot         

graph_series <- function(table, variable, station_name, model, periods)
{
    nrows <- 192 + periods
    ncol <- 2
    table_ori <- data.frame(matrix(NA, nrow = nrows, ncol = 2))
    table_model <- data.frame(matrix(NA, nrow = nrows, ncol = 2))
    colnames(table_ori) <- c("Time", "Value") 
    colnames(table_model) <- c("Time", "Value")
  
  if(variable == "X1")
  {
    table_ori$Time <- table$times
    table_ori$Value  <- table$X1
    
    table_model$Time <- table$times
    table_model$Value[193:nrows] <- model
    
    table_ori$group <- "X1"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))
    
    
  }
  if(variable == "X2")
  {
    table_ori <- data.frame(table$times,table$X2)
    table_model <- data.frame(table$times, model)
    colnames(table_ori) <- c("Time", "Value") 
    colnames(table_model) <- c("Time", "Value")
    table_ori$group <- "X2"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))

  }
  if(variable == "X3")
  {
    table_ori <- data.frame(table$times,table$X3)
    table_model <- data.frame(table$times, model)
    colnames(table_ori) <- c("Time", "Value") 
    colnames(table_model) <- c("Time", "Value")
    table_ori$group <- "X3"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))
    
  }
  if(variable == "X4")
  {
    table_ori <- data.frame(table$times,table$X4)
    table_model <- data.frame(table$times, model)
    colnames(table_ori) <- c("Time", "Value") 
    colnames(table_model) <- c("Time", "Value")
    table_ori$group <- "X4"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))
    
  }
  if(variable == "X5")
  {
    table_ori <- data.frame(table$times,table$X5)
    table_model <- data.frame(table$times, model)
    colnames(table_ori) <- c("Time", "Value") 
    colnames(table_model) <- c("Time", "Value")
    table_ori$group <- "X5"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))
    
  }
  if(variable == "X6")
  {
    table_ori <- data.frame(table$times,table$X6)
    table_model <- data.frame(table$times, model)
    colnames(table_ori) <- c("Time", "Value") 
    colnames(table_model) <- c("Time", "Value")
    table_ori$group <- "X6"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  return(graph)
  
}

regression_analysis <- function(table, variable, station_name)
{
  t<-seq(1:192)
  sin.t<-sin(2*pi*t)
  cos.t<-cos(2*pi*t)
  
  if(variable == "X1")
  {
    data <- table$X1
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  if(variable == "X2")
  {
    data <- table$X2
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  if(variable == "X3")
  {
    data <- table$X3
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  if(variable == "X4")
  {
    data <- table$X4
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  if(variable == "X5")
  {
    data <- table$X5
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  if(variable == "X6")
  {
    data <- table$X6
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  

  return(graph)

}

exponential_smooth <- function (table, variable, station_name, periods)
{

  if(variable == "X1")
  {
    series <- ts(table$X1, frequency = 12)
    holtWinter <- HoltWinters(series)
    predict <- predict(holtWinter, n.ahead= periods)
    value_regre <- as.numeric(predict)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  if(variable == "X2")
  {
    data <- table$X2
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  if(variable == "X3")
  {
    data <- table$X3
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  if(variable == "X4")
  {
    data <- table$X4
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  if(variable == "X5")
  {
    data <- table$X5
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  if(variable == "X6")
  {
    data <- table$X6
    regre <- lm(data~t+sin.t+cos.t)
    value_regre <- as.numeric(regre$fit)
    graph <- graph_series(table, variable, station_name, value_regre)
    print(graph)
  }
  
}
  
