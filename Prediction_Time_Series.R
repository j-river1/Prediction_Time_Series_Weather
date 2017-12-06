#PREDICTION FOR SERIES TIMES
library(ggplot2)
library(ts)
#Read files 

files <- lapply(list.files(path = "../TrainingData"), function(x) {
namefile <- x
name <- paste0("../TrainingData/", x)
file <- read.csv(name, header = TRUE)
#file <- file[rep(1:192),]
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

#average_series_time computes the average series time.

average_series_time <- function (data_series, variable)
{

  if(variable == "X1")
  {
    data <- data_series$X1
    for( i in 1:192)
    {
      index <- seq(i, length(data), by =192) 
      average_data[i] <- mean(data[index])
    }
    
  }
  if(variable == "X2")
  {
    data <- data_series$X2
    for( i in 1:192)
    {
      index <- seq(i, length(data), by =192) 
      average_data[i] <- mean(data[index])
    }
    
  }
  if(variable == "X3")
  {
    data <- data_series$X3
    for( i in 1:192)
    {
      index <- seq(i, length(data), by =192) 
      average_data[i] <- mean(data[index])
    }
    
  }
  if(variable == "X4")
  {
    data <- data_series$X4
    for( i in 1:192)
    {
      index <- seq(i, length(data), by =192) 
      average_data[i] <- mean(data[index])
    }
    
  }
  if(variable == "X5")
  {
    data <- data_series$X5
    for( i in 1:192)
    {
      index <- seq(i, length(data), by =192) 
      average_data[i] <- mean(data[index])
    }
    
  }
  
  if(variable == "X6")
  {
    data <- data_series$X6
    for( i in 1:192)
    {
      index <- seq(i, length(data), by =192) 
      average_data[i] <- mean(data[index])
    }
    
  }
  
  
  
  return(average_data)
}



join_series <- function(data_series)
{
  variabales_names <- c("X1","X2","X3","X4","X5","X6")
  tabla_resume <- data.frame(matrix(NA, nrow = 192, ncol = 6))
  colnames(tabla_resume) <- variabales_names
  
  tabla_resume$X1 <- average_series_time (data_series, "X1")
  tabla_resume$X2 <- average_series_time (data_series, "X2")
  tabla_resume$X3 <- average_series_time (data_series, "X3")
  tabla_resume$X4 <- average_series_time (data_series, "X4")
  tabla_resume$X5 <- average_series_time (data_series, "X5")
  tabla_resume$X6 <- average_series_time (data_series, "X6")
  Months <- c("Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec")
  tabla_resume$Month <- rep(Months,16) 

  return(tabla_resume)

}







#Graph_series plots series
#Arguments    - table. Series Time
#Return       - Plot         

graph_series <- function(table, variable, station_name, model, periods)
{
    nrows <- nrow(table) + periods
    ncol <- 2
    table_ori <- data.frame(matrix(NA, nrow = nrows, ncol = 2))
    table_model <- data.frame(matrix(NA, nrow = nrows, ncol = 2))
    colnames(table_ori) <- c("Time", "Value") 
    colnames(table_model) <- c("Time", "Value")
    table_ori$Time <- seq(1:nrows)
    table_model$Time <- seq(1:nrows)
  
  if(variable == "X1")
  {
    table_ori$Value[1:nrow(table)]  <- table$X1
    table_model$Value[(nrow(table) + 1):(nrow(table)+ periods)] <- model

    table_ori$group <- "X1"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))
    
    
  }
  if(variable == "X2")
  {
    table_ori$Value[1:nrow(table)]  <- table$X2
    table_model$Value[(nrow(table) + 1):(nrow(table)+ periods)] <- model
    
    table_ori$group <- "X2"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))
    
  }
  if(variable == "X3")
  {
    table_ori$Value[1:nrow(table)]  <- table$X3
    table_model$Value[(nrow(table) + 1):(nrow(table)+ periods)] <- model
    
    table_ori$group <- "X3"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))
    
  }
  if(variable == "X4")
  {
    table_ori$Value[1:nrow(table)]  <- table$X4
    table_model$Value[(nrow(table) + 1):(nrow(table)+ periods)] <- model
    
    table_ori$group <- "X4"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))
    
    
  }
  if(variable == "X5")
  {
    table_ori$Value[1:nrow(table)]  <- table$X5
    table_model$Value[(nrow(table) + 1):(nrow(table)+ periods)] <- model
    
    table_ori$group <- "X5"
    table_model$group <- "Model"
    table_all <- rbind(table_ori,table_model )
    graph <- ggplot(table_all, aes(x=Time, y= Value, group= group, col=group)) + geom_line() + ggtitle(paste(station_name, "\n", variable)) + theme(plot.title = element_text(hjust = 0.5))
    
  }
  if(variable == "X6")
  {
    table_ori$Value[1:nrow(table)]  <- table$X6
    table_model$Value[(nrow(table) + 1):(nrow(table)+ periods)] <- model
    
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
  table <- join_series(table)

  if(variable == "X1")
  {
    series <- ts(table$X1, frequency = 12)
    holtWinter <- HoltWinters(series)
    predict <- predict(holtWinter, n.ahead= periods)
    value_regre <- as.numeric(predict)
    graph <- graph_series(table, variable, station_name, value_regre, periods)
    print(graph)
    
    #rsquared
    actual <- table$X1[(length(table$X1)-periods+1):length(table$X1)]
    #R2 <- 1 - (sum((actual-value_regre)^2)/sum((actual-mean(actual))^2))
    RMSE(value_regre,obs = actual)
    R2<- R2(value_regre,obs = actual)
  }
  if(variable == "X2")
  {
    series <- ts(table$X2, frequency = periods)
    holtWinter <- HoltWinters(series)
    predict <- predict(holtWinter, n.ahead= periods)
    value_regre <- as.numeric(predict)
    graph <- graph_series(table, variable, station_name, value_regre, periods)
    print(graph)
    
    #rsquared
    actual <- table$X1[(length(table$X1)-periods+1):length(table$X1)]
    #R2 <- 1 - (sum((actual-value_regre)^2)/sum((actual-mean(actual))^2))
    RMSE(value_regre,obs = actual)
    R2<- R2(value_regre,obs = actual)
    
  }
  if(variable == "X3")
  {
    series <- ts(table$X3, frequency = periods)
    holtWinter <- HoltWinters(series)
    predict <- predict(holtWinter, n.ahead= periods)
    value_regre <- as.numeric(predict)
    graph <- graph_series(table, variable, station_name, value_regre, periods)
    print(graph)
    
    actual <- table$X1[(length(table$X1)-periods+1):length(table$X1)]
    #R2 <- 1 - (sum((actual-value_regre)^2)/sum((actual-mean(actual))^2))
    RMSE(value_regre,obs = actual)
    R2 <- R2(value_regre,obs = actual)
    
  }
  if(variable == "X4")
  {
    series <- ts(table$X4, frequency = periods)
    holtWinter <- HoltWinters(series)
    predict <- predict(holtWinter, n.ahead= periods)
    value_regre <- as.numeric(predict)
    graph <- graph_series(table, variable, station_name, value_regre, periods)
    print(graph)
    
    actual <- table$X1[(length(table$X1)-periods+1):length(table$X1)]
    #R2 <- 1 - (sum((actual-value_regre)^2)/sum((actual-mean(actual))^2))
    RMSE(value_regre,obs = actual)
    R2 <- R2(value_regre,obs = actual)
    
  }
  if(variable == "X5")
  {
    series <- ts(table$X5, frequency = periods)
    holtWinter <- HoltWinters(series)
    predict <- predict(holtWinter, n.ahead= periods)
    value_regre <- as.numeric(predict)
    graph <- graph_series(table, variable, station_name, value_regre, periods)
    print(graph)
    
    actual <- table$X1[(length(table$X1)-periods+1):length(table$X1)]
    #R2 <- 1 - (sum((actual-value_regre)^2)/sum((actual-mean(actual))^2))
    RMSE(value_regre,obs = actual)
    R2 <- R2(value_regre,obs = actual)
    
  }
  if(variable == "X6")
  {
    series <- ts(table$X6, frequency = periods)
    holtWinter <- HoltWinters(series)
    predict <- predict(holtWinter, n.ahead= periods)
    value_regre <- as.numeric(predict)
    graph <- graph_series(table, variable, station_name, value_regre, periods)
    print(graph)
    
    actual <- table$X1[(length(table$X1)-periods+1):length(table$X1)]
    #R2 <- 1 - (sum((actual-value_regre)^2)/sum((actual-mean(actual))^2))
    RMSE(value_regre,obs = actual)
    R2 <- R2(value_regre,obs = actual)
    
  }
  
  return(R2)
}

rsquared <- function (table)
{
  
  
  
  
}
  
