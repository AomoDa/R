

# 关于批量读取文件的部分写的不错，可以以后参考下~



setwd("D:\\")


# Part 1

pollutantmean <- function(directory, pollutant = "sulfate", id = 1:332) {
    if(grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }
    mean_vector <- c()
    all_files <- as.character( list.files(directory) )
    file_paths <- paste(directory, all_files, sep="")
    for(i in id) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        head(current_file)
        pollutant
        na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
        mean_vector <- c(mean_vector, na_removed)
    }
    result <- mean(mean_vector)
    return(round(result, 3)) 
}

# 答案

pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate") 

#---------------------------------------------------------------------------------------
# part2
complete <- function(directory, id = 1:332) {
    if(grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }
    id_len <- length(id)
    complete_data <- rep(0, id_len)
    all_files <- as.character( list.files(directory) )
    file_paths <- paste(directory, all_files, sep="")
    j <- 1 
    for (i in id) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        complete_data[j] <- sum(complete.cases(current_file))
        j <- j + 1
    }
    result <- data.frame(id = id, nobs = complete_data)
    return(result)
} 

# 答案


complete("specdata", c(6,10,20,34,100,200,310))
complete("specdata", 54)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

#---------------------------------------------------------------------------------------


# part3
corr <- function(directory, threshold = 0) {  
    if(grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }
    complete_table <- complete("specdata", 1:332)
    nobs <- complete_table$nobs
    ids <- complete_table$id[nobs > threshold]
    id_len <- length(ids)
    corr_vector <- rep(0, id_len)
    all_files <- as.character( list.files(directory) )
    file_paths <- paste(directory, all_files, sep="")
    j <- 1
    for(i in ids) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
        j <- j + 1
    }
    result <- corr_vector
    return(result)   
}


# 答案
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)



cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
