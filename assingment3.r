
R version 4.0.0 (2020-04-24) -- "Arbor Day"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> getwd()
[1] "C:/Users/DELL/Desktop/rose"
> dir()
[1] "hospital-data.csv"              "Hospital_Revised_Flatfiles.pdf"
[3] "outcome-of-care-measures.csv"  
> best<-function(state, outcome){
+   data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
+   fd   <- as.data.frame(cbind(data[, 2],   
+                               data[, 7],   
+                               data[, 11],  
+                               data[, 17],  
+                               data[, 23]), 
+                         stringsAsFactors = FALSE)
+   colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
+   
+   
+   if(!state %in% fd[, "state"]){
+     stop('invalid state')
+   } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
+     stop('invalid outcome')
+   } else {
+     si <- which(fd[, "state"] == state)
+     ts <- fd[si, ]    
+     oi <- as.numeric(ts[, eval(outcome)])
+     min_val <- min(oi, na.rm = TRUE)
+     result  <- ts[, "hospital"][which(oi == min_val)]
+     output  <- result[order(result)]
+   }
+   return(output)
+ }
> best("SC", "heart attack")
[1] "MUSC MEDICAL CENTER"
Warning message:
In best("SC", "heart attack") : NAs introduced by coercion
> best("NY", "pneumonia")
[1] "MAIMONIDES MEDICAL CENTER"
Warning message:
In best("NY", "pneumonia") : NAs introduced by coercion
> best("AK", "pneumonia")
[1] "YUKON KUSKOKWIM DELTA REG HOSPITAL"
Warning message:
In best("AK", "pneumonia") : NAs introduced by coercion
> rankhospital <- function(state, outcome, rank = "best"){
+   
+   data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
+   fd   <- as.data.frame(cbind(data[, 2],  
+                               data[, 7],  
+                               data[, 11], 
+                               data[, 17],  
+                               data[, 23]), 
+                         stringsAsFactors = FALSE)
+   colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
+   
+   
+   if (!state %in% fd[, "state"]) {
+     stop('invalid state')
+   } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
+     stop('invalid outcome')
+   } else if (is.numeric(rank)) {
+     si <- which(fd[, "state"] == state)
+     ts <- fd[si, ]                     
+     ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
+     ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
+     output <- ts[, "hospital"][rank]
+   } else if (!is.numeric(rank)){
+     if (rank == "best") {
+       output <- best(state, outcome)
+     } else if (rank == "worst") {
+       si <- which(fd[, "state"] == state)
+       ts <- fd[si, ]    
+       ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
+       ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"], decreasing = TRUE), ]
+       output <- ts[, "hospital"][1]
+     } else {
+       stop('invalid rank')
+     }
+   }
+   return(output)
+ }
> rankhosital("NC" ,"heart attack", "worst")
Error in rankhosital("NC", "heart attack", "worst") : 
  could not find function "rankhosital"
> rankhospita("NC", "heart attack", "worst")
Error in rankhospita("NC", "heart attack", "worst") : 
  could not find function "rankhospita"
> rankhospital("NC" ,"heart attack" ,"worst")
[1] "WAYNE MEMORIAL HOSPITAL"
Warning message:
In rankhospital("NC", "heart attack", "worst") : NAs introduced by coercion
> rankhospital("WA" , "heart attack" , 7)
[1] "YAKIMA VALLEY MEMORIAL HOSPITAL"
Warning message:
In rankhospital("WA", "heart attack", 7) : NAs introduced by coercion
> rankhospital("TX" , "pneumonia", 10)
[1] "SETON SMITHVILLE REGIONAL HOSPITAL"
Warning message:
In rankhospital("TX", "pneumonia", 10) : NAs introduced by coercion
> rankhospital("NY","heart attack", 7)
[1] "BELLEVUE HOSPITAL CENTER"
Warning message:
In rankhospital("NY", "heart attack", 7) : NAs introduced by coercion
>   rankall <- function(outcome, num = "best"){
+   
+   data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
+   fd   <- as.data.frame(cbind(data[, 2],  
+                               data[, 7],  
+                               data[, 11],  
+                               data[, 17],  
+                               data[, 23]), 
+                         stringsAsFactors = FALSE)
+   colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
+   fd[, eval(outcome)] <- as.numeric(fd[, eval(outcome)])
+   
+   
+   
+   if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
+     stop('invalid outcome')
+   } else if (is.numeric(num)) {
+     by_state <- with(fd, split(fd, state))
+     ordered  <- list()
+     for (i in seq_along(by_state)){
+       by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
+                                            by_state[[i]][, "hospital"]), ]
+       ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
+     }
+     result <- do.call(rbind, ordered)
+     output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
+     names(output) <- c("hospital", "state")
+   } else if (!is.numeric(num)) {
+     if (num == "best") {
+       by_state <- with(fd, split(fd, state))
+       ordered  <- list()
+       for (i in seq_along(by_state)){
+         by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
+                                              by_state[[i]][, "hospital"]), ]
+         ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
+       }
+       result <- do.call(rbind, ordered)
+       output <- as.data.frame(result, stringsAsFactors = FALSE)
+       rownames(output) <- output[, 2]
+     } else if (num == "worst") {
+       by_state <- with(fd, split(fd, state))
+       ordered  <- list()
+       for (i in seq_along(by_state)){
+         by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
+                                              by_state[[i]][, "hospital"], 
+                                              decreasing = TRUE), ]
+         ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
+       }
+       result <- do.call(rbind, ordered)
+       output <- as.data.frame(result, stringsAsFactors = FALSE)
+       rownames(output) <- output[, 2]
+     } else {
+       stop('invalid num')
+     }
+   }
+   return(output)
+ }
> r<- rankall("hear attack",4)as.character(subset(r , state=="HI")$hospital)
Error: unexpected symbol in "r<- rankall("hear attack",4)as.character"
> r <- rankall("heart attack", 4)
Warning message:
In rankall("heart attack", 4) : NAs introduced by coercion
> r <- rankall(" heart attack",4 )
Error in `[.data.frame`(fd, , eval(outcome)) : undefined columns selected
> r <- rankall("heart attack" 4) as.character(subset( r, state== "HI")$hospital)
Error: unexpected numeric constant in "r <- rankall("heart attack" 4"
> 
