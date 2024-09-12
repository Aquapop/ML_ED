# Mode + PMM imputation
# Impute categorical variables using the mode
# mode_function computes the mode for categorical variables
mode_function <- function(x) {
}

factorCols <- c("RIDRETH1", "DMDEDUC","DMDMARTL","DMDHHSIZ","SMQ020", "ALQ110","PAD590","PAD320","KIQ400","BPQ040A","BPQ090D","DIQ010","MCQ220","KIQ081","KIQ101","KIQ106",
  "KIQ121","KIQ141","KIQ182","KIQ321","KIQ341","KIQ115","SXQ280","SXQ260","SXQ265","SXQ270","SXQ272","STI","CIDDSCOR","CIDGSCOR","CIDPSCOR","MentalHealth","ProstateExam",
  "CDQ001","CDQ010","CVDFITLV")

# Impute using mode
test6=read.csv('test6.csv') 

  test7 <- test6 %>%
    mutate(across(all_of(factorCols), ~{
      mode_val <- mode_function(.)  # Compute mode for the current column
      .[is.na(.)] <- mode_val  # Replace NA with mode
      factor(., levels = unique(.))  # Ensure the result is a factor type and reset levels
    }))  
  
  str(test7)

write.csv(test7,"D:/test7.csv")   
test7=read.csv('test7.csv')  

# PMM imputation for continuous variables
# Multiple imputation using PMM
install.packages("stats")
library(mice)

# Define a function to impute missing values
fix_na <- function(factorCols,data,n) {
  imp=mice(data,m=n,method="pmm", maxit = 5, printFlag = TRUE)
  print(summary(imp)) # Print summary information of the mice imputation object
  print("Imputation completed") # Print imputation completion message
  datas = list()
  for(i in 1:n) datas[[i]] <- complete(imp, action=i)
  print("Datasets merged") # Print datasets merged message
  colnames <- c()
  numeric_cols <- c()
  result <- 1 : length(datas[[1]][,1])
  allCols <- colnames(datas[[1]])
  for(colname in allCols) {
    temp <- 1 : length(datas[[1]][,1])
    for(data in datas) temp <- cbind(temp,data[[colname]])
    temp <- temp[,-1]
    if(colname %in% factorCols) {
      mode_value <- apply(temp,1,getmode)
      result <- cbind(result,mode_value)
    }else{
      mean_value <- apply(temp,1,mean)
      result <- cbind(result,mean_value)
      numeric_cols <- c(numeric_cols, colname)
    }
    colnames <- c(colnames,colname)
  }
  result <- as.data.frame(result[,-1])
  names(result) <- colnames
  for(i in factorCols) result[[i]] = as.factor(result[[i]])
  for(i in numeric_cols) result[[i]] = as.numeric(result[[i]])
  return (result)
}

# Call the function to impute missing values, where data is the data to be imputed, 5 for 5-fold imputation
dataPMM <- fix_na(factorCols, test8 ,5)
write.csv(dataPMM,"D:/dataPMM.csv")   


# RF imputation
library(missRanger)

test6=read.csv('test6.csv') 
# Convert categorical variables in the dataset to factor type


# Convert columns to factor variables in batch
test6 <- test6 %>%
  mutate(across(all_of(factorCols), as.factor))
str(test6)

# Use missRanger for imputation
dataRF <- missRanger(test6, num.trees = 500, pmm.k = 5, verbose = 1, maxiter = 5)                           

write.csv(dataRF,"dataRF.csv")    
