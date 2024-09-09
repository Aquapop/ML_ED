# Mode + PMM imputation
#用众数对分类变量进行插补
# mode_function计算分类变量众数
mode_function <- function(x) {
  ux <- na.omit(x)  # 移除NA值
  if(length(ux) == 0) return(NA_character_)  # 如果去除NA后没有数据，返回NA字符
  mode_val <- names(sort(table(ux), decreasing = TRUE))[1]  # 计算并返回众数
  return(mode_val)
}

factorCols <- c("RIDRETH1", "DMDEDUC","DMDMARTL","DMDHHSIZ","SMQ020", "ALQ110","PAD590","PAD320","KIQ400","BPQ040A","BPQ090D","DIQ010","MCQ220","KIQ081","KIQ101","KIQ106",
  "KIQ121","KIQ141","KIQ182","KIQ321","KIQ341","KIQ115","SXQ280","SXQ260","SXQ265","SXQ270","SXQ272","STI","CIDDSCOR","CIDGSCOR","CIDPSCOR","MentalHealth","ProstateExam",
  "CDQ001","CDQ010","CVDFITLV")


#用众数进行插补
test6=read.csv('test6.csv') 

  test7 <- test6 %>%
    mutate(across(all_of(factorCols), ~{
      mode_val <- mode_function(.)  # 对当前列计算众数
      .[is.na(.)] <- mode_val  # 替换NA为众数
      factor(., levels = unique(.))  # 确保结果为因子类型并重新设置水平
    }))  
  
  str(test7)

write.csv(test7,"D:/test7.csv")   
test7=read.csv('test7.csv')  

#pmm法对连续变量进行插补
#pmm多重插补#----------------------
install.packages("stats")
library(mice)

# 定义补充缺失值函数
fix_na <- function(factorCols,data,n) {
  imp=mice(data,m=n,method="pmm", maxit = 5, printFlag = TRUE)
  print(summary(imp)) # 输出mice插补对象的summary信息
  print("插补完成") # 输出插补完成信息
  datas = list()
  for(i in 1:n) datas[[i]] <- complete(imp, action=i)
  print("数据集合并完成") # 输出数据集合并信息
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

# 调用函数补充缺失值，其中参数data是待插补数据，5表示5重插补
dataPMM <- fix_na(factorCols, test8 ,5)
write.csv(dataPMM,"D:/dataPMM.csv")   



#RF插补
library(missRanger)

test6=read.csv('test6.csv') 
#将数据集中分类变量转变为因子类型


# 批量将列设置为因子变量
test6 <- test6 %>%
  mutate(across(all_of(factorCols), as.factor))
str(test6)

#使用missRanger进行插补
dataRF <- missRanger(test6, num.trees = 500, pmm.k = 5, verbose = 1, maxiter = 5)                           

write.csv(dataRF,"dataRF.csv")    
