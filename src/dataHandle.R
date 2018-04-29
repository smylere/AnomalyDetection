#library(proxy)

# data Preprocessing process
dataHandle = function(data){
    
    result = vector()
    noticeNumber = sort(unique(data[,1]))
    
    minmax = function(data){(data-min(data))/(max(data)-min(data))}
    
    for (i in 1:length(noticeNumber)) {
        
        # PRIMARYKEY : table PK 
        tmpData = subset(data,PRIMARYKEY == noticeNumber[,1])
        tmpDataLength = as.integer(length(tmpData[,1]))
        
        if((tmpDataLength<11)){
            
            #TARGET01 VALUE : SD-CV(표준편차 변동계수 계산)
            sdValue = (sd(tmpData[,5]) / (mean(tmpData[,5])))
            
            #TARGET02 VALUE : SD-CV(표준편차 변동계수 계산)
            tmpValue = tmpData[,4] - tmpValue[,5]
            tmpDifference = (sd(tmpValue) / mean(tmpValue))
            
            #TARGET03 VALUE : SD-CV(표준편차 변동계수 계산)
            tmpBidRate = (sd(tmpData[,6]) / (mean(tmpData[,6])))
            
            #TARGET03 VALUE : 평균계산
            tmpAvg = mean(tmpData[,6])
            
            #TARGET03 VALUE : 최대값 계산
            tmpMax = max(tmpData[,6])
            
            #TARGET03 VALUE : 최소값 계산
            tmpMin = min(tmpData[,6])
            
            #TARGET04 VALUE : 퍼센티지 계산
            checkIdx = which(tmpData$bid =="Y")
            tmpCostDifference = (((tmpData[checkIdx,3] - tmpData[checkIdx,5]) / tmpData[checkIdx,3]))
            
            #TARGET05 VALUE : SD-CV(표준편차 변동계수 계산)
            #tmpProposal = (sd(tmpData[,8]) / (mean(tmpData[,8])))
            
            tmpResult = c(as.character(tmpData[1,1]),
                          tmpDataLength,
                          sdValue,
                          tmpDifference,
                          tmpBidRate,
                          tmpAvg,
                          tmpMax,
                          tmpMin,
                          tmpCostDifference)
            
            print(tmpResult)
            result = append(result,tmpResult)
        }
    }
    
    processing = matrix(result,ncol=10,byrow=TRUE)
    dataResult = data.frame(processing,stringsAsFactors = FALSE)
    name(dataResult) =c("col1",
                        "col2",
                        "col3",
                        "col4",
                        "col5",
                        "col6",
                        "col7",
                        "col8",
                        "col9",
                        "col10")
    
    dataResult[,2] = as.integer(dataResult[,2])
    dataResult[,3] = as.numeric(dataResult[,3])
    dataResult[,4] = as.numeric(dataResult[,4])
    dataResult[,5] = as.numeric(dataResult[,5])
    dataResult[,6] = as.numeric(dataResult[,6])
    dataResult[,7] = as.numeric(dataResult[,7])
    dataResult[,8] = as.numeric(dataResult[,8])
    dataResult[,9] = as.numeric(dataResult[,9])
    
    dataResult[is.na(dataResult)] = 0
    
    
    return(dataResult)
}
