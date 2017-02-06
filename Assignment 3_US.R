#The function to get close price
getcloseprice<-function(q, i, p, f='c')
{
  #Read the url with the function's inputs
  url<-sprintf('http://www.google.com/finance/getprices?q=%s&i=%d&p=%dd&f=%s',q,i,p,f)
  #Read the database in the url using the function "read.table", storing into the variable "data"
  #The elements of the variable data is in format "factor"
  
  data<-read.csv(url)
  
  data = data[-c(1:7),]
  
  data = as.data.frame.numeric(as.double(as.character(data)))
  names(data) = q
  
  return(data)}

mutistock <- function(path) {
  lists = read.csv(path,header = T)
  data=list()
  
  for (i in seq(dim(lists)[1])) {
    data = append(data,getcloseprice(lists[i,1],lists[i,2],lists[i,3]))
  }
  
  len = c()
  
  for (i in seq(length(data))) {
    len = append(len, length(data[[i]]))
  }
  
  maxi = max(len)
  
  for (i in seq(length(data))) {
    data[[i]] <- c(data[[i]],rep(NA,maxi-length(data[[i]])))
  }
    
  data = as.data.frame(data)  
  
  write.csv(data,'result.csv')
  
  
}  