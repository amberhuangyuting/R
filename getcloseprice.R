#The function to get close price
getcloseprice<-function(q, i, p, f)
{
#Read the url with the function's inputs
  url<-sprintf('http://www.google.com/finance/getprices?q=%s&i=%d&p=%s&f=%s',q,i,p,f)
#Read the database in the url using the function "read.table", storing into the variable "data"
#The elements of the variable data is in format "factor"
  data<-read.table(url)
#Transform the element's format in variable "data" into character, transform the variable "data" into data frame structue, keep each elements as character vector, instead of factor
  data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
#Delete the first seven rows, keep the pure data in the variable "data"
  data<-data[-c(1:7),]
#split the elements by comma, and make the result in character vector format, instead of just a list
  data<-unlist(strsplit(data,','))
# If input parameter 'f' equals 'c', there is only one column in the variable "data", and the close price is just the variable "data"
 if(f=='c')
 {
   closeprice<-data
 }
#If input parameter 'f' equals 'd,o,h,l,c,v', we transform the character vector into matrix
#The column length of our ultimate matrix is solid as the length of 'f' as six, the row length is the length of the character vector 'data' divided by six
 else if(f=='d,o,h,l,c,v')
{
#Since the function 'matrix' transforms the vector into matrix column by column, we have to get the matrix whose row length is six first, then transpose it
  data<-matrix(data,nrow=6,ncol=length(data)/6)
  data<-t(data)
#The close price is the second column of the matrix
  closeprice<-data[,2]
 }
#Output the close price vector into an csv file using the "write.table" function
 name<-sprintf('output_q=%s&i=%d&p=%s&f=%s.csv',q,i,p,f)
 write.table(closeprice,file=name,row.names = FALSE,col.names = FALSE)
}
