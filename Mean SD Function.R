######### Expression data analysis function #########



###############################################
#Purpose: function returns the #n 
#         genes or samples with highest: 
#         expression & Standard deviation
#Date: 08 June 2022                           #
#Author Name: A.Elbaz                         #
##Session info:                               #
# R version 4.2.0 (2022-04-22 ucrt)           #
# Platform: x86_64-w64-mingw32/x64 (64-bit)   #
# Running under: Windows 10 x64 (build 19043) #            
###############################################
#No used libraries
#variables are defined as local variables inside the function:
#data --> the expression data
#results --> the variable containing the output supposed to be List of 2                             data frames

               
#Read as a table while header is True
               
genes_expression.data = read.table("D:/MAIN/STUDY/PROGRAMMING/R/Course/expData.txt", header= TRUE)
#Make sure the row names are the gene IDs
row.names(genes_expression.data)


#Create the function with required parameters
expr_func = function(data, top = 10, cut_off = 2 , by_gene=TRUE){
#Create empty lists to be filled during the code
gene.names = c()
sample.names = c()
average.values = c()
SD.values = c()
#the if statement to go for looping through genes or samples in line 28
if (by_gene == "TRUE"){
  
#looping through each gene via (for looping)
  for (i in 1:dim(data)[1]){
    gene.names[i] = row.names(data)[i]        #filling the gene.name list with gene IDs
    gene.vector = as.numeric(data[i,])        #sub-setting each row by cut_off value
    gene.vector = gene.vector[gene.vector > cut_off]
    average.values[i]= mean(gene.vector)      #filling the average.values list
    SD.values[i]= sd(gene.vector)             #filling the SD.values list
  }
#Creating a data.frame (column 1 --> gene IDs , column2 --> mean value for each gene across all samples)
  DF.Averages = data.frame(genes = gene.names, mean = average.values)
#order the data.frame in descending order according to the $mean column
  sorted.averages = DF.Averages[order(-DF.Averages$mean),][1:top,]
#Creating a data.frame (column 1 --> gene IDs , column2 --> standard deviation value for each gene across all samples)
  DF.standard_deviation = data.frame(genes = gene.names, standard_deviation = SD.values)
#order the data.frame in descending order according to the $standard_deviation column
  sorted.sd = DF.standard_deviation[order(-DF.standard_deviation$standard_deviation),][1:top,]
#The output of the function is a list consists of ($data.frame --> $gene IDs , $mean values && $another data.frame --> $gene IDs , $SD values)
  outPut = list(DF_Top_Averages = sorted.averages, DF_Top_SD = sorted.sd)
}

else{
  
  for (i in 1:dim(data)[2]){
    sample.names[i] = names(data)[i]
    sample.vector = as.numeric(data[,i])
    sample.vector = sample.vector[sample.vector > cut_off]
    average.values[i]= mean(sample.vector)
    SD.values[i]= sd(sample.vector)
  }
  DF.Averages = data.frame(samples = sample.names, mean = average.values)
  sorted.averages = DF.Averages[order(-DF.Averages$mean),][1:top,]
  DF.standard_deviation = data.frame(samples = sample.names, standard_deviation = SD.values)
  sorted.sd = DF.standard_deviation[order(-DF.standard_deviation$standard_deviation),][1:top,]
  outPut = list(DF_Top_Averages = sorted.averages, DF_Top_SD = sorted.sd)
} 
 return(outPut) 
}



#Calling the function
results = expr_func(genes_expression.data, by_gene = FALSE, top=5, cut_off = 3)
#print the top genes with highest expression mean as Data Frame in console
results$DF_Top_Averages
#print the top genes with highest standard deviation as Data Frame in console
results$DF_Top_SD


          