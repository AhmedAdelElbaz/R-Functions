######### Plotting Function #########



###############################################
#Purpose: function plots gene expression
#Date: 08 June 2022                           #
#Author Name: A.Elbaz                         #
##Session info:                               #
# R version 4.2.0 (2022-04-22 ucrt)           #
# Platform: x86_64-w64-mingw32/x64 (64-bit)   #
# Running under: Windows 10 x64 (build 19043) #            
###############################################
#No used libraries


#Reading the expression matrix as a table
expr_matrix = read.table("D:/MAIN/STUDY/PROGRAMMING/R/Course/gene_expression.csv", header = TRUE, sep = ",")
row.names(expr_matrix) = expr_matrix$time        #name rows by column $time
expr_matrix = expr_matrix[2:dim(expr_matrix)[2]] #removing the column $time


##Assignment Part
plot(seq(1,dim(expr_matrix)[2]), expr_matrix[5,], type= "h", col ="red", main = paste(as.character(row.names(expr_matrix)[5]) , "_Expression"), ylab = paste(as.character(row.names(expr_matrix)[5]) , "_Expression"), xlab = "samples " , lwd = 5)


########## Function plotting gene expression ##############
# The first parameter is the data Frame to be plotted 
# Second Parameter is gene number in the data frame or gene ID

expr_gene_plot = function(data , gene, type = "h", color = "red", line_width = 5){

  #the plot function depends on the gene entered data type:
    #if numeric will be extracted by its index from the Data Frame
  if (is.numeric(gene)){ 
  title = paste(as.character(row.names(data)[gene]) , "_Expression")
  #if Character will be extracted by its name 
  }else if (is.character(gene)){
    title = paste(gene, "_Expression")
  }
  plot(1:dim(data)[2], data[gene,], type = type, col = color, main = title, ylab = title, xlab = "Samples" , lwd = line_width)
}

expr_gene_plot(expr_matrix, "YAL022C")

         