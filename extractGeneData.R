extractGeneData <- function(Query_object, Gene.list = genes$gene_id , columns.to.check = names(clinical)){
  library(dplyr)
#Add all clinical data to clinical variable
 clinical <- SummarizedExperiment::colData(Query_Object)
#Convert to a dataframe
 clinical <- as.data.frame(Query_Object@colData)
#Show user what columns could be chosen to be pasted to expression matrix
 print(columns.to.check)
#Extract the gene expression matrix
 gene_expression.matrix <- SummarizedExperiment::assay(Query_Object)
#Convert to a data frame
 expression_df <- as.data.frame(gene_expression.matrix)
#Add the Ensemble ID to be a column instead of just being row,names
 expression_df <-dplyr::mutate(expression_df , "Ensemble ID" = row.names(expression_df) )
#makeing tidy form of the data for further converting the genes as columns for selection
 tidy_gene_expression <-dplyr::tbl_df(expression_df)%>%tidyr::gather(sample, expression , -"Ensemble ID")%>%tidyr::spread(`Ensemble ID`,expression)%>%select(sample , Gene.list)
#Subset the clinical data with just required columns
 subsetted_clinical =dplyr::tbl_df(clinical)%>%dplyr::select("barcode" , columns.to.check)
genes <- SummarizedExperiment::rowData(Query_Object)
#####################################
integrate_files <- function(to_dataframe  ,
                            from_dataframe ,
                            key_to ,
                            key_from ,
                            columns.to.check=key_from,
                            empty.values = "NULL")
{
  library(hash)
  to_dataframe <- as.data.frame(to_dataframe)
  from_dataframe <- as.data.frame(from_dataframe)
  for (each.column in columns.to.check)
  {
    to_dataframe[each.column] = empty.values
  }
  
  #Create a dictionary of the dataframe we wanna copy values from
  dict_from = hash()
  for (i in 1:nrow(from_dataframe)) #i = each row
  {
    #list_of_values = list()
    list_of_values = hash()
    for (each.column in columns.to.check)
    {
      list_of_values[[each.column]] = from_dataframe[i,each.column]
    }
    
    dict_from[[from_dataframe[i,key_from]]] = list_of_values
  }
  
  for (i in 1:nrow(to_dataframe))
  {
    key = to_dataframe[i,key_to]
    for (each.column in columns.to.check)
    {
      to_dataframe[i,each.column] = dict_from[[key]][[each.column]]
    }
  }
  return(to_dataframe)
}

#########################################
#USing Cbind instead of the integration function is too slow
 #total <- cbind(tidy_gene_expression , subsetted_clinical)%>%dplyr::select(sample , Gene.list, columns.to.check[which(columns.to.check != "sample")])
#Using integration files function to paste the required column to the gene expression data frame
total <- integrate_files(to_dataframe =  tidy_gene_expression , 
                        from_dataframe =  subsetted_clinical , 
                        key_to =  "sample" , 
                        key_from =  "barcode" , 
                        columns.to.check =  columns.to.check)
return(total)
}

#First Read the RDS file
filepath = "D:/"
Object_file_name = "TARGET-CCSK_Object.RDS"
#Query_Object <- readRDS(file = "D:/TCGA-LIHC_Object.RDS")
Query_Object <- readRDS(file = stringr::str_glue("{filepath}/{Object_file_name}"))



#Results <- extractGeneData(Query_Object ,Gene.list =  c("ENSG00000067113.17" , "ENSG00000066926.12"))
#Results <- extractGeneData(Query_Object, columns.to.check = "vital_status")
#Results <- extractGeneData(Query_Object , Gene.list =  c("ENSG00000067113.17" , "ENSG00000066926.12") , columns.to.check = c("patient" , "vital_status" , "days_to_death" , "days_to_last_follow_up" , "gender" , "tumor_stage"))
Results <- extractGeneData(Query_Object , Gene.list =  c("ENSG00000067113.17" , "ENSG00000066926.12"), columns.to.check = c("patient" , "vital_status" , "days_to_death" , "days_to_last_follow_up" , "gender" , "tumor_grade"))


write.csv(Results , file = "D:/results1.csv")

debug(extractGeneData)

# unique(genes$gene_type)
# unique(genes$source)
# unique(genes$type)
# unique(genes$score)
# unique(genes$phase)
# unique(genes$gene_id)
# unique(genes$gene_name)
# unique(genes$level)
# unique(genes$hgnc_id)
# unique(genes$havana_gene)
# Gene.list <- tbl_df(data.frame(gene_name = genes$gene_name , gene_id = genes$gene_id))%>%tidyr::spread(gene_id , gene_name)%>%select(Genes.list)