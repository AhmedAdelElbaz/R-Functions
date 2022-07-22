createTCGAobject <- function(project_id , selected.data.category,filepath = getwd()){
  #Create A Query includes the Files With only project ID and Selected Data Category
  query_TCGA = TCGAbiolinks::GDCquery(project = project_id,
                                      data.category = selected.data.category)
  #get the first output from the above code with this function
  Query_Results <- TCGAbiolinks::getResults(query_TCGA)
  dim(Query_Results)
  #############Project Chosen#########
  #Pick the right Sample Type####
  table(Query_Results$sample_type)
  print(unique(Query_Results$sample_type))
  
  if (length(unique(Query_Results$sample_type)) == 1){
    query_TCGA = query_TCGA
    sample.type = as.vector(unique(Query_Results$sample_type))
  }else{
    print("Choose Sample type")
    sample.type = scan(what = character())
    while (!sample.type %in% as.vector(unique(Query_Results$sample_type)) && sample.type != "all"){
      print("ERROR")
      print("Choose Sample type")
      sample.type = scan(what = character())
    }
    if (unique(sample.type == "all")== TRUE){
      query_TCGA = query_TCGA
      sample.type = as.vector(unique(Query_Results$sample_type))
    }else{
      query_TCGA = TCGAbiolinks::GDCquery(project = project_id,
                            data.category = selected.data.category,
                            sample.type = sample.type)
    } 
  }
  Query_Results <- TCGAbiolinks::getResults(query_TCGA)
  dim(Query_Results)
  ### Sample Type chosen ####
  #Pick the right data type####
  table(Query_Results$data_type)
  print(unique(Query_Results$data_type))
  
  if (length(unique(Query_Results$data_type)) == 1){
    query_TCGA = query_TCGA
    data.type = as.vector(unique(Query_Results$data_type))
  }else{
    print("Choose Data Type:")
    data.type = scan(what = character())
    while (length(data.type) > 1){
      print("ERROR: Wrong Data Type..")
      print("Choose Data Type:")
      data.type = scan(what = character())
    }
    while (!data.type %in% as.vector(unique(Query_Results$data_type ))&& data.type != "all"){
      print("ERROR: Wrong Data Type..")
      print("Choose Data Type:")
      data.type = scan(what = character())
    }
    if(unique(data.type == "all") == TRUE){
      query_TCGA = query_TCGA
      data.type = as.vector(unique(Query_Results$data_type))
    }else{
    query_TCGA = TCGAbiolinks::GDCquery(project = project_id,
                                      data.category = selected.data.category,
                                          sample.type = sample.type,
                                          data.type = data.type)
    }
    
  } 
  Query_Results <- TCGAbiolinks::getResults(query_TCGA)
  dim(Query_Results)
  ### Data type chosen ####
  #Pick only the open access#####
  table(Query_Results$access)
  print(unique(Query_Results$access))
  
  if (length(unique(Query_Results$access)) == 1 ){
      query_TCGA = query_TCGA
      access = as.vector(unique(Query_Results$access))
  }else{
    
    print("Choose File access")
    access = scan(what = character())
    while (!access %in% c("open" , "all")){
      print("ERROR")
      print("Choose Access:")
      access = scan(what = character())
    }
    if (unique(access == "all") == TRUE){
      query_TCGA = query_TCGA
      access = as.vector(unique(Query_Results$access))
    }else{
      query_TCGA = TCGAbiolinks::GDCquery(project = project_id,
                            data.category = selected.data.category,
                            sample.type = sample.type,
                            data.type = data.type,
                            access = access)
    }
  } 
  Query_Results <- TCGAbiolinks::getResults(query_TCGA)
  dim(Query_Results)
  ### Accesss chosen #####
  #pick the right data format #####
  table(Query_Results$data_format)
  print(unique(Query_Results$data_format))
  
  if (length(unique(Query_Results$data_format)) == 1 ){
    query_TCGA = query_TCGA
    data.format = as.vector(unique(Query_Results$data_format))
  }else{
    
    print("Choose Data Format")
    data.format = scan(what = character())
    while (!data.format %in% as.vector(unique(Query_Results$data_format )) && data.format != "all"){
      print("ERROR: Wrong Data Format")
      print("Choose Data Format:")
      data.format = scan(what = character())
    }
    if (unique(data.format == "all") == TRUE){
      query_TCGA = query_TCGA
      data.format = as.vector(unique(Query_Results$data_format))
    }else{
      query_TCGA = TCGAbiolinks::GDCquery(project = project_id,
                            data.category = selected.data.category,
                            sample.type = sample.type,
                            data.type = data.type,
                            access = access,
                            data.format = data.format)
    }
  } 
  Query_Results <- TCGAbiolinks::getResults(query_TCGA)
  dim(Query_Results)
  ### Data format chosen #####
  #pick the right expreimental strategy #####
  table(Query_Results$experimental_strategy)
  print(unique(Query_Results$experimental_strategy))
  
  if (length(unique(Query_Results$experimental_strategy)) == 1){
    query_TCGA = query_TCGA
    experimental.strategy = as.vector(unique(Query_Results$experimental_strategy))
  }else{
    
    print("Choose Experimental Strategy")
    experimental.strategy = scan(what = character())
    while (!experimental.strategy %in% as.vector(unique(Query_Results$experimental.strategy )) && experimental.strategy != "all"){
      print("ERROR: Wrong Experimental Strategy")
      print("Choose experimental.strategy:")
      experimental.strategy = scan(what = character())
    }
    if (unique(experimental.strategy == "all") == TRUE){
      query_TCGA = query_TCGA
      experimental.strategy = as.vector(unique(Query_Results$experimental_strategy))
    }else{
      query_TCGA = TCGAbiolinks::GDCquery(project = project_id,
                            data.category = selected.data.category,
                            sample.type = sample.type,
                            data.type = data.type,
                            access = access,
                            data.format = data.format,
                            experimental.strategy = experimental.strategy)
    }
  } 
  Query_Results <- TCGAbiolinks::getResults(query_TCGA)
  dim(Query_Results)
  #### Experimental strategy chosen #####
  #Pick the right work flow #####
  table(Query_Results$analysis_workflow_type)
  print(unique(Query_Results$analysis_workflow_type))
  
  if (length(unique(Query_Results$analysis_workflow_type)) == 1 ){
    query_TCGA = query_TCGA
    workflow.type = as.vector(unique(Query_Results$analysis_workflow_type))
  }else{
    print("Choose Workflow Type")
    workflow.type = scan(what = character())
    while (!workflow.type %in% as.vector(unique(Query_Results$analysis_workflow_type )) && workflow.type != "all"){
      print("ERROR")
      print("Choose Workflow Type:")
      workflow.type = scan(what = character())
    }
    if (unique(workflow.type == "all") == TRUE){
      query_TCGA = query_TCGA
      workflow.type = as.vector(unique(Query_Results$analysis_workflow_type))
    }else{
      query_TCGA = TCGAbiolinks::GDCquery(project = project_id,
                            data.category = selected.data.category,
                            sample.type = sample.type,
                            data.type = data.type,
                            access = access,
                            data.format = data.format,
                            experimental.strategy = experimental.strategy,
                            workflow.type = workflow.type)
    }
  } 
  Query_Results <- TCGAbiolinks::getResults(query_TCGA)
  dim(Query_Results)
  #### Work Flow Chosen #####
  #barcodes ???
  #pick the right platform??
  #pick the right file type??
  #Download and prepare the selected query####
  TCGAbiolinks::GDCdownload(query_TCGA)
  Query_Object = TCGAbiolinks::GDCprepare(query_TCGA)
  
  saveRDS(object = Query_Object , file = stringr::str_glue('{filepath}/{project_id}_Object.RDS') , compress = FALSE)
  return(Query_Object)
  ##############################
}

TARGET-CCSK_Object <- createTCGAobject(project_id = "TARGET-CCSK",selected.data.category =  "Transcriptome Profiling", filepath = "D:/MAIN")

TARGET-CCSK_Object <- createTCGAobject(project_id = "TARGET-CCSK",selected.data.category =  "Transcriptome Profiling")

debug(createTCGAobject)
undebug(createTCGAobject)

