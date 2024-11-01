require(bruceR)

# Formatted output to R Console -------------------------------------------
fprintf <- function(STR,...,end = '\n'){
  cat(paste0(sprintf(STR,...),end))
}

fwarn <- function(STR,...,end = '\n'){
  FormatedSTR = paste0(sprintf(STR,...),end)
  warning(FormatedSTR,call. = F,immediate. = T)
}

fstop <- function(STR,...,end = '\n'){
  FormatedSTR = paste0(sprintf(STR,...),end)
  stop(FormatedSTR,call. = F,domain = NULL)
}

s_kable <- function(table,print_nrows = 20,caption = NULL){
  TChars <- knitr::kable(head(table, n = print_nrows),
                         format = "simple",
                         digits = 2,
                         row.names = F,
                         align = 'c',
                         caption = caption,
                         label = NA)
  invisible(TChars)
}

# Path and R scripts utilities - Simple Warp -----------------------------------

fullfile <- function(...){
  return(normalizePath(paste(...,sep = .Platform$file.sep),winslash = '/',mustWork = F))
}

s_get_script_name <- function(){
  return(basename(rstudioapi::getSourceEditorContext()$path))
}

# Automatic Logging -------------------------------------------------------

s_start_sink <- function(LogFileDir, pause_flag = F){
  # Close all opened log files
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
  # Start a new logging session
  fprintf('Start a new log file: %s\n',LogFileDir)
  sink(file = LogFileDir,
       type = 'output',
       append = pause_flag,
       split = T)
  fprintf('|Logging Start| Directory: %s | Time: %s|',
          normalizePath(LogFileDir,winslash = '/',mustWork = T),
          Sys.time())
}

s_close_sink <- function(SourceScriptName = NULL){
  if (sink.number() == 0){
    fprintf("No division has been used. Skip!\n")
  }else{
    for(i in seq_len(sink.number())){
      fprintf("|Logging End| # %d Logging Stop| Finish Time:%s|\n",i,Sys.time())
      sink(file = NULL)
    }
    fprintf('All used sink divisions have been closed.\n')
  }
}

# Executing R Command formatted as a String -------------------------------

s_eval <- function(UserString,...,return.res = T){
  R_cmd_str <- sprintf(UserString,...)
  res = eval(parse(text = R_cmd_str))
  if (return.res){
    return(res)
  }else{
    invisible(R_cmd_str)
  }
}

# Files or Directory Operations -------------------------------------------

s_check_output_dir <- function(DIR_STR){
  DIR_STR_STD <- normalizePath(DIR_STR,winslash = '/',mustWork = F)
  if (dir.exists(DIR_STR_STD)){
    fprintf('|Output Destination: %s| Directory exists. Check Passed|',DIR_STR_STD)
  }else{
    fprintf('|Output Destination: %s| Directory is creating...',DIR_STR_STD,end = '\t')
    dir.create(DIR_STR_STD,recursive = F,showWarnings = F)
    fprintf('Done!|')
  }
}

s_generate_file_table <- function(ROOT_DIR_STR,pattern = '\\.rda$'){
  Files_List <- list.files(
    path = normalizePath(ROOT_DIR_STR,winslash = '/'),
    pattern = pattern,
    all.files = F,
    full.names = T,
    recursive = F
  )
  Files_Name <- tools::file_path_sans_ext(basename(Files_List))
  File_Table_df <- data.frame('Directory' = Files_List,
                              'FileName' = Files_Name)
  return(File_Table_df)
}

s_import <- function(file){
  SourceFileDir <- normalizePath(file,winslash = '/',mustWork = T)
  fprintf('|Loading data| Source File Directory: %s|',SourceFileDir)
  object <- bruceR::import(file = SourceFileDir,
                           encoding = 'UTF-8',
                           verbose = T)
  return(object)
}

s_export <- function(object,file,sheet = NULL){
  DestinationFileDir <- normalizePath(file,winslash = '/',mustWork = F)
  fprintf('|Saving data| R object: %s|',deparse(substitute(object)))
  fprintf('\t\t|Destination File Dir: %s|',DestinationFileDir)
  bruceR::export(object,
                 file = file,
                 encoding = 'UTF-8',
                 sheet = sheet)
}

s_table <- function(var,df=NA,counts=F) {
  if (!"data.frame" %in% class(df)){
    if (counts){
      Res <- table(var,useNA = 'ifany',dnn = deparse(substitute(var)))
    }else{
      if (is.factor(var) | is.character(var)){
        Res <- table(var,useNA = 'ifany',dnn = deparse(substitute(var)))
      }else if (is.numeric(var)){
        Res <- psych::describe(var,check = T)
      }
    }
  }else if (is.character(var)){
    if (counts){
      Res <- table(df[[var]],useNA = 'ifany',dnn = var)
      if (length(Res) > 20){
        Res <- table(cut(df[[var]],breaks = 10),useNA = 'ifany',dnn = var) 
      }
    }else{
      if (is.factor(df[[var]]) | is.character(df[[var]]) ){
        Res <-table(df[[var]],useNA = 'ifany',dnn = var)
      }else if (is.numeric(df[[var]])){
        Res <- psych::describe(df[[var]],check = T)
      }
    }
  }
  print(Res)
  invisible(Res)
}


