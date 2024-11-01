# 1. Common basic functions for WenJuanXing ----------------------------------

s_GetColMask <- function(data,codingbook){
  colmask <- rep(FALSE,times = ncol(data))
  for (icol in codingbook$SurverySysItem){
    # get location index for the i-th column
    iloc_dict <- which(codingbook$SurverySysItem %in% icol)
    iloc_data <- which(colnames(data) %in% icol)
    colmask[iloc_data] = ifelse(codingbook[iloc_dict,'IncludeFlag'] == 'Yes',
                                TRUE,FALSE)
  }
  fprintf("|Data Frame :%s| %d columns will be included.",
          deparse(substitute(data)),sum(colmask))
  return(colmask)
}

s_RenameNumericalOrder <- function(data,codingbook){
  data_colmask <- s_GetColMask(data,codingbook)
  new_data <- data[,data_colmask]
  fprintf("|Rename data frame columns|")
  for (irawcol in colnames(new_data)){
    # Get the location index for data frame
    idataloc = which(colnames(new_data) %in% irawcol)
    # Get the location index for coding book
    idictloc = which(codingbook$SurverySysItem %in% irawcol)
    # Mapping the new item name to raw item name
    colnames(new_data)[idataloc] <- codingbook$ItemName[idictloc]
    # Generate the new item descriptions
    codingbook$Descriptions[idictloc] <- paste(
      codingbook$Descriptions[idictloc],
      str_remove_all(codingbook$SurverySysItem[idictloc],
                     pattern = '^[0-9]+、'),
      sep = ': ') %>%
      str_remove_all('NA: ')
    fprintf("|%s --> %s| Description: %s|",
            codingbook$ItemName[idictloc],codingbook$ItemName[idictloc],codingbook$Descriptions[idictloc])
  }
  for (icol in (codingbook$ItemName[codingbook$NumericFlag == 'Yes'])){
    new_data[[icol]] <- as.numeric(new_data[[icol]])
    fprintf('|Column: %s| forced to numeric value.',icol)
  }
  new_codingbook <- codingbook %>%
    filter(IncludeFlag == 'Yes') %>%
    select(
      c(ItemName,
        ReverseCodingFlag,
        Descriptions)
    ) 
  return(list("recodedata" = new_data,
              "newcodebook" = new_codingbook))
}

s_GetReverseKeys <- function(data,codingbook,type = 'keys'){
  Item_Reverse <- codingbook %>%
    # filter(IncludeFlag == 'Yes') %>%
    filter(ReverseCodingFlag == 'Yes') %>%
    .$ItemName
  Item_Keys <- rep(1,ncol(data))
  Item_Keys[colnames(data) %in% Item_Reverse] <- -1
  fprintf("|Items need reverse coding| Item List:")
  Item_List <- colnames(data)[Item_Keys == -1]
  cat(Item_List)
  fprintf("\nExecuting reverse coding later....")
  if (type == 'keys'){
    return(Item_Keys)
  }else if (type == 'list'){
    return(Item_List)
  }
}

s_ReverseCoding <- function(data,codingbook){
  Item_List <- s_GetReverseKeys(data,codingbook,type = 'list')
  ranges <- apply(as.data.frame(data)[, Item_List], 
                  2, function(...) range(..., na.rm = TRUE)) %>%
    as.data.frame()
  for (iItem in Item_List){
    newitem <- paste0(iItem,'_rev')
    maxi <- ranges[[iItem]][2]
    fprintf('|Reverse Coding| Raw Item: %s| New Item: %s | Formula: %d + 1 - raw_value',
            iItem,newitem,maxi)
    data[[newitem]] <- maxi + 1 - data[[iItem]]
  }
  return(data)
}

# 2. Standard Demographic Part Processing ---------------------------------

s_proc_SocioDemo <- function(data){
  fprintf("|Recoding UUID, Age and Gender|")  
  new_data <- data %>%
    mutate(
      UUID_SysNo = str_pad(as.character(SuveryPlatformNo),width = 6,side = 'left',pad = '0'),
      UUID_StuID = str_trunc(as.character(StudentID),width = 14,side = 'right',ellipsis = ""),
      UUID_StuID = str_pad(UUID_StuID,width = 14,side='left',pad = '0')
    ) %>%
    mutate(SubjectUUID = str_c(UUID_SysNo,UUID_StuID),.keep = 'unused') %>%
    mutate(
      Gender = RECODE(Gender,"1 = 'Male'; 2 = 'Female';3 = 'Others'; else = NA; "),
      Gender = factor(Gender,levels = c('Male','Female','Others'))
    ) %>%
    mutate(
      DataCollection_Date = as.Date(DataCollection_Date,"%Y/%m/%d"),
      BirthDate = as.Date(BirthDate,"%Y-%m-%d"),
      AgeInDays = as.numeric(DataCollection_Date - BirthDate),
      AgeInMonths = AgeInDays / 30,
      AgeInYears = AgeInDays/365.25,
    ) 
  s_table('AgeInYears',new_data,counts = T)
  s_table('Gender',new_data,counts = T)
  return(new_data)
}

s_proc_SelfReportAge <- function(data,years_lb = 17,years_ub = 24){
  # Winsorizing AgeInYears & Replace AgeInMonths/AgeInDays
  new_data <- data %>%
    mutate(
      RowFlag_AgeOutlier = (AgeInYears < years_lb) | (AgeInYears > years_ub),
      AgeInYears = datawizard::winsorize(AgeInYears,threshold = c(years_lb,years_ub), method = "raw"),
      AgeInMonths = ifelse(RowFlag_AgeOutlier,
                           AgeInYears*12,AgeInMonths),
      AgeInDays  = ifelse(RowFlag_AgeOutlier,
                          round(AgeInYears*365.25),AgeInDays),
    ) 
  return(new_data)
}

s_proc_BMI <- function(data){
  # Body Mass Index (BMI)
  new_data <- data %>%
    mutate(
      BMI = Weight / ((Height/100) ^ 2),
      BMI = ifelse((BMI < 11) | (BMI > 36),NA,BMI),
      BMI = ifelse(is.infinite(BMI),NA,BMI)
    )
  return(new_data)
}

s_proc_SSESladder <- function(data){
  # Macarthur's ladder for subjective SES
  new_data <- data %>%
    mutate(
      MacArthurLadder_SES = round(MacArthurLadder_SES)
    )
  return(new_data)
}

s_proc_FamilyInfo <- function(data){
  # Macarthur's ladder for subjective SES
  new_data <- data %>%
    mutate(
      Household_Location_3L = RECODE(HouseholdLocation_Binary,
                                        "1 = 'Urban'; 2 = 'Suburban'; 3 = 'Rural';"),
      Household_Location_3L = factor(HouseholdLocation_Binary,
                                        c('Urban','Suburban','Rural')),
      ParentsMaritalStatus = factor(ParentsMaritalStatus,
                                    c(1,2,3,4,5),
                                    c('Married','Divorced','Remarried','Single','Others')),
      ParentsHighestEducation_F = factor(ParentsHighestEducation_F,
                                         c(1,2,3,4,5,6,7),
                                         c('High School and Below',
                                           'High School and Below',
                                           'High School and Below',
                                           'Some College',
                                           'Bachelor Degree',
                                           'Master Degree and above',
                                           'Unknown')),
      ParentsHighestEducation_M = factor(ParentsHighestEducation_M,
                                         c(1,2,3,4,5,6,7),
                                         c('High School and Below',
                                           'High School and Below',
                                           'High School and Below',
                                           'Some College',
                                           'Bachelor Degree',
                                           'Master Degree and above',
                                           'Unknown')),
      Household_Size = 1 + FamMemb_1 + FamMemb_2 + FamMemb_3 + FamMemb_4 + FamMemb_5 + FamMemb_6 + FamMemb_7 + FamMemb_8 + FamMemb_9 + FamMemb_10 + FamMemb_11,
      Household_Size = RECODE(Household_Size,"lo:2 = '2 or below';3 = '3';4 = '4';5 = '5';6:hi = '6 and above';"),
      Household_Size = factor(Household_Size,
                             levels = c('2 or below','3','4','5','6 and above'),
                             ordered = T),
      
      )
  return(new_data)
}


# 3. Electronic Screen-related Questionnaires -----------------------------

s_proc_ScreenDeviceOwnership <- function(data){
    new_data <- data %>%
      mutate(
        OS_Android = ifelse(OS_NoPhone == 1, 0, 1),
        OS_iPhone  = ifelse(OS_NoPhone == 1, 0, 1),
        OS_Tablet = as.numeric(OS_Tablet),
        OS_PC     = as.numeric(OS_Tablet)
      )
    return(new_data)
}

s_proc_ScreenContentOwnership <- function(data){
  # Ownership - Specific Screen Media Content
  new_data <- data %>%
    mutate(
      OS_VideoClips     = RECODE(OS_VideoClips, "1=0;2=1;"),
      OS_InternetNovels = RECODE(OS_InternetNovels, "1=0;2=1;"),
      OS_SocialMedia    = RECODE(OS_SocialMedia, "1=0;2=1;"),
      OS_OnlineShopping = RECODE(OS_OnlineShopping, "1=0;2=1;"),
      OS_Pornograhpy    = RECODE(OS_Pornograhpy, "1=0;2=1;"),
      OS_InternetGame   = RECODE(OS_InternetGame, "1=0;2=1;"),
    )
  return(new_data)
}


s_proc_SABT <- function(data){
  new_data <- data %>%
    # Screen usage Around Bed Time (SABT)
    mutate(
      SABT_Game        = ifelse(SABT_None == 1, 0, 1),
      SABT_VideoClips  = ifelse(SABT_None == 1, 0, 1),
      SABT_VideoStream = ifelse(SABT_None == 1, 0, 1),
      SABT_SocialNet   = ifelse(SABT_None == 1, 0, 1),
      SABT_Shopping    = ifelse(SABT_None == 1, 0, 1),
      SABT_Learning    = ifelse(SABT_None == 1, 0, 1),
      SABT_Reading     = ifelse(SABT_None == 1, 0, 1),
      SABT_Others      = ifelse(SABT_None == 1, 0, 1),
    )
  return(new_data)
}
s_proc_ABCDSTQ14 <- function(data){
  # ABCD-STQ 14 items version (12 for reacreation, 2 for school-related works)
  new_data <- data %>%
    mutate(
      STQ_Weekday_WatchingTV = RECODE(STQ_Weekday_WatchingTV,
                                      "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekday_VideoStreaming = RECODE(STQ_Weekday_VideoStreaming,
                                          "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekday_VideoGaming = RECODE(STQ_Weekday_VideoGaming,
                                       "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekday_SocialNetworking = RECODE(STQ_Weekday_SocialNetworking,
                                            "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekday_TextChatting = RECODE(STQ_Weekday_TextChatting,
                                        "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekday_VideoChatting = RECODE(STQ_Weekday_VideoChatting,
                                         "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekday_NovelsReading = RECODE(STQ_Weekday_NovelsReading,
                                         "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekend_WatchingTV = RECODE(STQ_Weekend_WatchingTV,
                                      "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekend_VideoStreaming = RECODE(STQ_Weekend_VideoStreaming,
                                          "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekend_VideoGaming = RECODE(STQ_Weekend_VideoGaming,
                                       "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekend_SocialNetworking = RECODE(STQ_Weekend_SocialNetworking,
                                            "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekend_TextChatting = RECODE(STQ_Weekend_TextChatting,
                                        "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekend_VideoChatting = RECODE(STQ_Weekend_VideoChatting,
                                         "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekend_NovelsReading = RECODE(STQ_Weekend_NovelsReading,
                                         "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekday_SchoolWork = RECODE(STQ_Weekday_SchoolWork,
                                      "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Weekend_SchoolWork = RECODE(STQ_Weekend_SchoolWork,
                                      "1=0;2=0.25;3=0.5;4=1;5=2;6=3;7=4;"),
      STQ_Comp_VideoCentricEntertainment = ((STQ_Weekday_WatchingTV + STQ_Weekday_VideoStreaming + STQ_Weekday_VideoGaming) * 5 +
                                              (STQ_Weekend_WatchingTV + STQ_Weekend_VideoStreaming + STQ_Weekend_VideoGaming) * 2)/7,
      STQ_Comp_SocialCentricCommunication = ((STQ_Weekday_SocialNetworking + STQ_Weekday_TextChatting + STQ_Weekday_VideoChatting) * 5 +
                                               (STQ_Weekend_SocialNetworking + STQ_Weekend_TextChatting + STQ_Weekend_VideoChatting) * 2)/7,
      STQ_OST_SchoolWork = (STQ_Weekday_SchoolWork*5+STQ_Weekend_SchoolWork*2)/7,
      STQ_OST_Recreation =  datawizard::winsorize(STQ_Comp_VideoCentricEntertainment + STQ_Comp_SocialCentricCommunication,
                                                  threshold = c(0,24),
                                                  method = 'raw')
    )
  return(new_data)
}

s_proc_GameQ <- function(data){
  require(lubridate)
  new_data <- data %>%
    mutate(
      Game_TIME_Weekday_PastYear = datawizard::winsorize(
        (Game_Weekday_Hour + (Game_Weekday_Mins/60)),
        threshold = c(0,24),
        method = 'raw'),
      Game_TIME_Weekend_PastYear = datawizard::winsorize(
        (Game_Weekend_Hour + (Game_Weekend_Mins/60)),
        threshold = c(0,24),
        method = 'raw'),
      Game_TIME_Longest_PastYear = Game_Longest_Hour + (Game_Longest_Mins/60),
      Game_VAS_Craving      = Game_Craving,
      Game_VAS_LifeSat      = Game_LifeSatisfaction,
      Game_VAS_LifeInf      = Game_LifeInfluence,
      Game_VAS_ChagMot      = Game_ChangeMotivation,
      Game_SYMP_DuraMonths = Game_SymptomDuration_Months,
      Game_OnsetDuration_Years  = datawizard::winsorize(
        Game_OnsetDuration_Years,
        threshold = c(2000,year(Sys.time())),
        method = 'raw'),
      Game_OnsetDuration_Months = datawizard::winsorize(
        Game_OnsetDuration_Months,
        threshold = c(1,12),
        method = 'raw'),
      Game_SYMP_OnsetDate = as.Date(
        str_c(
          as.character(Game_OnsetDuration_Years),
          '-',
          str_pad(as.character(Game_OnsetDuration_Months),2,side = 'left',pad="0"),
          '-01'
          ),
        format = "%Y-%m-%d"
        ),
      Game_SYMP_OnsetDate = ifelse((Game_SYMP_OnsetDate-DataCollection_Date)>31,
                                   NA,Game_SYMP_OnsetDate),
      Game_SYMP_OnsetAgeInYears = (as.Date(Game_SYMP_OnsetDate) - as.Date(BirthDate))/365.25,
      Game_SYMP_OnsetAgeInYears = datawizard::winsorize(
        as.numeric(Game_SYMP_OnsetAgeInYears),threshold = c(6,Inf),method = 'raw'
      ),
      Game_SYMP_OnsetAgeInYears = ifelse(Game_SYMP_OnsetAgeInYears>=AgeInYears,
                                         AgeInYears,Game_SYMP_OnsetAgeInYears),
      Game_SYMP_PersistPeriod_Months = Game_OnsetCumulative_Months,
      Game_SYMP_OnsetHistory_Years = AgeInYears - Game_SYMP_OnsetAgeInYears,
      .keep = 'unused'
    )
}

# 4. Proc Functions for Questionnaire formed in Text -----------------------------------------

s_MapTextColnames <- function(data){
  colnames(data) <- colnames(data) %>%
    str_remove_all('^[0-9]+、') %>%
    str_replace_all(pattern = c(
      '序号' = 'SuveryPlatformNo',
      '您的学号.*' = 'StudentID',
      '最近2个月，上网主要使用设备包括.*' = 'ScreenTime',
      '.*最近两周，使用的多功能.*' = 'APP',
      '.*您有几天做了重体力活动.*' = 'IPAQ_1',
      '.*通常会花多少时间在重体力活动上.*' = 'IPAQ_2',
      '.*您有几天做了中等强度体力活动.*' = 'IPAQ_3',
      '.*通常会花多少时间在中等强度体力活动上.*'  = 'IPAQ_4',
      '.*您有几天是步行.*' = 'IPAQ_5',
      '.*通常花多少时间在步行上.*' = 'IPAQ_6',
      '.*您有多久时间是坐着的.*' = 'IPAQ_7'
    ))
  fprintf(colnames(data))
  return(data)
}

s_proc_IPAQSF <- function(textdata){
  # International Physical Activity Questionnaire - Short Form
  new_textdata <- textdata %>%
    mutate(# 1st-level remove System default characters
      IPAQ_1 = str_replace_all(IPAQ_1,
                               c(
                                 "每周___天" = "",
                                 "无相关体育活动" = "0",
                                 "〖" = "",
                                 "〗" = "",
                                 "，|," = "")),
      IPAQ_3 = str_replace_all(IPAQ_3,
                               c(
                                 "每周___天" = "",
                                 "无相关体育活动" = "0",
                                 "无适度体育活动" = "0",
                                 "〖" = "",
                                 "〗" = "",
                                 "分钟" = "")),
      IPAQ_5 = str_replace_all(IPAQ_5,
                               c(
                                 "每周___天" = "",
                                 "没有步行" = "0",
                                 "〖" = "",
                                 "〗" = "")),
      IPAQ_2 = str_replace_all(IPAQ_2,
                               c(
                                 "每天___分钟" = "",
                                 "\\(跳过\\)" = "0",
                                 "不知道或不确定" = "NA",
                                 "〖" = "",
                                 "〗" = "")),
      IPAQ_4 = str_replace_all(IPAQ_4,
                               c(
                                 "每天___分钟" = "",
                                 "\\(跳过\\)" = "0",
                                 "不知道或不确定" = "NA",
                                 "〖" = "",
                                 "〗" = "")),
      IPAQ_6 = str_replace_all(IPAQ_6,
                               c(
                                 "每天___分钟" = "",
                                 "\\(跳过\\)" = "0",
                                 "不知道或不确定" = "NA",
                                 "〖" = "",
                                 "〗" = "")),
      IPAQ_7 = str_replace_all(IPAQ_7,
                               c(
                                 "每天___分钟" = "",
                                 "\\(跳过\\)" = "0",
                                 "不知道或不确定" = "NA",
                                 "〖" = "",
                                 "〗" = "")),
    ) %>%
    mutate(# 2nd-level trimming
      IPAQ_1 = str_trim(IPAQ_1),
      IPAQ_2 = str_trim(IPAQ_2),
      IPAQ_3 = str_trim(IPAQ_3),
      IPAQ_4 = str_trim(IPAQ_4),
      IPAQ_5 = str_trim(IPAQ_5),
      IPAQ_6 = str_trim(IPAQ_6),
      IPAQ_7 = str_trim(IPAQ_7),
      IPAQ_1 = ifelse(str_detect(IPAQ_1,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,IPAQ_1),
      IPAQ_2 = ifelse(str_detect(IPAQ_2,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,IPAQ_2),
      IPAQ_3 = ifelse(str_detect(IPAQ_3,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,IPAQ_3),
      IPAQ_4 = ifelse(str_detect(IPAQ_4,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,IPAQ_4),
      IPAQ_5 = ifelse(str_detect(IPAQ_5,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,IPAQ_5),
      IPAQ_6 = ifelse(str_detect(IPAQ_6,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,IPAQ_6),
      IPAQ_7 = ifelse(str_detect(IPAQ_7,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,IPAQ_7),
      IPAQ_1 = as.numeric(IPAQ_1),
      IPAQ_2 = as.numeric(IPAQ_2),
      IPAQ_3 = as.numeric(IPAQ_3),
      IPAQ_4 = as.numeric(IPAQ_4),
      IPAQ_5 = as.numeric(IPAQ_5),
      IPAQ_6 = as.numeric(IPAQ_6),
      IPAQ_7 = as.numeric(IPAQ_7),
      IPAQ_1 = ifelse(IPAQ_1>7,NA,IPAQ_1),
      IPAQ_2 = ifelse(IPAQ_2>180,180,round(IPAQ_2)),
      IPAQ_2 = ifelse(IPAQ_2<10,0,round(IPAQ_2)),
      IPAQ_3 = ifelse(IPAQ_3>7,NA,IPAQ_3),
      IPAQ_4 = ifelse(IPAQ_4>180,180,round(IPAQ_4)),
      IPAQ_4 = ifelse(IPAQ_4<10,0,round(IPAQ_4)),
      IPAQ_5 = ifelse(IPAQ_5>7,NA,IPAQ_5),
      IPAQ_6 = ifelse(IPAQ_6>180,180,round(IPAQ_6)),
      IPAQ_6 = ifelse(IPAQ_6<10,0,round(IPAQ_6)),
      IPAQ_7 = ifelse(IPAQ_7>180,180,round(IPAQ_7)),
      IPAQ_7 = ifelse(IPAQ_7<10,0,round(IPAQ_7)),
      IPAQ_Intensity = 8.0 * IPAQ_1 * IPAQ_2 + 4.0*IPAQ_3*IPAQ_4 + 3.3*IPAQ_5*IPAQ_6,
      IPAQ_Intensity = ifelse(IPAQ_2+IPAQ_4+IPAQ_6>960,
                              NA,
                              IPAQ_Intensity),
      IPAQ_Level = ifelse((IPAQ_1>=3&IPAQ_Intensity>1500)|
                            ((IPAQ_1+IPAQ_3+IPAQ_5)>=7&IPAQ_Intensity>3000),
                          "High",NA),
      IPAQ_Level = ifelse(is.na(IPAQ_Level)&
                            ((IPAQ_1>=3&IPAQ_2>=20)|
                               ((IPAQ_4>=30|IPAQ_6>=30)&(IPAQ_3+IPAQ_5>=5))|
                               ((IPAQ_1+IPAQ_3+IPAQ_5>=5)&IPAQ_Intensity>=600)),
                          "Middle",IPAQ_Level),
      IPAQ_Level = ifelse(is.na(IPAQ_Level)&
                            (!is.na(IPAQ_Intensity)),
                          "Low",IPAQ_Level),
      IPAQ_Level = factor(IPAQ_Level,
                          c("Low","Middle","High")),
      IPAQ_Intensity = datawizard::winsorize(IPAQ_Intensity,
                                             threshold = c(0,3000),
                                             method = "raw"),
    )
  return(new_textdata)
}

s_prep_STbyDevice <- function(textdata){
  ScreenTime_SelfReport <- textdata$ScreenTime %>%
    str_split('┋',simplify = T) 
  ST_dataframe <- data.frame(ST_Android = rep(NA,nrow(ScreenTime_SelfReport)), 
                             ST_iPhone  = rep(NA,nrow(ScreenTime_SelfReport)), 
                             ST_PC      = rep(NA,nrow(ScreenTime_SelfReport)),  
                             ST_Tablet  = rep(NA,nrow(ScreenTime_SelfReport)))
  for (isub in 1:nrow(ScreenTime_SelfReport)) {
    for (icol in 1:ncol(ScreenTime_SelfReport)) {
      textcell <- str_remove(ScreenTime_SelfReport[isub,icol],'〗')
      textcell_splited <- as.data.frame(str_split(textcell,'〖',simplify = T))
      if (textcell_splited$V1 == "安卓手机"){
        textdata_NCST_rec[isub,"ST_Android"] = textcell_splited$V2
      }else if (textcell_splited$V1 == "苹果手机"){
        textdata_NCST_rec[isub,"ST_iPhone"] = textcell_splited$V2
      }else if (textcell_splited$V1 == "电脑"){
        textdata_NCST_rec[isub,"ST_PC"] = textcell_splited$V2
      }else if (textcell_splited$V1 == "平板"){
        textdata_NCST_rec[isub,"ST_Tablet"] = textcell_splited$V2
      }
    }
  }
  ST_dataframe$ST_OverallTime <- SUM(ST_dataframe,
                                     vars = c("ST_Android",
                                              "ST_iPhone",
                                              "ST_PC",
                                              "ST_Tablet"),
                                     na.rm = T)
  ST_dataframe$SuveryPlatformNo <- textdata$SuveryPlatformNo
  ST_dataframe$StudentID        <- textdata$StudentID
  return(ST_dataframe)
}


s_proc_STbyDevice <- function(textdata){
  new_textdata <- textdata %>%
    mutate(
      ST_Android = str_remove_all(ST_Android,"(\\?)|(？)|(小)|(时)|(个)|(天)|(每)|(h)|(以)|(上)|(/)|(左右)|(\\+)|( )|(”)"),
      ST_Android = ifelse(str_detect(ST_Android,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,ST_Android),
      ST_Android = as.numeric(ST_Android),
      ST_Android = ifelse((ST_Android>24) & (ST_Android<1440),ST_Android/60,ST_Android),
      ST_Android = ifelse(ST_Android>1440,NA,ST_Android),
      ST_Android = ifelse(ST_Android==0,NA,ST_Android),
      ST_iPhone = str_remove_all(ST_iPhone,"(\\?)|(？)|(小)|(时)|(个)|(天)|(每)|(h)|(以)|(上)|(/)|(左右)|(\\+)|( )|(”)"),
      ST_iPhone = ifelse(str_detect(ST_iPhone,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,ST_iPhone),
      ST_iPhone = as.numeric(ST_iPhone),
      ST_iPhone = ifelse((ST_iPhone>24) & (ST_iPhone<1440),ST_iPhone/60,ST_iPhone),
      ST_iPhone = ifelse(ST_iPhone>1440,NA,ST_iPhone),
      ST_iPhone = ifelse(ST_iPhone==0,NA,ST_iPhone),
      ST_PC = str_remove_all(ST_PC,"(\\?)|(？)|(小)|(时)|(个)|(天)|(每)|(h)|(以)|(上)|(/)|(左右)|(\\+)|( )|(”)"),
      ST_PC = ifelse(str_detect(ST_PC,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,ST_PC),
      ST_PC = as.numeric(ST_PC),
      ST_PC = ifelse((ST_PC>24) & (ST_PC<1440),ST_PC/60,ST_PC),
      ST_PC = ifelse(ST_PC>1440,NA,ST_PC),
      ST_PC = ifelse(ST_PC==0,NA,ST_PC),
      ST_Tablet = str_remove_all(ST_Tablet,"(\\?)|(？)|(小)|(时)|(个)|(天)|(每)|(h)|(以)|(上)|(/)|(左右)|(\\+)|( )|(”)"),
      ST_Tablet = ifelse(str_detect(ST_Tablet,"[\u2E80-\u9FFF]|[a-z]|[A-Z]"),NA,ST_Tablet),
      ST_Tablet = as.numeric(ST_Tablet),
      ST_Tablet = ifelse((ST_Tablet>24) & (ST_Tablet<1440),ST_Tablet/60,ST_Tablet),
      ST_Tablet = ifelse(ST_Tablet>1440,NA,ST_Tablet),
      ST_Tablet = ifelse(ST_Tablet==0,NA,ST_Tablet),
    )
  new_textdata$SMA_OverallTime <- SUM(new_textdata,vars = c("ST_Android","ST_iPhone","ST_PC","ST_Tablet"),na.rm = T)
  new_textdata$SuveryPlatformNo <- textdata$SuveryPlatformNo
  new_textdata$StudentID <- textdata$StudentID
}
