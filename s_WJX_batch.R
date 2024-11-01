# 0. Prepare Env ----------------------------------------------------------

bruceR::set.wd()
source('s_WJX_utilities.R')
source('s_WJX_questionnaire.R')
source('s_WJX_scale.R')

s_start_sink(fullfile(LogFilesDir,LogFilesName))

# 1. load data, rename colmuns, and convert to numeric values ----------------------------------------

list_WJX <- s_RenameNumericalOrder(
  data = s_import(fullfile(DataFilesDir,WJXFileName_NumO)),
  codingbook = s_import(fullfile(DataFilesDir,DataDictFileName))
  )

# 2. Apply reverse coding based on data dictionary ------------------------

calcdata <- list_WJX$recodedata %>%
  s_ReverseCoding(data = .,
                  codingbook = list_WJX$newcodebook) %>%
# 3. Recode all items for Numerical Order Type WJX data -----------------------------------------------------
  s_proc_SocioDemo() %>%
  s_proc_SelfReportAge(years_lb = 14,years_ub = 22) %>%
  s_proc_BMI() %>%
  s_proc_SSESladder() %>%
  s_proc_ScreenDeviceOwnership() %>%
  s_proc_ScreenContentOwnership() %>%
  s_proc_SABT() %>%
  s_proc_ABCDSTQ14() %>%
  s_proc_YIAT() %>%
  s_proc_SABAS() %>%
  s_proc_SVAS() %>%
  s_proc_INAS() %>%
  s_proc_BSMAS() %>%
  s_proc_OSAS() %>%
  s_proc_IPAS() %>%
  s_proc_IGDS9SF() %>%
  s_proc_GameQ() %>%
  s_proc_IGDDSMV() %>%
  s_proc_GameEuphoria() %>%
  s_proc_GameYBOCS() %>%
  s_proc_PSQI() %>%
  s_proc_PHQ9() %>%
  s_proc_GAD7() %>%
  s_proc_TWoBogusItems() %>%
  s_proc_PRIMEPC() %>%
  s_proc_mYFAS2() %>%
  mutate(
    SubstanceUse_Smoke_YN = RECODE(SubstanceUse_Smoke_YN,"1=1;2=0;"),
    SubstanceUse_Alcohol_YN = RECODE(SubstanceUse_Alcohol_YN,"1=1;2=0;")
  ) %>%
  s_proc_PSS() %>%
  s_proc_COPE30() %>%
  s_proc_BSCS() %>%
  s_proc_BSSSC8() %>%
  s_proc_RICS() %>%
  s_proc_BISBAS() %>%
  s_proc_BDERS16() %>%
  s_proc_FASM() %>%
  s_proc_NSSIDSMV() %>%
  s_proc_IPPA() %>%
  s_proc_DBVSS() %>%
  s_proc_CBV() %>%
  s_proc_CBP() %>%
  s_proc_sEMBUC() %>%
  s_proc_RTPageSum()

  
  
  


calcdata_NCST_ValueText$SampleID <- "NCSTP1_Original"

# re-sort all columns by their item name
calcdata_NCST_ValueText <- calcdata_NCST_ValueText[sort(colnames(calcdata_NCST_ValueText))]
calcdata_NCST_ValueText <- calcdata_NCST_ValueText %>%
  select(
    c(SampleID,SubjectUUID,SuveryPlatformNo,StudentID,
    PassiveSensingLinkID,
    Gender,starts_with('AgeIn'),RowFlag_AgeOutlier,
    BirthDate,DataCollection_Date,
    Grade,Faculty,
    starts_with('Bogus_'),starts_with('BogusItem_'),
    starts_with('RT_'),starts_with('CIER_'),
    starts_with('VAS')),
    ParentsHighestEducation_F,
    ParentsHighestEducation_M,
    ParentsMaritalStatus,
    HouseholdSize,HouseholdLocation_Binary,HouseholdLocation_Region,
    MacArthurLadder_SES,  
    Height,Weight,BMI,
    OS_Android,OS_iPhone,OS_PC,OS_Tablet,OS_NoPhone,
    ST_Android,ST_iPhone,ST_PC,ST_Tablet,SMA_OverallTime,
    starts_with('STQ_'),
    SMA_Comp_VideoCentricEntertainment,SMA_Comp_SocialCentricCommunication,
    starts_with('SABT_'),
    starts_with('IAT_'),starts_with('SABAS'),
    OS_VideoClips,OS_InternetNovels,OS_SocialMedia,OS_OnlineShopping,OS_Pornograhpy,OS_InternetGame,
    starts_with('SVAS_'),starts_with('INAS_'),starts_with('BSMAS_'),starts_with('OSAS_'),starts_with('IPAS_'),starts_with('IGDS9SF_'),
    starts_with('mYFAS2_'),
    starts_with('PHQ9_'),starts_with('GAD7_'),starts_with('OCIR_'),starts_with('DARS_'),starts_with('PRIME_'),starts_with('NSSI_DSMV_'),
    starts_with('PSS_'),starts_with('BFISF_'),starts_with('BSCS_'),starts_with('RCBSTC_'),starts_with('DERS16_'),starts_with('BERS_'),starts_with('SSS_'),starts_with('BISBAS_'),starts_with('BIS11_'),starts_with('RICS_'),starts_with('COPE_'),starts_with('AIMS_'),starts_with('SKIN9_'),
    starts_with('ChildhoodNCE_'),starts_with('SSRS_'),starts_with('IPPA_'),starts_with('FRIENDS_'),starts_with('sEMBUC_'),
    starts_with('PSQI_'),starts_with('IPAQ_'),
    everything()
  )
var_labels_anchor <- s_import(fullfile(DataFilesDir,"DataDictionary_NCSTPart1_ManualCorrected.xlsx"))
df_var_labels_sorted <- merge(
  data.frame(ItemName = colnames(calcdata_NCST_ValueText)),
  var_labels_anchor,
  all = T,
  sort = F
)
var_labels_sorted <- df_var_labels_sorted$Descriptions
names(var_labels_sorted) 
autodatadict <- datadictionary::create_dictionary(calcdata_NCST_ValueText,
                                                  id_var = c('SampleID','SubjectUUID','SuveryPlatformNo','StudentID'),
                                                  var_labels = var_labels_sorted,
                                                  file = fullfile(DataFilesDir,'DataDictionary_NCSTPart1_Overall.xlsx'))
s_export(calcdata_NCST_ValueText,
         file = "../Res_3_IntermediateData/P06_NCSTPart1_Data_Overall_Merged_Final_ST.rda")
s_export(calcdata_NCST_ValueText,
         file = "../Res_3_IntermediateData/P06_NCSTPart1_Data_Overall_Merged_Final_ST.xlsx")
s_close_sink()
