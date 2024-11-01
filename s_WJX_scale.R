# 1. Assessment Pool: Digital Addiction -----------------------------------

s_proc_YIAT <- function(data){
  # Young's Internet Addiction Test
  new_data <- data %>%
    mutate(
      IAT_Sum = IAT_1 + IAT_2 + IAT_3 + IAT_4 + IAT_5 + IAT_6 + IAT_7 + IAT_8 +
        IAT_9 + IAT_10 + IAT_11 + IAT_12 + IAT_13 + IAT_14 + IAT_15 + IAT_16 +
        IAT_17 + IAT_18 + IAT_19 + IAT_20,
    )
  return(new_data)
}
s_proc_SABAS <- function(data){
  # Smartphone Application-Based Addiction Scale (SABAS)
  new_data <- data %>%
    mutate(
      SABAS_Sum = SABAS_1 + SABAS_2 + SABAS_3 + SABAS_4 + SABAS_5 + SABAS_6,
    )
  return(new_data)
}
s_proc_SVAS <- function(data){
  # Small Video (Video Clips) Addiction Scale (SVAS)
  new_data <- data %>%
    mutate(
      SVAS_Sum = SVAS_1 + SVAS_2 + SVAS_3 + SVAS_4 + SVAS_5 + SVAS_6,
    )
  return(new_data)
}
s_proc_INAS <- function(data){
  # Internet Novels Addiction Scale (INAS)
  new_data <- data %>%
    mutate(
      INAS_Sum = INAS_1 + INAS_2 + INAS_3 + INAS_4 + INAS_5 + INAS_6,  
      )
  return(new_data)
}
s_proc_BSMAS <- function(data){
  # Bergan Social Media Addiction Scale (BSMAS)
  new_data <- data %>%
    mutate(
      BSMAS_Sum = BSMAS_1 + BSMAS_2 + BSMAS_3 + BSMAS_4 + BSMAS_5 + BSMAS_6,
    )
      return(new_data)
}
s_proc_OSAS <- function(data){
  # Online Shopping Addiction Scale (OSAS)
  new_data <- data %>%
    mutate(
      OSAS_Sum = OSAS_1 + OSAS_2 + OSAS_3 + OSAS_4 + OSAS_5 + OSAS_6,
    )
  return(new_data)
}
s_proc_IPAS <- function(data){
  # Internet Pornography Addiction Scale (IPAS)
  new_data <- data %>%
    mutate(
      IPAS_Sum = IPAS_1 + IPAS_2 + IPAS_3 + IPAS_4 + IPAS_5 + IPAS_6,
    )
  return(new_data)
}
s_proc_IGDS9SF <- function(data){
  # Internet Gaming Disorder Scale - Short Form (IGDS9-SF)
  new_data <- data %>%
    mutate(
      IGDS9SF_Sum = IGDS9SF_1 + IGDS9SF_2 + IGDS9SF_3 + IGDS9SF_4 + IGDS9SF_5 + IGDS9SF_6 + IGDS9SF_7 + IGDS9SF_8 + IGDS9SF_9,
    )
  return(new_data)
}
s_proc_IGDDSMV <- function(data){
  # DSM-5-based Internet Gaming Disorder 9 items
  new_data <- data %>%
    mutate(
      Game_DSMV_1 = RECODE(Game_DSMV_1,"1=1;2=0;"),
      Game_DSMV_2 = RECODE(Game_DSMV_2,"1=1;2=0;"),
      Game_DSMV_3 = RECODE(Game_DSMV_3,"1=1;2=0;"),
      Game_DSMV_4 = RECODE(Game_DSMV_4,"1=1;2=0;"),
      Game_DSMV_5 = RECODE(Game_DSMV_5,"1=1;2=0;"),
      Game_DSMV_6 = RECODE(Game_DSMV_6,"1=1;2=0;"),
      Game_DSMV_7 = RECODE(Game_DSMV_7,"1=1;2=0;"),
      Game_DSMV_8 = RECODE(Game_DSMV_8,"1=1;2=0;"),
      Game_DSMV_9 = RECODE(Game_DSMV_9,"1=1;2=0;"),
      Game_SYMP_DSMV_Sum = Game_DSMV_1 + Game_DSMV_2 + Game_DSMV_3 + Game_DSMV_4 + 
        Game_DSMV_5 + Game_DSMV_6 + Game_DSMV_7 + Game_DSMV_8 + Game_DSMV_9,
    )
  return(new_data)
}
s_proc_GameEuphoria <- function(data){
  # Game-related Euphoria Subscale
  new_data <- data %>%
    mutate(
      Game_Euphoria_Sum = Game_Euphoria_1 + Game_Euphoria_2 + Game_Euphoria_3 + 
        Game_Euphoria_4,
    )
  return(new_data)
}
s_proc_GameYBOCS <- function(data){
  # Game-related Yale-Brown Obsessive-Compulsive Scale (YBOCS)
  new_data <- data %>%
    mutate(
      Game_Compulsivity_Sum = Game_Compulsivity_1 + Game_Compulsivity_2 + 
        Game_Compulsivity_3 + Game_Compulsivity_4 + Game_Compulsivity_5 + 
        Game_Compulsivity_6 + Game_Compulsivity_7 + Game_Compulsivity_8 + 
        Game_Compulsivity_9 + Game_Compulsivity_10 + Game_Compulsivity_11,
    )
  return(new_data)
}

# 2. Assessment Pool: Mental Health ---------------------------------------

s_proc_PHQ9 <- function(data){
  # Patient Health Questionnaire - 9 (PHQ-9)
  new_data <- data %>%
    mutate(
      PHQ9_1 = RECODE(PHQ9_1,"1=0; 2=1; 3=2; 4=3;"),
      PHQ9_2 = RECODE(PHQ9_2,"1=0; 2=1; 3=2; 4=3;"),
      PHQ9_3 = RECODE(PHQ9_3,"1=0; 2=1; 3=2; 4=3;"),
      PHQ9_4 = RECODE(PHQ9_4,"1=0; 2=1; 3=2; 4=3;"),
      PHQ9_5 = RECODE(PHQ9_5,"1=0; 2=1; 3=2; 4=3;"),
      PHQ9_6 = RECODE(PHQ9_6,"1=0; 2=1; 3=2; 4=3;"),
      PHQ9_7 = RECODE(PHQ9_7,"1=0; 2=1; 3=2; 4=3;"),
      PHQ9_8 = RECODE(PHQ9_8,"1=0; 2=1; 3=2; 4=3;"),
      PHQ9_9 = RECODE(PHQ9_9,"1=0; 2=1; 3=2; 4=3;"),
      PHQ9_Sum = PHQ9_1 + PHQ9_2 + PHQ9_3 + PHQ9_4 + PHQ9_5 + PHQ9_6 + PHQ9_7 + PHQ9_8 + PHQ9_9,
    ) 
  return(new_data)
}
s_proc_GAD7 <- function(data){
  # Generalized Anxiety Disorder (GAD-7)
  new_data <- data %>%
    mutate(
      GAD7_1 = RECODE(GAD7_1,"1=0; 2=1; 3=2; 4=3;"),
      GAD7_2 = RECODE(GAD7_2,"1=0; 2=1; 3=2; 4=3;"),
      GAD7_3 = RECODE(GAD7_3,"1=0; 2=1; 3=2; 4=3;"),
      GAD7_4 = RECODE(GAD7_4,"1=0; 2=1; 3=2; 4=3;"),
      GAD7_5 = RECODE(GAD7_5,"1=0; 2=1; 3=2; 4=3;"),
      GAD7_6 = RECODE(GAD7_6,"1=0; 2=1; 3=2; 4=3;"),
      GAD7_7 = RECODE(GAD7_7,"1=0; 2=1; 3=2; 4=3;"),
      GAD7_Sum = GAD7_1 + GAD7_2 + GAD7_3 + GAD7_4 + GAD7_5 + GAD7_6 + GAD7_7,
    )
  return(new_data)
}
s_proc_OCIR <- function(data){
  # Obsessive-Compulsive Inventory-Revised (OCIR)
  new_data <- data %>%
    mutate(
      OCIR_Sum = OCIR_1 + OCIR_2 + OCIR_3 + OCIR_4 + OCIR_5 + OCIR_6 + OCIR_7 +
        OCIR_8 + OCIR_9 + OCIR_10 + OCIR_11 + OCIR_12 + OCIR_13 + OCIR_14 + 
        OCIR_15 + OCIR_16 + OCIR_17 + OCIR_18,
    )
  return(new_data)
}
s_proc_mYFAS2 <- function(data){
  # modified Yale Food Addiction Scale 2.0
  new_data <- data %>%
    mutate(
      mYFAS2_Sum = mYFAS2_1 + mYFAS2_2 +  mYFAS2_3 + mYFAS2_4 + mYFAS2_5 + mYFAS2_6 + mYFAS2_7 + 
        mYFAS2_8 + mYFAS2_9 + mYFAS2_10 + mYFAS2_11 + mYFAS2_12 + mYFAS2_13,
    )
  return(new_data)
}
s_proc_DARS <- function(data){
  # Dimensional Anhedonia Rating Scale (DARS)
  new_data <- data %>%
    mutate(
      DARS_PastimesHobbies_Sum = DARS_PastimesHobbies_1 + DARS_PastimesHobbies_2 + DARS_PastimesHobbies_3 + DARS_PastimesHobbies_4,
      DARS_FoodsDrinks_Sum = DARS_FoodsDrinks_1 + DARS_FoodsDrinks_2 + DARS_FoodsDrinks_3 + DARS_FoodsDrinks_4,
      DARS_SocialActivities_Sum = DARS_SocialActivities_1 + DARS_SocialActivities_2 + DARS_SocialActivities_3 + DARS_SocialActivities_4,
      DARS_SensoryExperiences_Sum = DARS_SensoryExperiences_1 + DARS_SensoryExperiences_2 + DARS_SensoryExperiences_3 + DARS_SensoryExperiences_4 + DARS_SensoryExperiences_5,
      DARS_Sum = DARS_PastimesHobbies_Sum + DARS_FoodsDrinks_Sum + DARS_SocialActivities_Sum + DARS_SensoryExperiences_Sum,
    ) 
  return(new_data)
}
s_proc_PRIMEPC <- function(data){
  # Prodromal Psychosis Symptoms (PRIME-PC)
  new_data <- data %>%
    mutate(
      PRIME_Sum = PRIME_1 + PRIME_2 + PRIME_3 + PRIME_4 + PRIME_5 + PRIME_6 +
        PRIME_7 + PRIME_8 + PRIME_9 + PRIME_10 + PRIME_11 + PRIME_12,
    )
  return(new_data)
}
s_proc_NSSIDSMV <- function(data){
  ## DSM-5-based NSSI Diagnosis (NSSI_DSMV)
  new_data <- data %>%
    mutate(
      NSSI_DSMV_1 = RECODE(NSSI_DSMV_1,"1 = 1; 2 = 0;"),
      NSSI_DSMV_2 = as.numeric((NSSI_DSMV_2a + NSSI_DSMV_2b + NSSI_DSMV_2c + NSSI_DSMV_2d) >= 1),
      NSSI_DSMV_2 = ifelse(NSSI_DSMV_2e == 1,0,NSSI_DSMV_2),
      NSSI_DSMV_3 = as.numeric((NSSI_DSMV_3a + NSSI_DSMV_3b + NSSI_DSMV_3c) >= 1),
      NSSI_DSMV_3 = ifelse(NSSI_DSMV_3e == 1,0,NSSI_DSMV_3),
      NSSI_DSMV_4 = RECODE(NSSI_DSMV_4,"1 = 1; 2 = 0;"),
      NSSI_DSMV_5 = RECODE(NSSI_DSMV_5,"1 = 1; 2 = 0;"),
      NSSI_DSMV_6 = RECODE(NSSI_DSMV_6,"1 = 1; 2 = 0;"),
      NSSI_DSMV_Sum = NSSI_DSMV_1 + NSSI_DSMV_2 + NSSI_DSMV_3 + NSSI_DSMV_4 + NSSI_DSMV_5,
      NSSI_DSMV_Sum = ifelse((NSSI_DSMV_1 == 0)|(NSSI_DSMV_6 == 1),0,NSSI_DSMV_Sum),
    )
  return(new_data)
}
s_proc_FASM <- function(data){
  # Functional Assessment of Self-Mutilation (FASM)
  new_data <- data %>%
    mutate(
      FASM_Sum = FASM_1 + FASM_2 + FASM_3 + FASM_4 + FASM_5 +FASM_6 + FASM_7 + FASM_8 + FASM_9 + FASM_10 + FASM_11 + FASM_12,
      FASM_Sum = ifelse((FASM_NotApply == 1)|(FASM_NotKnow == 1),
                        0,FASM_Sum),
    ) 
  return(new_data)
}
s_proc_DBVSS <- function(data){
  # Delaware Bullying Victimization Scale-Student (DBVS-S)
  new_data <- data %>%
    mutate(
      DBVSS_REL = DBVSS_3 + DBVSS_6 + DBVSS_9 + DBVSS_12,
      DBVSS_VER = DBVSS_1 + DBVSS_4 + DBVSS_7 + DBVSS_10,
      DBVSS_PHY = DBVSS_2 + DBVSS_5 + DBVSS_8 + DBVSS_11,
      DBVSS_Sum = DBVSS_1 + DBVSS_2 + DBVSS_3 + DBVSS_4 + DBVSS_5 + DBVSS_6 +
        DBVSS_7 + DBVSS_8 + DBVSS_9 + DBVSS_10 + DBVSS_11 + DBVSS_12,
    )
  return(new_data)
}
s_proc_CBV <- function(data){
  # Cyber-Bullying Victim Questionnaire (CBV)
  new_data <- data %>%
    mutate(
      CBV_Sum = CBV_1 + CBV_2 + CBV_3 + CBV_4 + CBV_5 + 
        CBV_6 + CBV_7 + CBV_8 + CBV_9 + CBV_10,
    ) 
  return(new_data)
}
s_proc_CBP <- function(data){
  # Cyber-Bullying Perpetrator Questionnaire (CBP)
  new_data <- data %>%
    mutate(
      CBP_Sum = CBP_1 + CBP_2 + CBP_3 + CBP_4 + CBP_5 + 
        CBP_6 + CBP_7 + CBP_8 + CBP_9 + CBP_10,
    )
  return(new_data)
}
s_proc_Loneliness <- function(data){
  # UCLA Loneliness Scale (Loneliness)
  new_data <- data %>%
    mutate(
      Loneliness_Sum = Loneliness_1_rev +Loneliness_2 + Loneliness_3 + 
        Loneliness_4 + Loneliness_5_rev + Loneliness_6_rev + Loneliness_7 + 
        Loneliness_8 + Loneliness_9_rev + Loneliness_10_rev + Loneliness_11 + 
        Loneliness_12 + Loneliness_13 + Loneliness_14 + Loneliness_15_rev + 
        Loneliness_16_rev + Loneliness_17 + Loneliness_18 + Loneliness_19_rev + 
        Loneliness_20_rev,
    )
  return(new_data)
}
s_proc_LOI <- function(data){
  # Leyton Obsessive Inventory (LOI)
  new_data <- data %>%
    mutate(
      LOI_CompTho = LOI_1 + LOI_2 + LOI_12 + LOI_13 + LOI_16,
      LOI_ConClea = LOI_4 + LOI_5 + LOI_6 + LOI_7 + LOI_8 +  LOI_9 + LOI_10,
      LOI_RepChek = LOI_3 + LOI_11 + LOI_14 + LOI_17 + LOI_18,
      LOI_SupComp = LOI_15 + LOI_19 + LOI_20,
      LOI_Sum = LOI_CompTho + LOI_ConClea + LOI_RepChek + LOI_SupComp,
    )
  return(new_data)
}

# 3. Assessment Pool: Trait-like Measures ---------------------------------

s_proc_PSS <- function(data){
  # Perceived Stress Scale (PSS)
  new_data <- data %>%
    mutate(
      PSS_TEN = PSS_1 + PSS_2 + PSS_3 + PSS_8 + PSS_12 + PSS_14,
      PSS_LOC = PSS_4_rev + PSS_5_rev + PSS_6_rev + PSS_7_rev + PSS_9_rev + PSS_10_rev + PSS_13_rev,
      PSS_Sum = PSS_TEN + PSS_LOC,
    ) 
  return(new_data)
}
s_proc_BFISF <- function(data){
  # Big Five Personality - Short Form (Adapted from the CFPS study)
  new_data <- data %>%
    mutate(
      BFISF_CON = BFISF_1 + BFISF_7_rev + BFISF_11,
      BFISF_OPE = BFISF_4 + BFISF_9 + BFISF_14,
      BFISF_NEU = BFISF_5 + BFISF_10 + BFISF_15_rev,
      BFISF_EXT = BFISF_2 + BFISF_8 + BFISF_12_rev,
      BFISF_AGR = BFISF_3_rev + BFISF_6 + BFISF_13,
    )
  return(new_data)
}
s_proc_BSCS <- function(data){
  # Brief Self-Control Scale (BSCS)
  new_data <- data %>%
    mutate(
      BSCS_Sum = BSCS_1 + BSCS_2_rev + BSCS_3 + BSCS_4_rev + BSCS_5 + BSCS_6_rev + BSCS_7_rev,
    )
  return(new_data)
}
s_proc_RCBSTC <- function(data){
  # Richmond Compulsive Buying Scale - Translated Chinese (RCBSTC)
  new_data <- data %>%
    mutate(
      RCBSTC_Sum = RCBSTC_1 + RCBSTC_2 + RCBSTC_3 + RCBSTC_4 + RCBSTC_5 + RCBSTC_6,
    ) 
  return(new_data)
}
s_proc_BDERS16 <- function(data){
  # Difficulties in Emotional Regulation Scale 16 items (DERS16)
  new_data <- data %>%
    mutate(
      BDERS16_Sum = BDERS16_1 + BDERS16_2 + BDERS16_3 + BDERS16_4 + BDERS16_5 +
        BDERS16_6 + BDERS16_7 + BDERS16_8 + BDERS16_9 + BDERS16_10 + BDERS16_11 +
        BDERS16_12 + BDERS16_13 + BDERS16_14 + BDERS16_15 + BDERS16_16,
    )
  return(new_data)
}
s_proc_BERS <- function(data){
  # Brief Emoition Reactivity Scale (BERS)
  new_data <- data %>%
    mutate(
      BERS_Sum = BERS_1 + BERS_2 + BERS_3 + BERS_4 + BERS_5 + BERS_6,
    )
  return(new_data)
}
s_proc_SSS <- function(data){
  # Sensation Seeking Scale (SSS, Child Version, 14 items)
  new_data <- data %>%
    mutate(
      SSS_Sum = SSS_1 + SSS_2 + SSS_3 + SSS_4 + SSS_5 + SSS_6 + SSS_7 + SSS_8 +
        SSS_9 + SSS_10 + SSS_11 + SSS_12 + SSS_13 + SSS_14,
    )
  return(new_data)
}
s_proc_BSSSC8 <- function(data){
  # Brief Sensation Seeking Scale - Chinese version
  new_data <- data %>%
    mutate(
      BSSSC8_Sum = BSSSC8_1 + BSSSC8_2 + BSSSC8_3 + BSSSC8_4 + BSSSC8_5 + 
        BSSSC8_6 + BSSSC8_7 + BSSSC8_8,
      )
  return(new_data)
}
s_proc_BISBAS <- function(data){
  # Behavioral Inhibition System/Behavioral Activation System (BISBAS)
  new_data <- data %>%
    mutate(
      BISBAS_BASDrive_Sum = BISBAS_1 + BISBAS_6 + BISBAS_8 + BISBAS_16,
      BISBAS_BASRewardResp_Sum = BISBAS_2 + BISBAS_4 + BISBAS_13 + BISBAS_17,
      BISBAS_BASFunSeek_Sum = BISBAS_3 + BISBAS_7 + BISBAS_10 + BISBAS_11 + BISBAS_15,
      BISBAS_BAS_Sum = BISBAS_BASDrive_Sum + BISBAS_BASRewardResp_Sum + BISBAS_BASFunSeek_Sum,
      BISBAS_BIS_Sum = BISBAS_5 + BISBAS_9 + BISBAS_12 + BISBAS_14 + BISBAS_18,
    )
  return(new_data)
}
s_proc_BIS11 <- function(data){
  # Barret Impusivity Scale - 11 Version (BIS11)
  new_data <- data %>%
    mutate(
      BIS11_NOP = BIS11_1_rev + BIS11_4_rev + BIS11_7_rev + BIS11_10_rev + BIS11_13_rev + BIS11_16_rev + 
        BIS11_19_rev + BIS11_22_rev + BIS11_25_rev + BIS11_28_rev,
      BIS11_COG = BIS11_3_rev + BIS11_6_rev + BIS11_9_rev + BIS11_12_rev + BIS11_15_rev + BIS11_18_rev + 
        BIS11_21_rev + BIS11_24_rev + BIS11_27_rev + BIS11_30_rev,
      BIS11_ACT = BIS11_2 + BIS11_5 + BIS11_8 + BIS11_11 + BIS11_14 + BIS11_17 + BIS11_20 + BIS11_23 + BIS11_26 + BIS11_29,
      BIS11_Sum = BIS11_NOP + BIS11_COG + BIS11_ACT,
    )
  return(new_data)
}
s_proc_RICS <- function(data){
  # Connor-Davidson Resilience Scale (RICS) 
  new_data <- data %>%
    mutate(
      RICS_OPT = RICS_2 + RICS_3 + RICS_4 + RICS_6,
      RICS_STR = RICS_1 + RICS_5 + RICS_7 + RICS_8 + RICS_9 + RICS_10 + RICS_24 + RICS_25,
      RICS_TEN = RICS_11 + RICS_12 + RICS_13 + RICS_14 + RICS_15 + RICS_16 + RICS_17 +
        RICS_18 + RICS_19 + RICS_20_rev + RICS_21 + RICS_22 + RICS_23,
      RICS_Sum = RICS_OPT + RICS_STR + RICS_TEN,
    )
  return(new_data)
}
s_proc_COPE30 <- function(data){
  # 30-item Copying Style Questionnaire (COPE)
  new_data <- data %>%
    mutate(
      COPE_PrS = COPE_8 + COPE_7 + COPE_6 + COPE_3 + COPE_5 + COPE_2 + COPE_4 + COPE_1,
      COPE_HeS = COPE_11 + COPE_12 + COPE_13 + COPE_15 + COPE_10 + COPE_9 + COPE_14,
      COPE_Tol = COPE_28 + COPE_30 + COPE_29,
      COPE_Wit = COPE_18 + COPE_20 + COPE_19 + COPE_17 + COPE_16, 
      COPE_FeV = COPE_24 + COPE_23 + COPE_21 + COPE_22,
      COPE_Fan = COPE_25 + COPE_27 + COPE_26,
      COPE_Pos_Sum = COPE_PrS + COPE_HeS,
      COPE_Neg_Sum = COPE_Tol + COPE_Wit + COPE_FeV + COPE_Fan,
    )
  return(new_data)
}
s_proc_AIMS10 <- function(data){
  # Adjustable Impulsivity Scale (AIMS)
  new_data <- data %>%
    mutate(
      AIMS_Sum = AIMS_1 + AIMS_2 + AIMS_3_rev + AIMS_4 + AIMS_5 + AIMS_6 +
        AIMS_7 + AIMS_8 + AIMS_9_rev + AIMS_10,
    )
  return(new_data)
}

# 4. Assessment Pool: Culture & Environment -------------------------------

s_proc_SKIN9 <- function(data){
  # Informational and Normative Conformity Scale (SKI-N)
  new_data <- data %>%
    mutate(
      SKIN9_Sum = SKIN9_1 + SKIN9_2 + SKIN9_3 + SKIN9_4 + SKIN9_5 + SKIN9_6 + SKIN9_7 + SKIN9_8 + SKIN9_9,
    )
  return(new_data)
}
s_proc_ChildhoodNCE <- function(data){
  # Childhood Neighborhood Collective Efficacy Scale (ChildhoodNCE)
  new_data <- data %>%
    mutate(
      ChildhoodNCE_SCO_Sum = ChildhoodNCE_SCO_1 + ChildhoodNCE_SCO_2 +
        ChildhoodNCE_SCO_3 + ChildhoodNCE_SCO_4 + ChildhoodNCE_SCO_5,
      ChildhoodNCE_ISC_Sum = ChildhoodNCE_ISC_1 + ChildhoodNCE_ISC_2 +
        ChildhoodNCE_ISC_3 + ChildhoodNCE_ISC_4 + ChildhoodNCE_ISC_5,
    )
  return(new_data)
}
s_proc_SSRS <- function(data){
  # Social Support Rating Scale
  new_data <- data %>%
    mutate(
      SSRS_5 = SSRS_5A + SSRS_5B + SSRS_5C + SSRS_5D + SSRS_5E,
      SSRS_6Non = RECODE(SSRS_6Non,"1=0; 2=NA;"),
      SSRS_7Non = RECODE(SSRS_7Non,"1=0; 2=NA;"),
      SSRS_6 = ifelse(is.na(SSRS_6Non),
                      0,SSRS_6a+SSRS_6b+SSRS_6c+SSRS_6d+SSRS_6e+SSRS_6f+SSRS_6g+SSRS_6h),
      SSRS_7 = ifelse(is.na(SSRS_7Non),
                      0,SSRS_7a+SSRS_7b+SSRS_7c+SSRS_7d+SSRS_7e+SSRS_7f+SSRS_7g+SSRS_7h),
      SSRS_Sum = SSRS_1 + SSRS_2 + SSRS_3 + SSRS_4 + SSRS_5 + SSRS_6 + SSRS_7 + SSRS_8 + SSRS_9 + SSRS_10,
    )
  return(new_data)
}
s_proc_IPPA <- function(data){
  # Inventory of Parent and Peer Attachment (IPPA)
  new_data <- data %>%
    mutate(
      IPPA_TRU = IPPA_5 + IPPA_6 + IPPA_8 + IPPA_12 + IPPA_13 + IPPA_14 + IPPA_15 + IPPA_19 + IPPA_20 + IPPA_21,
      IPPA_COM = IPPA_1 + IPPA_2 + IPPA_3 + IPPA_7 + IPPA_16 + IPPA_17 + IPPA_24 + IPPA_25,
      IPPA_DIS = IPPA_4_rev + IPPA_9_rev + IPPA_10_rev + IPPA_11_rev + IPPA_18_rev + IPPA_22_rev + IPPA_23_rev,
      IPPA_Sum = IPPA_TRU + IPPA_COM + IPPA_DIS,
    ) 
  return(new_data)
}
s_proc_sEMBUC <- function(data){
  # Short-form Egna Minnenav Barndoms Uppfostran for Chinese (s-EMBU-C) 
  new_data <- data %>%
    mutate(
      sEMBUC_M_RJ = sEMBUC_M_1 + sEMBUC_M_2 + sEMBUC_M_3 + sEMBUC_M_4 + sEMBUC_M_5 + sEMBUC_M_6,
      sEMBUC_M_EW = sEMBUC_M_7 + sEMBUC_M_8 + sEMBUC_M_9 + sEMBUC_M_10 + sEMBUC_M_11 + sEMBUC_M_12 + sEMBUC_M_13,
      sEMBUC_M_OP = sEMBUC_M_14 + sEMBUC_M_15 + sEMBUC_M_16 + sEMBUC_M_17 + sEMBUC_M_18 + sEMBUC_M_19 + sEMBUC_M_20_rev + sEMBUC_M_21,
      sEMBUC_F_RJ = sEMBUC_F_1 + sEMBUC_F_2 + sEMBUC_F_3 + sEMBUC_F_4 + sEMBUC_F_5 + sEMBUC_F_6,
      sEMBUC_F_EW = sEMBUC_F_7 + sEMBUC_F_8 + sEMBUC_F_9 + sEMBUC_F_10 + sEMBUC_F_11 + sEMBUC_F_12 + sEMBUC_F_13,
      sEMBUC_F_OP = sEMBUC_F_14 + sEMBUC_F_15 + sEMBUC_F_16 + sEMBUC_F_17 + sEMBUC_F_18 + sEMBUC_F_19 + sEMBUC_F_20_rev + sEMBUC_F_21,
    )
  return(new_data)
}
s_proc_FRIENDS <- function(data){
  # Florida State Twin Registry - FRIENDS scale (FRIENDS)
  new_data <- data %>%
    mutate(
      FRIENDS_School_Sum = FRIENDS_1 + FRIENDS_5 + FRIENDS_9 + FRIENDS_13 + FRIENDS_17,
      FRIENDS_BAD_Sum    = FRIENDS_2 + FRIENDS_4 + FRIENDS_6 + FRIENDS_8 + FRIENDS_10 + 
        FRIENDS_12 + FRIENDS_14 + FRIENDS_16 + FRIENDS_18,
      FRIENDS_GOOD_Sum   = FRIENDS_3 + FRIENDS_7 + FRIENDS_11 + FRIENDS_15,
    )
  return(new_data)
}

# 5. Assessment Pool: Physical Health -------------------------------------

s_proc_PSQI <- function(data){
  # Pittsburgh Sleep Quality Index (PSQI)
  new_data <- data %>%
    mutate(
      PSQI_0 = str_replace_all(PSQI_0,c('零点前' = 'Before 0:00 PM','零点后' = 'After 0:00 PM')),
      PSQI_0 = factor(PSQI_0,c('Before 0:00 PM','After 0:00 PM')),
      PSQI_1 = PSQI_1a + PSQI_1b/60,
      PSQI_2 = RECODE(PSQI_2,"0:15 = 0; 16:30 = 1; 31:60=2; 60:hi = 3;"),
      PSQI_3 = PSQI_3a + PSQI_3b/60,
      PSQI_4 = PSQI_4a + PSQI_4b/60,
      PSQI_OnBedDura = ifelse(PSQI_0 == 'Before 0:00 PM',
                              12 - PSQI_1 + PSQI_3,
                              PSQI_3 - PSQI_1),
      PSQI_SleepEffi = PSQI_4/PSQI_OnBedDura * 100,
      PSQI_SleepEffi = ifelse(PSQI_SleepEffi < 0,
                              PSQI_4/(12 - PSQI_1 + PSQI_3) * 100,
                              PSQI_SleepEffi),
      PSQI_SleepEffi = ifelse(PSQI_SleepEffi > 500,
                              PSQI_4/(12 - PSQI_1 + PSQI_3) * 100,
                              PSQI_SleepEffi),
      PSQI_5a = RECODE(PSQI_5a,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_5b = RECODE(PSQI_5b,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_5c = RECODE(PSQI_5c,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_5d = RECODE(PSQI_5d,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_5e = RECODE(PSQI_5e,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_5f = RECODE(PSQI_5f,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_5g = RECODE(PSQI_5g,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_5h = RECODE(PSQI_5h,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_5i = RECODE(PSQI_5i,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_5j = RECODE(PSQI_5j,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_6 = RECODE(PSQI_6,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_7 = RECODE(PSQI_7,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_8 = RECODE(PSQI_8,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_9 = RECODE(PSQI_9,"1=0; 2=1; 3=2; 4=3;"),
      PSQI_CompA_Score = PSQI_6,
      PSQI_CompB_Score = RECODE(PSQI_2 + PSQI_5a,"0 = 0; 1:2 = 1; 3:4 = 2; 5:6 = 3;"),
      PSQI_CompC_Score = RECODE(PSQI_4,"8.00:hi = 0; 6.00:7.99 = 1; 5.00:6.99 = 2; lo:4.99 = 3;"),
      PSQI_CompD_Score = RECODE(PSQI_SleepEffi,"84.99:hi = 0; 74.99:84.99 = 1; 64.99:74.99 = 2; lo:64.99 = 3;"),
      PSQI_CompE_Score = RECODE(PSQI_5b + PSQI_5c + PSQI_5d + PSQI_5e + PSQI_5f + PSQI_5g + PSQI_5h + PSQI_5i + PSQI_5j,
                                "0 = 0; 1:9 = 1; 10:18 = 2; 19:27 = 3;"),
      PSQI_CompF_Score = PSQI_7,
      PSQI_CompG_Score = RECODE(PSQI_8 + PSQI_9,"0 = 0; 1:2 = 1; 3:4 = 2; 5:6 = 3;"),
      PSQI_Sum = PSQI_CompA_Score + PSQI_CompB_Score + PSQI_CompC_Score + PSQI_CompD_Score + PSQI_CompE_Score + PSQI_CompF_Score + PSQI_CompG_Score,
    )
  return(new_data)
}


# 6. Assessment Pool: Identifying CIER ------------------------------------

s_proc_TIS <- function(data){
  # The Infrequency Scale (TIS)
  new_data <- data %>%
    mutate(
      BogusItem_1 = RECODE(BogusItem_1,"1=1; 2=0;"),
      BogusItem_2 = RECODE(BogusItem_2,"1=1; 2=0;"),
      BogusItem_3 = RECODE(BogusItem_3,"1=1; 2=0;"),
      BogusItem_4 = RECODE(BogusItem_4,"1=1; 2=0;"),
      BogusItem_5 = RECODE(BogusItem_5,"1=1; 2=0;"),
      BogusItem_6 = RECODE(BogusItem_6,"1=1; 2=0;"),
      BogusItem_7 = RECODE(BogusItem_7,"1=1; 2=0;"),
      BogusItem_8 = RECODE(BogusItem_8,"1=1; 2=0;"),
      BogusItem_9 = RECODE(BogusItem_9,"1=1; 2=0;"),
      BogusItem_10 = RECODE(BogusItem_10,"1=1; 2=0;"),
      BogusItem_11 = RECODE(BogusItem_11,"1=1; 2=0;"),
      BogusItem_12 = RECODE(BogusItem_12,"1=1; 2=0;"),
      BogusItem_13 = RECODE(BogusItem_13,"1=1; 2=0;"),
      CIER_TIS_Sum = BogusItem_1 + BogusItem_2 + BogusItem_3 + BogusItem_4 + 
        BogusItem_5 + BogusItem_6 + BogusItem_7 + BogusItem_8 + BogusItem_9 + 
        BogusItem_10 + BogusItem_11 + BogusItem_12 + BogusItem_13,
      CIER_TIS_Flag = (CIER_TIS_Sum >= 4)
    )
}

s_proc_TWoBogusItems <- function(data){
  # CIER: Bogus Item Checking (Bogus 1 & 2)
  new_data <- data %>%
    mutate(
      CIER_Bogus1 = (Bogus_1 != 2),
      Bogus_2 = RECODE(Bogus_2,"1=0; 2=1; 3=2; 4=3;"),
      CIER_Bogus2 = (Bogus_2 != PHQ9_2),
    )
  return(new_data)
}

s_proc_RTPageSum <- function(data){
  s_ms2secs <- function(x){
    new_x = x/1000
    return(new_x)
  } 
  ColNo_RTPageSum <- grep('RT_PageSum_\\d+',colnames(data))
  TotalItems_PageLevelRT <- colnames(data)[ColNo_RTPageSum] %>%
    str_remove_all('^RT_PageSum_') %>%
    as.numeric() %>%
    sum()
  fprintf("|%d columns for page-level response time were found|Including %d items|",
          length(ColNo_RTPageSum),TotalItems_PageLevelRT)
  new_data <- data %>%
    mutate(
      across(all_of(colnames(data)[ColNo_RTPageSum]),s_ms2secs)
    )
  fprintf("|Summary Table for page-level response time|")
  print(psych::describe(new_data[,ColNo_RTPageSum]))
  new_data$RT_PageLevel_TPI <- SUM(new_data,vars = colnames(data)[ColNo_RTPageSum])/TotalItems_PageLevelRT
  return(new_data)
}

# s_proc_ <- function(data){
#   # 
#   new_data <- data %>%
#     mutate(
#       
#     )
#   return(new_data)
# }



# s_proc_ <- function(data){
#   # 
#   new_data <- data %>%
#     mutate(
#       
#     )
#   return(new_data)
# }