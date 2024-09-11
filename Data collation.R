# Extracting NHANES data from 2001-2004
# Reading data from xpt files
# 01-02

# Demographic data
demo.e <- read_xpt("E:/ED/0102/xpt/DEMO_C.XPT")
demo.e.data <- demo.e %>% 
  dplyr::select(SEQN,
         RIDAGEEX,
         RIAGENDR,
         RIDRETH1,
         DMDEDUC,
         DMDMARTL,
         WTMEC2YR,
         SDMVPSU,
         SDMVSTRA,
         INDFMPIR,
         DMDHHSIZ)

# BMI data
bmi.e <- read_xpt("E:/ED/0102/xpt/BMI.XPT")
bmi.e.data <- bmi.e %>%
  dplyr::select(SEQN,
         BMXBMI,
         BMXWAIST,
         BMXARMC,
         BMXHT)

# Testosterone data
Tes.e <- read_xpt("E:/ED/0102/xpt/TES.XPT")
Tes.e.data <- Tes.e %>%
  dplyr::select(SEQN,
         SSTESTO)

# Smoking data
SMQ.e <- read_xpt("E:/ED/0102/xpt/SMQ.XPT")
SMQ.e.data <- SMQ.e %>%
  dplyr::select(SEQN,
         SMQ020)

# Alcohol use data
ALQ.e <- read_xpt("E:/ED/0102/xpt/ALQ.XPT")
ALQ.e.data <- ALQ.e %>%
  dplyr::select(SEQN,
         ALQ110)

# Physical activity data
PAQ.e <- read_xpt("E:/ED/0102/xpt/PAQ.XPT")
PAQ.e.data <- PAQ.e %>%
  dplyr::select(SEQN,
         PAD590,
         PAD320)

# Blood pressure data
BPQ.e <- read_xpt("E:/ED/0102/xpt/BPQ.XPT")
BPQ.e.data <- BPQ.e %>%
  dplyr::select(SEQN,
         BPQ040A,
         BPQ090D)

# Diabetes data
DIQ.e <- read_xpt("E:/ED/0102/xpt/DIQ.XPT")
DIQ.e.data <- DIQ.e %>%
  dplyr::select(SEQN,
         DIQ010)

# Medical conditions data
MCQ.e <- read_xpt("E:/ED/0102/xpt/MCQ.XPT")
MCQ.e.data <- MCQ.e %>%
  dplyr:: select(SEQN,
         MCQ220,
         MCQ080)

# Carotenoids data
CAR.e <- read_xpt("E:/ED/0102/xpt/CAR.XPT")
CAR.e.data <- CAR.e %>%
  dplyr::select(SEQN,
         LBXCRY,
         LBXLUZ)

# Vitamin B data
VITB.e <- read_xpt("E:/ED/0102/xpt/VITB.XPT")
VITB.e.data <- VITB.e %>%
  dplyr::select(SEQN,
         LBDB12SI,
         LBDFOLSI)

# Vitamin D data
VITD.e <- read_xpt("E:/ED/0102/xpt/VITD.XPT")
VITD.e.data <- VITD.e %>%
  dplyr::select(SEQN,
         LBDVIDMS)

# Sexual health data
SEX.e <- read_xpt("E:/ED/0102/xpt/SEX.XPT")
SEX.e.data <- SEX.e %>%
  dplyr::select(SEQN,
         KIQ400,
         KIQ081,
         KIQ101,
         KIQ106,
         KIQ121,
         KIQ141,
         KIQ161,
         KID182,
         KIQ321,
         KIQ341)

# Prostate-specific antigen data
PSA.e <- read_xpt("E:/ED/0102/xpt/PSA.XPT")
PSA.e.data <- PSA.e %>%
  dplyr::select(SEQN,
         LBXP1,
         KIQ115)

# Additional sexual health data
SEX1.e <- read_xpt("E:/ED/0102/xpt/SEX1.XPT")
SEX1.e.data <- SEX1.e %>%
  dplyr::select(SEQN,
         SXQ280,
         SXD030,
         SXQ170,
         SXQ260,
         SXQ265,
         SXQ270,
         SXQ272)

# Lipid and amino acids data
L13AM.e <- read_xpt("E:/ED/0102/xpt/L13AM.XPT")
L13AM.e.data <- L13AM.e %>%
  dplyr::select(SEQN,
         LBXTR)

# More lipid data
L13B.e <- read_xpt("E:/ED/0102/xpt/L13B.XPT")
L13B.e.data <- L13B.e %>%
  dplyr::select(SEQN,
         LBDHDL)

# Hematology data
HB.e <- read_xpt("E:/ED/0102/xpt/HB.XPT")
HB.e.data <- HB.e %>%
  dplyr::select(SEQN,
         LBDLYMNO,
         LBXHGB,
         LBDNENO,
         LBXPLTSI)

# Glucose data
GLU.e <- read_xpt("E:/ED/0102/xpt/GLU.XPT")
GLU.e.data <- GLU.e %>%
  dplyr::select(SEQN,
                LBXGLU,
                WTSAF2YR)

# Sleep data
SSQ.e <- read_xpt("E:/ED/0102/xpt/SSQ.XPT")
SSQ.e.data <- SSQ.e %>%
  dplyr:: select(SEQN,
                 SSD021A)

# Depression score data
DEP.e <- read_xpt("E:/ED/0102/xpt/DEP.XPT")
DEP.e.data <- DEP.e %>%
  dplyr:: select(SEQN,
                 CIDDSCOR)

# Anxiety score data
GAD.e <- read_xpt("E:/ED/0102/xpt/GAD.XPT")
GAD.e.data <- GAD.e %>%
  dplyr:: select(SEQN,
                 CIDGSCOR)

# Panic score data
PANIC.e <- read_xpt("E:/ED/0102/xpt/PANIC.XPT")
PANIC.e.data <- PANIC.e %>%
  dplyr:: select(SEQN,
                 CIDPSCOR)

# Chronic disease questions
CDQ.e <- read_xpt("E:/ED/0102/xpt/CDQ.XPT")
CDQ.e.data <- CDQ.e %>%
  dplyr:: select(SEQN,
                 CDQ001,
                 CDQ010)

# Cardiovascular fitness data
CVX.e <- read_xpt("E:/ED/0102/xpt/CVX.XPT")
CVX.e.data <- CVX.e %>%
  dplyr:: select(SEQN,
                 CVDFITLV)

# Merging all extracted data into one table
data0102 <- full_join(demo.e.data,bmi.e.data,by="SEQN")
data0102 <- full_join(data0102,Tes.e.data,by="SEQN")
data0102 <- full_join(data0102,SMQ.e.data,by="SEQN")
data0102 <- full_join(data0102,ALQ.e.data,by="SEQN")
data0102 <- full_join(data0102,PAQ.e.data,by="SEQN")
data0102 <- full_join(data0102,BPQ.e.data,by="SEQN")
data0102 <- full_join(data0102,DIQ.e.data,by="SEQN")
data0102 <- full_join(data0102,MCQ.e.data,by="SEQN")
data0102 <- full_join(data0102,CAR.e.data,by="SEQN")
data0102 <- full_join(data0102,VITB.e.data,by="SEQN")
data0102 <- full_join(data0102,VITD.e.data,by="SEQN")
data0102 <- full_join(data0102,SEX.e.data,by="SEQN")
data0102 <- full_join(data0102,GLU.e.data,by="SEQN")
data0102 <- full_join(data0102,PSA.e.data,by="SEQN")
data0102 <- full_join(data0102,SEX1.e.data,by="SEQN")
data0102 <- full_join(data0102,L13AM.e.data,by="SEQN")
data0102 <- full_join(data0102,L13B.e.data,by="SEQN")
data0102 <- full_join(data0102,HB.e.data,by="SEQN")
data0102 <- full_join(data0102,SSQ.e.data,by="SEQN")
data0102 <- full_join(data0102,DEP.e.data,by="SEQN")
data0102 <- full_join(data0102,GAD.e.data,by="SEQN")
data0102 <- full_join(data0102,PANIC.e.data,by="SEQN")
data0102 <- full_join(data0102,CDQ.e.data,by="SEQN")
data0102 <- full_join(data0102,CVX.e.data,by="SEQN")

# Repeat the process for 03-04 data
# [Similar steps as above for 03-04 data files]

# Merge 01-02 and 03-04 datasets
colnames(data0102) <- colnames(data0304)
data01_04 <- rbind(data0102, data0304)

# Data cleaning

# Exclude females
test1 <- data01_04 %>%
  filter(!(RIAGENDR == 2)) %>%
  as.data.frame()

# Exclude individuals under 20 years old
test2 <- test1 %>%
  filter(RIDAGEEX >= 240 | is.na(RIDAGEEX)) %>%
  as.data.frame()

# Exclude cases with unknown or refusal responses for high blood pressure, high cholesterol, diabetes, cancer, prostate disease, cardiovascular conditions, circumcision, overweight, sexual partner's age, number of sexual partners, and partner support
test3 <- test2 %>%
  filter(BPQ040A != 7 | is.na(BPQ040A)) %>%
  filter(BPQ040A != 9 | is.na(BPQ040A)) %>%
  filter(BPQ090D != 7 | is.na(BPQ090D)) %>%
  filter(BPQ090D != 9 | is.na(BPQ090D)) %>%
  filter(DIQ010 != 7 | is.na(DIQ010)) %>%
  filter(DIQ010 != 9 | is.na(DIQ010)) %>%
  filter(DIQ010 != 3 | is.na(DIQ010)) %>%
  filter(MCQ220 != 7 | is.na(MCQ220)) %>%
  filter(MCQ220 != 9 | is.na(MCQ220)) %>%
  filter(MCQ080 != 7 | is.na(MCQ080)) %>%
  filter(MCQ080 != 9 | is.na(MCQ080)) %>%
  filter(KIQ081 != 7 | is.na(KIQ081)) %>%
  filter(KIQ081 != 9 | is.na(KIQ081)) %>%
  filter(KIQ101 != 7 | is.na(KIQ101)) %>%
  filter(KIQ101 != 9 | is.na(KIQ101)) %>%
  filter(KIQ106 != 7 | is.na(KIQ106)) %>%
  filter(KIQ106 != 9 | is.na(KIQ106)) %>%
  filter(KIQ121 != 7 | is.na(KIQ121)) %>%
  filter(KIQ121 != 9 | is.na(KIQ121)) %>%
  filter(KIQ141 != 7 | is.na(KIQ141)) %>%
  filter(KIQ141 != 9 | is.na(KIQ141)) %>%
  filter(KIQ182 != 7 | is.na(KIQ182)) %>%
  filter(KIQ182 != 9 | is.na(KIQ182)) %>%
  filter(KIQ321 != 7 | is.na(KIQ321)) %>%
  filter(KIQ321 != 9 | is.na(KIQ321)) %>%
  filter(KIQ341 != 7 | is.na(KIQ341)) %>%
  filter(KIQ341 != 9 | is.na(KIQ341)) %>%
  filter(KIQ115 != 7 | is.na(KIQ115)) %>%
  filter(KIQ115 != 9 | is.na(KIQ115)) %>%
  filter(SXQ280 != 7 | is.na(SXQ280)) %>%
  filter(SXQ280 != 9 | is.na(SXQ280)) %>%
  filter(SXQ260 != 7 | is.na(SXQ260)) %>%
  filter(SXQ260 != 9 | is.na(SXQ260)) %>%
  filter(SXQ265 != 7 | is.na(SXQ265)) %>%
  filter(SXQ265 != 9 | is.na(SXQ265)) %>%
  filter(SXQ270 != 7 | is.na(SXQ270)) %>%
  filter(SXQ270 != 9 | is.na(SXQ270)) %>%
  filter(SXQ272 != 7 | is.na(SXQ272)) %>%
  filter(SXQ272 != 9 | is.na(SXQ272)) %>%
  filter(CDQ001 != 7 | is.na(CDQ001)) %>%
  filter(CDQ001 != 9 | is.na(CDQ001)) %>%
  filter(CDQ010 != 7 | is.na(CDQ010)) %>%
  filter(CDQ010 != 9 | is.na(CDQ010)) %>%
  filter(SXD030 != 777 | is.na(SXD030)) %>%
  filter(SXD030 != 999 | is.na(SXD030)) %>%
  filter(SXQ170 != 77777 | is.na(SXQ170)) %>%
  filter(SXQ170 != 99999 | is.na(SXQ170)) %>%
  filter(SSQ021A != 77 | is.na(SSQ021A)) %>%
  filter(SSQ021A != 99 | is.na(SSQ021A)) %>%
  as.data.frame()

# Exclude cases with unknown or refusal responses for marriage, activity, smoking, and alcohol
test4 <- test3 %>%
  filter(DMDMARTL != 77 | is.na(DMDMARTL)) %>%
  filter(DMDMARTL != 99 | is.na(DMDMARTL)) %>%
  filter(DMDEDUC != 7 | is.na(DMDEDUC)) %>%
  filter(DMDEDUC != 9 | is.na(DMDEDUC)) %>%
  filter(PAD320 != 9 | is.na(PAD320)) %>%
  filter(PAD320 != 7 | is.na(PAD320)) %>%
  filter(PAD590 != 77 | is.na(PAD590)) %>%
  filter(PAD590 != 99 | is.na(PAD590)) %>%
  filter(SMQ020 != 9 | is.na(SMQ020)) %>%
  filter(SMQ020 != 7 | is.na(SMQ020)) %>%
  filter(ALQ110 != 7 | is.na(ALQ110)) %>%
  filter(ALQ110 != 9 | is.na(ALQ110)) %>%
  filter(KIQ400 != 7 | is.na(KIQ400)) %>%
  filter(KIQ400 != 9 | is.na(KIQ400)) %>%
  as.data.frame()

# Combine STI4 rows into one row
test5 <- test4 %>%
  mutate(STI = case_when(
    SXQ260 == 1 | SXQ265 == 1 | SXQ270 == 1 | SXQ272 == 1 ~ 1,
    SXQ260 == 2 & SXQ265 == 2 & SXQ270 == 2 & SXQ272 == 2 ~ 2,
    SXQ260 == 7 | SXQ265 == 7 | SXQ270 == 7 | SXQ272 == 7 ~ 7,
    SXQ260 == 9 | SXQ265 == 9 | SXQ270 == 9 | SXQ272 == 9 ~ 9,
    TRUE ~ NA_integer_ # For other cases, such as NA or all values being NA, mark as NA
  ))
test5 <- test5 %>%
  filter(STI != 7 | is.na(STI)) %>%
  filter(STI != 9 | is.na(STI)) %>%
  as.data.frame()

# Combine Mental Health into one
test5 <- test5 %>%
  mutate(MentalHealth = case_when(
    CIDDSCOR == 1 | CIDGSCOR == 1 | CIDPSCOR == 1 ~ 1,
    CIDDSCOR == 5 & CIDGSCOR == 5 & CIDPSCOR == 5 ~ 2,
    TRUE ~ NA_integer_ # For other cases, such as NA or all values being NA, mark as NA
  ))

# Combine Prostate Exam into one
test5 <- test5 %>%
  mutate(ProstateExam = case_when(
    KIQ321 == 1 | KIQ341 == 1 ~ 1,
    KIQ321 == 2 & KIQ341 == 2 ~ 2,
    TRUE ~ NA_integer_ # For other cases, such as NA or all values being NA, mark as NA
  ))

# Adjust target variable, remove missing values of the target variable
test5$KIQ400 <- ifelse(test5$KIQ400 < 3, 0, test5$KIQ400)
test5$KIQ400 <- ifelse(test5$KIQ400 >= 3, 1, test5$KIQ400)
test5 <- test5 %>% filter(!is.na(KIQ400))

write.csv(test5,"test5.csv")   
# In EXCEL, calculate METS-VF, METS-IR, WHtR, SII

test5=read.csv('test5.csv') 

# Check if data follows a normal distribution
results <- lapply(test5, function(x) {
  # Remove NA and check the number of unique values
  unique_non_na <- unique(na.omit(x))
  if (length(unique_non_na) > 1) {
    tryCatch(
      shapiro.test(x),
      error = function(e) NA  # If an error still occurs, return NA
    )
  } else {
    NA  # For insufficient unique values, return NA
  }
})
print(results)# All continuous variables do not follow a normal distribution

# Log-transform continuous variables
# First, convert categorical variables to factors
factorCols <- c(
  "RIDRETH1",
  "DMDEDUC",
  "DMDMARTL",
  "DMDHHSIZ",
  "SMQ020",
  "ALQ110",
  "PAD590",
  "PAD320",
  "KIQ400",
  "BPQ040A",
  "BPQ090D",
  "DIQ010",
  "MCQ220",
  "KIQ081",
  "KIQ101",
  "KIQ106",
  "KIQ121",
  "KIQ141",
  "KIQ182",
  "KIQ321",
  "KIQ341",
  "KIQ115",
  "SXQ280",
  "SXQ260",
  "SXQ265",
  "SXQ270",
  "SXQ272",
  "STI",
  "CIDDSCOR",
  "CIDGSCOR",
  "CIDPSCOR",
  "MentalHealth",
  "ProstateExam",
  "CDQ001",
  "CDQ010",
  "CVDFITLV")

# Batch convert columns to factor variables
for (i in factorCols) {
  test5[, i] <- as.factor(test5[, i])  
}  

str(test5)

# Then perform a log transformation on continuous variables
test6 <- test5 %>%
  mutate_if(is.numeric, ~log(. + 1))

str(test6)

# Conduct weighted comparisons between ED group and non-ED group in NHANES data
datat = read.csv("E:/ED/test.csv")

# Set up survey design
design <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~Weight, data = datat, nest = TRUE)

# Categorical variables
chi.test.12 <- svychisq(~ var + ED, subset(design, ED %in% c("0","1")))
print(chi.test.12)

# Calculate percentage of categorical variables by ED group
test5 %>%
  count(ED, var) %>%
  group_by(ED) %>%
  mutate(percentage = n / sum(n) * 100)

# Continuous variables
t_test_result <- svyttest(var ~ ED, design)
print(t_test_result)

# Calculate mean and standard deviation of continuous variables by ED group
test5 %>%
  group_by(ED) %>%
  summarise(
    mean = mean(var, na.rm = TRUE),
    sd = sd(var, na.rm = TRUE)
  )







