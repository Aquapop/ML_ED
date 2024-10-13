# data_collection.R
# This script extracts data from multiple .XPT files, selects required columns and merges the data(data01_04.csv).
# performs data cleaning, and exports the cleaned data as CSV files (test 5).

# Load necessary libraries
library(dplyr)
library(haven) 

# Function to read and select specific columns from .XPT files (in Data/XPT)
demo.e <- read_xpt("DEMO_C.XPT")
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


bmi.e <- read_xpt("BMI.XPT")
bmi.e.data <- bmi.e %>%
  dplyr::select(SEQN,
                BMXBMI,
                BMXWAIST,
                BMXARMC,
                BMXHT)

Tes.e <- read_xpt("TES.XPT")
Tes.e.data <- Tes.e %>%
  dplyr::select(SEQN,
                SSTESTO)

SMQ.e <- read_xpt("SMQ.XPT")
SMQ.e.data <- SMQ.e %>%
  dplyr::select(SEQN,
                SMQ020)

ALQ.e <- read_xpt("ALQ.XPT")
ALQ.e.data <- ALQ.e %>%
  dplyr::select(SEQN,
                ALQ110)

PAQ.e <- read_xpt("PAQ.XPT")
PAQ.e.data <- PAQ.e %>%
  dplyr::select(SEQN,
                PAD590,
                PAD320)

BPQ.e <- read_xpt("BPQ.XPT")
BPQ.e.data <- BPQ.e %>%
  dplyr::select(SEQN,
                BPQ040A,
                BPQ090D)

DIQ.e <- read_xpt("DIQ.XPT")
DIQ.e.data <- DIQ.e %>%
  dplyr::select(SEQN,
                DIQ010)

MCQ.e <- read_xpt("MCQ.XPT")
MCQ.e.data <- MCQ.e %>%
  dplyr:: select(SEQN,
                 MCQ220,
                 MCQ080)

CAR.e <- read_xpt("CAR.XPT")
CAR.e.data <- CAR.e %>%
  dplyr::select(SEQN,
                LBXCRY,
                LBXLUZ)

VITB.e <- read_xpt("VITB.XPT")
VITB.e.data <- VITB.e %>%
  dplyr::select(SEQN,
                LBDB12SI,
                LBDFOLSI)

VITD.e <- read_xpt("VITD.XPT")
VITD.e.data <- VITD.e %>%
  dplyr::select(SEQN,
                LBDVIDMS)


SEX.e <- read_xpt("SEX.XPT")
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

PSA.e <- read_xpt("PSA.XPT")
PSA.e.data <- PSA.e %>%
  dplyr::select(SEQN,
                LBXP1,
                KIQ115)

SEX1.e <- read_xpt("SEX1.XPT")
SEX1.e.data <- SEX1.e %>%
  dplyr::select(SEQN,
                SXQ280,
                SXD030,
                SXQ170,
                SXQ260,
                SXQ265,
                SXQ270,
                SXQ272)

L13AM.e <- read_xpt("L13AM.XPT")
L13AM.e.data <- L13AM.e %>%
  dplyr::select(SEQN,
                LBXTR)

L13B.e <- read_xpt("L13B.XPT")
L13B.e.data <- L13B.e %>%
  dplyr::select(SEQN,
                LBDHDL)

HB.e <- read_xpt("HB.XPT")
HB.e.data <- HB.e %>%
  dplyr::select(SEQN,
                LBDLYMNO,
                LBXHGB,
                LBDNENO,
                LBXPLTSI)

GLU.e <- read_xpt("GLU.XPT")
GLU.e.data <- GLU.e %>%
  dplyr::select(SEQN,
                LBXGLU,
                WTSAF2YR)

SSQ.e <- read_xpt("SSQ.XPT")
SSQ.e.data <- SSQ.e %>%
  dplyr:: select(SEQN,
                 SSD021A)

DEP.e <- read_xpt("DEP.XPT")
DEP.e.data <- DEP.e %>%
  dplyr:: select(SEQN,
                 CIDDSCOR)

GAD.e <- read_xpt("GAD.XPT")
GAD.e.data <- GAD.e %>%
  dplyr:: select(SEQN,
                 CIDGSCOR)

PANIC.e <- read_xpt("PANIC.XPT")
PANIC.e.data <- PANIC.e %>%
  dplyr:: select(SEQN,
                 CIDPSCOR)

CDQ.e <- read_xpt("CDQ.XPT")
CDQ.e.data <- CDQ.e %>%
  dplyr:: select(SEQN,
                 CDQ001,
                 CDQ010)

CVX.e <- read_xpt("CVX.XPT")
CVX.e.data <- CVX.e %>%
  dplyr:: select(SEQN,
                 CVDFITLV)

#merge to a dataset
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

# Repeat the process for Year 0304 (In Data/XPT0304)
demo.e <- read_xpt("DEMO_C.XPT")
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


bmi.e <- read_xpt("BMI.XPT")
bmi.e.data <- bmi.e %>%
  dplyr::select(SEQN,
                BMXBMI,
                BMXWAIST,
                BMXARMC,
                BMXHT)

Tes.e <- read_xpt("TES.XPT")
Tes.e.data <- Tes.e %>%
  dplyr::select(SEQN,
                SSTESTO)

SMQ.e <- read_xpt("SMQ.XPT")
SMQ.e.data <- SMQ.e %>%
  dplyr::select(SEQN,
                SMQ020)

ALQ.e <- read_xpt("ALQ.XPT")
ALQ.e.data <- ALQ.e %>%
  dplyr::select(SEQN,
                ALQ110)

PAQ.e <- read_xpt("PAQ.XPT")
PAQ.e.data <- PAQ.e %>%
  dplyr::select(SEQN,
                PAD590,
                PAD320)

BPQ.e <- read_xpt("BPQ.XPT")
BPQ.e.data <- BPQ.e %>%
  dplyr::select(SEQN,
                BPQ040A,
                BPQ090D)

DIQ.e <- read_xpt("DIQ.XPT")
DIQ.e.data <- DIQ.e %>%
  dplyr::select(SEQN,
                DIQ010)

MCQ.e <- read_xpt("MCQ.XPT")
MCQ.e.data <- MCQ.e %>%
  dplyr:: select(SEQN,
                 MCQ220,
                 MCQ080)

CAR.e <- read_xpt("CAR.XPT")
CAR.e.data <- CAR.e %>%
  dplyr::select(SEQN,
                LBXCRY,
                LBXLUZ)

VITB.e <- read_xpt("VITB.XPT")
VITB.e.data <- VITB.e %>%
  dplyr::select(SEQN,
                LBDB12SI,
                LBDFOLSI)

VITD.e <- read_xpt("VITD.XPT")
VITD.e.data <- VITD.e %>%
  dplyr::select(SEQN,
                LBDVIDMS)


SEX.e <- read_xpt("SEX.XPT")
SEX.e.data <- SEX.e %>%
  dplyr::select(SEQN,
                KIQ400,
                KIQ081,
                KIQ101,
                KIQ106,
                KIQ121,
                KIQ141,
                KIQ161,
                KIQ182,
                KIQ321,
                KIQ341)

PSA.e <- read_xpt("PSA.XPT")
PSA.e.data <- PSA.e %>%
  dplyr::select(SEQN,
                LBXP1,
                KIQ115)

SEX1.e <- read_xpt("SEX1.XPT")
SEX1.e.data <- SEX1.e %>%
  dplyr::select(SEQN,
                SXQ280,
                SXD030,
                SXQ170,
                SXQ260,
                SXQ265,
                SXQ270,
                SXQ272)

L13AM.e <- read_xpt("L13AM.XPT")
L13AM.e.data <- L13AM.e %>%
  dplyr::select(SEQN,
                LBXTR)

L13B.e <- read_xpt("L13B.XPT")
L13B.e.data <- L13B.e %>%
  dplyr::select(SEQN,
                LBXHDD)

GLU.e <- read_xpt("GLU.XPT")
GLU.e.data <- GLU.e %>%
  dplyr::select(SEQN,
                LBXGLU,
                WTSAF2YR)

HB.e <- read_xpt("L25_C.XPT")
HB.e.data <- HB.e %>%
  dplyr::select(SEQN,
                LBDLYMNO,
                LBXHGB,
                LBDNENO,
                LBXPLTSI)

SSQ.e <- read_xpt("SSQ.XPT")
SSQ.e.data <- SSQ.e %>%
  dplyr:: select(SEQN,
                 SSQ021A)

DEP.e <- read_xpt("DEP.XPT")
DEP.e.data <- DEP.e %>%
  dplyr:: select(SEQN,
                 CIDDSCOR)

GAD.e <- read_xpt("GAD.XPT")
GAD.e.data <- GAD.e %>%
  dplyr:: select(SEQN,
                 CIDGSCOR)

PANIC.e <- read_xpt("PANIC.XPT")
PANIC.e.data <- PANIC.e %>%
  dplyr:: select(SEQN,
                 CIDPSCOR)

CDQ.e <- read_xpt("CDQ.XPT")
CDQ.e.data <- CDQ.e %>%
  dplyr:: select(SEQN,
                 CDQ001,
                 CDQ010)

CVX.e <- read_xpt("CVX.XPT")
CVX.e.data <- CVX.e %>%
  dplyr:: select(SEQN,
                 CVDFITLV)


data0304 <- full_join(demo.e.data,bmi.e.data,by="SEQN")
data0304 <- full_join(data0304,Tes.e.data,by="SEQN")
data0304 <- full_join(data0304,SMQ.e.data,by="SEQN")
data0304 <- full_join(data0304,ALQ.e.data,by="SEQN")
data0304 <- full_join(data0304,PAQ.e.data,by="SEQN")
data0304 <- full_join(data0304,BPQ.e.data,by="SEQN")
data0304 <- full_join(data0304,DIQ.e.data,by="SEQN")
data0304 <- full_join(data0304,MCQ.e.data,by="SEQN")
data0304 <- full_join(data0304,CAR.e.data,by="SEQN")
data0304 <- full_join(data0304,VITB.e.data,by="SEQN")
data0304 <- full_join(data0304,VITD.e.data,by="SEQN")
data0304 <- full_join(data0304,SEX.e.data,by="SEQN")
data0304 <- full_join(data0304,GLU.e.data,by="SEQN")
data0304 <- full_join(data0304,PSA.e.data,by="SEQN")
data0304 <- full_join(data0304,SEX1.e.data,by="SEQN")
data0304 <- full_join(data0304,L13AM.e.data,by="SEQN")
data0304 <- full_join(data0304,L13B.e.data,by="SEQN")
data0304 <- full_join(data0304,HB.e.data,by="SEQN")
data0304 <- full_join(data0304,SSQ.e.data,by="SEQN")
data0304 <- full_join(data0304,DEP.e.data,by="SEQN")
data0304 <- full_join(data0304,GAD.e.data,by="SEQN")
data0304 <- full_join(data0304,PANIC.e.data,by="SEQN")
data0304 <- full_join(data0304,CDQ.e.data,by="SEQN")
data0304 <- full_join(data0304,CVX.e.data,by="SEQN")

# Adjust and merge columns
colnames(data0102)
colnames(data0304)
colnames(data0102) <- colnames(data0304)
data01_04 <- rbind(data0102, data0304)
colnames(data01_04)

write.csv(data01_04,"data01_04.csv")

#exclude female
test1 <- data01_04 %>%
  filter(!(RIAGENDR == 2)) %>%
  as.data.frame()

#exclude <20 years
test2 <- test1 %>%
  filter(RIDAGEEX >= 240 | is.na(RIDAGEEX)) %>%
  as.data.frame()

#exclude respondents with hypertension/high cholesterol/diabetes/cancer/prostate disease
#cardiovascular conditions/overweight/sexual condition who answered "don't know" or "refused"
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
  as.data.frame()

##exclude respondents with Marital Status/physical activity/alcohol/smoking/ED who answered "don't know" or "refused"
#婚姻/活动/吸烟饮酒  ED量表中不知道和拒绝情况的排除
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

#merge STI diseases
test5 <- test4 %>%
  mutate(STI = case_when(
    SXQ260 == 1 | SXQ265 == 1 | SXQ270 == 1 | SXQ272 == 1 ~ 1,
    SXQ260 == 2 & SXQ265 == 2 & SXQ270 == 2 & SXQ272 == 2 ~ 2,
    SXQ260 == 7 | SXQ265 == 7 | SXQ270 == 7 | SXQ272 == 7 ~ 7,
    SXQ260 == 9 | SXQ265 == 9 | SXQ270 == 9 | SXQ272 == 9 ~ 9,
    TRUE ~ NA_integer_ 
  ))
test5 <- test5 %>%
  filter(STI != 7 | is.na(STI)) %>%
  filter(STI != 9 | is.na(STI)) %>%
  as.data.frame() 

#merge to ProstateExam
test5 <- test5 %>%
  mutate(ProstateExam = case_when(
    KIQ321 == 1 | KIQ341 == 1 ~ 1,
    KIQ321 == 2 & KIQ341 == 2 ~ 2,
    TRUE ~ NA_integer_ 
  ))  


#Adjust the target variable, remove missing values
test5$KIQ400 <- ifelse(test5$KIQ400 < 3, 0, test5$KIQ400)
test5$KIQ400 <- ifelse(test5$KIQ400 >= 3, 1, test5$KIQ400)
test5 <- test5 %>% filter(!is.na(KIQ400))

write.csv(test5,"test5.csv")   
test5=read.csv('test5.csv') 
