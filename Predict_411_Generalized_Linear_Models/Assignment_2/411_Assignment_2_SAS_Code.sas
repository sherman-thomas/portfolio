{\rtf1\ansi\ansicpg1252\cocoartf1504\cocoasubrtf810
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\margl1440\margr1440\vieww13660\viewh12500\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 libname mydata '/sscc/home/s/sth932/Auto Insurance';\
\
proc contents data=mydata.logit_insurance;\
run;\
\
proc means data=mydata.logit_insurance(drop=INDEX) n nmiss min Q1 mean median \
		Q3 max stddev qrange ndec=3;\
run;\
\
\
data eda;\
set mydata.logit_insurance;\
if age = "." or " " then\
 age = 45;\
 \
if car_age = "." or car_age= "-3" then\
 car_age = 8; \
\
if home_val = "." or " " then\
 home_val = 161160; \
 \
if income = "." then \
 income = 54028;\
 \
if job = "." or job = " " then\
 job = "z_Blue Collar"; \
 \
if yoj = "." then \
yoj = 11;\
\
run;\
\
proc means data=eda min max Q1 median Q3 mean; run;\
\
proc means data=eda n nmiss printalltypes maxdec=3;\
	where TARGET_FLAG=1;\
run;\
\
proc FREQ data=eda;\
	TABLES TARGET_FLAG/ CROSSLIST TOTPCT;\
run;\
\
proc FREQ data=eda;\
	TABLES KIDSDRIV HOMEKIDS PARENT1 MSTATUS SEX EDUCATION JOB\
	CAR_USE CAR_TYPE RED_CAR REVOKED URBANICITY / CROSSLIST TOTPCT;\
run;\
\
proc FREQ data=eda;\
	where TARGET_FLAG=1;\
	TABLES KIDSDRIV HOMEKIDS PARENT1 MSTATUS SEX EDUCATION JOB\
	CAR_USE CAR_TYPE RED_CAR REVOKED URBANICITY / CROSSLIST TOTPCT;\
run;\
\
title 'TARGET FLAG RECORDS BY VALUE';\
proc sgplot data=eda;\
	histogram TARGET_FLAG;\
run;\
\
\
proc sgscatter data=eda;\
	where target_flag=1;\
	matrix AGE BLUEBOOK CAR_AGE HOME_VAL INCOME \
	TRAVTIME/ diagonal=(histogram normal \
		kernel);\
run;\
\
proc sgscatter data=eda;\
	where target_flag=1;\
	matrix CLM_FREQ	MVR_PTS OLDCLAIM TIF YOJ/ diagonal=(histogram normal \
		kernel);\
run;\
\
data eda_imp; \
 set eda;\
 \
IMP_AGE = AGE;\
IMP_AGE_I = 0;\
if missing(IMP_AGE) then do;\
IMP_AGE = 45;\
IMP_AGE_I=1; end;\
IMP_CAR_AGE = CAR_AGE; IMP_CAR_AGE_I = 0;\
if missing(IMP_CAR_AGE_I) then do;\
IMP_CAR_AGE = 8;\
IMP_CAR_AGE_I = 1; end;\
IMP_HOMEVAL = HOME_VAL; IMP_HOMEVAL_I = 0;\
if missing(IMP_HOMEVAL) then do;\
IMP_HOMEVAL = 161160;\
IMP_HOMEVAL_I=1; end;\
IMP_INCOME = INCOME; IMP_INCOME_I = 0;\
if missing(IMP_INCOME) then do;\
IMP_INCOME = 54028;\
IMP_INCOME_I=1; end;\
IMP_YOJ = YOJ;\
IMP_YOJ_I = 0;\
if missing(IMP_YOJ) then do;\
IMP_YOJ = 11;\
IMP_YOJ_I=1; end;\
if IMP_HOMEVAL=0 then HOMEOWN=0; else do HOMEOWN=1;\
end; \
run;\
\
proc means data=eda_imp n min max Q1 median Q3 mean; run;\
\
proc contents data=eda_imp;\
run;\
\
\
data eda_imp_2; \
 set eda_imp;\
 \
if IMP_AGE <39 then \
AGE_GROUP= 1;\
else if IMP_AGE < 45 then\
 AGE_GROUP = 2; \
 else if IMP_AGE < 51 then\
  AGE_GROUP = 3; \
  else AGE_GROUP=4;\
  \
if BLUEBOOK < 9280 then \
 BLUEBOOK_GROUP = 1;\
 else if BLUEBOOK < 14440 then \
 BLUEBOOK_GROUP = 2; \
 else if BLUEBOOK <20850 then \
 BLUEBOOK_GROUP = 3; \
 else BLUEBOOK_GROUP=4;\
 \
if IMP_CAR_AGE < 4 then\
 CAR_AGE_GROUP = 1;\
 else if IMP_CAR_AGE < 8 then\
 CAR_AGE_GROUP = 2; \
 else if IMP_CAR_AGE < 12 then \
 CAR_AGE_GROUP = 3; \
 else CAR_AGE_GROUP = 4;\
 \
if IMP_HOMEVAL < 154867 then \
 HOMEVAL_GROUP = 1;\
 else if IMP_HOMEVAL < 233352 then\
 HOMEVAL_GROUP = 2; \
 else if IMP_HOMEVAL < 311195 then \
 HOMEVAL_GROUP= 3 ; \
 else HOMEVAL_GROUP = 4;\
\
if IMP_INCOME < 29706 then \
 INCOME_GROUP = 1;\
 else if IMP_INCOME < 57386 then \
 INCOME_GROUP = 2; \
 else if IMP_INCOME < 83303 then \
 INCOME_GROUP = 3; \
 else INCOME_GROUP = 4;\
\
if oldclaim <4636 then \
 OLDCLAIM_GROUP = 1;\
 else if oldclaim < 9583 then \
 OLDCLAIM_GROUP=2; \
 else if oldclaim < 27090 then \
 OLDCLAIM_GROUP=3; \
 else OLDCLAIM_GROUP = 4;\
\
if travtime < 22 then \
COMMUTE = 1;\
else if travtime < 32 then\
 COMMUTE = 2; \
 else if travtime < 44 then \
COMMUTE = 3; \
 else COMMUTE = 4;\
\
\
if mvr_pts = 0 then \
	POINTS = 0;\
	else if mvr_pts <= 2 then \
	POINTS = 1; \
	else POINTS = 2;\
\
if kidsdriv > 0 then \
	DRIVE_KIDS = 1; \
	else DRIVE_KIDS = 0;\
\
if homekids > 0 then \
	KIDS_HOME = 1; \
	else KIDS_HOME = 0;\
\
if sex='z_F' then\
		sex=1;\
	else sex=0;\
\
		\
	if urbanicity='Highly Urban/ Urban' then\
		urbanicity=1;\
	else urbanicity=0;\
\
\
	if mstatus='Yes' then\
		mstatus=1;\
	else\
		mstatus=0;\
\
	if Parent1='Yes' then\
		Parent1=1;\
	else\
		Parent1=0;\
		\
if CAR_USE="Commercial" then\
 CAR_USE="0";\
 else CAR_USE="1";\
 \
if red_car='yes' then\
		red_car=1;\
	else\
		red_car=0;\
\
if revoked='Yes' then\
		revoked=1;\
	else\
		revoked=0;\
 \
run;\
\
proc means data=eda_imp_2 n nmiss min Q1 mean median \
		Q3 max stddev qrange ndec=3;\
run;\
\
proc contents data=eda_imp_2;\
run;\
\
proc FREQ data=eda_imp_2;\
	TABLES HOMEOWN BLUEBOOK_GROUP CAR_AGE_GROUP HOMEVAL_GROUP\
	INCOME_GROUP OLDCLAIM_GROUP TRAVTIME_GROUP COMMUTE POINTS\
	DRIVE_KIDS KIDS_HOME / CROSSLIST TOTPCT;\
run;\
\
proc FREQ data=eda_imp_2;\
	where TARGET_FLAG=1;\
	TABLES HOMEOWN BLUEBOOK_GROUP CAR_AGE_GROUP HOMEVAL_GROUP\
	INCOME_GROUP OLDCLAIM_GROUP TRAVTIME_GROUP COMMUTE POINTS\
	DRIVE_KIDS KIDS_HOME / CROSSLIST TOTPCT;\
run;\
\
************************************************************************;\
\
proc logistic data=eda_imp_2;\
class car_type (param=ref ref='Minivan');\
model target_flag(Event='1')= car_type / clodds = pl;\
run;\
\
\
proc logistic data=eda_imp_2;\
class education (param=ref ref='PhD');\
model target_flag(Event='1')= education/ clodds = pl;\
run;\
\
\
proc logistic data=eda_imp_2;\
class job (param=ref ref='Doctor');\
model target_flag(Event='1')= job/ clodds = pl;\
run;\
\
proc logistic data=eda_imp_2;\
class CAR_USE (param=ref);\
model target_flag(Event='1')= CAR_USE/ clodds = pl;\
run;\
\
proc logistic data=eda_imp_2;\
class sex (param=ref);\
model target_flag(Event='1')= sex/ clodds = pl;\
run;\
\
proc logistic data=eda_imp_2;\
class AGE_GROUP (param=ref);\
model target_flag(Event='1')= AGE_GROUP/ clodds = pl;\
run;\
\
title'CATEGORICAL VARIABLE FREQUENCY BY TARGET_FLAG';\
proc FREQ data=eda_imp_2;\
	TABLES TARGET_FLAG * (AGE AGE_GROUP BLUEBOOK BLUEBOOK_GROUP)/ PLOTS=FREQPLOT;\
run;\
\
proc UNIVARIATE data=eda_imp_2;\
	id TARGET_FLAG;\
	var AGE_GROUP BLUEBOOK_GROUP CAR_AGE_GROUP COMMUTE;\
	histogram;\
	probplot / normal (mu=est sigma=est);\
run;\
\
title 'Where Target_Flag=1 by Predictor';\
proc sgscatter data=eda_imp_2;\
	where target_flag=1;\
	compare y=target_amt x=(AGE_GROUP BLUEBOOK_GROUP INCOME_GROUP OLDCLAIM_GROUP\
	POINTS;\
run;\
\
proc logistic data=eda_imp_2 descending plots(MAXPOINTS=NONE)=ROC(id=prob);\
class AGE_GROUP (param=ref) BLUEBOOK_GROUP (param=ref) CAR_AGE_GROUP (param=ref) COMMUTE (param=ref)\
HOMEVAL_GROUP (param=ref) INCOME_GROUP (param=ref) KIDS_HOME (param=ref) OLDCLAIM_GROUP (param=ref)\
POINTS (param=ref) TRAVTIME_GROUP (param=ref) job (param=ref ref='Doctor') education (param=ref ref='PhD')\
car_type (param=ref ref='Minivan')car_use(param=ref) sex(param=ref) Parent1(param=ref) urbanicity(param=ref)\
revoked(param=ref) mstatus(param=ref) red_car(param=ref) CLM_FREQ(param=ref);\
model target_flag(event='1')= AGE_GROUP BLUEBOOK_GROUP CAR_AGE_GROUP HOMEVAL_GROUP INCOME_GROUP\
KIDS_HOME OLDCLAIM_GROUP POINTS job education car_type car_use sex Parent1 urbanicity revoked mstatus red_car CLM_FREQ\
/ selection= STEPWISE sls=.05 include=0 link=logit;\
output p=phat;\
title 'Logistic Regression Model via Stepwise Selection';\
\
\
\
\
proc logistic data=eda_imp_2 descending plots(MAXPOINTS=NONE)=ROC(id=prob);\
class AGE_GROUP (param=ref) BLUEBOOK_GROUP (param=ref) CAR_AGE_GROUP (param=ref) COMMUTE (param=ref)\
HOMEVAL_GROUP (param=ref) INCOME_GROUP (param=ref) KIDS_HOME (param=ref) OLDCLAIM_GROUP (param=ref)\
POINTS (param=ref) TRAVTIME_GROUP (param=ref) job (param=ref ref='Doctor') education (param=ref ref='PhD')\
car_type (param=ref ref='Minivan')car_use(param=ref) sex(param=ref) Parent1(param=ref) urbanicity(param=ref)\
revoked(param=ref) mstatus(param=ref) red_car(param=ref) CLM_FREQ(param=ref);\
model target_flag(event='1')= AGE_GROUP BLUEBOOK_GROUP CAR_AGE_GROUP HOMEVAL_GROUP INCOME_GROUP\
KIDS_HOME OLDCLAIM_GROUP POINTS job education car_type car_use sex Parent1 urbanicity revoked mstatus red_car CLM_FREQ\
/ selection=backward sls=.05 include=0 link=logit ; output out=pred p=phat ;\
title 'Logistic Regression Model via Backward Selection';\
run;\
\
\
proc logistic data=eda_imp_2 descending plots(MAXPOINTS=NONE)=ROC(id=prob);\
class AGE_GROUP (param=ref) BLUEBOOK_GROUP (param=ref) CAR_AGE_GROUP (param=ref) COMMUTE (param=ref)\
HOMEVAL_GROUP (param=ref) INCOME_GROUP (param=ref) KIDS_HOME (param=ref) OLDCLAIM_GROUP (param=ref)\
POINTS (param=ref) TRAVTIME_GROUP (param=ref) job (param=ref ref='Doctor') education (param=ref ref='PhD')\
car_type (param=ref ref='Minivan')car_use(param=ref) sex(param=ref) Parent1(param=ref) urbanicity(param=ref)\
revoked(param=ref) mstatus(param=ref) red_car(param=ref) CLM_FREQ(param=ref);\
model target_flag(event='1')= AGE_GROUP BLUEBOOK_GROUP CAR_AGE_GROUP HOMEVAL_GROUP INCOME_GROUP\
KIDS_HOME OLDCLAIM_GROUP POINTS job education car_type car_use sex Parent1 urbanicity revoked mstatus red_car CLM_FREQ\
/ selection= Forward sls=.05 include=0 link=logit;\
output p=phat;\
title 'Logistic Regression Model via Forward Selection';\
************************************************************;\
\
\
\
\
\
}