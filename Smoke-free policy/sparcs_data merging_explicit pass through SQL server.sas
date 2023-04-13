
/* Explicit Pass Through in SQL server to set length for Character String */
/* Created by Elle Anastasiou */
/* Last edited on 10/28/2022 */

OPTIONS SASTRACE=',,,d' SASTRACELOC=SASLOG NOSTSUFFIX; 

/* QUERY WITH LIBNAME STATEMENT - IMPLICIT */
LIBNAME WKSP IMPALA DSN=HADOOP_PROD SCHEMA=SPARCS_THORPE_LAB_WKSP; RUN;
PROC CONTENTS DATA= WKSP.sparcs_full_29 VARNUM; RUN; /* HERE, THE CHAR VAR ALL HAVE LENGTH AND FORMATS OF 32767 - THIS IS DEFAULT FOR HIVE DATA TYPE FROM HADOOP */


/* PASS-THROUGH QUERY SYNTAX FOR IMPALA INTERFACE */
PROC SQL; 
CONNECT TO IMPALA (DSN=HADOOP_PROD);
CREATE TABLE WORK.NEW AS 
SELECT

cbg_comp_stroke_quarter length 2 format $2. informat $2. 
,cbg_int_stroke_quarter length 2 format $2. informat $2. 
,cbg_comp_mi_quarter length 2 format $2. informat $2. 
,cbg_int_mi_quarter length 2 format $2. informat $2.
,cbg_comp_asthma_quarter length 2 format $2. informat $2. 
,cbg_int_asthma_quarter length 2 format $2. informat $2.
,age_cat_2 length 2 format $2. informat $2. 
,cbg_int_stroke length 2 format $2. informat $2. 
,cbg_int_mi length 2 format $2. informat $2. 
,cbg_comp_stroke length 2 format $2. informat $2. 
,adult_stroke length 2 format $2. informat $2.
,stroke length 2 format $2. informat $2. 
,cbg_comp_mi length 2 format $2. informat $2.
,adult_mi length 2 format $2. informat $2. 
,mi length 2 format $2. informat $2. 
,cbg_comp_asthma length 2 format $2. informat $2.
,cbg_int_asthma length 2 format $2. informat $2. 
,ped_asthma length 2 format $2. informat $2. 
,asthma length 2 format $2. informat $2.
,race_eth length 5 format $5. informat $5. 
,age_cat length 2 format $2. informat $2.
,quarter length 3 format $3. informat $3. 
,month length 4 format $4. informat $4.
,clm_trans_id length 8 format 20. informat 20.
,cbg length 12 format $12. informat $12.
,gs_bbl length 12 format $12. informat $12.
,cbg_int length 2 format $2. informat $2.
,geoid_1 length 12 format $12. informat $12.
,cbg_ctrl length 2 format $2. informat $2.
,disch_yr length 8 format 11. informat 11.
,disch_dt length 8 format 11. informat 11.
,age length 8 format 11. informat 11. 
,gender_cd length 2 format $2. informat $2.
,emerg_ind length 2 format $2. informat $2. 
,dx_cd length 10 format $10. informat $10. 
,pat_ethnic_cd length 5 format $5. informat $5.
,pat_race_cd length 5 format $5. informat $5. 
,race_ethnic_cd length 5 format $5. informat $5.
,admit_dt length 8 format $8. informat $8. 

FROM CONNECTION TO IMPALA
(SELECT 
cbg_comp_stroke_quarter 
,cbg_int_stroke_quarter 
,cbg_comp_mi_quarter 
,cbg_int_mi_quarter 
,cbg_comp_asthma_quarter  
,cbg_int_asthma_quarter 
,age_cat_2 
,cbg_int_stroke  
,cbg_int_mi 
,cbg_comp_stroke 
,adult_stroke 
,stroke 
,cbg_comp_mi 
,adult_mi 
,mi 
,cbg_comp_asthma 
,cbg_int_asthma  
,ped_asthma  
,asthma 
,race_eth 
,age_cat 
,quarter 
,month 
,clm_trans_id 
,cbg 
,gs_bbl 
,cbg_int 
,geoid_1 
,cbg_ctrl  
,disch_yr  
,disch_dt 
,age 
,gender_cd 
,emerg_ind  
,dx_cd  
,pat_ethnic_cd 
,pat_race_cd  
,race_ethnic_cd 
,admit_dt 

FROM SPARCS_THORPE_LAB_WKSP.sparcs_full_29);
DISCONNECT FROM IMPALA;
QUIT;
OPTIONS SASTRACE=OFF;

PROC CONTENTS DATA=WORK.NEW VARNUM; RUN;

libname sparcs "R:\ThorpeLab\SFH-SPARCS";
run;

data sparcs.sparcs_hadoop_data_full;
set new;
run;

/*creating a data subset that matches the outputted variables from new SPARCS 2020 data */
proc sql;
create table sparcs1 as select
clm_trans_id
,cbg
,admit_dt
,disch_dt
,disch_yr
,age
,gender_cd
,emerg_ind
,dx_cd
,pat_ethnic_cd
,pat_race_cd
,race_ethnic_cd
from new ;
quit;

/*creating outcome variables for 3 primary health outcomes of interest + additional health outcomes */
proc format;
value yesno
0="no"
1="yes"
;
run;
*asthma ;
data sparcs2;
set sparcs1;
asthma=-99;
if dx_cd in ("J452", "J4520", "J4521", "J4522", "J453", "J4530", "J4531", "J4532", "J454", "J4540", "J4541", "J4542", "J455", "J4550", "J4551", "J4552", "J459", "J4590"
,"J45901", "J45902", "J45909", "J4599", "J45990", "J45998") then asthma=1;
else asthma=0;
format asthma yesno.;
run;

*MI ;
data sparcs3;
set sparcs2;
MI=-99;
if dx_cd in ("I210", "I2101", "I2102", "I2109", "I211", "I2111", "I2119", "I212", "I2121", "I2129", "I213", "I214", "I219", "I21A", "I21A1", "I22", "I220", "I221", "I222", "I228", "I229") then MI=1;
else MI=0;
format MI yesno.;
run;

* stroke ;
data sparcs4;
set sparcs3;
stroke=-99;
if dx_cd in ("I61","I610","I611","I612","I613","I614","I615","I616","I618","I619","I62","I620","I6200","I6201","I6202","I6203","I621","I629","I63","I630","I6300","I6301","I63011","I63012","I63013","I63019","I6302","I6303","I63031","I63032",
"I63033","I63039","I6309","I631","I6310","I6311","I63111","I63112","I63113","I63119","I6312","I6313","I63131","I63132","I63133","I63139","I6319","I632","I6320","I6321","I63211","I63212","I63213","I63219","I6322","I6323","I63231","I63232",
"I63233","I63239","I6329","I633","I6330","I6331","I63311","I63312","I63313","I63319","I6332","I63321","I63322","I63323","I63329","I6333","I63331","I63332","I63333","I63339","I6334","I63341","I63342","I63343","I63349","I6339","I634",
"I6340","I6341","I63411","I63412","I63413","I63419","I6342","I63421","I63422","I63423","I63429","I6343","I63431","I63432","I63433","I63439","I6344","I63441","I63442","I63443","I63449","I6349","I635","I6350","I6351","I63511","I63512","I63513",
"I63519","I6352","I63521","I63522","I63523","I63529","I6353","I63531","I63532","I63533","I63539","I6354","I63541","I63542","I63543","I63549","I6359","I636","I638","I639") then stroke=1;
else stroke=0;
format stroke yesno.;
run;

*all CVD ;
data sparcs5; 
set sparcs4;
CVD=-99;
if dx_cd in ("I20","I200","I201","I208","I209","I21","I210","I2101","I2102","I2109","I211","I2111","I2119","I212","I2121","I2129","I213","I214","I219","I21A","I21A1",
"I21A9","I22","I220","I221","I222","I228","I229","I23","I230","I231","I232","I233","I234","I235","I236","I237","I238","I24","I240","I241","I248","I249","I25","I251","I2510","I2511",
"I25110","I25111","I25118","I25119","I252","I253","I254","I2541","I2542","I255","I256","I257","I2570","I25700","I25701","I25708","I25709","I2571","I25710","I25711","I25718",
"I25719","I2572","I25720","I25721","I25728","I25729","I2573","I25730","I25731","I25738","I25739","I2575","I25750","I25751","I25758","I25759","I2576","I25760","I25761",
"I25768","I25769","I2579","I25790","I25791","I25798","I25799","I258","I2581","I25810","I25811","I25812","I2582","I2583","I2584","I2589","I259") then CVD=1;
else CVD=0;
format CVD yesno.;
run;

*heart failure ;
data sparcs6; 
set sparcs5;
HF=-99;
if dx_cd in ("I50","I501","I502","I5020","I5021","I5022","I5023","I503","I5030","I5031","I5032","I5033","I504","I5040","I5041","I5042",
"I5043","I508","I5081","I50810","I50811","I50812","I50813","I50814","I5082","I5083","I5084","I5089","I509") then HF=1;
else HF=0;
format HF yesno.;
run;

/* saving data */
data sparcs.sparcs_full_dataset; /* change filename here */
set sparcs6; 
run;

/*creating health outcome for LRTI health outcomes */
data sparcs7;
set sparcs6; 

flu=-99;
if dx_cd in ("J09","J09X", "J09X1", "J09X2", "J09X3", "J09X9", "J10", "J100", "J1000", "J1001", "J1008", "J101", "J102", "J108",
"J1081", "J1082", "J1083", "J1089", "J11", "J110", "J1100", "J1108", "J111", "J112", "J118", "J1181", "J1182", "J1183", "J1189",
"J12", "J120", "J121", "J122", "J123", "J128", "J1281", "J1289", "J129", "J13", "J14", "J15", "J150", "J151", "J152", "J1520", 
"J1521", "J15211", "J15212", "J1529", "J153","J154", "J155", "J156", "J157", "J158", "J159", "J16", "J160", "J168", "J17" ,"J18", 
"J180", "J181", "J182", "J188", "J189")then flu=1;
else flu=0;
format flu yesno.;

COPD=-99;
if dx_cd in ("J44", "J440", "J441", "J449") then COPD=1;
else COPD=0;
format COPD yesno.;

run;

/*importing list of CCS diagnostic codes for injuries (negative control outcomes) */
proc import datafile="R:\ThorpeLab\SFH-SPARCS\Dx Codes\CCS_dx_codes.xlsx"
out=codes
dbms=xlsx replace;
getnames=yes;
run;

proc contents data=codes varnum; run;

/* creating a data subset with CCS codes for injury */
proc sql;
create table injuries as select
ICD_10_CM_Code,
Beta_Version_CCS_Category
from codes
where Beta_Version_CCS_Category in (225,226,227,228,229,230,231,232,233,234,235,236,239,240,244,2601,2602,2603,2604,2605,2606,2607,2608,2609,2610);
quit;

proc contents data=injuries varnum;
run;

data injuries; set injuries;
rename ICD_10_CM_Code=dx_cd;
run;

/* merging CCS and ICD-10-CM-codes with SPARCS diagnostic codes */
proc sql;
create table sparcs8 as select
sparcs7.*
,injuries.*
from sparcs7 left join injuries on sparcs7.dx_cd=injuries.dx_cd;
quit;

*data check;
proc freq data=sparcs8;
table Beta_Version_CCS_Category/list;
run;

/*creating health outcomes for negative controls */
data sparcs8;
set sparcs8;

injury=-99;
if Beta_Version_CCS_Category in (225,226,227,228,229,230,231,232,233,234,235,236,239,240,244,2601,2602,2603,2604,2605,2606,2607,2608,2609,2610) then injury=1;
else injury=0;
format injury yesno.;
run;

*data check;
proc freq data=sparcs8;
table injury/list;
run;

/* creating a data subset with health outcomes (asthma, MI ,stroke, LTRI, injury, CVD and HF) */
proc sql;
create table sparcs9 as select
clm_trans_id
,cbg
,admit_dt
,disch_dt
,disch_yr
,age
,gender_cd
,emerg_ind
,dx_cd
,pat_ethnic_cd
,pat_race_cd
,race_ethnic_cd
,asthma
,MI
,stroke
,CVD
,HF
,flu
,COPD
,injury
from sparcs8
where (asthma=1 or MI=1 or stroke=1 or CVD=1 or HF=1 or flu=1 or COPD=1 or injury=1);
quit;

/* saving data */
data sparcs.sparcs_full_dataset; /* change filename here */
set sparcs9; 
run;


/* adding in additional LTRIs */
data sparcs10;
set sparcs8;

ear=-99;
if dx_cd in ("H6500", "H6501","H6502","H6503","H6504","H6505","H6506","H6507","H65111","H65112","H65113","H65114","H65115","H65116","H65117","H65119","H65191",
"H65192","H65193","H65194","H65195","H65196","H65197","H65199","H6520","H6521","H6522","H6523","H6530","H6531","H6532","H6533","H65411","H65412","H65413","H65419",
"H65491","H65492","H65493","H65499","H6590","H6591","H6592","H6593","H66001","H66002","H66003","H66004","H66005","H66006","H66007","H66009","H66011","H66012",
"H66013","H66014","H66015","H66016","H66017","H66019","H6610","H6611","H6612","H6613","H6620","H6621","H6622","H6623","H663X1","H663X2","H663X3","H663X9",
"H6640","H6641","H6642","H6643","H6690","H6691","H6692","H6693","H671","H672","H673","H679") then ear=1;
else ear=0;
format ear yesno.;


URTI=-99;
if dx_cd in ("J0100","J0101","J0110","J0111","J0120","J0121","J0130","J0131","J0140","J0141","J0180","J0181","J0190","J0191","J020",
"J028","J029","J0300","J0301","J040","J0410","J0411","J042","J0430","J0431","J050","J0510","J0511","J060","J069") then URTI=1;
else URTI=0;
format URTI yesno.;

run;

proc sql;
create table sparcs11 as select
clm_trans_id
,cbg
,admit_dt
,disch_dt
,disch_yr
,age
,gender_cd
,emerg_ind
,dx_cd
,pat_ethnic_cd
,pat_race_cd
,race_ethnic_cd
,asthma
,MI
,stroke
,CVD
,HF
,flu
,COPD
,injury
,ear
,URTI
from sparcs10
where (asthma=1 or MI=1 or stroke=1 or CVD=1 or HF=1 or flu=1 or COPD=1 or injury=1 or ear=1 or URTI=1);
quit;

/* saving data */
data sparcs.sparcs_full_dataset; /* change filename here */
set sparcs11; 
run;

/*eliminating the old 2020 data */
proc sql;
create table sparcs12 as select
clm_trans_id
,cbg
,admit_dt
,disch_dt
,disch_yr
,age
,gender_cd
,emerg_ind
,dx_cd
,pat_ethnic_cd
,pat_race_cd
,race_ethnic_cd
,asthma
,MI
,stroke
,CVD
,HF
,flu
,COPD
,injury
,ear
,URTI
from sparcs11
where disch_yr in (2015, 2016, 2017, 2018, 2019);
quit;

/*merging in new SPARCS 2020 data */
data sparcs_2020;
set sparcs.sparcs_2020_full;
run;

proc contents data=sparcs_2020 varnum;run;

/* extracting key variables from 2020 dataset to match sparcs full data */
proc sql;
create table sparcs_2020_a as select
clm_trans_id 
,cbg
,admit_dt
,disch_dt
,disch_yr
,age
,gender_cd
,emerg_ind
,dx_cd
,pat_ethnic_cd
,pat_race_cd
,asthma
,MI
,stroke
,CVD
,HF
,flu
,COPD
,injury
,ear
,URTI
from sparcs_2020;
quit;

data new;
set sparcs_2020_a;
clm_trans_id_n= input(clm_trans_id, 20.);
run;

proc contents data=new varnum ; run;

data new; set new;
rename clm_trans_id_n=clm_trans_id;
drop clm_trans_id;
run;

data sparcs13;
set sparcs12;
CBG_n=input (cbg, 20.);
run;

proc contents data=sparcs14 varnum;run;

data sparcs14;
set sparcs13;
drop cbg;
rename cbg_n=cbg;
run;

proc sort data=sparcs14; by clm_trans_id disch_yr; run;
proc sort data=new; by clm_trans_id disch_yr; run;

data new;set new;
disch_yr_n=input (disch_yr, 20.);
disch_dt_n=input (disch_dt, 20.);
rename disch_yr_n=disch_yr;
rename disch_dt_n=disch_dt;
drop disch_yr disch_dt;
run;

proc contents data= new; run;

data sparcs15;
merge sparcs14 new;
by clm_trans_id disch_yr;
run;

*data check;
proc freq data=sparcs15;
table disch_yr/list;
run;

/* saving data */
data sparcs.sparcs_full_dataset; /* change filename here */
set sparcs15; 
run;

/*obtaining aggregate counts for each health outcome by CBG*/
proc sql;
create table sparcs16 as select
clm_trans_id 
,cbg
,admit_dt
,disch_dt
,disch_yr
,age
,gender_cd
,emerg_ind
,dx_cd
,pat_ethnic_cd
,pat_race_cd
,sum (asthma) as asthma
,sum (MI) as MI
,sum (stroke) as stroke
,sum (CVD) as CVD
,sum (HF) as HF
,sum (flu) as flu
,sum (COPD) as COPD
,sum (injury) as injury
,sum (ear) as ear
,sum(URTI) as URTI

from sparcs15
group by cbg
order by cbg;
quit;
