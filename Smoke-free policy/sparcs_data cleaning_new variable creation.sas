/* creating a SAS data library */
libname sparcs "R:\ThorpeLab\SFH-SPARCS";
run;

proc format;
value yesno
1="yes"
0="no"
;
run;

data sparcs;
set sparcs.NEW_sparcs_yr_2016to2020_full;
run;

proc contents data= sparcs varnum;
run;

/* creating quarterly and monthly variables */
proc sql;
create table sparcs1 as select
substr(admit_dt, 3,4) as month,
sparcs.*
from sparcs;
quit;

proc sql;
create table sparcs2 as select
case
when sparcs1.month in ("1510", "1511", "1512") then "1"
when sparcs1.month in ("1601", "1602", "1603") then "2"
when sparcs1.month in ("1604", "1605", "1606") then "3"
when sparcs1.month in ("1607", "1608", "1609") then "4"
when sparcs1.month in ("1610", "1611", "1612") then "5"
when sparcs1.month in("1701", "1702", "1703")  then "6"
when sparcs1.month in ("1704", "1705", "1706") then "7"
when sparcs1.month in ("1707", "1708", "1709") then "8"
when sparcs1.month in ("1710", "1711", "1712") then "9"
when sparcs1.month in("1801", "1802", "1803")  then "10"
when sparcs1.month in ("1804", "1805", "1806") then "11"
when sparcs1.month in ("1807", "1808", "1809") then "12"
when sparcs1.month in ("1810", "1811", "1812") then "13"
when sparcs1.month in ("1901", "1902", "1903") then "14"
when sparcs1.month in ("1904", "1905", "1906") then "15"
when sparcs1.month in ("1907", "1908", "1909") then "16"
when sparcs1.month in ("1910", "1911", "1912") then "17"
when sparcs1.month in ("2001", "2002", "2003") then "18"
when sparcs1.month in ("2004", "2005", "2006") then "19"
when sparcs1.month in ("2007", "2008", "2009") then "20"
when sparcs1.month in ("2010", "2011", "2012") then "21"
end as quarter,
sparcs1.*
from sparcs1;
quit;

/*importing CBG list from R Drive */
proc import datafile="R:\ThorpeLab\SFH-SPARCS\CBG list.xlsx"
dbms=xlsx replace 
out= CBG;
getnames=yes;
run;

proc sql;
create table sparcs3 as select
sparcs2.*
,CBG.*
from sparcs2 left join CBG on sparcs2.cbg=cbg.cbg;
quit;

*data check;
proc freq data=sparcs3;
table int/list;
run;

/* creating count cases for intervention */
proc sql;
create table sparcs4 as select
case
when sparcs3.age in (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17) then "1"
when sparcs3.age in (18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49) then "2"
when sparcs3.age in (50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100) then "3"

end as age_cat,
sparcs3.*
from sparcs3;
quit;

/* ASTHMA */
proc sql;
create table sparcs5 as select
case
when age_cat in ("1") and asthma in (1) then "1"
end as ped_asthma,
sparcs4.*
from sparcs4;
quit; 

/*CBG*/
proc sql;
create table sparcs6 as select
case
when ped_asthma in ("1") and INT in (1) then "1"
end as CBG_int_asthma, 
sparcs5.*
from sparcs5; 
quit;

proc sql;
create table sparcs7 as select
case
when ped_asthma in ("1") and INT in (0) then "1"
end as CBG_comp_asthma,
sparcs6.*
from sparcs6;
quit;

/* MI */
proc sql;
create table sparcs8 as select
case
when age_cat in ("3") and MI in (1) then "1"
end as adult_MI,
sparcs7.*
from sparcs7;

create table sparcs9 as select
case
when adult_MI in ("1") and INT in (1) then "1"
end as CBG_int_MI,
sparcs8.*
from sparcs8;

create table sparcs10 as select
case
when adult_MI in ("1") and INT in (0) then "1"
end as CBG_comp_MI,
sparcs9.*
from sparcs9;
quit;

/* Stroke */
proc sql;
create table sparcs11 as select
case
when age_cat in ("3") and stroke in (1) then "1"
end as adult_stroke,
sparcs10.*
from sparcs10;

create table sparcs12 as select
case 
when adult_stroke in ("1") and INT in (1) then "1"
end as CBG_int_stroke,
sparcs11.*
from sparcs11;

create table sparcs13 as select
case 
when adult_stroke in ("1") and INT in (0) then "1"
end as CBG_comp_stroke,
sparcs12.*
from sparcs12;
quit;

/*flu and COPD - LRTIs and URTIs*/
proc sql;
create table sparcs14 as select
case
when age_cat in ("3") and flu in (1) then "1"
when age_cat in ("3") and COPD in (1) then "1"
end as adult_LRTI,
sparcs13.*
from sparcs13;
quit;

proc sql;
create table sparcs15 as select
case
when adult_LRTI in ("1") and INT in (1) then "1"
end as CBG_int_LRTI,
sparcs14.*
from sparcs14;

create table sparcs16 as select
case
when adult_LRTI in ("1") and INT in (0) then "1"
end as CBG_comp_LRTI,
sparcs15.*
from sparcs15;

create table sparcs17 as select
case
when age_cat in ("3") and URTI in (1) then "1"
end as adult_URTI,
sparcs16.*
from sparcs16;

create table sparcs18 as select
case
when adult_URTI in ("1") and INT in (1) then "1"
end as CBG_int_URTI,
sparcs17.*
from sparcs17;

create table sparcs19 as select
case
when adult_URTI in ("1") and INT in (0) then "1"
end as CBG_comp_URTI,
sparcs18.*
from sparcs18;
quit;

/* negative control - injuries */
proc sql;
create table sparcs20 as select
case
when age_cat in ("1") and injury in (1) then "1"
end as ped_injury,
sparcs19.*
from sparcs19;

create table sparcs21 as select
case
when age_cat in ("3") and injury in (1) then "1"
end as adult_injury,
sparcs20.*
from sparcs20;

create table sparcs22 as select
case
when ped_injury in ("1") and INT in (1) then "1"
end as CBG_int_ped_injury,
sparcs21.*
from sparcs21;

create table sparcs23 as select
case
when adult_injury in ("1") and INT in (1) then "1"
end as CBG_int_adult_injury,
sparcs22.*
from sparcs22;

create table sparcs24 as select
case
when ped_injury in ("1") and INT in (0) then "1"
end as CBG_comp_ped_injury,
sparcs23.*
from sparcs23;

create table sparcs25 as select
case
when adult_injury in ("1") and INT in (0) then "1"
end as CBG_comp_adult_injury,
sparcs24.*
from sparcs24;
quit;

/* OBTAINING HEALTH OUTCOME QUARTERLY RATES */

data sparcs26;
set sparcs25;

/*asthma quarterly counts*/
if (cbg_int_asthma=1 and quarter=1)or (cbg_comp_asthma=1 and quarter=1) then q1_asthma=1;
if (cbg_int_asthma=1 and quarter=2)or(cbg_comp_asthma=1 and quarter=2) then q2_asthma=1;
if (cbg_int_asthma=1 and quarter=3) or (cbg_comp_asthma=1 and quarter=3) then q3_asthma=1;
if (cbg_int_asthma=1 and quarter=4) or (cbg_comp_asthma=1 and quarter=4) then q4_asthma=1;
if (cbg_int_asthma=1 and quarter=5) or (cbg_comp_asthma=1 and quarter=5) then q5_asthma=1;
if (cbg_int_asthma=1 and quarter=6) or (cbg_comp_asthma=1 and quarter=6) then q6_asthma=1;
if (cbg_int_asthma=1 and quarter=7) or (cbg_comp_asthma=1 and quarter=7) then q7_asthma=1;
if (cbg_int_asthma=1 and quarter=8) or (cbg_comp_asthma=1 and quarter=8) then q8_asthma=1;
if (cbg_int_asthma=1 and quarter=9) or (cbg_comp_asthma=1 and quarter=9) then q9_asthma=1;
if (cbg_int_asthma=1 and quarter=10) or (cbg_comp_asthma=1 and quarter=10) then q10_asthma=1;
if (cbg_int_asthma=1 and quarter=11) or (cbg_comp_asthma=1 and quarter=11) then q11_asthma=1;
if (cbg_int_asthma=1 and quarter=12) or (cbg_comp_asthma=1 and quarter=12) then q12_asthma=1;
if (cbg_int_asthma=1 and quarter=13) or (cbg_comp_asthma=1 and quarter=13) then q13_asthma=1;
if (cbg_int_asthma=1 and quarter=14) or (cbg_comp_asthma=1 and quarter=14) then q14_asthma=1;
if (cbg_int_asthma=1 and quarter=15) or (cbg_comp_asthma=1 and quarter=15) then q15_asthma=1;
if (cbg_int_asthma=1 and quarter=16) or (cbg_comp_asthma=1 and quarter=16) then q16_asthma=1;
if (cbg_int_asthma=1 and quarter=17) or (cbg_comp_asthma=1 and quarter=17) then q17_asthma=1;
if (cbg_int_asthma=1 and quarter=18) or (cbg_comp_asthma=1 and quarter=18) then q18_asthma=1;
if (cbg_int_asthma=1 and quarter=19) or (cbg_comp_asthma=1 and quarter=19) then q19_asthma=1;
if (cbg_int_asthma=1 and quarter=20) or (cbg_comp_asthma=1 and quarter=20) then q20_asthma=1;
if (cbg_int_asthma=1 and quarter=21) or (cbg_comp_asthma=1 and quarter=21) then q21_asthma=1;

/*MI quarterly counts */
if (cbg_int_mi=1 and quarter=1)or (cbg_comp_mi=1 and quarter=1) then q1_mi=1;
if (cbg_int_mi=1 and quarter=2)or(cbg_comp_mi=1 and quarter=2) then q2_mi=1;
if (cbg_int_mi=1 and quarter=3) or (cbg_comp_mi=1 and quarter=3) then q3_mi=1;
if (cbg_int_mi=1 and quarter=4) or (cbg_comp_mi=1 and quarter=4) then q4_mi=1;
if (cbg_int_mi=1 and quarter=5) or (cbg_comp_mi=1 and quarter=5) then q5_mi=1;
if (cbg_int_mi=1 and quarter=6) or (cbg_comp_mi=1 and quarter=6) then q6_mi=1;
if (cbg_int_mi=1 and quarter=7) or (cbg_comp_mi=1 and quarter=7) then q7_mi=1;
if (cbg_int_mi=1 and quarter=8) or (cbg_comp_mi=1 and quarter=8) then q8_mi=1;
if (cbg_int_mi=1 and quarter=9) or (cbg_comp_mi=1 and quarter=9) then q9_mi=1;
if (cbg_int_mi=1 and quarter=10) or (cbg_comp_mi=1 and quarter=10) then q10_mi=1;
if (cbg_int_mi=1 and quarter=11) or (cbg_comp_mi=1 and quarter=11) then q11_mi=1;
if (cbg_int_mi=1 and quarter=12) or (cbg_comp_mi=1 and quarter=12) then q12_mi=1;
if (cbg_int_mi=1 and quarter=13) or (cbg_comp_mi=1 and quarter=13) then q13_mi=1;
if (cbg_int_mi=1 and quarter=14) or (cbg_comp_mi=1 and quarter=14) then q14_mi=1;
if (cbg_int_mi=1 and quarter=15) or (cbg_comp_mi=1 and quarter=15) then q15_mi=1;
if (cbg_int_mi=1 and quarter=16) or (cbg_comp_mi=1 and quarter=16) then q16_mi=1;
if (cbg_int_mi=1 and quarter=17) or (cbg_comp_mi=1 and quarter=17) then q17_mi=1;
if (cbg_int_mi=1 and quarter=18) or (cbg_comp_mi=1 and quarter=18) then q18_mi=1;
if (cbg_int_mi=1 and quarter=19) or (cbg_comp_mi=1 and quarter=19) then q19_mi=1;
if (cbg_int_mi=1 and quarter=20) or (cbg_comp_mi=1 and quarter=20) then q20_mi=1;
if (cbg_int_mi=1 and quarter=21) or (cbg_comp_mi=1 and quarter=21) then q21_mi=1;

/*stroke quarterly counts */
if (cbg_int_stroke=1 and quarter=1)or (cbg_comp_stroke=1 and quarter=1) then q1_stroke=1;
if (cbg_int_stroke=1 and quarter=2)or(cbg_comp_stroke=1 and quarter=2) then q2_stroke=1;
if (cbg_int_stroke=1 and quarter=3) or (cbg_comp_stroke=1 and quarter=3) then q3_stroke=1;
if (cbg_int_stroke=1 and quarter=4) or (cbg_comp_stroke=1 and quarter=4) then q4_stroke=1;
if (cbg_int_stroke=1 and quarter=5) or (cbg_comp_stroke=1 and quarter=5) then q5_stroke=1;
if (cbg_int_stroke=1 and quarter=6) or (cbg_comp_stroke=1 and quarter=6) then q6_stroke=1;
if (cbg_int_stroke=1 and quarter=7) or (cbg_comp_stroke=1 and quarter=7) then q7_stroke=1;
if (cbg_int_stroke=1 and quarter=8) or (cbg_comp_stroke=1 and quarter=8) then q8_stroke=1;
if (cbg_int_stroke=1 and quarter=9) or (cbg_comp_stroke=1 and quarter=9) then q9_stroke=1;
if (cbg_int_stroke=1 and quarter=10) or (cbg_comp_stroke=1 and quarter=10) then q10_stroke=1;
if (cbg_int_stroke=1 and quarter=11) or (cbg_comp_stroke=1 and quarter=11) then q11_stroke=1;
if (cbg_int_stroke=1 and quarter=12) or (cbg_comp_stroke=1 and quarter=12) then q12_stroke=1;
if (cbg_int_stroke=1 and quarter=13) or (cbg_comp_stroke=1 and quarter=13) then q13_stroke=1;
if (cbg_int_stroke=1 and quarter=14) or (cbg_comp_stroke=1 and quarter=14) then q14_stroke=1;
if (cbg_int_stroke=1 and quarter=15) or (cbg_comp_stroke=1 and quarter=15) then q15_stroke=1;
if (cbg_int_stroke=1 and quarter=16) or (cbg_comp_stroke=1 and quarter=16) then q16_stroke=1;
if (cbg_int_stroke=1 and quarter=17) or (cbg_comp_stroke=1 and quarter=17) then q17_stroke=1;
if (cbg_int_stroke=1 and quarter=18) or (cbg_comp_stroke=1 and quarter=18) then q18_stroke=1;
if (cbg_int_stroke=1 and quarter=19) or (cbg_comp_stroke=1 and quarter=19) then q19_stroke=1;
if (cbg_int_stroke=1 and quarter=20) or (cbg_comp_stroke=1 and quarter=20) then q20_stroke=1;
if (cbg_int_stroke=1 and quarter=21) or (cbg_comp_stroke=1 and quarter=21) then q21_stroke=1;

/* LRTI */
if (cbg_int_LRTI=1 and quarter=1)or (cbg_comp_LRTI=1 and quarter=1) then q1_LRTI=1;
if (cbg_int_LRTI=1 and quarter=2)or(cbg_comp_LRTI=1 and quarter=2) then q2_LRTI=1;
if (cbg_int_LRTI=1 and quarter=3) or (cbg_comp_LRTI=1 and quarter=3) then q3_LRTI=1;
if (cbg_int_LRTI=1 and quarter=4) or (cbg_comp_LRTI=1 and quarter=4) then q4_LRTI=1;
if (cbg_int_LRTI=1 and quarter=5) or (cbg_comp_LRTI=1 and quarter=5) then q5_LRTI=1;
if (cbg_int_LRTI=1 and quarter=6) or (cbg_comp_LRTI=1 and quarter=6) then q6_LRTI=1;
if (cbg_int_LRTI=1 and quarter=7) or (cbg_comp_LRTI=1 and quarter=7) then q7_LRTI=1;
if (cbg_int_LRTI=1 and quarter=8) or (cbg_comp_LRTI=1 and quarter=8) then q8_LRTI=1;
if (cbg_int_LRTI=1 and quarter=9) or (cbg_comp_LRTI=1 and quarter=9) then q9_LRTI=1;
if (cbg_int_LRTI=1 and quarter=10) or (cbg_comp_LRTI=1 and quarter=10) then q10_LRTI=1;
if (cbg_int_LRTI=1 and quarter=11) or (cbg_comp_LRTI=1 and quarter=11) then q11_LRTI=1;
if (cbg_int_LRTI=1 and quarter=12) or (cbg_comp_LRTI=1 and quarter=12) then q12_LRTI=1;
if (cbg_int_LRTI=1 and quarter=13) or (cbg_comp_LRTI=1 and quarter=13) then q13_LRTI=1;
if (cbg_int_LRTI=1 and quarter=14) or (cbg_comp_LRTI=1 and quarter=14) then q14_LRTI=1;
if (cbg_int_LRTI=1 and quarter=15) or (cbg_comp_LRTI=1 and quarter=15) then q15_LRTI=1;
if (cbg_int_LRTI=1 and quarter=16) or (cbg_comp_LRTI=1 and quarter=16) then q16_LRTI=1;
if (cbg_int_LRTI=1 and quarter=17) or (cbg_comp_LRTI=1 and quarter=17) then q17_LRTI=1;
if (cbg_int_LRTI=1 and quarter=18) or (cbg_comp_LRTI=1 and quarter=18) then q18_LRTI=1;
if (cbg_int_LRTI=1 and quarter=19) or (cbg_comp_LRTI=1 and quarter=19) then q19_LRTI=1;
if (cbg_int_LRTI=1 and quarter=20) or (cbg_comp_LRTI=1 and quarter=20) then q20_LRTI=1;
if (cbg_int_LRTI=1 and quarter=21) or (cbg_comp_LRTI=1 and quarter=21) then q21_LRTI=1;

/* URTI */

if (cbg_int_URTI=1 and quarter=1)or (cbg_comp_URTI=1 and quarter=1) then q1_URTI=1;
if (cbg_int_URTI=1 and quarter=2)or(cbg_comp_URTI=1 and quarter=2) then q2_URTI=1;
if (cbg_int_URTI=1 and quarter=3) or (cbg_comp_URTI=1 and quarter=3) then q3_URTI=1;
if (cbg_int_URTI=1 and quarter=4) or (cbg_comp_URTI=1 and quarter=4) then q4_URTI=1;
if (cbg_int_URTI=1 and quarter=5) or (cbg_comp_URTI=1 and quarter=5) then q5_URTI=1;
if (cbg_int_URTI=1 and quarter=6) or (cbg_comp_URTI=1 and quarter=6) then q6_URTI=1;
if (cbg_int_URTI=1 and quarter=7) or (cbg_comp_URTI=1 and quarter=7) then q7_URTI=1;
if (cbg_int_URTI=1 and quarter=8) or (cbg_comp_URTI=1 and quarter=8) then q8_URTI=1;
if (cbg_int_URTI=1 and quarter=9) or (cbg_comp_URTI=1 and quarter=9) then q9_URTI=1;
if (cbg_int_URTI=1 and quarter=10) or (cbg_comp_URTI=1 and quarter=10) then q10_URTI=1;
if (cbg_int_URTI=1 and quarter=11) or (cbg_comp_URTI=1 and quarter=11) then q11_URTI=1;
if (cbg_int_URTI=1 and quarter=12) or (cbg_comp_URTI=1 and quarter=12) then q12_URTI=1;
if (cbg_int_URTI=1 and quarter=13) or (cbg_comp_URTI=1 and quarter=13) then q13_URTI=1;
if (cbg_int_URTI=1 and quarter=14) or (cbg_comp_URTI=1 and quarter=14) then q14_URTI=1;
if (cbg_int_URTI=1 and quarter=15) or (cbg_comp_URTI=1 and quarter=15) then q15_URTI=1;
if (cbg_int_URTI=1 and quarter=16) or (cbg_comp_URTI=1 and quarter=16) then q16_URTI=1;
if (cbg_int_URTI=1 and quarter=17) or (cbg_comp_URTI=1 and quarter=17) then q17_URTI=1;
if (cbg_int_URTI=1 and quarter=18) or (cbg_comp_URTI=1 and quarter=18) then q18_URTI=1;
if (cbg_int_URTI=1 and quarter=19) or (cbg_comp_URTI=1 and quarter=19) then q19_URTI=1;
if (cbg_int_URTI=1 and quarter=20) or (cbg_comp_URTI=1 and quarter=20) then q20_URTI=1;
if (cbg_int_URTI=1 and quarter=21) or (cbg_comp_URTI=1 and quarter=21) then q21_URTI=1;

/*Negative control - injury */
if (ped_injury=1 and quarter=1) or (adult_injury=1 and quarter=1) then q1_injury=1;
if (ped_injury=1 and quarter=2) or (adult_injury=1 and quarter=2) then q2_injury=2;
if (ped_injury=1 and quarter=3) or (adult_injury=1 and quarter=3) then q3_injury=3;
if (ped_injury=1 and quarter=4) or (adult_injury=1 and quarter=4) then q4_injury=4;
if (ped_injury=1 and quarter=5) or (adult_injury=1 and quarter=5) then q5_injury=5;
if (ped_injury=1 and quarter=6) or (adult_injury=1 and quarter=6) then q6_injury=6;
if (ped_injury=1 and quarter=7) or (adult_injury=1 and quarter=7) then q7_injury=7;
if (ped_injury=1 and quarter=8) or (adult_injury=1 and quarter=8) then q8_injury=8;
if (ped_injury=1 and quarter=9) or (adult_injury=1 and quarter=9) then q9_injury=9;
if (ped_injury=1 and quarter=10) or (adult_injury=1 and quarter=10) then q10_injury=10;
if (ped_injury=1 and quarter=11) or (adult_injury=1 and quarter=11) then q11_injury=11;
if (ped_injury=1 and quarter=12) or (adult_injury=1 and quarter=12) then q12_injury=12;
if (ped_injury=1 and quarter=13) or (adult_injury=1 and quarter=13) then q13_injury=13;
if (ped_injury=1 and quarter=14) or (adult_injury=1 and quarter=14) then q14_injury=14;
if (ped_injury=1 and quarter=15) or (adult_injury=1 and quarter=15) then q15_injury=15;
if (ped_injury=1 and quarter=16) or (adult_injury=1 and quarter=16) then q16_injury=16;
if (ped_injury=1 and quarter=17) or (adult_injury=1 and quarter=17) then q17_injury=17;
if (ped_injury=1 and quarter=18) or (adult_injury=1 and quarter=18) then q18_injury=18;
if (ped_injury=1 and quarter=19) or (adult_injury=1 and quarter=19) then q19_injury=19;
if (ped_injury=1 and quarter=20) or (adult_injury=1 and quarter=20) then q20_injury=20;
if (ped_injury=1 and quarter=21) or (adult_injury=1 and quarter=21) then q21_injury=21;

run;

/* need to get aggregate counts of cases for each health outcome by unique CBG */
proc sql;
create table sparcs27 as select 
 /*asthma*/
sum(q1_asthma) as q1_asthma,
sum(q2_asthma) as q2_asthma,
sum(q3_asthma) as q3_asthma,
sum(q4_asthma) as q4_asthma,
sum(q5_asthma) as q5_asthma,
sum(q6_asthma) as q6_asthma,
sum(q7_asthma) as q7_asthma,
sum(q8_asthma) as q8_asthma,
sum(q9_asthma) as q9_asthma,
sum(q10_asthma) as q10_asthma,
sum(q11_asthma) as q11_asthma,
sum(q12_asthma) as q12_asthma,
sum(q13_asthma) as q13_asthma,
sum(q14_asthma) as q14_asthma,
sum(q15_asthma) as q15_asthma,
sum(q16_asthma) as q16_asthma,
sum(q17_asthma) as q17_asthma,
sum(q18_asthma) as q18_asthma,
sum(q19_asthma) as q19_asthma,
sum(q20_asthma) as q20_asthma,
sum(q21_asthma) as q21_asthma,
/*MI */
sum(q1_MI) as q1_MI,
sum(q2_MI) as q2_MI,
sum(q3_MI) as q3_MI,
sum(q4_MI) as q4_MI,
sum(q5_MI) as q5_MI,
sum(q6_MI) as q6_MI,
sum(q7_MI) as q7_MI,
sum(q8_MI) as q8_MI,
sum(q9_MI) as q9_MI,
sum(q10_MI) as q10_MI,
sum(q11_MI) as q11_MI,
sum(q12_MI) as q12_MI,
sum(q13_MI) as q13_MI,
sum(q14_MI) as q14_MI,
sum(q15_MI) as q15_MI,
sum(q16_MI) as q16_MI,
sum(q17_MI) as q17_MI,
sum(q18_MI) as q18_MI,
sum(q19_MI) as q19_MI,
sum(q20_MI) as q20_MI,
sum(q21_MI) as q21_MI,
/*stroke*/
sum(q1_stroke) as q1_stroke,
sum(q2_stroke) as q2_stroke,
sum(q3_stroke) as q3_stroke,
sum(q4_stroke) as q4_stroke,
sum(q5_stroke) as q5_stroke,
sum(q6_stroke) as q6_stroke,
sum(q7_stroke) as q7_stroke,
sum(q8_stroke) as q8_stroke,
sum(q9_stroke) as q9_stroke,
sum(q10_stroke) as q10_stroke,
sum(q11_stroke) as q11_stroke,
sum(q12_stroke) as q12_stroke,
sum(q13_stroke) as q13_stroke,
sum(q14_stroke) as q14_stroke,
sum(q15_stroke) as q15_stroke,
sum(q16_stroke) as q16_stroke,
sum(q17_stroke) as q17_stroke,
sum(q18_stroke) as q18_stroke,
sum(q19_stroke) as q19_stroke,
sum(q20_stroke) as q20_stroke,
sum(q21_stroke) as q21_stroke,
/*LRTI */
sum(q1_LRTI) as q1_LRTI,
sum(q2_LRTI) as q2_LRTI,
sum(q3_LRTI) as q3_LRTI,
sum(q4_LRTI) as q4_LRTI,
sum(q5_LRTI) as q5_LRTI,
sum(q6_LRTI) as q6_LRTI,
sum(q7_LRTI) as q7_LRTI,
sum(q8_LRTI) as q8_LRTI,
sum(q9_LRTI) as q9_LRTI,
sum(q10_LRTI) as q10_LRTI,
sum(q11_LRTI) as q11_LRTI,
sum(q12_LRTI) as q12_LRTI,
sum(q13_LRTI) as q13_LRTI,
sum(q14_LRTI) as q14_LRTI,
sum(q15_LRTI) as q15_LRTI,
sum(q16_LRTI) as q16_LRTI,
sum(q17_LRTI) as q17_LRTI,
sum(q18_LRTI) as q18_LRTI,
sum(q19_LRTI) as q19_LRTI,
sum(q20_LRTI) as q20_LRTI,
sum(q21_LRTI) as q21_LRTI,
/* URTI*/
sum(q1_URTI) as q1_URTI,
sum(q2_URTI) as q2_URTI,
sum(q3_URTI) as q3_URTI,
sum(q4_URTI) as q4_URTI,
sum(q5_URTI) as q5_URTI,
sum(q6_URTI) as q6_URTI,
sum(q7_URTI) as q7_URTI,
sum(q8_URTI) as q8_URTI,
sum(q9_URTI) as q9_URTI,
sum(q10_URTI) as q10_URTI,
sum(q11_URTI) as q11_URTI,
sum(q12_URTI) as q12_URTI,
sum(q13_URTI) as q13_URTI,
sum(q14_URTI) as q14_URTI,
sum(q15_URTI) as q15_URTI,
sum(q16_URTI) as q16_URTI,
sum(q17_URTI) as q17_URTI,
sum(q18_URTI) as q18_URTI,
sum(q19_URTI) as q19_URTI,
sum(q20_URTI) as q20_URTI,
sum(q21_URTI) as q21_URTI,
/*NEGATIVE CONTROL */
sum(q1_injury) as q1_injury,
sum(q2_injury) as q2_injury,
sum(q3_injury) as q3_injury,
sum(q4_injury) as q4_injury,
sum(q5_injury) as q5_injury,
sum(q6_injury) as q6_injury,
sum(q7_injury) as q7_injury,
sum(q8_injury) as q8_injury,
sum(q9_injury) as q9_injury,
sum(q10_injury) as q10_injury,
sum(q11_injury) as q11_injury,
sum(q12_injury) as q12_injury,
sum(q13_injury) as q13_injury,
sum(q14_injury) as q14_injury,
sum(q15_injury) as q15_injury,
sum(q16_injury) as q16_injury,
sum(q17_injury) as q17_injury,
sum(q18_injury) as q18_injury,
sum(q19_injury) as q19_injury,
sum(q20_injury) as q20_injury,
sum(q21_injury) as q21_injury,
sparcs26.*
from sparcs26
group by cbg
order by cbg;
quit;

/*creating a data subset with quarterly health outcomes variables for unique CBGs*/
proc sort data=sparcs27 out=sparcs28 nodupkey; by cbg; run;

data sparcs28;
set sparcs28;
if INT in (" ") then delete;
run;

proc contents data=sparcs28 varnum; run;

data sparcs29;
set sparcs28;

drop       
age_cat        
quarter       
month        
clm_trans_id  
admit_dt  
disch_dt  
disch_yr 
age  
gender_cd  
emerg_ind  
dx_cd 
pat_ethnic_cd  
pat_race_cd  
race_ethnic_cd 
asthma      
MI      
stroke      
CVD      
HF     
flu   
COPD    
injury      
ear     
URTI 
;
run; 

/*saving datasets to R drive */
data sparcs.sparcs_quarterly_health_outcomes;
set sparcs29;
run;

data sparcs.sparcs_health_outcomes_full;
set sparcs27;
run;

proc contents data=sparcs29 varnum; run;

data sparcs;
set sparcs.new_sparcs_quarterly_outcomes;
run;

proc export data=sparcs
outfile="R:\ThorpeLab\SFH-SPARCS\sparcs_quarterly_outcomes.x;sx"
dbms=xlsx replace;
run;
