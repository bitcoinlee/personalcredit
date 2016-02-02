data _null_;
ismonth = month(today());
if 2<ismonth<11 then call symput('upmonth',cat(put(year(today()),$4.), "0" ,put(month(today())-1,$1.)));
else if ismonth >= 11 then call symput('upmonth',cat(put(year(today()),$4.), put(month(today()) -1 ,$2.)));
else call symput('upmonth',cat(put(year(today())-1,$4.),'12'));
run;
data _null_;
ismonth = month(today());
if ismonth<10 then call symput('currmonth',cat(put(year(today()),$4.),"年",put(month(today()),$1.),"月"));
else call symput('currmonth',cat(put(year(today()),$4.),"年",put(month(today()),$2.),"月"));
run;
data _null_;
ismonth=month(today());
if ismonth<10 then 
call symput('curr_month',cat(put(year(today()),$4.),"0",put(month(today()),$1.)));
else call symput('curr_month',cat(put(year(today()),$4.),put(month(today()),$2.)));
run;

%INCLUDE "E:/新建文件夹/SAS/常用代码/自动化/000_FORMAT.sas";
/*%include "E:/新建文件夹/SAS/基础宏.sas";*/
%FORMAT;
options compress=yes mprint mlogic noxwait;
/*libname nfcs oracle user=datauser password=zlxdh7jf path=p2p;*/
libname nfcs "D:/数据/&curr_month.";
%let outfile = D:/逻辑校验结果/&curr_month./;
data _null_;
call symputx('firstday',intnx("month",today(),-1,'b'));
call symputx('firstday_two',intnx("month",today(),-2,'b'));
run;
/*导入机构名称、简称、专管员映射关系*/
PROC IMPORT OUT= WORK.soc DATAFILE= "E:/新建文件夹/&curr_month./NFCS/soc.xlsx" DBMS=EXCEL REPLACE;
     SHEET="sheet1$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
data soc;
	set soc;
rename
_COL0 = sorgcode
_COL1 = shortname
_COL2 = person
;
run;

proc sql;
	create table config as select
T1.*
from soc as T1
left join (select distinct sorgcode from nfcs.sino_msg) as T2
on T1.sorgcode = T2.sorgcode
where T2.sorgcode is not null
order by person
;
quit;

/*导入NFCS全部标签名*/
%macro AddLabel(table);
%if &table. = "" %then %do;
proc sql noprint;
        select
            catx("=",ITABLECOLUMNNAME,SCOLNAME) into:label separated by " "
        from nfcs.Sino_msg_column
        ;
quit;
%end;
%else %do;
proc sql noprint;
        select
            catx("=",ITABLECOLUMNNAME,SCOLNAME) into:label separated by " "
        from nfcs.Sino_msg_column as T1
		where T1.ITABLENAME = "&table."
        ;
quit;
%end;
%mend;
/*示例程序*/
/*%AddLabel(sino_person);*/
/*data sino_person;*/
/*	set nfcs.sino_person;*/
/*	label*/
/*	&label.*/
/*;*/
/*run;*/
