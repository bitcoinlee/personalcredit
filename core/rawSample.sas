
* ---------------------------;
* ---- Raw sample -----------;
* ---------------------------;
* raw sampleָ��Level1���ݼ������������,�����ݸ�ʽ��������ʽ����sample��ͬ;
* raw sample sampleId�����������;
*	raw sample��sampleId��������������ָ������ͳһ��������;
* 	sampleId���ɹ���
*		B[��׼����]_[������������]_[���ɺ����];
*		���л������������ǶԸ��������������ĸ��������ĺ���ĸ���;
*		����B20150101_DEMOGRAPHICS;
* 	������������
*		[����������]_[��׺]�����к�׺�����»������ֵĶ���ֶ�;
* 		���ڽϼ򵥵ı�����һ������ȫ����Ϊ����������SEX;
*		�������ƽϳ��Ļ����ν϶�ı�����һ������ȫ������д��Ϊ���������������������η����磺LC_NORMAL;
* raw sample����;
* 	grs��L1���ݿ���ȡ���ݣ�L1����lib��·��ͨ��ȫ�ֺ����L1PATH��ȡ;
* 	raw sample��grsϵ�к����ɣ����и�ϵ�к����ٰ���baseDate��res��������������res���ڷ������ɵ�����id;
* 	1��grs��������ɶ��������ÿ���������԰����������;
* 	grs˵���б�����ʾ���иú�������ɵ���������������;

* ��׼ʱ�����ȡʱ��;
*	������;
* ʱ�䷶Χ����;
*	����ʱ�䷶Χ������ͷ����βԭ�򡱱�ʾ����baseDateΪ20150101,��Ӧʱ�䷶ΧΪ(-inf,20150101);
*	��baseDateΪ20150101�����Ϊ6���£���ʱ�䷶ΧΪ[20150701,20150101);


* ---- macro grsDemographics ----;
* ����demographics�������;
* �����������ƣ�DEMOGRAPHICS;
* sampleId�б�B20150101_DEMOGRAHPICS;
* �����б�sex marriage highestEducationLevel highestEducationDegreeObtained age;
* input:
*	baseDate	��׼ʱ��;
*	res			���������������ݲ�����;
%macro grsDemographics(baseDate=,res=) /parmbuff;
	%put grsDemographics: start to generate demographics! param=&syspbuff;
	%if &baseDate= %then %error(grsDemographics: Required param is empty! param=&syspbuff);

	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate dtvGetDate t t2 t3 t4;
	%local sample sampleId;

	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	%let sampleId=B&baseDate._DEMOGRAPHICS;	

	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;

	%let &sample.baseDate=&baseDate;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=grsDemographics;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=DEMOGRAHPICS;
	%let &sample.enumVars=sex marriage eduLevel eduDegree;
	%let &sample.amountVars=age;
	%let &sample.defaultValue=%str(sex=9 marriage=90 eduLevel=99 eduDegree=9);

	* -- �������ɴ��� --;
	
	* ��nfcs.person����ȡ��¼������������Ϣ;	
	proc sql;
		create table &t as (
			select 	spin as spin,
					dgetdate,
					igender as sex,
					imarriage as marriage,
					iedulevel as eduLevel,
					iedudegree as eduDegree,
					intck('dtyear',dbirthday,&dtvBaseDate) as age
			from &nfcs..sino_person
			where dgetdate<&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(grsDemographics: No matched record!);

	* ��dgetdate��ȡ���µ�һ����¼;
	proc sort data=&t;
		by spin dgetdate;
	quit;

	%clearLabel(&t);

	* ���ݹ���������;
	data &&&sample.data;
		set &t;
		if age<14 or age>70 then delete;
		keep spin sex marriage eduLevel eduDegree age;
		by spin dgetdate;
		if last.spin then output;
	run;

	* -- grs��׼������������ --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);

%mend;
* ---- macro grsRecordNum ----;
* ����RecordNum�������;
* �����������ƣ�RECORDNUM;
* sampleId�б�B20150101_RECORDNUM;
* �����б�spin getnum allnum lastdate;
* input:
*	baseDate	��׼ʱ��;
%macro grsRecordNum(baseDate=,endDate=) /parmbuff;
	%put grsRecordNum: start to generate RecordNum! param=&syspbuff;
	%if &baseDate= %then %error(grsRecordNum: Required param is empty! param=&syspbuff);

	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate dtvEndDate dtvGetDate t t2 t3 t4;
	%local sample sampleId;

	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	%let dtvEndDate=%dsToDtv(&endDate.);
	%let sampleId=B&baseDate._RECORDNUM;	

	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;

	%let &sample.baseDate=&baseDate;
	%let &sample.endDate=&endDate.;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=grsRecordNum;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=RECORDNUM;
	%let &sample.amountVars=getnum allnum;
	%let &sample.defaultValue=%str(getnum=1 allnum=1);

	* -- �������ɴ��� --;
	
	* ��nfcs.sino_credit_record����ȡ��¼;	
    data &t.;
     set &nfcs..sino_credit_record(keep=sname SCERTTYPE SCERTNO DREQUESTTIME IREQUESTTYPE
                                  where=(&dtvBaseDate. ge drequesttime le &dtvEndDate.));
     
	run;
/*	���ɲ�ѯ�� getnum����� allnum��ѯ��*/
	proc sort data=&t.;
     by sname scerttype scertno drequesttime;
	run;
	data &t.;
     set &t.;
	 by sname scerttype scertno drequesttime;
	 retain getnum;
	 retain allnum;
	 if first.scertno then do;
      getnum=0;
	  allnum=0;
	 end;
	 if IREQUESTTYPE in(0 1 2 6) then getnum+1;
	 allnum+1;
	 if last.scertno;
	run;
/*	��person_certification��ȡspin*/
	data &t2.;
     set &nfcs..sino_person_certification(keep=spin sname scerttype scertno dgetdate);
	run;
	proc sort data=&t2.;
     by sname scerttype scertno dgetdate;
	run;
	data &t2.;
     set &t2.;
	 by sname scerttype scertno dgetdate;
	 if last.scertno;
	run;
/*	�ϲ���������spinΪ����*/
	data &&&sample.data(drop=sname SCERTTYPE SCERTNO IREQUESTTYPE dgetdate);
     merge &t.(in=ina) &t2.(in=inb);
	 by sname scerttype scertno;
	 if ina;
	 if inb;
	run;

/*	%if &SQLOBS<1 %then %error(grsDemographics: No matched record!);*/


	%clearLabel(&t);

	* ���ݹ���������;

	* -- grs��׼������������ --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);

%mend;

* ---- macro grsEmployment ----;
* ����employment�������;
* �����������ƣ�EMPLOYMENT;
* sampleId�б�B20150101_EMPLOYMENT;
* �����б�occupation title technicalTitleLevel annualIncome;
* input:
*	baseDate	��׼ʱ��;
*	res			���������������ݲ�����;

%macro grsEmployment(baseDate=,res=) /parmbuff;
	%put grsEmployment: start to generate employment! param=&syspbuff;
	%if &baseDate= %then %error(grsEmployment: Required param is empty! param=&syspbuff);

	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate dtvGetDate t t2 t3 t4;
	%local sample sampleId;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	%let sampleId=B&baseDate._EMPLOYMENT;	
	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;
	
	%let &sample.baseDate=&baseDate;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=grsEmployment;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=EMPLOYMENT;
	%let &sample.enumVars=occupation title techTitle;
	%let &sample.moneyVars=annualIncome;
	%let &sample.defaultValue=%str(occupation='Z' title=9 techTitle=9);

	* -- �������ɴ��� --;
	
	* ��person_employment����ȡ��¼;
	proc sql noprint;
		create table &t as (
			select 	spin as spin,
					dgetdate,
					soccupation as occupation,
					iposition as title,
					ititle as techTitle,
					iannualincome as annualIncome
			from &nfcs..person_employment
			where dgetdate<&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(grsEmployment: No matched record!);

	* �޳��ظ���¼,��dgetdateȡ���µ�һ����¼;
	proc sort data=&t;
		by spin dgetdate;
	quit;

	%clearLabel(&t);

	data &&&sample.data;
		set &t;
		keep spin occupation title techTitle annualIncome;
		by spin dgetdate;
		if last.spin then output;
	run;

	* -- grs��׼������������ --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro grsLoanCount ----;
* ָ�����ƣ�ʱ�������������;
* ָ��˵��������ָ��ʱ�㣬�������ڳ����Ĵ������;
* ָ�����ͣ�debt profile;
* �����������ƣ�LC;
* sampleId�б�B20150101_LOANCOUNT;
* �����б�
*	LC					ʱ����Ч�������;
*	LC_UNSECURED		ʱ����Ч���ô������;
*	LC_SECURED			ʱ����Ч��Ѻ�������;
* input:
*	baseDate			��׼ʱ��;
*	res					���������������ݲ�����;

%macro grsLoanCount(baseDate=,res=) /parmbuff;
	%put grsLoanCount: start to generate loanCount! param=&syspbuff;
	%if &baseDate= %then %error(grsLoanCount: Required param is empty! param=&syspbuff);

	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate dtvGetDate t t2 t3 t4;
	%local sample sampleId;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	%let sampleId=%upcase(B&baseDate._loanCount);	
	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;
	
	%let &sample.baseDate=&baseDate;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=grsLoanCount;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=loanCount;
	%let &sample.amountVars=LC LC_UNSECURED LC_SECURED;
	%let &sample.defaultValue=%do_over(values=&&&sample.amountVars,phrase=%str(?=0));
	%let &sample.defaultValue=%str(&&&sample.defaultValue);

	* -- �������ɴ��� --;

	* ��1�� ɸѡ;
	* ��Ч����������ޱ����ס��׼����;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,dgetdate,iaccountstat,iguaranteeway 
			from &nfcs..loan
			where ddateopened<&dtvBaseDate and ddateclosed>=&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(grsLoanCount: No matched record!);

	* ��2�� �޳�;
	* ��Ϣ���£�ѡ��ÿ�ʴ���ľ����׼��������ļ�¼;
	* ����״̬������ѡ��״̬Ϊ�����Ĵ���;
	proc sort data=&t;
		by spin sorgcode saccount dgetdate;
	run;

	data &t;
		set &t;
		by spin sorgcode saccount dgetdate;
		if last.saccount and iaccountstat=1 then output;
	run;

	proc sort data=&t;
		by spin sorgcode saccount;
	run;

	* ��3�� �������������ָ��;
	
	data &&&sample.data;
		retain LC 0 LC_UNSECURED 0 LC_SECURED 0;
		keep spin LC LC_UNSECURED LC_SECURED;
		set &t;
		by spin sorgcode saccount;
		if iguaranteeway=4 then LC_UNSECURED=LC_UNSECURED+1;
		else LC_SECURED=LC_SECURED+1;
		LC=LC+1;
		if last.spin then do;
			output;
			LC=0;
			LC_UNSECURED=0;
			LC_SECURED=0;
		end;
	run;

	* -- grs��׼������������ --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro grsNewLoanCount ----;
* ָ�����ƣ������������;
* ָ��˵��������ָ��ʱ�䷶Χ�ڣ������Ĵ������;
* ָ�����ͣ�debt profile;
* �����������ƣ�NLC;
* sampleId�б�B20150101_NEWLOANCOUNT;
* �����б�
*	NLC					�����������;
*	NLC_UNSECURED		�������ô������;
*	NLC_SECURED			���������ô������;
* ������׺��
*	_T[inc][int��ʶ��]	�����ڼ��׺�����磺NLC_UNSECURED_T6M��ʾ�Ի�׼����6����������;
* input:
*	baseDate	��׼ʱ��;
*	int			�ڼ�ʱ�䵥λ;	
*	inc			�ڼ䳤��;
*	res			���������������ݲ�����;

%macro grsNewLoanCount(baseDate=,int=,inc=,res=) /parmbuff;
	%put grsLoanCount: start to generate loanCount! param=&syspbuff;
	%if &baseDate= or &int= or &inc= %then %error(grsLoanCount: Required param is empty! param=&syspbuff);

	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate dtvGetDate dtvStartDate t t2 t3 t4;
	%local sample sampleId;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	%let sampleId=%upcase(B&baseDate._newLoanCount);	
	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;
	
	%let &sample.baseDate=&baseDate;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=grsNewLoanCount;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=newLoanCount;

	* -- �������ɴ��� --;
	%if %upcase(&int)=%upcase(dtyear) %then %let intTag=Y;
	%else %if %upcase(&int)=%upcase(dtmonth) %then %let intTag=M;
	%else %if %upcase(&int)=%upcase(dtday) %then %let intTag=D;
	%else %error(grsNewLoanCount: INT must be dtyear|dtmonth|dtday ! int=&int);
	
	%let &sample.amountVars=%do_over(values=&inc,phrase=%str(NLC_T?&intTag NLC_SECURED_T?&intTag NLC_UNSECURED_T?&intTag));
	%let &sample.defaultValue=%do_over(values=&&&sample.amountVars,phrase=%str(?=0));
	%let &sample.defaultValue=%str(&&&sample.defaultValue);

	%local arrayVars;%let arrayVars=%arrayVars(name=incs,values=&inc);
	%local &arrayVars;%array(incs,values=&inc);
	%local dtvStartDate scopeSuffix i interval;

	%put grsNewLoanCount: vars list:;
	%do i=1 %to &incsN;
		%if &&incs&i<=0 %then %error(grsNewLoanCount: All INC must be larger then 0! inc=&inc);
		%let dtvStartDate=%intnx(&int,&dtvBaseDate,%eval(-&&incs&i));
		%put grsNewLoanCount: %dtvToDs(&dtvStartDate)-&baseDate NLC_T&&incs&i..&intTag NLC_SECURED_T&&incs&i..&intTag NLC_UNSECURED_T&&incs&i..&intTag; 
	%end;

	* Step1 �ڼ�ɸѡ;
	proc sql;	
		create table &t as (
			select spin,sorgcode,saccount,iguaranteeway,ddateopened,dgetdate
			from &nfcs..loan
			where ddateopened<&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(grsNewLoanCount: No matched record!);

	* Step2 ��ÿ�ʴ���ȡ���µ�һ����¼;
	proc sort data=&t;
		by spin sorgcode saccount dgetdate;
	quit;
	data &t;
		set &t;
		by spin sorgcode saccount dgetdate;
		if last.saccount then output;
	run;
	proc sort data=&t;
		by spin sorgcode saccount;
	quit;

	* Step3 ָ�����;
	data &&&sample.data;
		set &t;
		retain %do_over(values=&inc,phrase=%str(NLC_T?&intTag 0 NLC_SECURED_T?&intTag 0 NLC_UNSECURED_T?&intTag 0));
		keep spin %do_over(values=&inc,phrase=%str(NLC_T?&intTag NLC_SECURED_T?&intTag NLC_UNSECURED_T?&intTag));
		by spin sorgcode saccount;
		%do i=1 %to &incsN;
			%let interval=&&incs&i;
			%let dtvStartDate=%intnx(&int,&dtvBaseDate,%eval(-&interval));
			%let scopeSuffix=T&interval.&intTag;
			if ddateopened>=&dtvStartDate then do;
				NLC_&scopeSuffix=NLC_&scopeSuffix + 1;
				if iguaranteeway=4 then NLC_UNSECURED_&scopeSuffix=NLC_UNSECURED_&scopeSuffix + 1;
				else NLC_SECURED_&scopeSuffix = NLC_SECURED_&scopeSuffix + 1;
			end;
		%end;
	
		if last.spin then do;
			output;
			%do_over(values=&inc,phrase=%str(NLC_T?&intTag=0;NLC_SECURED_T?&intTag=0;NLC_UNSECURED_T?&intTag=0;));
		end;
	run;

	* -- grs��׼������������ --;

	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro genLoanList ----;
* ����ָ��ʱ�䷶Χ�ڵĴ����嵥;
* ����;
* intput:
*	baseDate			��׼ʱ��;
* output:
*	LL�����嵥���ݼ�	����spin,sorgcode,saccount,iguaranteeway���緵������Ϊ������Ŀ��д������;

%macro genLoanList(baseDate=,startDate=,out=) /parmbuff;
	%put genLoanList: start to generate LoanDefaultList! param=&syspbuff;
	%if &baseDate= or &startDate= or &out= %then %error(genLoanList: Required param is empty! param=&syspbuff);

	* -- ��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate t t2 t3 t4;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;

	%let dtvBaseDate=%dsToDtv(&baseDate);
	%let dtvStartDate=%dsToDtv(&startDate);
	
	* Step 1 ����ɸѡ;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,iguaranteeway,dgetdate,iamountpastdue
			from &nfcs..loan
			where ddateopened<&dtvBaseDate and ddateclosed>=&dtvStartDate
		);
	quit;

	%if &SQLOBS=0 %then %goto CLEANUP;
	
	* Step 2 ɸѡÿ�ʴ��������һ����¼;
	proc sort data=&t;
		by spin sorgcode saccount dgetdate;
	quit;

	data &out(index=(sss=(spin sorgcode saccount) /unique));
		set &t;
		keep spin sorgcode saccount iguaranteeway;
		by spin sorgcode saccount dgetdate;
		if last.saccount then output;
	run;

	* ��������;
	%CLEANUP:
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro grsHasLoan ----;
* ָ�����ƣ��д���ҵ����;
* ָ��˵�����û���ָ����ʱ����ڣ��д���ҵ�����;
* ָ�����ͣ�debt profile;
* �����������ƣ�HASHLOAN;
* sampleId�б�B20150101_HASLOAN;
* �����б�
*	SLC_T6M					�����ڴ�������;
*	HL_T6M					�������д���;
* input:
*	baseDate				��׼ʱ��;
*	res;

%macro grsHasLoan(baseDate=,int=,inc=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%let baseName=HasLoan;
	%let abbrName=HL;

	%put &macro: start to generate HasLoan! param=&syspbuff;
	%if &baseDate= or &int= or &inc= %then %error(&macro: Required param is empty! param=&syspbuff);

	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate dtvGetDate dtvStartDate t t2 t3 t4;
	%local sample sampleId;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	%let sampleId=%upcase(B&baseDate._&baseName);
	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;
	
	%let &sample.baseDate=&baseDate;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=&macro;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=&baseName &abbrName;

	* -- �������ɴ��� --;
	%local intTag;
	%local i startDate;
	%local SLCs totalSLCs;
	%local tempLL tempSLC;
	%local startDate interval scopeSuffix suffixes dvSLC dvHL;

	%if %upcase(&int)=%upcase(dtyear) %then %let intTag=Y;
	%else %if %upcase(&int)=%upcase(dtmonth) %then %let intTag=M;
	%else %if %upcase(&int)=%upcase(dtday) %then %let intTag=D;
	%else %error(&macro: INT must be dtyear|dtmonth|dtday ! int=&int);
	
	%local arrayVars;%let arrayVars=%arrayVars(name=incs,values=&inc);
	%local &arrayVars;%array(incs,values=&inc);


	* inc�������;
	%do i=1 %to &incsN;
		%if &&incs&i<=0 %then %error(&macro: All INC must be larger then 0! inc=&inc);
	%end;
	
	* Step1 ���ɻ�׼ʱ������ڴ�������baseDLL;
	%let SLCs=%str();
	%let suffixes=%str();
	%do i=1 %to &incsN;

		%let interval=&&incs&i;			* �����ڳ���;
		%let tempLL=%createTempDs;		* �����嵥;
		%let tempSLC=%createTempDs; 	* ����ͳ�����ݱ�;
		%let startDate=%dsIntnx(&int,&baseDate,-&interval);
		%let scopeSuffix=_T&interval.&intTag;
		%put &macro: Sample &&&sample.sampleId: start to generated var: SLC&scopeSuffix scope=&startDate.-&baseDate;

		* Step 1 ���ɹ۲��ڼ俪ʼʱ���tempDLL;
		%genLoanList(baseDate=&baseDate,startDate=&startDate,out=&tempLL);
	
		* Step 2 �����������ͳ��;
		* tempSLC��ʽΪ��spin,SLC_T6M;
		proc sql;
			create table &tempSLC as (
				select SPIN,count(*) as SLC&scopeSuffix
				from &tempLL
				group by spin
			);
		quit;
		%if &SQLOBS<1 %then %goto CONTINUE;
		proc sql;
			create index SPIN on &tempSLC;
		quit;
		%let SLCs=&SLCs &tempSLC; * �ǿ�HL���ݼ��嵥;
		%let suffixes=&suffixes &scopeSuffix;

		%CONTINUE:
		%let totalSLCs=totalSLCs &tempSLC; * ȫ��NDC���ݼ��嵥;
	%end;

	%if &suffixes= %then %error(&macro: All INC return 0 record!);

	* Step5 �ϲ��ǿյ�ΥԼ�������ݼ������ȱʧֵ --;
	data &&&sample.data;
		merge &SLCs ;
		by spin;
		%do_over(values=&suffixes,phrase=%str(
			if missing(SLC?) then SLC?=0;
			if SLC?>0 then HL?=1;else HL?=0;
		));
	run;

	%let &sample.amountVars=%do_over(values=&suffixes,phrase=SLC?);
	%let &sample.binaryVars=%do_over(values=&suffixes,phrase=HL?);
	%let dvSLC=%do_over(values=&suffixes,phrase=%str(SLC?=0));
	%let dvHL=%do_over(values=&suffixes,phrase=%str(HL?=0));
	%let &sample.defaultValue=%str(&dvSLC &dvHL);

	* -- �������� --;
	%dropDs(&totalSLCs);

	* -- grs��׼������������ --;

	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;


* --- macro grsLoanBalance ----;
* ָ�����ƣ�ʱ��������������;
* ָ��˵��������ָ��ʱ��㣬�û�������δ����������ܶ�;
* ָ�����ͣ�debt profile;
* �����������ƣ�LOANBALANCE;
* sampleId�б�B20150101_LOANBALANCE;
* �����б�
*	LB					ʱ��������������;
*	LB_UNSECURED		ʱ����������ô������;
*	LB_SECURED			ʱ������������ô������;
* input:
*	baseDate			��׼ʱ��;
*	res;

%macro grsLoanBalance(baseDate=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%let baseName=LoanBalance;
	%let abbrName=LB;

	%put &macro: Start to generate &baseName! param=&syspbuff;
	%if &baseDate= %then %error(&macro: Required param is empty! param=&syspbuff);

	
	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate t t2 t3 t4;
	%local sample sampleId;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	%let sampleId=%upcase(B&baseDate._&baseName);	
	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;
	
	%let &sample.baseDate=&baseDate;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=&macro;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=&baseName &abbrName;
	%let &sample.moneyVars=&abbrName &abbrName._UNSECURED &abbrName._SECURED;
	%let &sample.defaultValue=%do_over(values=&&&sample.moneyVars,phrase=%str(?=0));
	%let &sample.defaultValue=%str(&&&sample.defaultValue);

	* -- �������ɴ��� --;

	* ��1�� ����ɸѡ;
	* �����ʱ���ڻ�׼����ǰ;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,dgetdate,iaccountstat,iguaranteeway,ibalance
			from &nfcs..sino_loan
			where ddateopened<&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(&macro: No matched record!);

	* ��2�� ɸѡ;
	* ��Ϣ���£�ѡ��ÿ�ʴ���ľ����׼��������ļ�¼;
	* �ǽ������޳����д���״̬Ϊ����Ĵ���;
	proc sort data=&t;
		by spin sorgcode saccount dgetdate;
	run;

	data &t;
		set &t;
		by spin sorgcode saccount dgetdate;
		if last.saccount and iaccountstat^=3 then output;
	run;

	proc sort data=&t;
		by spin sorgcode saccount;
	run;

	* ��3�� ������������ָ��;
	
	data &&&sample.data;
		retain &abbrName 0 &abbrName._UNSECURED 0 &abbrName._SECURED 0;
		keep spin &abbrName &abbrName._UNSECURED &abbrName._SECURED;
		set &t;
		by spin sorgcode saccount;
		if iguaranteeway=4 then &abbrName._UNSECURED=&abbrName._UNSECURED+ibalance;
		else &abbrName._SECURED=&abbrName._SECURED+ibalance;
		&abbrName=&abbrName+ibalance;
		if last.spin then do;
			output;
			&abbrName=0;
			&abbrName._UNSECURED=0;
			&abbrName._SECURED=0;
		end;
	run;

	* -- grs��׼������������ --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro genDefaultLoanList ----;
* ����ָ��ʱ�������ڴ�������;
* �������������ڸ��������������ڴ�������;
* intput:
*	baseDate			��׼ʱ��;
* output:
*	DLL�����嵥���ݼ�	����spin,sorgcode,saccount,iguaranteeway���緵������Ϊ������Ŀ��д������;
%macro genDefaultLoanList(baseDate=,out=) /parmbuff;
	%put genLoanDefaultList: start to generate LoanDefaultList! param=&syspbuff;
	%if &baseDate= or &out= %then %error(genLoanDefaultList: Required param is empty! param=&syspbuff);

	* -- ��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate t t2 t3 t4;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	* Step 1 ����ɸѡ;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,iguaranteeway,dgetdate,iamountpastdue
			from &nfcs..loan
			where ddateopened<&dtvBaseDate
		);
	quit;

	%if &SQLOBS=0 %then %goto CLEANUP;
	
	* Step 2 ɸѡÿ�ʴ��������һ����¼�������ڽ�����0����ͬ���˻���ǰ״̬Ϊ����;
	proc sort data=&t;
		by spin sorgcode saccount dgetdate;
	quit;

	data &out(index=(sss=(spin sorgcode saccount) /unique));
		set &t;
		keep spin sorgcode saccount iguaranteeway;
		by spin sorgcode saccount dgetdate;
		if last.saccount and iamountpastdue>0 then output;
	run;

	* ��������;
	%CLEANUP:
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro grsNewDefaultCount ----;
* ָ�����ƣ�����ΥԼ�������;
* ָ��˵��������ָ��ʱ�䷶Χ�ڣ�������ΥԼ�������;
* ָ�����ͣ�bad profile;
* ָ���߼���
*	�ֱ��������������ʱ���ΥԼ�����嵥��������ʱ���嵥L0�г����˽���ʱ���嵥L-t��δ���ֵ�ΥԼ�������Ϊ������;
*	����δ������L-t�а������������δΥԼ�������ݣ����������������ͬ��ͬ;
* �����������ƣ�NDC;
* sampleId�б�B20150101_NEWDEFAULTCOUNT;
* �����б�
*	NDC_T6M				������Ϊ6���µ�����ΥԼ�������;
*	HD_T6M				����ΥԼ�����������1;

* ������׺��
*	_T[inc][int��ʶ��]	�����ڼ��׺�����磺NLC_T6M��ʾ�Ի�׼���ڻ���6����������ΥԼ�������;
* input:
*	baseDate	��׼ʱ��;
*	int			�ڼ�ʱ�䵥λ;	
*	inc			�ڼ䳤��;
*	res			���������������ݲ�����;

%macro grsNewDefaultCount(baseDate=,int=,inc=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%put &macro: start to generate loanCount! param=&syspbuff;
	%if &baseDate= or &int= or &inc= %then %error(&macro: Required param is empty! param=&syspbuff);

	* -- ������������ --;

	%let baseName=NewDefaultCount;
	%let abbrName=NDC;

	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate;
	%local sample sampleId;
	%let dtvBaseDate=%dsToDtv(&baseDate);

	%let sampleId=%upcase(B&baseDate._&baseName);
	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;
	%let &sample.baseDate=&baseDate;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=&macro;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=&baseName &abbrName;

	* -- �������ɴ��� --;
	%local intTag;
	%local baseDLL tempDLL tempNDL tempNDC;
	%local NDCs totalNDCs;
	%local i startDate varName vars;

	%if %upcase(&int)=%upcase(dtyear) %then %let intTag=Y;
	%else %if %upcase(&int)=%upcase(dtmonth) %then %let intTag=M;
	%else %if %upcase(&int)=%upcase(dtday) %then %let intTag=D;
	%else %error(&macro: INT must be dtyear|dtmonth|dtday ! int=&int);
	
	%local arrayVars;%let arrayVars=%arrayVars(name=incs,values=&inc);
	%local &arrayVars;%array(incs,values=&inc);
	%local dtvStartDate i interval scopeSuffix suffixes dvNDC dvHD;

	* inc�������;
	%do i=1 %to &incsN;
		%if &&incs&i<=0 %then %error(&macro: All INC must be larger then 0! inc=&inc);
	%end;

	* Step1 ���ɻ�׼ʱ������ڴ�������baseDLL;
	%let baseDLL=%createTempDs;
	%genDefaultLoanList(baseDate=&baseDate,out=&baseDLL);
	%getDsObs(ds=&baseDLL,res=&tres);%put baseDLL &baseDLL obs=&&&tres;
	%let NDCs=%str();
	%let suffixes=%str();
	%do i=1 %to &incsN;
		%let interval=&&incs&i;		* ��ʼʱ��Ŀ����ڳ���;
		%let tempDLL=%createTempDs; * ��ʼʱ������ڴ�������;
		%let tempNDL=%createTempDs; * �ڼ����������ڴ�������;
		%let tempNDC=%createTempDs; * �ڼ����������ڴ�������;
		%let startDate=%dsIntnx(&int,&baseDate,-&interval);
		%let scopeSuffix=_T&interval.&intTag;
		%let varName=&abbrName.&scopeSuffix;
		%put &macro: Sample &&&sample.sampleId: start to generated var: &varName scope=&startDate.-&baseDate;

		* Step2 ���ɹ۲��ڼ俪ʼʱ���tempDLL;
		%genDefaultLoanList(baseDate=&startDate,out=&tempDLL);

		* Step3 ��������ΥԼ�����嵥;
		proc sql;
			create table &tempNDL as (
				select a.spin,a.sorgcode,a.saccount
				from &baseDLL as a
				left join &tempDLL as b
				on a.spin=b.spin and a.sorgcode=b.sorgcode and a.saccount=b.saccount
				where b.spin is null and b.sorgcode is null and b.saccount is null
			);
		quit;
		%if &SQLOBS<1 %then %goto CONTINUE;

		%getDsObs(ds=&tempNDL,res=&tres);%put tempNDL &tempNDL obs=&&&tres;
		* Step4 ��������ΥԼ��������;
		proc sql;
			create table &tempNDC as (
				select spin,count(*) as &varName from &tempNDL group by spin
			);
			create index spin on &tempNDC;
		quit;
		%let NDCs=&NDCs &tempNDC; * �ǿ�NDC���ݼ��嵥;
		%let suffixes=&suffixes &scopeSuffix;

		%CONTINUE:
		%let totalNDCs=totalNDCs &tempNDC; * ȫ��NDC���ݼ��嵥;
		%dropDs(&tempDLL);
		%dropDs(&tempNDL);
	%end;
	%put &macro: Sample &&&sample.sampleId: vars=&vars;
	%if &suffixes= %then %error(&macro: All INC return 0 record!);

	* Step5 �ϲ��ǿյ�ΥԼ�������ݼ������ȱʧֵ --;
	data &&&sample.data;
		merge &NDCs ;
		by spin;
		%do_over(values=&suffixes,phrase=%str(
			if missing(NDC?) then NDC?=0;
			if NDC?>0 then HD?=1;else HD?=0;
		));
	run;

	%let &sample.amountVars=%do_over(values=&suffixes,phrase=NDC?);
	%let &sample.binaryVars=%do_over(values=&suffixes,phrase=HD?);
	%let dvNDC=%do_over(values=&suffixes,phrase=%str(NDC?=0));
	%let dvHD=%do_over(values=&suffixes,phrase=%str(HD?=0));
	%let &sample.defaultValue=%str(&dvNDC &dvHD);

	* -- �������� --;
	%dropDs(&totalNDCs);
	%dropDs(&baseDLL);

	* -- grs��׼������������ --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
%mend;

* --- macro  grsPastDueStatus ----;
* ָ�����ƣ����ڴ����ܶ�;
* ָ��˵��������ָ��ʱ��㣬������������ڴ������������ڴ����ƽ�����ڶ�;
* ָ��ʱ�����ͣ�SCOPE;
* ָ����Ϣ���ͣ�bad profile;
* �������ƣ�PastDueStatus;
* ������д��PDA;
* sampleId�б�B20150101_PASTDUEHISTORY;
* �����б�
*	PDC					��ǰ���ڴ�������;
*	PDAA				��ǰ���ڵ�ƽ�����;

* input:
*	baseDate			��׼ʱ��;

%macro grsPastDueStatus(baseDate=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%let baseName=PastDueStatus;
	%let abbrName=%str();

	%put &macro: Start to generate &baseName! param=&syspbuff;
	%if &baseDate= %then %error(&macro: Required param is empty! param=&syspbuff);

	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate t t2 t3 t4;
	%local sample sampleId;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	* -- ������Ϣ���� --;
	%let sampleId=%upcase(B&baseDate._&baseName);	
	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;
	%let &sample.baseDate=&baseDate;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=&macro;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=&baseName &abbrName;
	%let &sample.moneyVars=PDAA;
	%let &sample.amountVars=PDC;
	%let &sample.defaultValue=%str(PDC=0 PDAA=0);
	
	* -- �������ɴ��� --;

	* ��1�� ����ɸѡ;
	* �����ʱ���ڻ�׼����ǰ;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,dgetdate,iamountpastdue
			from &nfcs..loan
			where ddateopened<&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(&macro: No matched record!);

	* ��2�� ɸѡ;
	* ��Ϣ���£�ѡ��ÿ�ʴ���ľ����׼��������ļ�¼;
	proc sort data=&t;
		by spin sorgcode saccount dgetdate;
	run;

	data &t;
		set &t;
		by spin sorgcode saccount dgetdate;
		if last.saccount then output;
	run;

	proc sort data=&t;
		by spin sorgcode saccount;
	run;

	* ��3�� ���������ΥԼ����;
	
	proc sql;
		create table &&&sample.data as (
			select 	spin as SPIN,
					count(*) as PDC,
					mean(iamountpastdue) as PDAA
			from &t
			where iamountpastdue>0
			group by spin
		);
	quit;

	* -- grs��׼������������ --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro grsPastDueAmount ----;
* ָ�����ƣ����ڴ����ܶ�;
* ָ��˵��������ָ��ʱ��㣬�����ڴ�����;
* ָ�����ͣ�bad profile;
* �������ƣ�PastDueAmount;
* ������д��PDA;
* sampleId�б�B20150101_PASTDUEAMOUNT;
* �����б�
*	PDA					���ڴ�����;
*	PDA_30D				����30�������;
*	PDA_60D				����60�������;
*	PDA_90D				����90�������;
*	PDA_180D			����180�������;

* input:
*	baseDate			��׼ʱ��;

%macro grsPastDueAmount(baseDate=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%let baseName=PastDueAmount;
	%let abbrName=PDA;

	%put &macro: Start to generate &baseName! param=&syspbuff;
	%if &baseDate= %then %error(&macro: Required param is empty! param=&syspbuff);

	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate t t2 t3 t4;
	%local sample sampleId;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	%let sampleId=%upcase(B&baseDate._&baseName);	
	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;
	%let &sample.baseDate=&baseDate;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=&macro;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=&baseName &abbrName;
	%let &sample.moneyVars=PDA PDA_30D PDA_60D PDA_90D PDA_180D;
	%let &sample.defaultValue=%do_over(values=&&&sample.moneyVars,phrase=%str(?=0));
	%let &sample.defaultValue=%str(&&&sample.defaultValue);

	* -- �������ɴ��� --;

	* ��1�� ����ɸѡ;
	* �����ʱ���ڻ�׼����ǰ;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,dgetdate,iamountpastdue,iamountpastdue30,iamountpastdue60,iamountpastdue90,iamountpastdue180
			from &nfcs..loan
			where ddateopened<&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(&macro: No matched record!);

	* ��2�� ɸѡ;
	* ��Ϣ���£�ѡ��ÿ�ʴ���ľ����׼��������ļ�¼;
	proc sort data=&t;
		by spin sorgcode saccount dgetdate;
	run;

	data &t;
		set &t;
		by spin sorgcode saccount dgetdate;
		if last.saccount then output;
	run;

	proc sort data=&t;
		by spin sorgcode saccount;
	run;

	* ��3�� ������������ָ��;
	
	proc sql;
		create table &&&sample.data as (
			select 	spin as spin,
					sum(iamountpastdue) as PDA,
					sum(iamountpastdue30) as PDA_30D,
					sum(iamountpastdue60) as PDA_60D,
					sum(iamountpastdue90) as PDA_90D,
					sum(iamountpastdue180) as PDA_180D
			from &t
			where iamountpastdue>0
			group by spin
		);
	quit;

	* -- grs��׼������������ --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;


* --- macro genDefaultLoanList ----;
* ����ָ��ʱ�������ڴ�������;
* �������������ڸ��������������ڴ�������;
* intput:
*	baseDate			��׼ʱ��;
* output:
*	DLL�����嵥���ݼ�	����spin,sorgcode,saccount,iguaranteeway���緵������Ϊ������Ŀ��д������;
%macro genLoanPastDueStatus(baseDate=,startDate=,out=) /parmbuff;
	%put genLoanDefaultList: start to generate LoanDefaultList! param=&syspbuff;
	%if &baseDate= or &out= %then %error(genLoanDefaultList: Required param is empty! param=&syspbuff);

	* -- ��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate t t2 t3 t4;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;

	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	* Step 1 ����ɸѡ;
	%if &startDate= or &startDate=0 or %upcase(&startDate)=INF %then %let whereStr=%str(where ddateopened<&dtvBaseDate);
	%else %let whereStr=%str(where ddateopened<&dtvBaseDate and ddateclosed>=&dtvStartDate);
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,iguaranteeway,dgetdate,iamountpastdue
			from &nfcs..loan
			&whereStr
		);
	quit;

	%if &SQLOBS=0 %then %goto CLEANUP;
	
	* Step 2 ɸѡÿ�ʴ��������һ����¼�������ڽ�����0����ͬ���˻���ǰ״̬Ϊ����;
	proc sort data=&t;
		by spin sorgcode saccount dgetdate;
	quit;

	data &t;
		set &t;
		keep spin sorgcode saccount iguaranteeway;
		by spin sorgcode saccount dgetdate;
		if last.saccount then do;
			output;
		end;
	run;

	data &out(index=(sss=(spin sorgcode saccount) /unique));
		set &t;
		LPDA_GT180D=iamountpastdue180;
		LPDA_GT90D=iamountpastdue90+PDA_180D;
		LPDA_GT60D=iamountpastdue60+PDA_90D;
		LPDA_GT30D=iamountpastdue30+PDA_60;
		LPDA=iamountpastdue;
		LHPD=0;
		LHPD_GT30D=0;
		LHPD_GT60D=0;
		LHPD_GT90D=0;
		LHPD_GT18D=0;
		if LPDA>0 then LHPD=1;
		if LPDA_GT30D>0 then LHPD_GT30D=1;
		if LPDA_GT60D>0 then LHPD_GT60D=1;
		if LPDA_GT90D>0 then LHPD_GT60D=1;
		if LPDA_GT180D>0 then LHPD_GT60D=1;
	run;

	* ��������;
	%CLEANUP:
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

/*
* --- macro grsHasPastDue ----;
* ָ�����ƣ�����ΥԼ�������;
* ָ��˵��������ָ��ʱ�䷶Χ�ڣ�������ΥԼ�������;
* ָ�����ͣ�bad profile;
* ָ���߼���
*	�ֱ��������������ʱ���ΥԼ�����嵥��������ʱ���嵥L0�г����˽���ʱ���嵥L-t��δ���ֵ�ΥԼ�������Ϊ������;
*	����δ������L-t�а������������δΥԼ�������ݣ����������������ͬ��ͬ;
* �����������ƣ�NDC;
* sampleId�б�B20150101_NEWDEFAULTCOUNT;
* �����б�
*	NDC					����ΥԼ�������;
* �����б�
*	HPD_30D_T6M				���ڳ���30�죬������6����;
*	HPD_30D_T12M			���ڳ���30�죬������12����;
*	HPD_60D_T6M				���ڳ���60�죬������6����;
*	HPD_60D_T12M			���ڳ���60�죬������12����;
* input:
*	baseDate				��׼ʱ��;
*	outcomePeriod			�����ڳ���;	
*	good					�϶�Ϊ0�����������ڽ���������ѡ��0 30 60 90 180�����磬��ֵΪ30ʱ����ʾ�������ڴ���30���Ϊ0ʱ���϶�Ϊ���ã���Ӧ0
*	bad						�϶�Ϊ1�����������ڽ���������ѡ��0 30 60 90 180�����磬��ֵΪ60ʱ����ʾ�������ڴ���60�������0ʱ���϶�Ϊ���ڣ���Ӧ1;
* 	remove
*	res						���������������ݲ�����;



%macro grsHasPastDue(baseDate=,outcomPeriod=,good=,bad=,removeOldPastDue=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%put &macro: start to generate HasPastDue! param=&syspbuff;
	%if &baseDate= or &int= or &inc= %then %error(&macro: Required param is empty! param=&syspbuff);

	* -- ������������ --;

	%let baseName=NewDefaultCount;
	%let abbrName=NDC;

	* -- grs��׼��������׼�� --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate;
	%local sample sampleId;
	%let dtvBaseDate=%dsToDtv(&baseDate);

	%let sampleId=%upcase(B&baseDate._&baseName);
	%createSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;
	%let &sample.baseDate=&baseDate;
	%let &sample.genDate=%timestamp;
	%let &sample.genMacro=&macro;
	%let &sample.genParam=&syspbuff;
	%let &sample.inSampleIds=L1;
	%let &sample.desc=&baseName &abbrName;

	* -- �������ɴ��� --;
	%local intTag;
	%local baseDLL tempDLL tempNDL tempNDC;
	%local NDCs totalNDCs;
	%local i startDate varName vars;

	%if %upcase(&int)=%upcase(dtyear) %then %let intTag=Y;
	%else %if %upcase(&int)=%upcase(dtmonth) %then %let intTag=M;
	%else %if %upcase(&int)=%upcase(dtday) %then %let intTag=D;
	%else %error(&macro: INT must be dtyear|dtmonth|dtday ! int=&int);
	
	%local arrayVars;%let arrayVars=%arrayVars(name=incs,values=&inc);
	%local &arrayVars;%array(incs,values=&inc);
	%local dtvStartDate scopeSuffix i interval;

	* inc�������;
	%do i=1 %to &incsN;
		%if &&incs&i<=0 %then %error(&macro: All INC must be larger then 0! inc=&inc);
	%end;

	* Step1 ���ɻ�׼ʱ������ڴ���״̬baseLPDS;
	%let baseDLL=%createTempDs;
	%genLoanPastDueStatus(baseDate=,startDate=,out=);

	%do i=1 %to &incsN;
		%let interval=&&incs&i;		* ��ʼʱ��Ŀ����ڳ���;
		%let tempDLL=%createTempDs; * ��ʼʱ������ڴ�������;
		%let tempNDL=%createTempDs; * �ڼ����������ڴ�������;
		%let tempNDC=%createTempDs; * �ڼ����������ڴ�������;
		%let startDate=%dsIntnx(&int,&baseDate,-&interval);
		%let varName=&abbrName._T&interval.&intTag;
		%put &macro: Sample &&&sample.sampleId: start to generated var: &varName scope=&startDate.-&baseDate;

		* ���ɻ�׼/����ʱ������ڴ�����Ϣ��;
		%genLoanPastDueStatus(baseDate=&baseDate,startDate=&startDate,out=&baseLPDS);

		%if &removeOldPastDueLoan=0 %then %do;
			* ������ܵ�������Ϣ;
			proc sql;
				create table &t as (
					select spin as SPIN,
						sum(LHPD) as SLHPD,
						sum(LHPD_GT30D) as SLHPD_GT30D,
						sum(LHPD_GT60D) as SLHPD_GT60D,
						sum(LHPD_GT90D) as SLHPD_GT90D,
						sum(LHPD_GT180D) as SLHPD_GT180D,
					from &baseLPDS group by spin);
			quit;
			data &t;
				set &t;
				if SLHPD>0 then HPD=1 else HPD=0;
				if SLHPD_GT30D>0 then HPD_GT30D=1 else HPD_GT30D=0;
				if SLHPD_GT60D>0 then HPD_GT60D=1 else HPD_GT60D=0;
				if SLHPD_GT90D>0 then HPD_GT90D=1 else HPD_GT90D=0;
				if SLHPD_GT180D>0 then HPD_GT180D=1 else HPD_GT180D=0;
			run;
		%end;
		%else %do;
			* ���ɿ�ʼʱ������ڴ�����Ϣ;
			%genLoanPastDueStatus(baseDate=&startDate,out=&startLPDS);

			* �޸������Ʋ����½�������;
			data &startLPDS(index=(SSS=(spin sorgcode saccount) /unique));
				keep spin sorgcode saccount START_LHPD_GT30D START_LHPD_GT60D START_LHPD_GT90D START_LHPD_GT180D;
				set &startLPDS;
				START_LHPD_GT30D=LHPD_GT30D;
				START_LHPD_GT60D=LHPD_GT60D;
				START_LHPD_GT90D=LHPD_GT90D;
				START_LHPD_GT180D=LHPD_GT180D;
			run;

			* �޳���ʼʱ�Ѿ��ﵽΥԼ��׼�����ڴ���;
			data;
				set baseLPDS;
				set startLPDS key=SSS;
				keep LHPD_GT30D LHPD_GT60D LHPD_GT90D LHPD_GT180D
				if _IORC_=0 then do;
					output;
				end;
				else if START_LHPD_GT30D>0 and LHPD_GT30D>0 then LHPD_GT30D=-1;
				else if START_LHPD_GT60D>0 and LHPD_GT30D>0 then LHPD_GT60D=-1;
				else if START_LHPD_GT90D>0 and LHPD_GT30D>0 then LHPD_GT90D=-1;
				else if START_LHPD_GT180D>0 and LHPD_GT30D>0 then LHPD_GT180D=-1;
				end;
			run;
		%end;
	%end;
	%put &macro: Sample &&&sample.sampleId: vars=&vars;
	%if &vars= %then %error(&marco: All INC return 0 record!);

	* Step5 �ϲ��ǿյ�ΥԼ�������ݼ������ȱʧֵ --;
	data &&&sample.data;
		merge &NDCs ;
		by spin;
		%do_over(values=&vars,phrase=%str(
			if missing(?) then ?=0;
		));
	run;

	%let &sample.amountVars=&vars;
	%let &sample.defaultValue=%do_over(values=&&&sample.amountVars,phrase=%str(?=0));
	%let &sample.defaultValue=%str(&&&sample.defaultValue);

	* -- �������� --;
	%dropDs(&totalNDCs);
	%dropDs(&baseDLL);

	* -- grs��׼������������ --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
%mend;

*/
