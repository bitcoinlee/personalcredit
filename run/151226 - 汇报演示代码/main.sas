%include 'D:\server\sas\lib\utili.sas';
%let root=D:\Server\Sas\main\model\model_010\;
%include "&root.lib\globals.sas";
%include "&root.lib\base.sas";
%include "&root.lib\sample.sas";
%include "&root.lib\model.sas";
%include "&root.lib\rawSample.sas";
%include "&root.lib\score.sas";
%include "&root.lib\perf.sas";

options nonotes;

* ��������������ʾ��;
* 	B20150101_DEMOGRAPHICS				ԭʼ����ϵ��;
* 	B20150101_OP6					������������ָ�������б�����baseDateΪ�����baseDate��OP6��ʾ�������baseDate�����Ա���6����;
*	B20150101_OP6_SIMPLE_N10000				�����������������ͬ��MAIN����������������10000;
*	B20150101_OP6_BALANCE_N10000_P50		ƽ�������������ͬ��MAIN����������������10000��ƽ��pΪ50%;
*	S1_T1								��׼��������ѵ����������ָ����ʱ�䷶Χ����;
*	S1_T1V1								��׼����������֤���������ʶ���ʱ�䷶Χ����;


%macro run_sample_raw;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local i start;

	* ��������:ָ�����;
	* ����[2013��11��,2015��11��]���л�׼ʱ�������ݣ�����24����;
	%let start=20140101;
	%let dur=24;

	* point variables;

	%logbreak(Point variable generation start!);
	%do i=0 %to %eval(&dur-1);
		%let baseDate=%dsIntnx(dtmonth,&start,&i,same);
		%grsDemographics(baseDate=&baseDate,res=&tres);
		%grsEmployment(baseDate=&baseDate,res=&tres);
		%grsLoanCount(baseDate=&baseDate,res=&tres);
		%grsLoanBalance(baseDate=&baseDate,res=&tres);
		%grsPastDueAmount(baseDate=&baseDate,res=&tres);
		%grsPastDueStatus(baseDate=&baseDate,res=&tres);
	%end;

	* 201402 201403 201404 ����3��;
	%let start=20140301;
	%let dur=3;
	%do i=0 %to %eval(&dur-1);
		%let baseDate=%dsIntnx(dtmonth,&start,&i,same);
		%grsNewLoanCount(baseDate=&baseDate,int=dtmonth,inc=3);	
		%grsNewDefaultCount(baseDate=&baseDate,int=dtmonth,inc=3);
		%grsHasLoan(baseDate=&baseDate,int=dtmonth,inc=3);
	%end;

	* 201405 201406 201407 201408 201409 201410 ����6��;
	%let start=20140601;
	%let dur=6;
	%do i=0 %to %eval(&dur-1);
		%let baseDate=%dsIntnx(dtmonth,&start,&i,same);
		%grsNewLoanCount(baseDate=&baseDate,int=dtmonth,inc=3 6);	
		%grsNewDefaultCount(baseDate=&baseDate,int=dtmonth,inc=3 6);
		%grsHasLoan(baseDate=&baseDate,int=dtmonth,inc=3 6);
	%end;

	* 201411 ... 201512 ����13��;
	%let start=20141201;
	%let dur=13;
	%do i=0 %to %eval(&dur-1);
		%let baseDate=%dsIntnx(dtmonth,&start,&i,same);
		%grsNewLoanCount(baseDate=&baseDate,int=dtmonth,inc=3 6 12);	
		%grsNewDefaultCount(baseDate=&baseDate,int=dtmonth,inc=3 6 12);
		%grsHasLoan(baseDate=&baseDate,int=dtmonth,inc=3 6 12);
	%end;
%mend;

* ����������������;

%macro gssTemp;
	%grsNewLoanCount(baseDate=20151201,int=dtmonth,inc=3 6 12);	
	%grsNewDefaultCount(baseDate=20151201,int=dtmonth,inc=3 6 12);
	%grsHasLoan(baseDate=20151201,int=dtmonth,inc=3 6 12);
%mend;



* -- �����ϲ� --;
* ����Ա��������ϲ�;
* ���������B20150101_OP6,B20150101_OP12;
* ��������Ԥ���ڼ䡢��׼ʱ����������;

%macro gssMain(baseDate=,outcomePeriod=);
	%local tres;%let tres=%createTempVar;%local &tres;
	%local depBaseDate indepBaseDate;
	%local inSampleIds outSampleId sample;

	%let depBaseDate=&baseDate;
	%let indepBaseDate=%dsIntnx(dtmonth,&baseDate,-&outcomePeriod);

	%put baseDate=&baseDate depBaseDate=&depBaseDate indepBaseDate=&indepBaseDate;

	* ��hasLoan��Ϊ���ӻ�׼��;
	%let inSampleIds=B&depBaseDate._HASLOAN
					B&indepBaseDate._DEMOGRAPHICS
					B&indepBaseDate._EMPLOYMENT
					B&indepBaseDate._LOANCOUNT
					B&indepBaseDate._NEWLOANCOUNT
					B&indepBaseDate._LOANBALANCE
					B&indepBaseDate._PASTDUESTATUS
					B&indepBaseDate._PASTDUEAMOUNT
					B&depBaseDate._NEWDEFAULTCOUNT
					;
	
	* mainSampleIdʾ��: B20150101_OP6;
	%let outSampleId=B&baseDate._OP&outcomePeriod;
	%deleteSample(sampleId=&outSampleId);

	%put gssMain: 	inSampleIds=&inSampleIds;
	%put gssMain:	outSampleId=&outSampleId;
	%mergeSample(inSampleIds=&inSampleIds,outSampleId=&outSampleId,type=left,sameBaseDate=0,res=&tres,force=1);

	* ��������;
	%loadSample(sampleId=&outSampleId,res=&tres);%let sample=&&&tres;
	%if %dsVarExist(&&&sample.data,HL_T&outcomePeriod.M)=0 %then %error(gssMain: No required HL var! HL_T&outcomePeriod.M);

	%put sample obs before filtering: &&&sample.obs;

	data &&&sample.data;
		set &&&sample.data;
		retain missCounter 0;
		*drop missCounter;
		
		* ���·���;
		if HL_T&outcomePeriod.M=0 then delete;
		if SEX=0 then SEX=9;
		if Marriage=21 or Marriage=22 or Marriage=23 then Marriage=20;
		if EDULEVEL=50 or  EDULEVEL=60 or EDULEVEL=70 or EDULEVEL=80 or EDULEVEL=90 then EDULEVEL=40;
		if EDUDEGREE=0 then EDUDEGREE=9;
		if OCCUPATION='Y' then OCCUPATION='Z';

		* ȱʧ����ͳ�������;
		missCounter=0;

		if SEX=9 then missCounter=missCounter+1;
		if MARRIAGE=90 then missCounter=missCounter+1;
		if EDULEVEL=99 then missCounter=missCounter+1;
		if EDUDEGREE=9 then missCounter=missCounter+1;
		if missing(AGE) or AGE=0 or AGE>70 or AGE<14 then missCounter=missCounter+1;
		if OCCUPATION='Z' then missCounter=missCounter+1;
		if TITLE=9 then missCounter=missCounter+1;
		if TECHTITLE=9 then missCounter=missCounter+1;
		if missing(ANNUALINCOME) then missCounter=missCounter+1;
		if missCounter>4 then delete;
	run;

	%saveSample(sample=&sample);
	%put sample obs after filtering: &&&sample.obs;
	%dropSample(&sample);
	
	* misDummy����;
	* ��Ҫ���age annualIncome;
	%gsMisDummy(inSampleId=&outSampleId,defaultEqMiss=1);

	* logMoney����;
	%gsLogMoney(inSampleId=&outSampleId,res=&tres);%put gsLogMoney result:&&&tres;

%mend;


%macro run_sample_op;

	%gssMain(baseDate=20140901,outcomePeriod=6);
	%gssMain(baseDate=20141001,outcomePeriod=6);
	%gssMain(baseDate=20141101,outcomePeriod=6);
	%gssMain(baseDate=20141201,outcomePeriod=6);

	%gssMain(baseDate=20150101,outcomePeriod=6);
	%gssMain(baseDate=20150201,outcomePeriod=6);
	%gssMain(baseDate=20150301,outcomePeriod=6);
	%gssMain(baseDate=20150401,outcomePeriod=6);
	%gssMain(baseDate=20150501,outcomePeriod=6);
	%gssMain(baseDate=20150601,outcomePeriod=6);
	%gssMain(baseDate=20150701,outcomePeriod=6);
	%gssMain(baseDate=20150801,outcomePeriod=6);
	%gssMain(baseDate=20150901,outcomePeriod=6);
	%gssMain(baseDate=20151001,outcomePeriod=6);
	%gssMain(baseDate=20151101,outcomePeriod=6);
	%gssMain(baseDate=20151201,outcomePeriod=6);

	
	%gssMain(baseDate=20150301,outcomePeriod=12);
	%gssMain(baseDate=20150401,outcomePeriod=12);
	%gssMain(baseDate=20150501,outcomePeriod=12);
	%gssMain(baseDate=20150601,outcomePeriod=12);
	%gssMain(baseDate=20150701,outcomePeriod=12);
	%gssMain(baseDate=20150801,outcomePeriod=12);
	%gssMain(baseDate=20150901,outcomePeriod=12);
	%gssMain(baseDate=20151001,outcomePeriod=12);
	%gssMain(baseDate=20151101,outcomePeriod=12);
	%gssMain(baseDate=20151201,outcomePeriod=12);

%mend;

%macro run_show_op_info;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	
	%local i baseStartDate sample;

	%let baseStartDate=20141201;

	%do i=0 %to 12;
		%let baseDate=%dsIntnx(dtmonth,&baseStartDate,&i);
		%let sampleId=B&baseDate._OP6;
		%loadSample(sampleId=B&baseDate._OP6,res=&tres);%let sample=&&&tres;
		title "&baseDate sampleId=&sampleId";
		proc freq data=&&&sample.data;
			tables HD_T6M;
		quit;
		%dropSample(&sample);
	%end;
%mend;
/*
%macro gssSimpleSample(baseStartDate=,baseEndDate=,outcomePeriod=,n=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%if &baseStartDate= or &baseEndDate= or &outcomePeriod= or &n= %then %error(&macro: Requried param is empty! param=&syspbuff);

	%local basePeriod baseDate;
	%local inSampleId outSampleId;
	%local i;

	%let basePeriod=%dsIntck(dtmonth,&baseEndDate,&baseStartDate);
	
	%do i=0 %to %eval(&basePeriod-1);

		%let baseDate=%dsIntnx(dtmonth,&baseStartDate,&i);
		
		* simpleSample���� B20150101_OP6_SIMPLE_N6000;
		%let inSampleId=B&baseDate._OP&outcomePeriod;
		%let outSampleId=B&baseDate._OP&outcomePeriod._SIMPLE_N&n.;
		%gsSimpleSample(inSampleId=&mainSampleId,outSampleId=&simpleSampleId,n=&n);
	%end;
%mend;

%macro gssBalancedSample(baseStartDate=,baseEndDate=,outcomePeriod=,n=,p=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%if &baseStartDate= or &baseEndDate= or &outcomePeriod= or &n= or &p= %then %error(&macro: Requried param is empty! param=&syspbuff);

	%local basePeriod baseDate;
	%local inSampleId outSampleId;
	%local i;

	%let basePeriod=%dsIntck(dtmonth,&baseEndDate,&baseStartDate);
	
	%do i=0 %to %eval(&basePeriod-1);

		%let baseDate=%dsIntnx(dtmonth,&baseStartDate,&i);
		
		* simpleSample���� B20150101_OP6_SIMPLE_N10000;
		%let inSampleId=B&baseDate._OP&outcomePeriod;
		%let outSampleId=B&baseDate._OP&outcomePeriod._BALANCED_N&n._P%floor(%sysevalf(&p*100));
		%gsBalancedSample(inSampleId=&mainSampleId,outSampleId=&simpleSampleId,n=&n,p=&p);
	%end;
%mend;
*/

* �����������ƽ�����;
%macro run_sample_simple;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;


	%deleteSample(sampleId=B20150601_OP6_BS_N6000_P50);
	%deleteSample(sampleId=B20150701_OP6_BS_N6000_P50);
	%deleteSample(sampleId=B20150801_OP6_BS_N6000_P50);
	%deleteSample(sampleId=B20150901_OP6_BS_N6000_P50);
	%deleteSample(sampleId=B20151001_OP6_BS_N6000_P50);
	%deleteSample(sampleId=B20151101_OP6_BS_N6000_P50);
	%deleteSample(sampleId=B20151201_OP6_BS_N6000_P50);

	%gsBalancedSample(inSampleId=B20150601_OP6,outSampleId=B20150601_OP6_BS_N6000_P50,n=6000,p=0.5,varRsp=HD_T6M,force=1,event=1,res=&tres);%put result=&&&tres;
	%gsBalancedSample(inSampleId=B20150701_OP6,outSampleId=B20150701_OP6_BS_N6000_P50,n=6000,p=0.5,varRsp=HD_T6M,force=1,event=1,res=&tres);%put result=&&&tres;
	%gsBalancedSample(inSampleId=B20150801_OP6,outSampleId=B20150801_OP6_BS_N6000_P50,n=6000,p=0.5,varRsp=HD_T6M,force=1,event=1,res=&tres);%put result=&&&tres;
	%gsBalancedSample(inSampleId=B20150901_OP6,outSampleId=B20150901_OP6_BS_N6000_P50,n=6000,p=0.5,varRsp=HD_T6M,force=1,event=1,res=&tres);%put result=&&&tres;
	%gsBalancedSample(inSampleId=B20151001_OP6,outSampleId=B20151001_OP6_BS_N6000_P50,n=6000,p=0.5,varRsp=HD_T6M,force=1,event=1,res=&tres);%put result=&&&tres;
	%gsBalancedSample(inSampleId=B20151101_OP6,outSampleId=B20151101_OP6_BS_N6000_P50,n=6000,p=0.5,varRsp=HD_T6M,force=1,event=1,res=&tres);%put result=&&&tres;
	%gsBalancedSample(inSampleId=B20151201_OP6,outSampleId=B20151201_OP6_BS_N6000_P50,n=6000,p=0.5,varRsp=HD_T6M,force=1,event=1,res=&tres);%put result=&&&tres;
	
%mend;
	
* ������׼����������;
* ʾ����
* startDate��20150101;
* ѵ������:6��;
* Ԥ���ڳ��ȣ�6����;
* ��������������ʽ��ƽ�����10000����ƽ�����5000;
* ����id��ʽ��
*	[Prefix]_T1			��1��ѵ������;
*	[Prefix]_V1			��1����֤����(���������);
* 	[Prefix]_T1V0		��1��T+0��֤����;
* 	[Prefix]_T1V1		��1��T+1��֤����;
* 	[Prefix]_T1V2		��1��T+2��֤����;
* 	...
* 	[Prefix]_T1V6		��1��T+6��֤����;
*	[Prefix]_T2			��2��ѵ������;
* 	[Prefix]_T2V1		��2��T+1��֤����;
*	����;
*  	[Prefix]_T6V6		��6��T+6��֤����;
*	[Prefix]
* 	���ư���ѵ������6������֤����36��;

%macro gssStandard(outSamplePrefix=,startBaseDate=,outcomePeriod=,rollPeriod=,trainP=,trainN=,validPeriod=,validN=,validP=,depVar=) /parmbuff;
	
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local baseDate validDate;
	%local sourceSampleId trainSampleId validSampleId validSourceSampleId;
	%local ts;
	%local i j;

	%if &outSamplePrefix= or &startBaseDate= or &outcomePeriod= or &rollPeriod= or &trainP= or &trainN= or &validN= or &validP= or &depVar= %then %error(&macro: Required param is empty! param=&syspbuff);

	* ����������;
	%do i=0 %to %eval(&rollPeriod-1);
		%let baseDate=%dsIntnx(dtmonth,&startBaseDate,&i);
		%let sourceSampleId=B&baseDate._OP&outcomePeriod;
		%let trainSampleId=&outSamplePrefix._T&i;

		* ѵ���������� - ƽ�����;
		%put &macro: start to generate train sample: &trainSampleId n=&trainN varRsp=&depVar p=&trainP;
		%gsBalancedSample(inSampleId=&sourceSampleId,outSampleId=&trainSampleId,n=&trainN,varRsp=&depVar,p=&trainP,force=1,res=&tres);

		* woe��������;
		%gsWoe(inSampleId=&trainSampleId,varRsp=&depVar);

		%let validSampleId=&outSamplePrefix._V&i;
		* ��֤�������� - �������� + ƽ�����;
		%do j=0 %to &outcomePeriod;
			%let validDate=%dsIntnx(dtmonth,&baseDate,&j);

			%let validSourceSampleId=B&validDate._OP&outcomePeriod;
			%let validSampleId=&outSamplePrefix._T&i.V&j;

			%let ts=%genId(prefix=TS,len=20);
			
			%put &macro: start to generate valid sample: &validSampleId sourceSample=&validSourceSampleId trainSample=&trainSampleId;

			* ��������;
			%filterSample(inSampleId=&validSourceSampleId,outSampleId=&ts,filterSampleId=&trainSampleId,isBlacklist=1,res=&tres);

			* ����ƽ�����;
			%gsBalancedSample(inSampleId=&ts,outSampleId=&validSampleId,n=&validN,varRsp=&depVar,p=&validP,force=1,event=1,res=&tres);

			* woe����;
			%gsWoe(inSampleId=&validSampleId,formatSampleId=&trainSampleId,woeSampleId=&trainSampleId,varRsp=&depVar); 

			%deleteSample(sampleId=&ts); 
		%end;
	%end;
%mend;


%macro run_sample_standard;

	%gssStandard(outSamplePrefix=S,startBaseDate=20141201,rollPeriod=7,outcomePeriod=6,trainP=0.5,trainN=3000,validP=0.5,validN=3000,depVar=HD_T6M);

%mend;


* ------------------------------------------------;
* ----------------	ģ�ͻ��� ----------------------;
* ------------------------------------------------;

* ��ģ�ͱȽ�

* ѵ������id��B20150101_OP6_SIMPLE_10000;
* ģ��id��M1;




%macro run_model_3;
	
	%let inSampleId=B20150601_OP6_BS_N6000_P50;
	%let modelId=M3;
	%let depVars=HD_T6M;
	%let indepVars=SEX MARRIAGE EDULEVEL EDUDEGREE OCCUPATION TITLE TECHTITLE AGE LC_UNSECURED LC_SECURED NLC_SECURED_T6M NLC_UNSECURED_T6M ANNUALINCOME LB_UNSECURED LB_SECURED PDAA PDA;

	%logit(inSampleId=&inSampleId,modelId=&modelId,depVars=HD_T6M,indepVars=&indepVars);
	%perfKS(predId=&modelId,varProb=_P_,varRsp=HD_T6M);
%mend;

%macro run_model_4;
	
	%let inSampleId=B20150601_OP6_BS_N6000_P50;
	%let modelId=M4;
	%let depVars=HD_T6M;
	%let indepVars=SEX MARRIAGE EDULEVEL EDUDEGREE OCCUPATION TITLE TECHTITLE AGE LC_UNSECURED LC_SECURED NLC_SECURED_T6M NLC_UNSECURED_T6M PDC LOG_ANNUALINCOME LOG_LB_UNSECURED LOG_LB_SECURED LOG_PDAA;

	%logit(inSampleId=&inSampleId,modelId=&modelId,depVars=HD_T6M,indepVars=&indepVars);
	%perfKS(predId=&modelId,varProb=_P_,varRsp=HD_T6M);
%mend;

%macro run_model_5;
	
	%let inSampleId=B20150601_OP6_BS_N6000_P50;
	%let modelId=M5;
	%let depVars=HD_T6M;
	%let indepVars= SEX MARRIAGE EDULEVEL EDUDEGREE OCCUPATION TITLE TECHTITLE AGE LC_UNSECURED LC_SECURED NLC_SECURED_T6M NLC_UNSECURED_T6M PDC LOG_ANNUALINCOME LOG_LB_UNSECURED LOG_LB_SECURED LOG_PDAA LOG_PDA_30D LOG_PDA_60D LOG_PDA_90D;
	%logit(inSampleId=&inSampleId,modelId=&modelId,depVars=HD_T6M,indepVars=&indepVars);
	%perfKS(predId=&modelId,varProb=_P_,varRsp=HD_T6M);
%mend;

%macro run_model_6;
	
	%let inSampleId=B20150601_OP6_BS_N6000_P50;
	%let modelId=M6;
	%let depVars=HD_T6M;
	%let indepVars=AGE LC_UNSECURED LC_SECURED NLC_SECURED_T6M NLC_UNSECURED_T6M PDC LOG_ANNUALINCOME LOG_LB_UNSECURED LOG_LB_SECURED LOG_PDAA LOG_PDA_30D LOG_PDA_60D LOG_PDA_90D WOE_SEX WOE_MARRIAGE WOE_EDULEVEL WOE_EDUDEGREE WOE_OCCUPATION WOE_TITLE WOE_TECHTITLE;
	%logit(inSampleId=&inSampleId,modelId=&modelId,depVars=HD_T6M,indepVars=&indepVars);
	%perfKS(predId=&modelId,varProb=_P_,varRsp=HD_T6M);
%mend;



%macro run_model_7;
	
	%let inSampleId=B20150601_OP6_BS_N6000_P50;
	%let modelId=M7;
	%let depVars=HD_T6M;
	%let indepVars= SEX MARRIAGE EDULEVEL EDUDEGREE OCCUPATION TITLE TECHTITLE AGE LC_UNSECURED LC_SECURED NLC_SECURED_T6M NLC_UNSECURED_T6M PDC LOG_ANNUALINCOME LOG_LB_UNSECURED LOG_LB_SECURED LOG_PDAA LOG_PDA_30D LOG_PDA_60D LOG_PDA_90D;
	%logit(inSampleId=&inSampleId,modelId=&modelId,depVars=HD_T6M,indepVars=&indepVars);
	%perfKS(predId=&modelId,varProb=_P_,varRsp=HD_T6M);
%mend;




%macro run_show_score_model3;
	%local sampleId;
	%let sampleId=M3;
	%gsScore(inSampleId=&sampleId,probVar=_P_,scoreVar=_SCORE_);
	%showScoreDist(sampleId=&sampleId,classVars=HD_T6M,overlay=1,scoreVar=_SCORE_,maxScore=950,minScore=350);
%mend;

%macro run_show_score_model4;
	%local sampleId;
	%let sampleId=M4;
	%gsScore(inSampleId=&sampleId,probVar=_P_,scoreVar=_SCORE_);
	%showScoreDist(sampleId=&sampleId,classVars=HD_T6M,overlay=1,scoreVar=_SCORE_,maxScore=950,minScore=350);
%mend;


%macro run_show_score_model5;
	%local sampleId;
	%let sampleId=M5;
	%gsScore(inSampleId=&sampleId,probVar=_P_,scoreVar=_SCORE_);
	%showScoreDist(sampleId=&sampleId,classVars=HD_T6M,overlay=1,scoreVar=_SCORE_,maxScore=950,minScore=350);
%mend;

* STEP 2 ���������չ��;

* ѵ������id��B20150101_OP6_SIMPLE_10000;
* ģ��id��M1;

%macro run_performance_1;

	* ͬ������չ��;
	* ѵ��������S1_T1;
	%performance(planFile=D:\Server\Sas\main\model\model_010\plan\plantest.xlsx);
	
	/*
	* ��������� - ��ͬspin;
	* ѵ��������S1_T1;
	* ��֤������S1_T1V0;
	%performance(planId=plan_out_2);

	* ��������� - ��ͬ��;
	* ѵ��������S1_T1;
	* ��֤������S1_V2 S1_V3 ... S1_V7;
	%performance(planId=plan_out_3);

	* ��������� - ��ȫ;
	* ѵ��������S1_T1;
	* ��֤������S1_T1V1 - S1_T1V6;
	%performance(planId=plan_out_4);
	*/
%mend;


* ��ģ��ܱȽ� - ������̬�仯;

%macro run_frame_log;
	
	* base vs log vs bin vs bin+woe;	
	%performance(planFile=D:\Server\Sas\main\model\model_010\plan\plan2.xlsx);

%mend;

* ��ģ��ܱȽ� - ����ת��;

%macro run_frame_woe;
	
	* base vs log vs bin vs bin+woe;	
	%performance(planFile=D:\Server\Sas\main\model\model_010\plan\plan3.xlsx);

%mend;

* ��ģ��ܱȽ� - ��ģ��;

%macro run_frame_subModel;
	
	%performance(planFile=D:\Server\Sas\main\model\model_010\plan\plan4.xlsx);
	
%mend;

* ��ģ��ܱȽ� - ��������;

%macro run_frame_sampling;
	
	%performance(planId=plan_balance_30);
	%performance(planId=plan_balance_50);
	%performance(planId=plan_balance_70);

%mend;


* ѵ�����Ƚ� - ��������;

%macro run_frame_sampling;
	
	%performance(planId=plan_balance_30);
	%performance(planId=plan_balance_50);
	%performance(planId=plan_balance_70);

%mend;


%macro run_show_perf;
	%showPerf(where=%str(predID like 'P_TRANS_LOG_%'),xVar=predDate,yVar=pfFractionKS,groupVar=modelId);
%mend;

* --------------------------------- MAIN RUN ------------------------------;

%*run_sample_raw;
%*run_sample_op;
%*run_show_op_info;
%*run_sample_standard;
%*run_sample_simple;

%*run_model_3;
%*run_model_4;
%*run_model_5;


%*run_show_score_model3;
%*run_show_score_model4;
%*run_performance_1;
%run_show_perf;

%*run_frame_log;
%*run_frame_woe;
%*run_frame_subModel;
