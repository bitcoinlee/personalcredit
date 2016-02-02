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

* 样本构成与命名示例;
* 	B20150101_DEMOGRAPHICS				原始样本系列;
* 	B20150101_OP6					主样本，包含指定的所有变量，baseDate为因变量baseDate，OP6表示因变量的baseDate早于自变量6个月;
*	B20150101_OP6_SIMPLE_N10000				简单随机抽样样本，自同期MAIN样本抽样，抽样数10000;
*	B20150101_OP6_BALANCE_N10000_P50		平衡抽样样本，自同期MAIN样本抽样，抽样数10000，平衡p为50%;
*	S1_T1								标准样本集，训练样本，自指定的时间范围抽样;
*	S1_T1V1								标准样本集，验证样本，资质东的时间范围抽样;


%macro run_sample_raw;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local i start;

	* 样本生成:指标计算;
	* 生成[2013年11月,2015年11月]所有基准时间点的数据，共计24个月;
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

	* 201402 201403 201404 共计3期;
	%let start=20140301;
	%let dur=3;
	%do i=0 %to %eval(&dur-1);
		%let baseDate=%dsIntnx(dtmonth,&start,&i,same);
		%grsNewLoanCount(baseDate=&baseDate,int=dtmonth,inc=3);	
		%grsNewDefaultCount(baseDate=&baseDate,int=dtmonth,inc=3);
		%grsHasLoan(baseDate=&baseDate,int=dtmonth,inc=3);
	%end;

	* 201405 201406 201407 201408 201409 201410 共计6期;
	%let start=20140601;
	%let dur=6;
	%do i=0 %to %eval(&dur-1);
		%let baseDate=%dsIntnx(dtmonth,&start,&i,same);
		%grsNewLoanCount(baseDate=&baseDate,int=dtmonth,inc=3 6);	
		%grsNewDefaultCount(baseDate=&baseDate,int=dtmonth,inc=3 6);
		%grsHasLoan(baseDate=&baseDate,int=dtmonth,inc=3 6);
	%end;

	* 201411 ... 201512 共计13期;
	%let start=20141201;
	%let dur=13;
	%do i=0 %to %eval(&dur-1);
		%let baseDate=%dsIntnx(dtmonth,&start,&i,same);
		%grsNewLoanCount(baseDate=&baseDate,int=dtmonth,inc=3 6 12);	
		%grsNewDefaultCount(baseDate=&baseDate,int=dtmonth,inc=3 6 12);
		%grsHasLoan(baseDate=&baseDate,int=dtmonth,inc=3 6 12);
	%end;
%mend;

* 调整补充样本生成;

%macro gssTemp;
	%grsNewLoanCount(baseDate=20151201,int=dtmonth,inc=3 6 12);	
	%grsNewDefaultCount(baseDate=20151201,int=dtmonth,inc=3 6 12);
	%grsHasLoan(baseDate=20151201,int=dtmonth,inc=3 6 12);
%mend;



* -- 样本合并 --;
* 完成自变量样本合并;
* 输出样本：B20150101_OP6,B20150101_OP12;
* 按给定的预测期间、基准时点生成样本;

%macro gssMain(baseDate=,outcomePeriod=);
	%local tres;%let tres=%createTempVar;%local &tres;
	%local depBaseDate indepBaseDate;
	%local inSampleIds outSampleId sample;

	%let depBaseDate=&baseDate;
	%let indepBaseDate=%dsIntnx(dtmonth,&baseDate,-&outcomePeriod);

	%put baseDate=&baseDate depBaseDate=&depBaseDate indepBaseDate=&indepBaseDate;

	* 以hasLoan作为连接基准表;
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
	
	* mainSampleId示例: B20150101_OP6;
	%let outSampleId=B&baseDate._OP&outcomePeriod;
	%deleteSample(sampleId=&outSampleId);

	%put gssMain: 	inSampleIds=&inSampleIds;
	%put gssMain:	outSampleId=&outSampleId;
	%mergeSample(inSampleIds=&inSampleIds,outSampleId=&outSampleId,type=left,sameBaseDate=0,res=&tres,force=1);

	* 样本过滤;
	%loadSample(sampleId=&outSampleId,res=&tres);%let sample=&&&tres;
	%if %dsVarExist(&&&sample.data,HL_T&outcomePeriod.M)=0 %then %error(gssMain: No required HL var! HL_T&outcomePeriod.M);

	%put sample obs before filtering: &&&sample.obs;

	data &&&sample.data;
		set &&&sample.data;
		retain missCounter 0;
		*drop missCounter;
		
		* 重新分类;
		if HL_T&outcomePeriod.M=0 then delete;
		if SEX=0 then SEX=9;
		if Marriage=21 or Marriage=22 or Marriage=23 then Marriage=20;
		if EDULEVEL=50 or  EDULEVEL=60 or EDULEVEL=70 or EDULEVEL=80 or EDULEVEL=90 then EDULEVEL=40;
		if EDUDEGREE=0 then EDUDEGREE=9;
		if OCCUPATION='Y' then OCCUPATION='Z';

		* 缺失计数统计与过滤;
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
	
	* misDummy处理;
	* 主要针对age annualIncome;
	%gsMisDummy(inSampleId=&outSampleId,defaultEqMiss=1);

	* logMoney处理;
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
		
		* simpleSample生成 B20150101_OP6_SIMPLE_N6000;
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
		
		* simpleSample生成 B20150101_OP6_SIMPLE_N10000;
		%let inSampleId=B&baseDate._OP&outcomePeriod;
		%let outSampleId=B&baseDate._OP&outcomePeriod._BALANCED_N&n._P%floor(%sysevalf(&p*100));
		%gsBalancedSample(inSampleId=&mainSampleId,outSampleId=&simpleSampleId,n=&n,p=&p);
	%end;
%mend;
*/

* 简单随机采样与平衡采样;
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
	
* 构建标准评价样本集;
* 示例：
* startDate：20150101;
* 训练长度:6期;
* 预测期长度：6个月;
* 评价样本采样方式：平衡采样10000，再平衡采样5000;
* 样本id格式：
*	[Prefix]_T1			第1期训练样本;
*	[Prefix]_V1			第1期验证样本(纯随机采样);
* 	[Prefix]_T1V0		第1期T+0验证样本;
* 	[Prefix]_T1V1		第1期T+1验证样本;
* 	[Prefix]_T1V2		第1期T+2验证样本;
* 	...
* 	[Prefix]_T1V6		第1期T+6验证样本;
*	[Prefix]_T2			第2期训练样本;
* 	[Prefix]_T2V1		第2期T+1验证样本;
*	……;
*  	[Prefix]_T6V6		第6期T+6验证样本;
*	[Prefix]
* 	共计包含训练样本6个，验证样本36个;

%macro gssStandard(outSamplePrefix=,startBaseDate=,outcomePeriod=,rollPeriod=,trainP=,trainN=,validPeriod=,validN=,validP=,depVar=) /parmbuff;
	
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local baseDate validDate;
	%local sourceSampleId trainSampleId validSampleId validSourceSampleId;
	%local ts;
	%local i j;

	%if &outSamplePrefix= or &startBaseDate= or &outcomePeriod= or &rollPeriod= or &trainP= or &trainN= or &validN= or &validP= or &depVar= %then %error(&macro: Required param is empty! param=&syspbuff);

	* 样本集生成;
	%do i=0 %to %eval(&rollPeriod-1);
		%let baseDate=%dsIntnx(dtmonth,&startBaseDate,&i);
		%let sourceSampleId=B&baseDate._OP&outcomePeriod;
		%let trainSampleId=&outSamplePrefix._T&i;

		* 训练样本生成 - 平衡采样;
		%put &macro: start to generate train sample: &trainSampleId n=&trainN varRsp=&depVar p=&trainP;
		%gsBalancedSample(inSampleId=&sourceSampleId,outSampleId=&trainSampleId,n=&trainN,varRsp=&depVar,p=&trainP,force=1,res=&tres);

		* woe变量生成;
		%gsWoe(inSampleId=&trainSampleId,varRsp=&depVar);

		%let validSampleId=&outSamplePrefix._V&i;
		* 验证样本生成 - 过滤样本 + 平衡采样;
		%do j=0 %to &outcomePeriod;
			%let validDate=%dsIntnx(dtmonth,&baseDate,&j);

			%let validSourceSampleId=B&validDate._OP&outcomePeriod;
			%let validSampleId=&outSamplePrefix._T&i.V&j;

			%let ts=%genId(prefix=TS,len=20);
			
			%put &macro: start to generate valid sample: &validSampleId sourceSample=&validSourceSampleId trainSample=&trainSampleId;

			* 样本过滤;
			%filterSample(inSampleId=&validSourceSampleId,outSampleId=&ts,filterSampleId=&trainSampleId,isBlacklist=1,res=&tres);

			* 样本平衡采样;
			%gsBalancedSample(inSampleId=&ts,outSampleId=&validSampleId,n=&validN,varRsp=&depVar,p=&validP,force=1,event=1,res=&tres);

			* woe处理;
			%gsWoe(inSampleId=&validSampleId,formatSampleId=&trainSampleId,woeSampleId=&trainSampleId,varRsp=&depVar); 

			%deleteSample(sampleId=&ts); 
		%end;
	%end;
%mend;


%macro run_sample_standard;

	%gssStandard(outSamplePrefix=S,startBaseDate=20141201,rollPeriod=7,outcomePeriod=6,trainP=0.5,trainN=3000,validP=0.5,validN=3000,depVar=HD_T6M);

%mend;


* ------------------------------------------------;
* ----------------	模型环节 ----------------------;
* ------------------------------------------------;

* 简单模型比较

* 训练样本id：B20150101_OP6_SIMPLE_10000;
* 模型id：M1;




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

* STEP 2 样本外表现展现;

* 训练样本id：B20150101_OP6_SIMPLE_10000;
* 模型id：M1;

%macro run_performance_1;

	* 同期评分展现;
	* 训练样本：S1_T1;
	%performance(planFile=D:\Server\Sas\main\model\model_010\plan\plantest.xlsx);
	
	/*
	* 样本外表现 - 非同spin;
	* 训练样本：S1_T1;
	* 验证样本：S1_T1V0;
	%performance(planId=plan_out_2);

	* 样本外表现 - 非同期;
	* 训练样本：S1_T1;
	* 验证样本：S1_V2 S1_V3 ... S1_V7;
	%performance(planId=plan_out_3);

	* 样本外表现 - 完全;
	* 训练样本：S1_T1;
	* 验证样本：S1_T1V1 - S1_T1V6;
	%performance(planId=plan_out_4);
	*/
%mend;


* 建模框架比较 - 变量形态变化;

%macro run_frame_log;
	
	* base vs log vs bin vs bin+woe;	
	%performance(planFile=D:\Server\Sas\main\model\model_010\plan\plan2.xlsx);

%mend;

* 建模框架比较 - 变量转换;

%macro run_frame_woe;
	
	* base vs log vs bin vs bin+woe;	
	%performance(planFile=D:\Server\Sas\main\model\model_010\plan\plan3.xlsx);

%mend;

* 建模框架比较 - 子模型;

%macro run_frame_subModel;
	
	%performance(planFile=D:\Server\Sas\main\model\model_010\plan\plan4.xlsx);
	
%mend;

* 建模框架比较 - 采样比例;

%macro run_frame_sampling;
	
	%performance(planId=plan_balance_30);
	%performance(planId=plan_balance_50);
	%performance(planId=plan_balance_70);

%mend;


* 训练集比较 - 采样比例;

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
