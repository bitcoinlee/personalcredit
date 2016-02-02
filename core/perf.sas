%global CTABLE_MEMBERS;
%let CTABLE_MEMBERS=freqP freqN freq1 freq0 freq freqP1 freqN1 freqP0 freqN0 pctP0 pctP1 pctN0 pctN1 cpP_0 cpN_0 cpP_1 cpN_1 cp0_P cp1_P cp0_N cp1_N sens spec ppv npv fp fn accu;



* -----------------------;
* ---- ��ģ��չʾ�� ------;
* -----------------------;

* ����ģ�ͱ���ɢ��ͼ;
* input;
*	data						�������ݼ�������Ϊ�գ�Ϊ��ʱʹ��pinfo;
*	where						�������ݼ�ɸѡ����;
*	groupVar					�������ݱ�������trainSampleId;
*	xVar						x��ָ�꣬ѡ����Ϊx��ı�������baseDate;
*	yVar						y��ָ�꣬ѡ����Ҫչʾ��pfָ�꣬��pfAUC;
*	labelVar					��ʶ������һ��ΪpredId;
* output;
*	
*;
%macro showPerf(data=,where=,xVar=,yVar=,labelVar=,tipVar=,groupVar=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;		

	%local infoLib;
	%local labelStr groupStr whereStr tipStr;
	%local tempDs;

	%let tempDs=%createTempDs;
	
	%if &xVar= or &yVar= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &data= %then %do;
		%initInfoLib(res=&tres);%let infoLib=&&&tres;
		%let data=&infoLib..pinfo;
	%end;

	%let labelStr=%str();
	%let groupStr=%str();
	%let whereStr=%str();
	
	%if &where ne %then %let whereStr=%str((where=(&where)));
	%if &labelVar ne %then %let labelStr=%str(datalabel=&labelVar);
	%if &groupVar ne %then %let groupStr=%str(group=&groupVar);
	%if &tipVar ne %then %let tipStr=%str(tip=(&tipVar));
	
	
	data &tempDs;
		set &data &whereStr;
		format modelBaseDate YYMM. sampleBaseDate YYMM.;
		modelBaseDate=input(modelDate,B8601DA.);
		sampleBaseDate=input(predDate,B8601DA.);
		pfFractionKS_TEMP=input(pfFractionKS,best12.);
		pfProbKS_TEMP=input(pfProbKS,best12.);
		pfAUC_TEMP=pfAUC;
	run;

	proc sort data=&tempDs;
		by &groupVar &xVar;
	run;


	proc print data=&tempDs;
	run;

	ods graphics on/imagemap=on;
	proc sgplot data=&tempDs;
		series y=&yVar._TEMP x=sampleBaseDate /&labelStr &groupStr GROUPMS=&groupVar  &tipStr;
	run;

	* ��������;
	%dropLib(&infoLib);
	%dropDs(&tempDs);
%mend;





* �ۺϱ��ּ��㺯��;
* ���ƻ�ѵ��ģ�ͣ�����������ָ�꼯;
* input;
*	plan			ѵ������֤�ƻ�ds;
*	 				planΪ���ݼ������а���ģ��ѵ�������������в�������ʽ����;
*						planId			�ƻ�id;
*						predId			Ԥ��id������;
*						modelId			ģ��id������;
*						trainMacro		ѵ���꣬����;
*						trainParam		ѵ�������������;
*						trainSampleId	ѵ������������;
*						predMacro		Ԥ��꣬����;
*						predParam		Ԥ��������ѡ��;
*						predSampleId	Ԥ������id������;
*						depVars			���������;
*	out				ѵ������֤�������ʽ��predinfoһ�£�����ȡplan��Ӧ��predInfo��Ϣ;

* ִ���߼�;
*	plan�е�ÿ����¼����Ϊstep;
*	performance����modelId��predId��step�������򣬶���ͬһmodelId�Ķ��step�����׸�stepѵ����ģ��;
*	ͬһmodelId����Ӧ��trainMacro��trainParam������ͬ(�ֽ׶��ݲ�����У����);
*	step�е�predId��predMacro��predParam��Ӧ����ѵ��������������Ϣ��ѵ�����������ۻ���ģ��ѵ�����Զ����У�����Ҫָ��;
* id���ɹ���;
*	ѵ��������Ԥ���predIdΪmodelId;
*	ѵ��������Ԥ���predIdΪָ����predId;
*	pred���������idΪpredId;
* �����;
*	trainParam��predParam�б����Ϊ���к�����У�plan�����ֶ�������������Ϣ;

%macro performance(plan=,planFile=,out=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%put &macro: Start! param=&syspbuff;

	%if &plan= and &planFile= %then %error(&macro: Required param is empty!);
	
	%local i tempDs;
	%local fullTrainMacro countPredId countModelId countTrainSampleId countPredSampleId;
	%local obs newObs;
	%local step;
	%local predId modelId trainMacro trainParam trainSampleId predMacro predParam predSampleId indepVars depVars;
	%local infoLib;
	%local isTempPlan;

	%let tempDs=%createTempDs;
	%let isTempPlan=0;

	* -- plan�Ϸ��Լ�� --;

	%if &planFile ne %then %do;
		%let plan=%createTempDs;
		%let isTempPlan=1;
		PROC IMPORT OUT=&plan DATAFILE="&planFile" DBMS=xlsx ;
		RUN;
	%end;

	%getDsObs(ds=&plan,res=&tres);%let obs=&&&tres;
	%if &obs=0 %then %error(&macro: The plan is empty! param=&syspbuff);
	data _null_;
		set &plan end=last;
		retain countPredId 0 countModelId 0 countTrainSampleId 0 countPredSampleId 0 fullMacro 1;
		
		if not missing(predId) then countPredId=countPredId+1;
		if not missing(modelId) then countModelId=countModelId+1;
		if not missing(trainSampleId) then countTrainSampleId=countTrainSampleId+1;
		if not missing(predSampleId) then countPredSampleId=countPredSampleId+1;
		if missing(trainMacro) then fullTrainMacro=0;
		if last then do;
			call symput('fullMacro',fullMacro);
			call symput('countPredId',countPredId);
			call symput('countModelId',countModelId);
			call symput('countTrainSampleId',countTrainSampleId);
			call symput('countPredSampleId',countPredSampleId);
		end;
	run;

	%if &fullTrainMacro=0 %then %error(&macro: TrainMacro must be full!);
	%if &countPredId^=&obs %then %error(&macro: PredId must be full! countPredId=&countPredId);
	%if &countModelId^=&obs %then %error(&macro: ModelId must be full! countModelId=&countModelId);
	%if &countTrainSampleId^=&obs %then %error(&macro: TrainSampleId must be full!  countTrainSampleId=&countTrainSampleId);
	%if &countPredSampleId^=&obs %then %error(&macro: PredSampleId must be full!  countPredSampleId=&countPredSampleId);

	* predId Ψһ�Լ��;
	proc sort data=&plan out=&tempDs nodupkey;
		by predId;
	run;
	%getDsObs(ds=&tempDs,res=&tres);%let newObs=&&&tres;
	%if &obs^=&newObs %then %error(&macro: predId must be unique!);
	
	* -- �趨ִ��˳����ѵ��ָ�� --;
	proc sort data=&plan out=&tempDs;
		by modelId predId;
	run;
	data &tempDs;
		set &tempDs;
		by modelId predId;
		seq=_n_;
		trainModel=0;
		if first.modelId then trainModel=1;
	run;

	* -- ��ִ��ѭ�� --;
	%do i=1 %to &obs;
		%logbreak(Plan step &i start!);
		%getDsRecord(ds=&tempDs,where=%str(seq=&i),res=&tres);%let step=&&&tres;
		%let planId=%strip(&&&step.planId);
		%let predId=&planId._%strip(&&&step.predId);
		%let modelId=&planId._%strip(&&&step.modelId);
		%logbreak(%str(modelId=&modelId predId=&predId));
	
		%let trainMacro=&&&step.trainMacro;
		%let trainParam=&&&step.trainParam;
		%let trainSampleId=&&&step.trainSampleId;	

		%let predMacro=&&&step.predMacro;
		%let predParam=&&&step.predParam; 
		%let predSampleId=&&&step.predSampleId;

		%let depVars=&&&step.depVars;
	
		%if &&&step.trainModel=1 %then %do;
			%logbreak(Start to train model! modelId=&modelId);
			%if %length(&trainParam)=0 %then %do;
				%&trainMacro(inSampleId=&trainSampleId,modelId=&modelId,predId=&predId);
			%end;
			%else %do;
				%&trainMacro(&trainParam,inSampleId=&trainSampleId,modelId=&modelId,predId=&predId);
			%end;
		%end;
		
		%logbreak(Start to predict! modelId=&modelId predSampleId=&predSampleId);
		%if %length(&predParam)=0 %then %do;
			%&predMacro(inSampleId=&predSampleId,modelId=&modelId,predId=&predId);
		%end;
		%else %do;
			%&predMacro(&predParam,inSampleId=&predSampleId,modelId=&modelId,predId=&predId);
		%end;
		%logbreak(Start to performance! predId=&predId);
		%perfKS(predId=&predId,varProb=P_1,varRsp=&depVars);
		%perfProbCtable(predId=&predId,varProb=P_1,varRsp=&depVars,n=3);
		
	%end;

	* ---- ������ȡ ----;
	%if &out ne %then %do;
		%initInfoLib(res=&tres);%let infoLib=&&&tres;
		%leftjoin(base=&plan,table=&infoLib..pinfo,key=predId,out=&out);
	%end;

	* ---- �������� ----;
	%dropLib(&infoLib);
	%if &isTempPlan=1 %then %dropDs(&plan);

%mend;


* -----------------------;
* ---- ���ߺ����� --------;
* -----------------------;

* ���ʾܾ����߷�ʽ;

* ������Ԥ���ΥԼ���ʣ�����ж��Ƿ�ܾ�;
%macro p2dProb(data=,varProb=,out=,varPred=);
	data &out;
		set &data;
		if missing(&varProb) then delete;
		if &varProb>rand('UNIFORM') then &varPred=1;
		else &varPred=0;
	run;
%mend;

* ��ֵ�ܾ����߷�ʽ;
%macro p2dThreshold(data=,varProb=,threshold=,out=,varPred=);
	data &out;
		set &data;
		if &varProb>&threshold then &varPred=1 else &varPred=0;
	run;
%mend;

* ����ctable;
%macro ctable(data=,record=,varPred=,varRsp=) /parmbuff;

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%if data= or record= or varPred= or varRsp= %then %error(Required param is empty! param=&syspbuff);
	
	%local var i tempDs;
	%local &CTABLE_MEMBERS;

	%let tempDs=%createTempDs;
	
	proc freq data=&data noprint;
		tables &varPred * &varRsp /out=&tempDs outpct;
	run;
 
	%dictGet(data=&tempDs,where=%str(&varPred=1 and &varRsp=1),var=COUNT,res=freqP1);
	%dictGet(data=&tempDs,where=%str(&varPred=0 and &varRsp=1),var=COUNT,res=freqN1);
	%dictGet(data=&tempDs,where=%str(&varPred=1 and &varRsp=0),var=COUNT,res=freqP0);
	%dictGet(data=&tempDs,where=%str(&varPred=0 and &varRsp=0),var=COUNT,res=freqN0);
	
	* Ƶ��;

	%let freqP=%sysevalf(&freqP1+&freqP0);
	%let freqN=%sysevalf(&freqN1+&freqN0);
	%let freq1=%sysevalf(&freqP1+&freqN1);
	%let freq0=%sysevalf(&freqP0+&freqN0);
	
	%let freq=%eval(&freqP+&freqN);
	
	* ���԰ٷֱ�/����������;

	%let pctP0=%sysevalf(&freqP0/&freq);
	%let pctP1=%sysevalf(&freqP1/&freq);
	%let pctN0=%sysevalf(&freqN0/&freq);
	%let pctN1=%sysevalf(&freqN1/&freq);

	* ��԰ٷֱ�/��������;
	* �������ʱ�ʾ��һ����ʾ������ǰһ����ʾ�¼�;

	%let cpP_0=%sysevalf(&freqP0/&freq0);
	%let cpN_0=%sysevalf(&freqN0/&freq0);
	%let cpP_1=%sysevalf(&freqP1/&freq1);
	%let cpN_1=%sysevalf(&freqN1/&freq1);
	
	%let cp0_P=%sysevalf(&freqP0/&freqP);
	%let cp1_P=%sysevalf(&freqP1/&freqP);
	%let cp0_N=%sysevalf(&freqN0/&freqN);
	%let cp1_N=%sysevalf(&freqN1/&freqN);

	* ����ָ��;

	%let sens=&cpP_1;
	%let spec=&cpN_0;

	%let ppv=&cp1_P;
	%let npv=&cp0_N;

	%let fp=&cp0_P;
	%let fn=&cp1_N;

	%let accu=%sysevalf(&pctP1+&pctN0);
	
	* ��ֵ;
	%local arrayVars;%let arrayVars=%arrayVars(name=varList,values=&CTABLE_MEMBERS);
	%local &arrayVars;%array(varList,values=&CTABLE_MEMBERS);
	%do i=1 %to &varListN;
		%let var=&&varList&i;
		%global &record.&var;
		%let &record.&var=&&&var;
	%end;
	%dropDs(&tempDs);
%mend;

* -----------------------;
* ---- ����ָ������� ----;
* -----------------------;

* KSָ����㺯��;
* input;
*	predId						Ԥ��id;
*	varProb						�������۵�Ԥ����ʱ���;
* 	varRsp						ʵ���¼�����;
* output;
*	pred��Աָ��;	
*		pfProbKS;
*		pfFractionKS;
*	EDF����;
*;

%macro perfKS(predId=,varProb=,varRsp=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%if &predId= or &varProb= %then %error(&macro: Required param is empty! param=&syspbuff);

	%local tempDs rankVar sample predResult;
	%let tempDs=%createTempDs;
	%let varRank=%createTempVar;

	%loadPred(predId=&predId,type=perf,res=&tres);%let pred=&&&tres;
	%let sample=&&&pred.outSample;
	%let predResult=&&&pred.result;

	* ����;
	proc rank data=&&&sample.data out=&tempDs fraction descending;
		var &varProb;
		ranks &varRank;
	run;
	* EDF��KS;
	%clearOutput(file=report,path=%str(&&&pred.path)&macro._REPORT\,noTimestamp=1);
	title "predId=predId KS";
	proc npar1way edf plots=edfplot data=&tempDs;
		var &varProb &varRank;
		class &varRsp;
		output out=&predResult._KS ks;	
	run;
	%dictGet(data=&predResult._KS,keyVar=_VAR_,key=&varProb,var=_KS_,res=&pred.pfProbKS);
	%dictGet(data=&predResult._KS,keyVar=_VAR_,key=&varRank,var=_KS_,res=&pred.pfFractionKS);
	%savePred(pred=&pred);
	%dropPred(&pred);
%mend;

* ---- macro %perfProbCtable ----;
* ʹ��p2dProb���ߺ���������CTABLE;
* input;
*	predId						Ԥ��id;
*	varProb						�������۵�Ԥ����ʱ���;
* 	varRsp						ʵ���¼�����;
*	n							MontoCarlo���������Ĭ��Ϊ100;
* output;
*	����ָ��						�μ�CTABLE_MEMBERS;
*								CTABLE��ָ����ú�׺��ʶ�����㷨��Ϣ��������p2dProb���ߺ���������100�Σ�����SENSָ��ΪSENS_P100;

%macro perfProbCtable(predId=,varProb=,varRsp=,n=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%if &predId= or &varProb= or &varRsp= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &n= %then %let n=100;

	%local sample pred tempDs totalDs resultDs tempRecord resultRecord varPred i;

	%loadPred(predId=&predId,type=perf,res=&tres);%let pred=&&&tres;
	
	%let sample=&&&pred.outSample;

	%let tempDs=%createTempDs;
	%let totalDs=%createTempDs;
	%let resultDs=%createTempDs;
	%let varPred=%createTempVar;
	%recordNew(res=&tres);%let tempRecord=&&&tres;
	%recordNew(res=&tres);%let resultRecord=&&&tres;

	%do i=1 %to &n;
		%let &tempRecord.round=&i;
		%p2dProb(data=&&&sample.data,out=&tempDs,varProb=&varProb,varPred=&varPred);
		%ctable(data=&tempDs,record=&tempRecord,varPred=&varPred,varRsp=&varRsp);
		%setDsRecord(ds=&totalDs,key=round,record=&tempRecord);
	%end;

	%global &CTABLE_MEMBERS;
	proc means data=&totalDs NOPRINT;
		var &CTABLE_MEMBERS;
		output out=&resultDs;
	run;

	%getDsRecord(ds=&resultDs,where=%str(_STAT_='MEAN'),res=&tres);%let resultRecord=&&&tres;
	
	%recordCopy(from=&resultRecord,to=&pred,prefix=pf,suffix=_PR&n,members=&CTABLE_MEMBERS,overwrite=1);
	
	%savePred(pred=&pred);

	%dropDs(&tempDs);
	%dropDs(&resultDs);
	%dropRecord(&tempRecord);
	%dropRecord(&resultRecord);
%mend;
