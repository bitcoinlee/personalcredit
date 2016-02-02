
* -------------------------;
* ---- ģ�����ݿ�ܽ��� ----;
* -------------------------;


* -------------------;
* ---- ������� -----;
* -------------------;

* --- macro %logit ----;
* ͨ����logitģ��ѵ������;
* input:
*	root;
*	inSampleId			;
*	modelId				ģ��id������;
*	outSampleId			Ԥ��������id����ѡ��Ĭ��Ϊ[modelId];
* 	predId				ѵ��������Ԥ��id��ѡ�䣬Ĭ��Ϊ[modelId];
*	depVars				����������嵥������;
*	indepVars			�Ա��������嵥��ѡ��;
*							��indepVarsΪ�գ���%logitʹ��indepSample�е�����modelVar��Ϊ�Ա������ڵ�һ�����£����޳�depVars��;
*							��indepVars�ǿգ���ֱ��ʹ������ָ���ı���;
*	stepwise			�Ƿ�ʹ��stepwise;
*	useWoeEnum			����enumȫ��ʹ��woe��ʽ��woeת�������Ƚ��У�%logit������ת��;
*	useLogMoney			����moneyȫ��ʹ��log��ʽ��logת�������Ƚ��У�%logit������ת��;
*	by					��ģ�ͱ����嵥;

* output;
*	Ԥ���������			��������inSample�����б�����ͬʱ����һ��_P_������ʾԤ��ĸ��ʣ�_P_ΪamountVar����Ĭ��ֵ;
%macro logit(root=,inSampleId=,modelId=,predId=,outSampleId=,depVars=,indepVars=,useWoeEnum=,useLogMoney=,stepwise=,by=,res=) /parmbuff;

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	* variables definition;
	%local inSample outSample model pred;
	%local idpVars classVars;
	%local classStatement byStatement modelStatement procOptions; 
	%local modelResult predResult;

	* parameters checking;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &inSampleId= %then %error(&marco: No input sampleId! param=&syspbuff);
	%if &root= %then %error(&macro: ROOT is empty! param=&syspbuff);
	%if &depVars= %then %error(&macro: DEPVARS is empty! param=&syspbuff);
	%if &modelId= and %refExist(&res)=0 %then %error(&macro: MODELID and RES cannot be empty at the same time!);
	%if &stepwise= %then %let stepwise=0;
	%if &useWoeEnum= %then %let useWoeEnum=0;
	%if &useLogMoney= %then %let useLogMoney=0;
	%if &outSampleId= %then %let outSampleId=&modelId;
	%if &predId= %then %let predId=&modelId;

	* sample loading;
	%put &macro: sample info:;
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;

	* model initialization;
	%createModel(root=&root,modelId=&modelId,res=&tres);%let model=&&&tres;
	%let &model.genDate=%timestamp;
	%let &model.genMacro=&macro;
	%let &model.genParam=&syspbuff;
	%let &model.inSampleId=&inSampleId;
	%let &model.outSampleId=&outSampleId;
	%let &model.predId=&predId;
	%let &model.stepwise=&stepwise;
	%let &model.by=&by;
	%let &model.useWoeEnum=&useWoeEnum;
	%let &model.useLogMoney=&useLogMoney;
	%let &model.baseDate=&&&inSample.baseDate;

	* vars logic;
	%if &indepVars ne %then %do;* ָ���Ա���;
		%let idpVars=&indepVars;
		%varsAnd(a=&idpVars,b=&&&inSample.classVars,res=&tres);
		%let classVars=&&&tres;
	%end;
	%else %do;* ��ָ���Ա���;
		* class vars;
		%let idpVars=&&&sample.binaryVars;
		%if &useWoeEnum=1 %then %do;
			%varsOr(a=&idpVars,b=&&&inSample.woeEnumVars,res=&tres);%let idpVars=&&&tres;
			%let classVars=&&&inSample.binaryVars;
		%end;
		%else %do;
			%varsOr(a=&idpVars,b=&&&inSample.enumVars,res=&tres);%let idpVars=&&&tres;
			%let classVars=&idpVars;
		%end;
		* num vars;
		%varsOr(a=&idpVars,b=&&&inSample.amountVars,res=&tres);%let idpVars=&&&tres;
		%if &useLogMoney=1 %then %do;
			%varsOr(a=&idpVars,b=&&&inSample.logMoneyVars,res=&tres);%let idpVars=&&&tres;
		%end;
		%else %do;
			%varsOr(a=&idpVars,b=&&&inSample.moneyVars,res=&tres);%let idpVars=&&&tres;
		%end;
	%end;
	%varsSub(a=&idpVars,b=&depVars,res=&tres);%let idpVars=&&&tres;
	%put &macro: independant vars=&idpVars;

	%let &model.indepVars=&idpVars;
	%let &model.depVars=&depVars;

	* stepwise settings;
	%if &stepwise=1 %then %let &model.modelOptions=%str(lackfit ctable rsquare stepwise);
	%else %let &model.modelOptions=%str(lackfit ctable rsquare);
	
	* pred initialization;
	%createPred(predId=&predId,model=&model,inSample=&inSample,outSampleId=&outSampleId,use=pred,res=&tres);%let pred=&&&tres;
	%let &pred.genMacro=&macro;
	%let &pred.genParam=&syspbuff;
	%let &pred.genDate=%timestamp;
	%let outSample=&&&pred.outSample;

	* model statement settings;

	%let modelResult=&&&model.result;
	%let predResult=&&&pred.result;

	%let modelStatement=%str(&&&model.modelId: model &depVars (event="1") = &idpVars /&&&model.modelOptions outroc=&predResult._roc);
	%if &classVars= %then %let classStatement=%str();%else %let classStatement=%str(class &classVars /param=ref);
	%if &by ne %then %do;
		%let byStatement=%str(by &by);
		proc sort data=&&&inSample.data force;
			&byStatement;
		quit;
	%end;
	%else %let byStatement=%str();
	%let procOptions=%str(data=&&&inSample.data outmodel=&&&model.model outest=&modelResult._param plots=(roc));

	* model output settings;
	%clearOutput(file=report,path=&&&model.path,noTimestamp=1);
	
	ods output FitStatistics=&predResult._FitStatistics;* ģ�����ͳ������AIC SC lnL��;
	ods output LackFitChiSq=&predResult._LackFitChiSq;* Ԥ�����׼ȷ��ָ��;
	ods output ROCAssociation=&predResult._ROCAssociation; * ROC�������ͳ����;

	* model training;

	%logbreak(proc logistic info);
	%put procOptions=&procOptions;
	%put modelStatement=&modelStatement;
	%put byStatement=&byStatement;
	%put classStatement=&classStatement;

	proc logistic &procOptions;
		&classStatement;
		&byStatement;
		&modelStatement;
		output out=&&&outSample.data prob=_P_ XBETA=_XB_;
		roc 'base';
		roccontrast reference('base') / estimate e;
	run;
	ods output close;
	ods html close;

	* outSample vars settings;
	%varsOr(a=&&&inSample.amountVars,b=_P_,res=&outSample.amountVars); 

	* Extract performance;

	%if stepwise=1 %then %do;
		%local realStep;
		%getDsVarMax(ds=&predResult._FitStatistics,var=Step,res=&tres);%let realStep=&&&tres;
		%let &model.realStep=&realStep;
		%dictGet(data=&predResult._FitStatistics,var=InterceptAndCovariates,where=%str(Step=&realStep and Criterion='AIC'),res=&pred.pfAIC);%let &pred.pfAIC=&&&tres;
		%dictGet(data=&predResult._FitStatistics,var=InterceptAndCovariates,where=%str(Step=&realStep and Criterion='SC'),res=&tres);%let &pred.pfSC=&&&tres;
		%dictGet(data=&predResult._FitStatistics,var=InterceptAndCovariates,where=%str(Step=&realStep and Criterion='-2 Log L'),res=&tres);%let &pred.pfLogL=&&&tres;
	%end;
	%else %do;
		%dictGet(data=&predResult._FitStatistics,var=InterceptAndCovariates,where=%str(Criterion='AIC'),res=&tres);%let &pred.pfAIC=&&&tres;
		%dictGet(data=&predResult._FitStatistics,var=InterceptAndCovariates,where=%str(Criterion='SC'),res=&tres);%let &pred.pfSC=&&&tres;
		%dictGet(data=&predResult._FitStatistics,var=InterceptAndCovariates,where=%str(Criterion='-2 Log L'),res=&tres);%let &pred.pfLogL=&&&tres;
	%end;

	* AUC;
	%dictGet(data=&predResult._ROCAssociation,key=&modelId,var=Area,keyVar=ROCModel,res=&tres);%let &pred.pfAUC=&&&tres;

	* Rank Statistics;
	%dictGet(data=&predResult._ROCAssociation,key=&modelId,var=SomersD,keyVar=ROCModel,res=&tres);%let &pred.pfSomersD=&&&tres;
	%dictGet(data=&predResult._ROCAssociation,key=&modelId,var=TauA,keyVar=ROCModel,res=&tres);%let &pred.pfTauA=&&&tres;
	%dictGet(data=&predResult._ROCAssociation,key=&modelId,var=Gamma,keyVar=ROCModel,res=&tres);%let &pred.pfGamma=&&&tres;

	* HL Statistics;
	%dictGet(data=&predResult._LackFitChiSq,var=ChiSq,res=&tres);%let &pred.pfHL=&&&tres;
	
	* Saving;
	%saveModel(model=&model);
	%savePred(pred=&pred);

	%if %refExist(&res)=1 %then %let &res=&&&model.modelId;

	* ��������;
	%dropSample(&inSample);
	%dropModel(&model);
	%dropPred(&pred);

%mend;

* --- macro %logitPred ----;
* ʹ��ָ��ģ�ͣ���ָ�����ݽ���Ԥ������Ԥ�����;
* input;
*	root			��Ŀ¼;
*	predId			Ԥ��id������Ϊ�գ�Ϊ��ʱ���Զ�����;
* 	modelId			Ԥ��ʹ�õ�ģ��id;
*	inSampleId		Ԥ��ʹ�õ�����id;
*	outSampleId		Ԥ�����ı�������id;
*	predId			Ԥ��id;
* output:;
*	ģ�ͻ�����Ϣ��ģ���ļ���ģ�ͱ���;

%macro logitPred(modelId=,inSampleId=,outSampleId=,predId=) /parmbuff;

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	%* -- ������ʼ�� --;
	%local inSample outSample model pred;

	%* -- ������� --;
	%if &modelId= or &inSampleId= %then %error(&marco: Required param is empty! param=&syspbuff);
	%if &outSampleId= and &predId= %then %do;
		%let predId=&modelId._&inSampleId;
		%let outSampleId=&predId;
	%end;
	%else %if &outSampleId= %then %let outSampleId=&predId;
	%else %if &predId= %then %let predId=&outSampleId;

	* pred initialization;
	%createPred(predId=&predId,modelId=&modelId,inSampleId=&inSampleId,outSampleId=&outSampleId,use=pred,res=&tres);%let pred=&&&tres;
	%let &pred.genDate=%timestamp;
	%let &pred.genMacro=&macro;
	%let &pred.genParam=&syspbuff;
	%let &pred.desc=Logit Model Prediction;

	%let model=&&&pred.model;
	%let inSample=&&&pred.inSample;
	%let outSample=&&&pred.outSample;

	* predict;
	%let predResult=&&&pred.result;
	ods output ScoreFitStat=&predResult._ScoreFitStat;
	proc logistic inmodel=&&&model.model;
		score data=&&&inSample.data out=&&&outSample.data outroc=&predResult._roc FITSTAT;
	run;

	* out sample settings;
	%varsOr(a=&&&inSample.amountVars,b=P_1,res=&outSample.amountVars);

	* extract predict result;

	%dictGet(data=&predResult._ScoreFitStat,var=LogLike,res=&tres);%let &pred.pfLogLike=&&&tres;
	%dictGet(data=&predResult._ScoreFitStat,var=MisClass,res=&tres);%let &pred.pfMisClass=&&&tres;
	%dictGet(data=&predResult._ScoreFitStat,var=AUC,res=&tres);%let &pred.pfAUC=&&&tres;
	%dictGet(data=&predResult._ScoreFitStat,var=AIC,res=&tres);%let &pred.pfAIC=&&&tres;
	%dictGet(data=&predResult._ScoreFitStat,var=AICC,res=&tres);%let &pred.pfAICC=&&&tres;
	%dictGet(data=&predResult._ScoreFitStat,var=BIC,res=&tres);%let &pred.pfBIC=&&&tres;
	%dictGet(data=&predResult._ScoreFitStat,var=SC,res=&tres);%let &pred.pfSC=&&&tres;
	%dictGet(data=&predResult._ScoreFitStat,var=RSquare,res=&tres);%let &pred.pfRSqure=&&&tres;
	%dictGet(data=&predResult._ScoreFitStat,var=AdjRSquare,res=&tres);%let &pred.pfAdjRSquare=&&&tres;
	%dictGet(data=&predResult._ScoreFitStat,var=BrierScore,res=&tres);%let &pred.pfBrierScore=&&&tres;

	%savePred(pred=&pred);

	* cleanup;
	%dropPred(&pred)
	
%mend;



