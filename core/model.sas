
* -------------------------;
* ---- 模型数据框架介绍 ----;
* -------------------------;


* -------------------;
* ---- 功能组成 -----;
* -------------------;

* --- macro %logit ----;
* 通用性logit模型训练程序;
* input:
*	root;
*	inSampleId			;
*	modelId				模型id，必输;
*	outSampleId			预测结果样本id，可选，默认为[modelId];
* 	predId				训练样本内预测id，选输，默认为[modelId];
*	depVars				因变量变量清单，必输;
*	indepVars			自变量变量清单，选输;
*							如indepVars为空，则%logit使用indepSample中的所有modelVar作为自变量（在单一样本下，将剔除depVars）;
*							如indepVars非空，则直接使用其中指定的变量;
*	stepwise			是否使用stepwise;
*	useWoeEnum			对于enum全部使用woe形式，woe转换需事先进行，%logit不负责转换;
*	useLogMoney			对于money全部使用log形式，log转换需事先进行，%logit不负责转换;
*	by					子模型变量清单;

* output;
*	预测输出样本			样本包含inSample的所有变量，同时增加一个_P_变量表示预测的概率，_P_为amountVar，无默认值;
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
	%if &indepVars ne %then %do;* 指定自变量;
		%let idpVars=&indepVars;
		%varsAnd(a=&idpVars,b=&&&inSample.classVars,res=&tres);
		%let classVars=&&&tres;
	%end;
	%else %do;* 非指定自变量;
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
	
	ods output FitStatistics=&predResult._FitStatistics;* 模型拟合统计量，AIC SC lnL等;
	ods output LackFitChiSq=&predResult._LackFitChiSq;* 预测概率准确度指标;
	ods output ROCAssociation=&predResult._ROCAssociation; * ROC曲线相关统计量;

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

	* 环境清理;
	%dropSample(&inSample);
	%dropModel(&model);
	%dropPred(&pred);

%mend;

* --- macro %logitPred ----;
* 使用指定模型，对指定数据进行预测生成预测概率;
* input;
*	root			根目录;
*	predId			预测id，可以为空，为空时将自动生成;
* 	modelId			预测使用的模型id;
*	inSampleId		预测使用的样本id;
*	outSampleId		预测结果的保存样本id;
*	predId			预测id;
* output:;
*	模型基本信息、模型文件、模型报告;

%macro logitPred(modelId=,inSampleId=,outSampleId=,predId=) /parmbuff;

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	%* -- 环境初始化 --;
	%local inSample outSample model pred;

	%* -- 参数检查 --;
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



