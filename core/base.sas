* ---------------------------;
* ---- 总体介绍 -------------;
* ---------------------------;

* -- 模型术语 --;
* 模型/model						可以实现预测功能的一整套函数与相关参数，例如：一个logisitic输出的model就是一个模型;
* 建模框架/modelling frame 		用以训练模型的方法和相关参数，例如：logisitic做为一种建模方法可以称为一个模型生成框架，又如一个变量列表、搭配使用stepwise方法进行logisitic回归，也可以被看做是一个建模框架;
* 模型预测						指模型在给定样本的预测结果，根据模型的不同，其预测结果可能为事件概率，也可能为事件结果;
* 模型表现指标					指根据模型预测结果计算的，用以衡量预测表现好坏的指标，例如：auc;

* -- 功能组成 --;
* 模型生成与评价部分包括以下几个组成部分：;
* 模型训练函数 					主要完成模型的训练;
* 模型预测函数					主要负责进行样本外预测;		
* 决策函数						主要负责进行样本预测结果的转换（如：将logisitic的概率预测结果转换为01的结果预测），主要服务于表现计算函数;
* 表现计算函数					主要负责跟进样本预测结果进行表现计算;

* -- 文件管理规范 --;

* 汇总信息表;
* /info/minfo					模型信息表;
* /info/sinfo					样本信息表;
* /info/pinfo					预测与表现信息表;

* 样本数据;
* /sample/[sampleId]/data		样本数据;

* 模型数据;
* /model/[modelId]/model		模型;
* /model/[modelId]/out			模型输出数据;
* /model/[modelId]/report		模型训练报告;
* /model/[modelId]/result_		模型训练输出数据表前缀;

* 预测数据; 
* /pred/[predId]/report			预测报告;
* /pred/[predId]/result_		预测输出数据表前缀;			
* 单一指标型的表现被记录于pinfo表中，predId文件夹中保存图、表格等更为丰富形式的表现指标，如ROC曲线;

* -- 对象结构 --;
* 包含data、sample、model、pred三种对象;

* data对象成员;
*	基本信息;
*		key					数据名;
*		type				变量类型;
* 	临时执行信息;
*		table				所在ds;

* sample对象成员;
*	基本信息;
*		sampleId			样本id;
*		groupId				样本所属组id;
*		groupSeq			样本所属组内序号;
*		tags				样本标签;
*		desc				样本描述;
*	生成信息;
*		genDate				生成时间;
*		genMacro			生成宏;
*		genParam			生成参数;
*		inSampleIds			前一步样本id;
*		baseDate			基准时间,即该样本数据的最早可得时点;
*	变量信息（参见变量类型）;
*		vars				全变量;
*		modelVars			可以直接进入模型的变量;
*		textVars			文本变量;
*		classVars			分类变量;
*		numVars				连续变量;
*		binaryVars			01变量;
*		enumVars			非01分类变量;
*		moneyVars			金额变量;
*		amountVars			非金额连续变量;
*		logMoneyVars		log金额变量;
*		woeEnumVars			woe变量;
*		regVars				必输变量，该类变量若存在missing值，则整条记录将被抛弃;
*	变量缺失值;
*		defaultValue		缺失值赋值，格式为var=value1 var2=value2，如：NLC=0 NLC_UNSECURED=0;
*	文件信息;
*		root				根路径;
*		path				文件路径;
*		obs					观测数量;
*		key					键值变量/索引变量，将依据此变量建立索引;
*		index				键值索引名称，当为单变量索引时名称为变量名称，当为多变量索引时，索引名称为索引变量的hash（待实现）;
*	临时执行信息;
*		lib					临时lib名;
*		dinfo				存储dinfo的ds;
*		data				存储data的ds;
*		self				对象名;
*		err					最后一步的执行结果;
*	个性信息;

%global SAMPLE_MEMBERS;
%let SAMPLE_MEMBERS=	sampleId groupId groupSeq tags desc
						genDate genMacro genParam inSampleIds baseDate
						vars modelVars textVars numVars classVars binaryVars enumVars moneyVars amountVars woeEnumVars logMoneyVars
						defaultValue
						root path obs key
						lib result data dinfo self err;

* model对象成员;
*	基本信息;
*		modelId				模型id;
*		groupId				模型所属组id;
*		groupSeq			模型所属组内序号;
*		tags				模型标签;
*		desc				模型描述信息;
*	生成信息;
*		genDate				训练时间;
*		genMacro			训练宏;
*		genParam			训练参数，所有关于模型训练的参数均应可以通过genMacro与genParam确定;
*		baseDate			基准时间，由所使用数据中的最晚baseDate确定;
*		eaDate				模型的最早可得时间，由训练数据集中的最晚的eaDate确定;
*		inSampleId			训练样本id;
*		outSampleId			训练预测样本id;
* 		predId				预测id;
*		effDate				模型生效日期;
*		expDate				模型失效结束;
* 	模型参数;
*		indepVars			训练自变量列表;
*		depVars				训练因变量列表;
*		modelOptions		model语句模型配置信息;
*		by					子模型变量;
*		stepwise			是否启用stepwise;
*		realStep			stepwise下，实际执行的步数;
*		useWoeEnum			是否全部使用woeEnum变量;
*		useLogMoney			是否全部使用logMoney变量;
*	文件信息;
*		root				根路径
*		path				文件路径;
*	临时执行信息;
*		lib					临时lib名;
*		model				存储模型的数据集;
*		self				临时的对象/记录名;
*		err					最后一步的执行结果;
*		result				存储模型各类输出数据的ds前缀;
* 	模型配置信息;


*		
%global MODEL_MEMBERS;
%let MODEL_MEMBERS=		modelId groupId groupSeq tags desc
						genDate genMacro genParam inSampleId outSampleId predId
						baseDate eaDate effDate expDate
						indepVars depVars modelOptions by stepwise useWoeEnum useLogMoney realStep
						root path
						lib model self err result;

* pred对象成员;
*	基本信息;
*		predId				预测id;
*		groupId				预测所属组id;
*		groupSeq			预测所属组内序号;
*		tags				预测标签;
*		desc				预测描述信息;
*	生成信息;
*		genDate				预测时间;
*		genMacro			预测宏;
*		genParam			预测宏参数;
*		inSampleId			预测样本id;
*		outSampleId			预测结果样本id;
*		modelId				预测模型id;
*		modelDate
*		predDate;
*	文件信息;
*		root				根路径
*		path				文件路径;
*	临时执行信息;
*		use					pred执行用途，包含pred|perf两种，
*								pred	预测+评价，自动创建outSample对象，save时同时保存outSample;
*								perf	仅评价，不保存outSample;
*		lib					临时lib名;
*		self				临时的对象/记录名;
*		err					最后一步的执行结果;
*		inSample			输入样本对象;
*		outSample			输出样本对象;
*		model				模型对象;
*		result				输出数据表前缀;
*	评价信息;
*		pfAUC				AUC指标;
*		pfAIC				AIC指标;
*		pfAICC				AICC指标;
*		pfBIC				BIC指标;
*		pfSC				SC指标;
*		pfLogLike			;
*		pfLogL				;
*		pfMisClass			;
*		pfRSquare			R方;
*		pfAdjRSquare		调整R方;
*		pfBrierScore		BrierScore;
*		pfSomersD			Somers D;
*		pfTauA				Tau alpha;
*		pfGamma				Gamma;
*		pfHL				HL统计量;
*		pfProbKS			基于预测概率计算的ks指标;
*		pfFractionKS		基于累计样本比例k计算的ks指标;
*	附加模型与样本信息（用于统计分析与汇总展现）;
*		trainSampleId		训练样本id;
*		modelBaseDate		模型基准时间;

%global PRED_MEMBERS;
%let PRED_MEMBERS=		predId groupId groupSeq tags desc
						genDate genMacro genParam inSampleId outSampleId modelId
						modelDate predDate
						root path
						lib self err use inSample outSample model result
						pfAUC pfAIC pfAICC pfBIC pfSC pfLogLike pfLogL pfMisClass pfRSquare pfAdjRSquare pfBrierScore pfProbKS pfFractionKS
						pfSomersD pfTauA pfGamma pfHL
;

* -- 变量类型 --;
* vars = modelVars + textVars;
* modelVars = numVars + classVars;
* numVars = moneyVars + amountVars;
* classVars = binaryVars + enumVars;
* woeVars如非空，则必须与enumVars一一对应;
* logMoneyVars如非空，则必须与moneyVars一一对应;

* -- 代码结构 --;
* base.sas						info表管理操作代码;
* sample.sas					样本加工函数;
* rawSample.sas					原始样本加工函数;
* model.sas						各类模型训练函数;
* pred.sas						表现指标计算函数;

* -- base.sas --;

* info表操作函数;
*	initInfoLib								初始化info表的lib;
*	getSampleInfo(root=,sample=)			自数据库读取一个sample信息;
*	setSampleInfo(root=,sample=)			自数据库读取一个sample信息;
*	getModelInfo(root=,model=)				自数据库读取一个model信息;
*	setModelInfo(root=,model=)				自数据库读取一个model信息;
*	getPredInfo(root=,pred=)				自数据库读取一个pred信息;
*	setPredInfo(root=,pred=)				自数据库读取一个pred信息;

* sample操作函数;
*	newSample(sample)						创建一个空sample对象;
*	createSample(root=,sampleId=,res=)		创建样本,包含样本路径检查/创建、lib导入、临时lib与数据文件命名等;
*	loadSample(root=,sampleId=,res=)			装载样本;
*	saveSample(root=,sample=) 				存储样本;
*	dropSample(sample);

* model操作函数;
*	newModel								创建一个空model对象;
*	createModel								创建model;
*	loadModel								装载model;
*	saveModel								存储model;

* pred操作函数;
*	newPred									创建一个空pred对象;
*	createPred								创建pred;
*	loadPred								装载pred;				
*	savePred								存储pred;

* dinfo操作函数;
*	dataInfoToSampleInfo（已无用，拟删除）	将dataInfo转换为sampleInfo格式;

* -- model.sas --;

* -- sample.sas --;

* -- pred.sas --;

* -- 模型评价总体思路 --;
* 模型评价使用层级汇总的方式：预测表现指标->预测表现->模型表现->建模框架表现;
* 表现的比较可以在不同层级进行，更高层次的表现由低层次表现汇总而来;

* -----------------------------;
* ----- utili function -------;
* -----------------------------;
%macro isValidVarType(type);
	%local res;
	%let res=0;
	%if %upcase(&type)=ENUM or %upcase(&type)=BINARY or %upcase(&type)=AMOUNT or %upcase(&type)=MONEY or %upcase(&type)=TEXT %then %let res=1;
	&res.
%mend;

* -----------------------------;
* ----- Info management -------;
* -----------------------------;
* 主要进行各类info表的操作，包括minfo、sinfo、dinfo;

* ---- function initInfoLib ----;
* 初始化info lib，返回临时表名;
%macro initInfoLib(root=,res=) /parmbuff;
	%local path lib obs;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &res= %then %error(initInfoLib: Required param is empty! param=&syspbuff);
	%let &res=%str();
	%let path=&root.info\;
	%checkFolder(&path);
	%importTempLib(&path,&res);
%mend;

* ---- function setSampleInfo ----;
* 设置sample info;
* input;
*	root	根路径;
*	info	输入的sample info,sample对象名;
%macro setSampleInfo(root=,sample=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%if &root= or &sample= or %refExist(&sample.sampleId)=0 %then %error(setSampleInfo: Required param is empty! param=&syspbuff);
	%local infoLib;
	%initInfoLib(root=&root,res=&tres); %let infoLib=&&&tres;
	%setDsRecord(ds=&infoLib..sinfo,record=&sample,key=sampleId);
	%dropLib(&infoLib);
%mend;

* ---- function setSampleInfo ----;
* 设置model info;
%macro setModelInfo(root=,model=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local infoLib;
	%if &root= or &model= or %refExist(&model.modelId)=0 %then %error(setModelInfo: Required param is empty! param=&syspbuff);
	%initInfoLib(root=&root,res=&tres);%let infoLib=&&&tres;
	%setDsRecord(ds=&infoLib..minfo,record=&model,key=modelId);
	%dropLib(&infoLib);
%mend;

* ---- function setPredInfo ----;
* 设置pred info;
%macro setPredInfo(root=,pred=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local infoLib;
	%if &root= or &pred= or %refExist(&pred.predId)=0 %then %error(setPredInfo: Required param is empty! param=&syspbuff);
	%initInfoLib(root=&root,res=&tres);%let infoLib=&&&tres;
	%setDsRecord(ds=&infoLib..pinfo,record=&pred,key=predId);
	%dropLib(&infoLib);
%mend;

* ---- function getSampleInfo ----;
* 获取sample info;
* input;
*	root			根路径;
*	sampleId		样本id;
*	res				返回sample对象名;
* output;
*	返回sample对象;
%macro getSampleInfo(root=,sample=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local infoLib;

	%if &root= or &sample= %then %error(getSampleInfo: Required param is empty! param=&syspbuff);
	%if %refExist(&sample.sampleId)=0 %then %error(getSampleInfo: sampleId is empty! param=&syspbuff);

	%initInfoLib(root=&root,res=&tres);%let infoLib=&&&tres;
	%getDsRecord(ds=&infoLib..sinfo,record=&sample,where=%str(sampleId="&&&sample.sampleId"));
	%if &&&sample^=0 %then %error(getSampleInfo: No sample info! param=&syspbuff err=&&&sample sampleId=&&&sample.sampleId);
	%dropLib(&infoLib);
%mend;

* ---- function getModelInfo ----;
* 获取model info;
%macro getModelInfo(root=,model=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local infoLib;
	%if &root= or &model= %then %error(getModelInfo: Required param is empty! param=&syspbuff);
	%if %refExist(&model.modelId)=0 %then %error(getModelInfo: modelId is empty! param=&syspbuff);
	
	%initInfoLib(root=&root,res=&tres);%let infoLib=&&&tres;
	%getDsRecord(ds=&infoLib..minfo,record=&model,where=%str(modelId="&&&model.modelId"));
	%if &&&model^=0 %then %error(getModelInfo: No model info! param=&syspbuff err=&&&model modelId=&&&model.modelId);
	%dropLib(&infoLib);
%mend;

* ---- function getPredInfo ----;
* 获取pred info;
%macro getPredInfo(root=,pred=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local infoLib;
	%if &root= or &pred= %then %error(getPredInfo: Required param is empty! param=&syspbuff);
	%if %refExist(&pred.predId)=0 %then %error(getPredInfo: predId is empty! param=&syspbuff);

	%initInfoLib(root=&root,res=&tres);%let infoLib=&&&tres;
	%getDsRecord(ds=&infoLib..pinfo,record=&pred,where=%str(predId="&&&pred.predId"));
	%if &&&pred^=0 %then %error(getPredInfo: No pred info! param=&syspbuff err=&&&pred predId=&&&pred.predId);
	%dropLib(&infoLib);
%mend;

* ---- dataInfoToSampleInfo ----;
* 将数据信息转换为样本信息，主要完成关键信息的收集;
* input;
*	data		datainfo数据集;
*	info		返回的sampleInfo的record名称;
* 备注;
* dinfo使用dict进行操作，因此所有字段都是string;


%macro dataInfoToSampleInfo(data=,sample=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%if not %dsExist(&data) %then %error(dataInfoToSampleInfo: Dataset doesnot exist! param=&syspbuff);
	%if &sample= %then %error(dataInfoToSampleInfo: Target info doesnot exist! param=&syspbuff);
	%global &sample.vars &sample.numVars &sample.textVars &sample.classVars &sample.binaryVars &sample.enumVars &sample.amountVars &sample.moneyVars &sample.modelVars;
	proc sql noprint;
		select distinct key into :&sample.vars separated by ' ' from &data;
		select distinct key into :&sample.textVars separated by ' ' from &data where type='TEXT';
		select distinct key into :&sample.amountVars separated by ' ' from &data where type='AMOUNT';
		select distinct key into :&sample.moneyVars separated by ' ' from &data where type='MONEY';
		select distinct key into :&sample.binaryVars separated by ' ' from &data where type='BINARY';
		select distinct key into :&sample.enumVars separated by ' ' from &data where type='ENUM';
	quit;
	%varsOr(a=&&&sample.binaryVars,b=&&&sample.enumVars,res=&sample.classVars);
	%varsOr(a=&&&sample.amountVars,b=&&&sample.moneyVars,res=&sample.numVars);
	%varsOr(a=&&&sample.classVars,b=&&&sample.numVars,res=&sample.modelVars);
	%varsOr(a=&&&sample.modelVars,b=&&&sample.textVars,res=&tres);%let sumVars=&&&tres;
	%varsEqual(a=&&&sample.vars,b=&sumVars,res=&tres);
	%if &&&tres=0 %then %error(dataInfoToSampleInfo: sumVars not equal vars!);
%mend;

* -----------------------------;
* ----- Sample management -----;
* -----------------------------;

* ---- function sampleExsit ----;
%macro sampleExist(root=,sampleId=,res=);
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local infoLib sampleLib path;

	%if &sampleId= or %refExist(&res)=0 %then %error(&macro: Required param is empty! param=&syspbuff);
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	
	%let &res=0;
	
	%initInfoLib(root=&root,res=&tres);%let infoLib=&&&tres;

	%if %dsExist(&infoLib..sinfo)=0 %then %goto FINAL;

	proc sql noprint;
		select * from &infoLib..sinfo where sampleId="&sampleId";
	quit;
	%if &SQLOBS=0 %then %goto FINAL;

	%let path=&root.sample\&sampleId.\;
	%if %folderExist(&path)=0 %then %goto FINAL;

	%importTempLib(&path,&tres);%let sampleLib=&&&tres;
	%if %dsExist(&sampleLib..data)=0 %then %goto FINAL;

	%let &res=1;

	%FINAL:
	%dropLib(&infoLib);
	%dropLib(&sampleLib);
%mend;

* ---- function newSample ----;
* 创建新的sample对象，类似于构造函数，但仅包含变量定义与最基本的赋值;
* 使用方法： %let sample=%createSample;
* input;
*	sample		样本名，可以为空，为空时将随机创建一个对象名称;
* output;
*	直接返回新创建的样本对象名称;

%macro newSample(sample=,res=);
	%local tres;%let tres=%createTempVar;%local &tres;
	%global SAMPLE_MEMBERS;

	%recordNew(members=&SAMPLE_MEMBERS,record=&sample,res=&tres);%let sample=&&&tres;
	%let &sample.err=0;
	%let &sample.self=&sample;
	%let &res=&sample;
%mend;

* ---- function createSample ----;
* 创建新的sample,主要完成对象相关初始化工作，类似于构造函数，主要初始化工作包含路径创建、lib创建、各类ds名的创建;
* input:
*	root;
*	sampleId;
*	res					sample对象名称的容器变量;
* output;
*	返回创建的sample对象名称;

%macro createSample(root=,sampleId=,res=) /parmbuff;
	* 环境创建;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local path lib sample;
	
	* 参数检查;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= %then %error(createSample: ROOT is empty! param=&syspbuff);
	%if %refExist(&res)=0 %then %error(createSample: RES doesnot exist! param=&syspbuff);
	
	%if &sampleId= %then %let sampleId=%genId(prefix=S,len=20);
	* 文件环境创建;
	
	%newSample(res=&tres);%let sample=&&&tres;

	%let path=&root.sample\&sampleId.\;
	
	%checkFolder(&path);
	%importTempLib(&path,&tres);%let lib=&&&tres;
	%clearLib(&lib);

	%let &sample.sampleId=&sampleId;
	%let &sample.root=&root;
	%let &sample.lib=&lib;
	%let &sample.path=&path;
	%let &sample.data=&lib..data;
	%let &sample.dinfo=&lib..dinfo;
	%let &sample.result=&lib..result;

	%let &res=&sample;

%mend;

* ---- function loadSample ----;
* 主要完成sample的导入功能，主要包括样本路径检查、数据文件检查、样本临时lib创建、自sinfo表读出样本信息、各临时ds名创建等;
* input;
*	root;
*	sampleId;
*	res					sample对象名称的容器变量;
* 使用示例;
%*	%local tres;
%*	%let tres=%createTempVar;
%*	%loadSample(root=&root,sampleId=&sampleId,res=&tres);
%*	%let sample=&&&tres;
%* 	%let err=&&&sample.err;

%macro loadSample(root=,sampleId=,res=) /parmbuff;

	* 环境准备;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local lib path obs sample infoLib;

	* 参数检查;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &sampleId= or not %refExist(&res) %then %error(loadSample: Required param is empty! param=&syspbuff);

	* 样本信息读取;
	%newSample(res=&tres);%let sample=&&&tres;
	%let &sample.sampleId=&sampleId;
	%getSampleInfo(root=&root,sample=&sample);

	* 样本数据路径检查;
	%let path=&root.sample\&sampleId.\;
	%if not %folderExist(&path) %then %error(loadSample: Target path does not exist! path=&path);

	* 样本data文件检查;
	%importTempLib(&path,&tres);%let lib=&&&tres;
	%if %dsExist(&lib..data)=0 %then %error(loadSample: Target sample data does not exist! path=&path);

	* 样本观测检查;
	%getDsObs(ds=&lib..data,res=&tres);%let obs=&&&tres;
	%if &obs<1 %then %error(loadSample: Target sample data has no record! path=&path);

	* 样本临时信息写入;
	%let &sample.root=&root;
	%let &sample.lib=&lib;
	%let &sample.path=&path;
	%let &sample.data=&lib..data;
	%let &sample.result=&lib..result;
	%let &sample.dinfo=&lib..dinfo;
	%let &sample.obs=&obs;

	* 返回值写入;
	%let &res=&sample;
%mend;

* ---- function saveSample ----;
* 主要完成sample的写入功能;
* input;
*	root;
*	sample				sample对象名;
*	通过sample成员传递的参数;
*		sample.sampleId		id;

* output:
*	通过sample.err返回运行状态(暂未启用，出错均为%error);
* 使用说明:;
*  此版本的saveSample只完成sinfo写入工作;

%macro saveSample(root=,sample=) /parmbuff;
	%local macro;%let macro=%getSelf; 
	%local tres;%let tres=%createTempVar;%local &tres;
	* 参数检查;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &sample= %then %error(saveSample: Required param is empty! param=&syspbuff);
	%if %dsExist(&&&sample.data)=0 %then %error(saveSample: The sample data does not exist! param=&syspbuff);
	%getDsObs(ds=&&&sample.data,res=&tres);%let &sample.obs=&&&tres;
	
	%local sumVars indexName;


	* 变量类型处理与检查;
	%varsOr(a=&&&sample.binaryVars,b=&&&sample.enumVars,res=&sample.classVars);
	%varsOr(a=&&&sample.amountVars,b=&&&sample.moneyVars,res=&sample.numVars);
	%varsOr(a=&&&sample.classVars,b=&&&sample.numVars,res=&sample.modelVars);
	%varsOr(a=&&&sample.modelVars,b=&&&sample.textVars,res=&sample.vars);

	* vars类成员规整化;
	%let &sample.inSampleIds=%str(%removeDupBlanks(&&&sample.inSampleIds));

	%let &sample.vars=%str(%removeDupBlanks(&&&sample.vars));
	%let &sample.modelVars=%str(%removeDupBlanks(&&&sample.modelVars));
	%let &sample.textVars=%str(%removeDupBlanks(&&&sample.textVars));
	%let &sample.numVars=%str(%removeDupBlanks(&&&sample.numVars));
	%let &sample.classVars=%str(%removeDupBlanks(&&&sample.classVars));
	%let &sample.binaryVars=%str(%removeDupBlanks(&&&sample.binaryVars));
	%let &sample.enumVars=%str(%removeDupBlanks(&&&sample.enumVars));
	%let &sample.amountVars=%str(%removeDupBlanks(&&&sample.amountVars));
	%let &sample.moneyVars=%str(%removeDupBlanks(&&&sample.moneyVars));
	%let &sample.woeEnumVars=%str(%removeDupBlanks(&&&sample.woeEnumVars));
	%let &sample.logMoneyVars=%str(%removeDupBlanks(&&&sample.logMoneyVars));

	%let &sample.defaultValue=%str(%removeDupBlanks(%str(&&&sample.defaultValue)));

	* 索引创建;
	%if &&&sample.key= %then %let &sample.key=SPIN;
	%varsCount(vars=&&&sample.key,res=&tres);
	%if &&&tres=1 %then %let indexName=&&&sample.key;
	%else %let indexName=I%hash(&&&sample.key);
	proc sql;
		create unique index &indexName on &&&sample.data (%sasVarsToSql(&&&sample.key));
	quit;

	* 样本信息存储;
	%setSampleInfo(root=&root,sample=&sample);	
	%put &macro: Sample &&&sample.sampleId saved! obs=&&&sample.obs;
%mend;

* ---- function dropSample ----;
* 主要完成sample及其相关资源的释放，类似析构函数;
* input;
*	sample				sample对象名;
%macro dropSample(sample) /parmbuff;
	%local macro;%let macro=%getSelf; 
	%local tres;%let tres=%createTempVar;%local &tres;

	* 参数检查;
	%if &sample= %then %error(&macro: Required param is empty! param=&syspbuff);

	* 资源释放;
	%if %refExist(&sample.lib)=1 %then %dropLib(&&&sample.lib);
	%recordDrop(&sample);
%mend;

* ---- function deleteSample ----;
* 主要完成sample及其相关资源的删除，包括sampleId文件夹与sinfo的相关记录;
* deleteSample按sampleId确定待删除的文件信息，与sinfo表中记录的临时执行信息无关;
* input;
*	sampleId				sampleId;
* output:
*	通过sample.err返回运行状态(暂未启用，出错均为%error);
%macro deleteSample(root=,sampleId=) /parmbuff;
	%local macro;%let macro=%getSelf; 
	%local tres;%let tres=%createTempVar;%local &tres;
	%local path;

	* 参数检查;
	%if &sampleId= %then %error(&macro: Required param is empty! param=&syspbuff);
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);

	* 文件删除;
	%let path=&root.sample\&sampleId.\;
	%if %folderExist(&path)=1 %then %removeFolder(&path);

	* 记录删除;
	%initInfoLib(res=&tres);%let lib=&&&tres;
	proc sql noprint;
		delete * from &lib..sinfo where sampleId="&sampleId";
	quit;
	%dropLib(&lib);
%mend;


* -----------------------------;
* ----- Model management -----;
* -----------------------------;

* ---- function newModel ----;
* input;
*	model		指定的model对象名;
*	res			返回对象名;

%macro newModel(model=,res=);
	%local tres;%let tres=%createTempVar;%local &tres;
	%global MODEL_MEMBERS;
	%recordNew(members=&MODEL_MEMBERS,record=&model,res=&tres);%let model=&&&tres;
	%let &model.err=0;
	%let &model.self=&model;
	%let &res=&model;
%mend;

* ---- function createModel ----;
* input;
*	root;
*	modelId			如为空将自动创建，格式为MD+随机字符;
*	res;
%macro createModel(root=,modelId=,res=) /parmbuff;
	* 环境准备;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local path lib model;
	* 参数检查;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or not %refExist(&res) %then %error(createModel: Required param is empty! param=&syspbuff);
	%if &modelId= %then %let modelId=%genId(prefix=M,len=20);
	%let path=&root.model\&modelId.\;
	%checkFolder(&path);
	%importTempLib(&path,&tres);%let lib=&&&tres;
	%clearLib(&lib);
	%newModel(res=&tres);%let model=&&&tres;
	%let &model.modelId=&modelId;
	%let &model.root=&root;
	%let &model.lib=&lib;
	%let &model.path=&path;
	%let &model.result=&lib..result;
	%let &model.model=&lib..model;
	%let &model.out=&lib..out;
	%let &res=&model;
%mend;

* ---- function loadModel ----;

%macro loadModel(root=,modelId=,res=) /parmbuff;
	* 环境准备;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local lib path model;

	* 参数检查;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &modelId= or not %refExist(&res) %then %error(loadModel: Required param is empty! param=&syspbuff);
	
	* 信息读取;
	%newModel(res=&tres);%let model=&&&tres;
	%let &model.modelId=&modelId;
	%getmodelInfo(root=&root,model=&model);

	* 模型路径检查;
	%let path=&root.model\&modelId.\;
	%if not %folderExist(&path) %then %error(loadModel: Target path does not exist! path=&path);

	* 模型文件检查;
	%importTempLib(&path,&tres);%let lib=&&&tres;
	%if not %dsExist(&lib..model) %then %error(loadModel: Target model does not exist! path=&path);
	
	* 临时执行信息写入;
	%let &model.root=&root;
	%let &model.path=&path;
	%let &model.lib=&lib;
	%let &model.model=&lib..model;
	%let &model.result=&lib..result;

	* 返回值写入;
	%let &res=&model;
%mend;

* ---- function saveModel ----;

%macro saveModel(root=,model=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;

	* 参数检查;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &model= %then %error(saveModel: Required param is empty! param=&syspbuff);

	* 样本信息存储;
	%setModelInfo(root=&root,model=&model);	
%mend;

* ---- function dropModel ----;

%macro dropModel(model) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;

	* 参数检查;
	%if &model= %then %error(dropModel: Required param is empty! param=&syspbuff);

	* 资源释放;
	%if %refExist(&model.lib)=1 %then %dropLib(&&&model.lib);
	%recordDrop(&model);
%mend;



* -----------------------------;
* ----- Pred management -----;
* -----------------------------;

* Pred自动创建outSample时，outSample成员设定规则;
* sample对象成员;
*	基本信息;
*		sampleId			不可修改，creatPred时生成;
*		groupId				可修改;
*		groupSeq			可修改;
*		tags				可修改;
*		desc				可修改;
*	生成信息;
*		genDate				不可修改，取自pred对应值，savePred时生成;
*		genMacro			不可修改，取自pred对应值，savePred时生成;
*		genParam			不可修改，取自pred对应值，savePred时生成;
*		inSampleIds			不可修改，取自inSampleId，savePred时生成;
*		baseDate			不可修改，取自inSample对应值，savePred时生成;
*	变量信息;
*		vars				不可修改，savePred时计算生成，savePred时生成;
*		modelVars			不可修改，savePred时生成，savePred时生成;
*		textVars			可修改，默认为inSample的对应值，savePred时生成;
*		classVars			不可修改，savePred时计算生成，savePred时生成;
*		numVars				不可修改，savePred时计算生成，savePred时生成;
*		binaryVars			可修改，默认为inSample的对应值，savePred时生成;
*		enumVars			可修改，默认为inSample的对应值，savePred时生成;
*		moneyVars			可修改，默认为inSample的对应值，savePred时生成;
*		amountVars			可修改，默认为inSample的对应值，savePred时生成;
*		logMoneyVars		不可修改，inSample的对应值，savePred时生成;
*		woeEnumVars			不可修改，inSample的对应值，savePred时生成;
*	变量缺失值;
*		defaultValue		可修改，默认为inSample的对应值;
*	文件信息;
*		root				不可修改，creatPred时生成;
*		path				不可修改，creatPred时生成;
*		obs					不可修改，creatPred时生成;
*		key					不可修改，取自inSample对应值，savePred时生成;
*	临时执行信息;
*		lib					不可修改，creatPred时生成;
*		dinfo				不可修改，creatPred时生成;
*		data				不可修改，creatPred时生成;
*		self				不可修改，creatPred时生成;
*		err					不可修改，creatPred时生成;
;

* 变量类型、默认值设置规则;
*	变量类型与默认值初始为空，可自行手工设置，当调用savePred时如仍为空，则使用inSample的对应值;
*	
* ---- function newPred ----;
* input;
*	pred		指定的pred对象名;
*	res			返回对象名;

%macro newPred(pred=,res=);
	%local tres;%let tres=%createTempVar;%local &tres;
	%global PRED_MEMBERS;
	%recordNew(members=&PRED_MEMBERS,record=&pred,res=&tres);%let pred=&&&tres;
	%let &pred.err=0;
	%let &pred.self=&pred;
	%let &res=&pred;
%mend;

* ---- function createPred ----;
* input:
*	root;
*	predId			选输，为空时将自动创建;
*	inSampleId		输入样本id;
*	inSample		输入样本对象，inSample与inSampleId二者必输其一;
*	modelId			模型id;
*	model			模型对象，model与modelId二者必输其一;
*	outSampleId		输出样本id，选输，为空时将自动创建;
*	use				用途;
*	res				返回创建的pred对象;
%macro createPred(root=,predId=,modelId=,model=,inSampleId=,inSample=,outSampleId=,use=,res=) /parmbuff;

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local path lib pred outSample;

	* 参数检查;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &inSampleId= and &inSample= %then %error(&macro: No input sample! param=&syspbuff);
	%if &modelId= and &model= %then %error(&macro: No input model! param=&syspbuff);
	%if &predId= and %refExist(&res)=0 %then %error(&macro: No return method! param=&syspbuff); 
	%if &predId= %then %let predId=%genId(prefix=P,len=20);
	%if &use= %then %let use=perf;
	
	* 基本信息初始化;
	%newPred(res=&tres);%let pred=&&&tres;
	%let &pred.predId=&predId;
	%let &pred.root=&root;
	%let path=&root.pred\&predId.\;
	%checkFolder(&path);
	%importTempLib(&path,&tres);%let lib=&&&tres;
	%clearLib(&lib);
	%let &pred.lib=&lib;
	%let &pred.path=&path;
	%let &pred.result=&lib..result;
	%let &pred.use=&use;

	* 读入模型;
	%if &model ne %then %let &pred.model=&model;
	%else %loadModel(modelId=&modelId,res=&pred.model);
	%let model=&&&pred.model;
	%let &pred.modelId=&&&model.modelId;

	* 读入样本;
	%if &inSample ne %then %let &pred.inSample=&inSample;
	%else %loadSample(sampleId=&inSampleId,res=&pred.inSample);
	%let inSample=&&&pred.inSample;
	%let &pred.inSampleId=&&&inSample.sampleId;
	
	* 创建输出样本;
	%createSample(sampleId=&outSampleId,res=&pred.outSample);
	%let outSample=&&&pred.outSample;
	%let &pred.outSampleId=&&&outSample.sampleId;

	%let &res=&pred;
%mend;

* ---- function loadPred ----;

%macro loadPred(root=,predId=,use=,res=) /parmbuff;

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local lib path obs model;

	* 参数检查;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &predId= or &res= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if not %symexist(&res) %then %error(&macro: RES is empty! param=&syspbuff);
	%if &use= %then %let use=perf;

	* pred信息读取;
	%newPred(res=&tres);%let pred=&&&tres;
	%let &pred.predId=&predId;
	%getPredInfo(root=&root,pred=&pred);
	
	* pred临时执行信息写入;
	%let path=&root.pred\&predId.\;
	%if not %folderExist(&path) %then %error(loadPred: Target path does not exist! path=&path);
	%importTempLib(&path,&tres);%let lib=&&&tres;
	%let &pred.root=&root;
	%let &pred.path=&path;
	%let &pred.lib=&lib;
	%let &pred.result=&lib..result;
	%let &pred.use=&use;

	* sample与model读取;
	%if &&&pred.inSampleId= or &&&pred.outSampleId= or &&&pred.modelId= %then %error(&macro: Target pred info is not valid! inSampleId=&&&pred.inSampleId outSampleId=&&&pred.outSampleId modelId=&&&pred.modelId);
	%loadSample(sampleId=&&&pred.inSampleId,res=&pred.inSample);
	%loadSample(sampleId=&&&pred.outSampleId,res=&pred.outSample);
	%loadModel(modelId=&&&pred.modelId,res=&pred.model);

	* 返回值写入;
	%let &res=&pred;
%mend;

* ---- function savePred ----;

%macro savePred(root=,pred=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	
	%local outSample inSample model;

	* 参数检查;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &pred= %then %error(&macro: Required param is empty! param=&syspbuff);

	%if %upcase(&&&pred.use)=%upcase(pred) %then %do;
		%let model=&&&pred.model;
		%let &pred.modelDate=&&&model.baseDate;
		%let inSample=&&&pred.inSample;
		%let &pred.predDate=&&&inSample.baseDate;
	%end;
	* 样本信息存储;
	%setPredInfo(root=&root,pred=&pred);	

	* 输出样本存储;
	%if %upcase(&&&pred.use)=%upcase(pred) %then %do;
		%let outSample=&&&pred.outSample;
		%let inSample=&&&pred.inSample;

		%let &outSample.genMacro=&&&pred.genMacro;
		%let &outSample.genParam=&&&pred.genParam;
		%let &outSample.genDate=&&&pred.genDate;

		%let &outSample.baseDate=&&&inSample.baseDate;
		%let &outSample.inSampleIds=&&&inSample.sampleId;
		%let &outSample.key=&&&inSample.key;

		%if &&&outSample.textVars= %then %let &outSample.textVars=&&&inSample.textVars;
		%if &&&outSample.enumVars= %then %let &outSample.enumVars=&&&inSample.enumVars;
		%if &&&outSample.binaryVars= %then %let &outSample.binaryVars=&&&inSample.binaryVars;
		%if &&&outSample.moneyVars= %then %let &outSample.moneyVars=&&&inSample.moneyVars;
		%if &&&outSample.amountVars= %then %let &outSample.amountVars=&&&inSample.amountVars;

		%let &outSample.woeEnumVars=&&&inSample.woeEnumVars;
		%let &outSample.logMoneyVars=&&&inSample.logMoneyVars;

		%if &&&outSample.defaultValue= %then %let &outSample.defaultValue=&&&inSample.defaultValue;
		
		%saveSample(sample=&outSample);
	%end;
%mend;

* ---- function dropPred ----;

%macro dropPred(pred) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;

	* 参数检查;
	%if &pred= %then %error(dropPred: Required param is empty! param=&syspbuff);

	* 资源释放;
	%if %refExist(&pred.inSample)=1 %then %dropSample(&&&pred.inSample);
	%if %refExist(&pred.outSample)=1 %then %dropSample(&&&pred.outSample);
	%if %refExist(&pred.model)=1 %then %dropModel(&&&pred.model);
	%if %refExist(&pred.lib)=1 %then %dropLib(&&&pred.lib);
	%recordDrop(&pred);
%mend;
