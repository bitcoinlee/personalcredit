* ---------------------------;
* ---- ������� -------------;
* ---------------------------;

* -- ģ������ --;
* ģ��/model						����ʵ��Ԥ�⹦�ܵ�һ���׺�������ز��������磺һ��logisitic�����model����һ��ģ��;
* ��ģ���/modelling frame 		����ѵ��ģ�͵ķ�������ز��������磺logisitic��Ϊһ�ֽ�ģ�������Գ�Ϊһ��ģ�����ɿ�ܣ�����һ�������б�����ʹ��stepwise��������logisitic�ع飬Ҳ���Ա�������һ����ģ���;
* ģ��Ԥ��						ָģ���ڸ���������Ԥ����������ģ�͵Ĳ�ͬ����Ԥ��������Ϊ�¼����ʣ�Ҳ����Ϊ�¼����;
* ģ�ͱ���ָ��					ָ����ģ��Ԥ��������ģ����Ժ���Ԥ����ֺû���ָ�꣬���磺auc;

* -- ������� --;
* ģ�����������۲��ְ������¼�����ɲ��֣�;
* ģ��ѵ������ 					��Ҫ���ģ�͵�ѵ��;
* ģ��Ԥ�⺯��					��Ҫ�������������Ԥ��;		
* ���ߺ���						��Ҫ�����������Ԥ������ת�����磺��logisitic�ĸ���Ԥ����ת��Ϊ01�Ľ��Ԥ�⣩����Ҫ�����ڱ��ּ��㺯��;
* ���ּ��㺯��					��Ҫ�����������Ԥ�������б��ּ���;

* -- �ļ�����淶 --;

* ������Ϣ��;
* /info/minfo					ģ����Ϣ��;
* /info/sinfo					������Ϣ��;
* /info/pinfo					Ԥ���������Ϣ��;

* ��������;
* /sample/[sampleId]/data		��������;

* ģ������;
* /model/[modelId]/model		ģ��;
* /model/[modelId]/out			ģ���������;
* /model/[modelId]/report		ģ��ѵ������;
* /model/[modelId]/result_		ģ��ѵ��������ݱ�ǰ׺;

* Ԥ������; 
* /pred/[predId]/report			Ԥ�ⱨ��;
* /pred/[predId]/result_		Ԥ��������ݱ�ǰ׺;			
* ��һָ���͵ı��ֱ���¼��pinfo���У�predId�ļ����б���ͼ�����ȸ�Ϊ�ḻ��ʽ�ı���ָ�꣬��ROC����;

* -- ����ṹ --;
* ����data��sample��model��pred���ֶ���;

* data�����Ա;
*	������Ϣ;
*		key					������;
*		type				��������;
* 	��ʱִ����Ϣ;
*		table				����ds;

* sample�����Ա;
*	������Ϣ;
*		sampleId			����id;
*		groupId				����������id;
*		groupSeq			���������������;
*		tags				������ǩ;
*		desc				��������;
*	������Ϣ;
*		genDate				����ʱ��;
*		genMacro			���ɺ�;
*		genParam			���ɲ���;
*		inSampleIds			ǰһ������id;
*		baseDate			��׼ʱ��,�����������ݵ�����ɵ�ʱ��;
*	������Ϣ���μ��������ͣ�;
*		vars				ȫ����;
*		modelVars			����ֱ�ӽ���ģ�͵ı���;
*		textVars			�ı�����;
*		classVars			�������;
*		numVars				��������;
*		binaryVars			01����;
*		enumVars			��01�������;
*		moneyVars			������;
*		amountVars			�ǽ����������;
*		logMoneyVars		log������;
*		woeEnumVars			woe����;
*		regVars				����������������������missingֵ����������¼��������;
*	����ȱʧֵ;
*		defaultValue		ȱʧֵ��ֵ����ʽΪvar=value1 var2=value2���磺NLC=0 NLC_UNSECURED=0;
*	�ļ���Ϣ;
*		root				��·��;
*		path				�ļ�·��;
*		obs					�۲�����;
*		key					��ֵ����/���������������ݴ˱�����������;
*		index				��ֵ�������ƣ���Ϊ����������ʱ����Ϊ�������ƣ���Ϊ���������ʱ����������Ϊ����������hash����ʵ�֣�;
*	��ʱִ����Ϣ;
*		lib					��ʱlib��;
*		dinfo				�洢dinfo��ds;
*		data				�洢data��ds;
*		self				������;
*		err					���һ����ִ�н��;
*	������Ϣ;

%global SAMPLE_MEMBERS;
%let SAMPLE_MEMBERS=	sampleId groupId groupSeq tags desc
						genDate genMacro genParam inSampleIds baseDate
						vars modelVars textVars numVars classVars binaryVars enumVars moneyVars amountVars woeEnumVars logMoneyVars
						defaultValue
						root path obs key
						lib result data dinfo self err;

* model�����Ա;
*	������Ϣ;
*		modelId				ģ��id;
*		groupId				ģ��������id;
*		groupSeq			ģ�������������;
*		tags				ģ�ͱ�ǩ;
*		desc				ģ��������Ϣ;
*	������Ϣ;
*		genDate				ѵ��ʱ��;
*		genMacro			ѵ����;
*		genParam			ѵ�����������й���ģ��ѵ���Ĳ�����Ӧ����ͨ��genMacro��genParamȷ��;
*		baseDate			��׼ʱ�䣬����ʹ�������е�����baseDateȷ��;
*		eaDate				ģ�͵�����ɵ�ʱ�䣬��ѵ�����ݼ��е������eaDateȷ��;
*		inSampleId			ѵ������id;
*		outSampleId			ѵ��Ԥ������id;
* 		predId				Ԥ��id;
*		effDate				ģ����Ч����;
*		expDate				ģ��ʧЧ����;
* 	ģ�Ͳ���;
*		indepVars			ѵ���Ա����б�;
*		depVars				ѵ��������б�;
*		modelOptions		model���ģ��������Ϣ;
*		by					��ģ�ͱ���;
*		stepwise			�Ƿ�����stepwise;
*		realStep			stepwise�£�ʵ��ִ�еĲ���;
*		useWoeEnum			�Ƿ�ȫ��ʹ��woeEnum����;
*		useLogMoney			�Ƿ�ȫ��ʹ��logMoney����;
*	�ļ���Ϣ;
*		root				��·��
*		path				�ļ�·��;
*	��ʱִ����Ϣ;
*		lib					��ʱlib��;
*		model				�洢ģ�͵����ݼ�;
*		self				��ʱ�Ķ���/��¼��;
*		err					���һ����ִ�н��;
*		result				�洢ģ�͸���������ݵ�dsǰ׺;
* 	ģ��������Ϣ;


*		
%global MODEL_MEMBERS;
%let MODEL_MEMBERS=		modelId groupId groupSeq tags desc
						genDate genMacro genParam inSampleId outSampleId predId
						baseDate eaDate effDate expDate
						indepVars depVars modelOptions by stepwise useWoeEnum useLogMoney realStep
						root path
						lib model self err result;

* pred�����Ա;
*	������Ϣ;
*		predId				Ԥ��id;
*		groupId				Ԥ��������id;
*		groupSeq			Ԥ�������������;
*		tags				Ԥ���ǩ;
*		desc				Ԥ��������Ϣ;
*	������Ϣ;
*		genDate				Ԥ��ʱ��;
*		genMacro			Ԥ���;
*		genParam			Ԥ������;
*		inSampleId			Ԥ������id;
*		outSampleId			Ԥ��������id;
*		modelId				Ԥ��ģ��id;
*		modelDate
*		predDate;
*	�ļ���Ϣ;
*		root				��·��
*		path				�ļ�·��;
*	��ʱִ����Ϣ;
*		use					predִ����;������pred|perf���֣�
*								pred	Ԥ��+���ۣ��Զ�����outSample����saveʱͬʱ����outSample;
*								perf	�����ۣ�������outSample;
*		lib					��ʱlib��;
*		self				��ʱ�Ķ���/��¼��;
*		err					���һ����ִ�н��;
*		inSample			������������;
*		outSample			�����������;
*		model				ģ�Ͷ���;
*		result				������ݱ�ǰ׺;
*	������Ϣ;
*		pfAUC				AUCָ��;
*		pfAIC				AICָ��;
*		pfAICC				AICCָ��;
*		pfBIC				BICָ��;
*		pfSC				SCָ��;
*		pfLogLike			;
*		pfLogL				;
*		pfMisClass			;
*		pfRSquare			R��;
*		pfAdjRSquare		����R��;
*		pfBrierScore		BrierScore;
*		pfSomersD			Somers D;
*		pfTauA				Tau alpha;
*		pfGamma				Gamma;
*		pfHL				HLͳ����;
*		pfProbKS			����Ԥ����ʼ����ksָ��;
*		pfFractionKS		�����ۼ���������k�����ksָ��;
*	����ģ����������Ϣ������ͳ�Ʒ��������չ�֣�;
*		trainSampleId		ѵ������id;
*		modelBaseDate		ģ�ͻ�׼ʱ��;

%global PRED_MEMBERS;
%let PRED_MEMBERS=		predId groupId groupSeq tags desc
						genDate genMacro genParam inSampleId outSampleId modelId
						modelDate predDate
						root path
						lib self err use inSample outSample model result
						pfAUC pfAIC pfAICC pfBIC pfSC pfLogLike pfLogL pfMisClass pfRSquare pfAdjRSquare pfBrierScore pfProbKS pfFractionKS
						pfSomersD pfTauA pfGamma pfHL
;

* -- �������� --;
* vars = modelVars + textVars;
* modelVars = numVars + classVars;
* numVars = moneyVars + amountVars;
* classVars = binaryVars + enumVars;
* woeVars��ǿգ��������enumVarsһһ��Ӧ;
* logMoneyVars��ǿգ��������moneyVarsһһ��Ӧ;

* -- ����ṹ --;
* base.sas						info������������;
* sample.sas					�����ӹ�����;
* rawSample.sas					ԭʼ�����ӹ�����;
* model.sas						����ģ��ѵ������;
* pred.sas						����ָ����㺯��;

* -- base.sas --;

* info���������;
*	initInfoLib								��ʼ��info���lib;
*	getSampleInfo(root=,sample=)			�����ݿ��ȡһ��sample��Ϣ;
*	setSampleInfo(root=,sample=)			�����ݿ��ȡһ��sample��Ϣ;
*	getModelInfo(root=,model=)				�����ݿ��ȡһ��model��Ϣ;
*	setModelInfo(root=,model=)				�����ݿ��ȡһ��model��Ϣ;
*	getPredInfo(root=,pred=)				�����ݿ��ȡһ��pred��Ϣ;
*	setPredInfo(root=,pred=)				�����ݿ��ȡһ��pred��Ϣ;

* sample��������;
*	newSample(sample)						����һ����sample����;
*	createSample(root=,sampleId=,res=)		��������,��������·�����/������lib���롢��ʱlib�������ļ�������;
*	loadSample(root=,sampleId=,res=)			װ������;
*	saveSample(root=,sample=) 				�洢����;
*	dropSample(sample);

* model��������;
*	newModel								����һ����model����;
*	createModel								����model;
*	loadModel								װ��model;
*	saveModel								�洢model;

* pred��������;
*	newPred									����һ����pred����;
*	createPred								����pred;
*	loadPred								װ��pred;				
*	savePred								�洢pred;

* dinfo��������;
*	dataInfoToSampleInfo�������ã���ɾ����	��dataInfoת��ΪsampleInfo��ʽ;

* -- model.sas --;

* -- sample.sas --;

* -- pred.sas --;

* -- ģ����������˼· --;
* ģ������ʹ�ò㼶���ܵķ�ʽ��Ԥ�����ָ��->Ԥ�����->ģ�ͱ���->��ģ��ܱ���;
* ���ֵıȽϿ����ڲ�ͬ�㼶���У����߲�εı����ɵͲ�α��ֻ��ܶ���;

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
* ��Ҫ���и���info��Ĳ���������minfo��sinfo��dinfo;

* ---- function initInfoLib ----;
* ��ʼ��info lib��������ʱ����;
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
* ����sample info;
* input;
*	root	��·��;
*	info	�����sample info,sample������;
%macro setSampleInfo(root=,sample=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%if &root= or &sample= or %refExist(&sample.sampleId)=0 %then %error(setSampleInfo: Required param is empty! param=&syspbuff);
	%local infoLib;
	%initInfoLib(root=&root,res=&tres); %let infoLib=&&&tres;
	%setDsRecord(ds=&infoLib..sinfo,record=&sample,key=sampleId);
	%dropLib(&infoLib);
%mend;

* ---- function setSampleInfo ----;
* ����model info;
%macro setModelInfo(root=,model=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local infoLib;
	%if &root= or &model= or %refExist(&model.modelId)=0 %then %error(setModelInfo: Required param is empty! param=&syspbuff);
	%initInfoLib(root=&root,res=&tres);%let infoLib=&&&tres;
	%setDsRecord(ds=&infoLib..minfo,record=&model,key=modelId);
	%dropLib(&infoLib);
%mend;

* ---- function setPredInfo ----;
* ����pred info;
%macro setPredInfo(root=,pred=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local infoLib;
	%if &root= or &pred= or %refExist(&pred.predId)=0 %then %error(setPredInfo: Required param is empty! param=&syspbuff);
	%initInfoLib(root=&root,res=&tres);%let infoLib=&&&tres;
	%setDsRecord(ds=&infoLib..pinfo,record=&pred,key=predId);
	%dropLib(&infoLib);
%mend;

* ---- function getSampleInfo ----;
* ��ȡsample info;
* input;
*	root			��·��;
*	sampleId		����id;
*	res				����sample������;
* output;
*	����sample����;
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
* ��ȡmodel info;
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
* ��ȡpred info;
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
* ��������Ϣת��Ϊ������Ϣ����Ҫ��ɹؼ���Ϣ���ռ�;
* input;
*	data		datainfo���ݼ�;
*	info		���ص�sampleInfo��record����;
* ��ע;
* dinfoʹ��dict���в�������������ֶζ���string;


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
* �����µ�sample���������ڹ��캯����������������������������ĸ�ֵ;
* ʹ�÷����� %let sample=%createSample;
* input;
*	sample		������������Ϊ�գ�Ϊ��ʱ���������һ����������;
* output;
*	ֱ�ӷ����´�����������������;

%macro newSample(sample=,res=);
	%local tres;%let tres=%createTempVar;%local &tres;
	%global SAMPLE_MEMBERS;

	%recordNew(members=&SAMPLE_MEMBERS,record=&sample,res=&tres);%let sample=&&&tres;
	%let &sample.err=0;
	%let &sample.self=&sample;
	%let &res=&sample;
%mend;

* ---- function createSample ----;
* �����µ�sample,��Ҫ��ɶ�����س�ʼ�������������ڹ��캯������Ҫ��ʼ����������·��������lib����������ds���Ĵ���;
* input:
*	root;
*	sampleId;
*	res					sample�������Ƶ���������;
* output;
*	���ش�����sample��������;

%macro createSample(root=,sampleId=,res=) /parmbuff;
	* ��������;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local path lib sample;
	
	* �������;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= %then %error(createSample: ROOT is empty! param=&syspbuff);
	%if %refExist(&res)=0 %then %error(createSample: RES doesnot exist! param=&syspbuff);
	
	%if &sampleId= %then %let sampleId=%genId(prefix=S,len=20);
	* �ļ���������;
	
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
* ��Ҫ���sample�ĵ��빦�ܣ���Ҫ��������·����顢�����ļ���顢������ʱlib��������sinfo�����������Ϣ������ʱds��������;
* input;
*	root;
*	sampleId;
*	res					sample�������Ƶ���������;
* ʹ��ʾ��;
%*	%local tres;
%*	%let tres=%createTempVar;
%*	%loadSample(root=&root,sampleId=&sampleId,res=&tres);
%*	%let sample=&&&tres;
%* 	%let err=&&&sample.err;

%macro loadSample(root=,sampleId=,res=) /parmbuff;

	* ����׼��;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local lib path obs sample infoLib;

	* �������;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &sampleId= or not %refExist(&res) %then %error(loadSample: Required param is empty! param=&syspbuff);

	* ������Ϣ��ȡ;
	%newSample(res=&tres);%let sample=&&&tres;
	%let &sample.sampleId=&sampleId;
	%getSampleInfo(root=&root,sample=&sample);

	* ��������·�����;
	%let path=&root.sample\&sampleId.\;
	%if not %folderExist(&path) %then %error(loadSample: Target path does not exist! path=&path);

	* ����data�ļ����;
	%importTempLib(&path,&tres);%let lib=&&&tres;
	%if %dsExist(&lib..data)=0 %then %error(loadSample: Target sample data does not exist! path=&path);

	* �����۲���;
	%getDsObs(ds=&lib..data,res=&tres);%let obs=&&&tres;
	%if &obs<1 %then %error(loadSample: Target sample data has no record! path=&path);

	* ������ʱ��Ϣд��;
	%let &sample.root=&root;
	%let &sample.lib=&lib;
	%let &sample.path=&path;
	%let &sample.data=&lib..data;
	%let &sample.result=&lib..result;
	%let &sample.dinfo=&lib..dinfo;
	%let &sample.obs=&obs;

	* ����ֵд��;
	%let &res=&sample;
%mend;

* ---- function saveSample ----;
* ��Ҫ���sample��д�빦��;
* input;
*	root;
*	sample				sample������;
*	ͨ��sample��Ա���ݵĲ���;
*		sample.sampleId		id;

* output:
*	ͨ��sample.err��������״̬(��δ���ã������Ϊ%error);
* ʹ��˵��:;
*  �˰汾��saveSampleֻ���sinfoд�빤��;

%macro saveSample(root=,sample=) /parmbuff;
	%local macro;%let macro=%getSelf; 
	%local tres;%let tres=%createTempVar;%local &tres;
	* �������;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &sample= %then %error(saveSample: Required param is empty! param=&syspbuff);
	%if %dsExist(&&&sample.data)=0 %then %error(saveSample: The sample data does not exist! param=&syspbuff);
	%getDsObs(ds=&&&sample.data,res=&tres);%let &sample.obs=&&&tres;
	
	%local sumVars indexName;


	* �������ʹ�������;
	%varsOr(a=&&&sample.binaryVars,b=&&&sample.enumVars,res=&sample.classVars);
	%varsOr(a=&&&sample.amountVars,b=&&&sample.moneyVars,res=&sample.numVars);
	%varsOr(a=&&&sample.classVars,b=&&&sample.numVars,res=&sample.modelVars);
	%varsOr(a=&&&sample.modelVars,b=&&&sample.textVars,res=&sample.vars);

	* vars���Ա������;
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

	* ��������;
	%if &&&sample.key= %then %let &sample.key=SPIN;
	%varsCount(vars=&&&sample.key,res=&tres);
	%if &&&tres=1 %then %let indexName=&&&sample.key;
	%else %let indexName=I%hash(&&&sample.key);
	proc sql;
		create unique index &indexName on &&&sample.data (%sasVarsToSql(&&&sample.key));
	quit;

	* ������Ϣ�洢;
	%setSampleInfo(root=&root,sample=&sample);	
	%put &macro: Sample &&&sample.sampleId saved! obs=&&&sample.obs;
%mend;

* ---- function dropSample ----;
* ��Ҫ���sample���������Դ���ͷţ�������������;
* input;
*	sample				sample������;
%macro dropSample(sample) /parmbuff;
	%local macro;%let macro=%getSelf; 
	%local tres;%let tres=%createTempVar;%local &tres;

	* �������;
	%if &sample= %then %error(&macro: Required param is empty! param=&syspbuff);

	* ��Դ�ͷ�;
	%if %refExist(&sample.lib)=1 %then %dropLib(&&&sample.lib);
	%recordDrop(&sample);
%mend;

* ---- function deleteSample ----;
* ��Ҫ���sample���������Դ��ɾ��������sampleId�ļ�����sinfo����ؼ�¼;
* deleteSample��sampleIdȷ����ɾ�����ļ���Ϣ����sinfo���м�¼����ʱִ����Ϣ�޹�;
* input;
*	sampleId				sampleId;
* output:
*	ͨ��sample.err��������״̬(��δ���ã������Ϊ%error);
%macro deleteSample(root=,sampleId=) /parmbuff;
	%local macro;%let macro=%getSelf; 
	%local tres;%let tres=%createTempVar;%local &tres;
	%local path;

	* �������;
	%if &sampleId= %then %error(&macro: Required param is empty! param=&syspbuff);
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);

	* �ļ�ɾ��;
	%let path=&root.sample\&sampleId.\;
	%if %folderExist(&path)=1 %then %removeFolder(&path);

	* ��¼ɾ��;
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
*	model		ָ����model������;
*	res			���ض�����;

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
*	modelId			��Ϊ�ս��Զ���������ʽΪMD+����ַ�;
*	res;
%macro createModel(root=,modelId=,res=) /parmbuff;
	* ����׼��;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local path lib model;
	* �������;
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
	* ����׼��;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local lib path model;

	* �������;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &modelId= or not %refExist(&res) %then %error(loadModel: Required param is empty! param=&syspbuff);
	
	* ��Ϣ��ȡ;
	%newModel(res=&tres);%let model=&&&tres;
	%let &model.modelId=&modelId;
	%getmodelInfo(root=&root,model=&model);

	* ģ��·�����;
	%let path=&root.model\&modelId.\;
	%if not %folderExist(&path) %then %error(loadModel: Target path does not exist! path=&path);

	* ģ���ļ����;
	%importTempLib(&path,&tres);%let lib=&&&tres;
	%if not %dsExist(&lib..model) %then %error(loadModel: Target model does not exist! path=&path);
	
	* ��ʱִ����Ϣд��;
	%let &model.root=&root;
	%let &model.path=&path;
	%let &model.lib=&lib;
	%let &model.model=&lib..model;
	%let &model.result=&lib..result;

	* ����ֵд��;
	%let &res=&model;
%mend;

* ---- function saveModel ----;

%macro saveModel(root=,model=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;

	* �������;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &model= %then %error(saveModel: Required param is empty! param=&syspbuff);

	* ������Ϣ�洢;
	%setModelInfo(root=&root,model=&model);	
%mend;

* ---- function dropModel ----;

%macro dropModel(model) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;

	* �������;
	%if &model= %then %error(dropModel: Required param is empty! param=&syspbuff);

	* ��Դ�ͷ�;
	%if %refExist(&model.lib)=1 %then %dropLib(&&&model.lib);
	%recordDrop(&model);
%mend;



* -----------------------------;
* ----- Pred management -----;
* -----------------------------;

* Pred�Զ�����outSampleʱ��outSample��Ա�趨����;
* sample�����Ա;
*	������Ϣ;
*		sampleId			�����޸ģ�creatPredʱ����;
*		groupId				���޸�;
*		groupSeq			���޸�;
*		tags				���޸�;
*		desc				���޸�;
*	������Ϣ;
*		genDate				�����޸ģ�ȡ��pred��Ӧֵ��savePredʱ����;
*		genMacro			�����޸ģ�ȡ��pred��Ӧֵ��savePredʱ����;
*		genParam			�����޸ģ�ȡ��pred��Ӧֵ��savePredʱ����;
*		inSampleIds			�����޸ģ�ȡ��inSampleId��savePredʱ����;
*		baseDate			�����޸ģ�ȡ��inSample��Ӧֵ��savePredʱ����;
*	������Ϣ;
*		vars				�����޸ģ�savePredʱ�������ɣ�savePredʱ����;
*		modelVars			�����޸ģ�savePredʱ���ɣ�savePredʱ����;
*		textVars			���޸ģ�Ĭ��ΪinSample�Ķ�Ӧֵ��savePredʱ����;
*		classVars			�����޸ģ�savePredʱ�������ɣ�savePredʱ����;
*		numVars				�����޸ģ�savePredʱ�������ɣ�savePredʱ����;
*		binaryVars			���޸ģ�Ĭ��ΪinSample�Ķ�Ӧֵ��savePredʱ����;
*		enumVars			���޸ģ�Ĭ��ΪinSample�Ķ�Ӧֵ��savePredʱ����;
*		moneyVars			���޸ģ�Ĭ��ΪinSample�Ķ�Ӧֵ��savePredʱ����;
*		amountVars			���޸ģ�Ĭ��ΪinSample�Ķ�Ӧֵ��savePredʱ����;
*		logMoneyVars		�����޸ģ�inSample�Ķ�Ӧֵ��savePredʱ����;
*		woeEnumVars			�����޸ģ�inSample�Ķ�Ӧֵ��savePredʱ����;
*	����ȱʧֵ;
*		defaultValue		���޸ģ�Ĭ��ΪinSample�Ķ�Ӧֵ;
*	�ļ���Ϣ;
*		root				�����޸ģ�creatPredʱ����;
*		path				�����޸ģ�creatPredʱ����;
*		obs					�����޸ģ�creatPredʱ����;
*		key					�����޸ģ�ȡ��inSample��Ӧֵ��savePredʱ����;
*	��ʱִ����Ϣ;
*		lib					�����޸ģ�creatPredʱ����;
*		dinfo				�����޸ģ�creatPredʱ����;
*		data				�����޸ģ�creatPredʱ����;
*		self				�����޸ģ�creatPredʱ����;
*		err					�����޸ģ�creatPredʱ����;
;

* �������͡�Ĭ��ֵ���ù���;
*	����������Ĭ��ֵ��ʼΪ�գ��������ֹ����ã�������savePredʱ����Ϊ�գ���ʹ��inSample�Ķ�Ӧֵ;
*	
* ---- function newPred ----;
* input;
*	pred		ָ����pred������;
*	res			���ض�����;

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
*	predId			ѡ�䣬Ϊ��ʱ���Զ�����;
*	inSampleId		��������id;
*	inSample		������������inSample��inSampleId���߱�����һ;
*	modelId			ģ��id;
*	model			ģ�Ͷ���model��modelId���߱�����һ;
*	outSampleId		�������id��ѡ�䣬Ϊ��ʱ���Զ�����;
*	use				��;;
*	res				���ش�����pred����;
%macro createPred(root=,predId=,modelId=,model=,inSampleId=,inSample=,outSampleId=,use=,res=) /parmbuff;

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local path lib pred outSample;

	* �������;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &inSampleId= and &inSample= %then %error(&macro: No input sample! param=&syspbuff);
	%if &modelId= and &model= %then %error(&macro: No input model! param=&syspbuff);
	%if &predId= and %refExist(&res)=0 %then %error(&macro: No return method! param=&syspbuff); 
	%if &predId= %then %let predId=%genId(prefix=P,len=20);
	%if &use= %then %let use=perf;
	
	* ������Ϣ��ʼ��;
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

	* ����ģ��;
	%if &model ne %then %let &pred.model=&model;
	%else %loadModel(modelId=&modelId,res=&pred.model);
	%let model=&&&pred.model;
	%let &pred.modelId=&&&model.modelId;

	* ��������;
	%if &inSample ne %then %let &pred.inSample=&inSample;
	%else %loadSample(sampleId=&inSampleId,res=&pred.inSample);
	%let inSample=&&&pred.inSample;
	%let &pred.inSampleId=&&&inSample.sampleId;
	
	* �����������;
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

	* �������;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &predId= or &res= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if not %symexist(&res) %then %error(&macro: RES is empty! param=&syspbuff);
	%if &use= %then %let use=perf;

	* pred��Ϣ��ȡ;
	%newPred(res=&tres);%let pred=&&&tres;
	%let &pred.predId=&predId;
	%getPredInfo(root=&root,pred=&pred);
	
	* pred��ʱִ����Ϣд��;
	%let path=&root.pred\&predId.\;
	%if not %folderExist(&path) %then %error(loadPred: Target path does not exist! path=&path);
	%importTempLib(&path,&tres);%let lib=&&&tres;
	%let &pred.root=&root;
	%let &pred.path=&path;
	%let &pred.lib=&lib;
	%let &pred.result=&lib..result;
	%let &pred.use=&use;

	* sample��model��ȡ;
	%if &&&pred.inSampleId= or &&&pred.outSampleId= or &&&pred.modelId= %then %error(&macro: Target pred info is not valid! inSampleId=&&&pred.inSampleId outSampleId=&&&pred.outSampleId modelId=&&&pred.modelId);
	%loadSample(sampleId=&&&pred.inSampleId,res=&pred.inSample);
	%loadSample(sampleId=&&&pred.outSampleId,res=&pred.outSample);
	%loadModel(modelId=&&&pred.modelId,res=&pred.model);

	* ����ֵд��;
	%let &res=&pred;
%mend;

* ---- function savePred ----;

%macro savePred(root=,pred=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	
	%local outSample inSample model;

	* �������;
	%let root=%ifEmptyThenGlobal(&root,ROOT_PATH);
	%if &root= or &pred= %then %error(&macro: Required param is empty! param=&syspbuff);

	%if %upcase(&&&pred.use)=%upcase(pred) %then %do;
		%let model=&&&pred.model;
		%let &pred.modelDate=&&&model.baseDate;
		%let inSample=&&&pred.inSample;
		%let &pred.predDate=&&&inSample.baseDate;
	%end;
	* ������Ϣ�洢;
	%setPredInfo(root=&root,pred=&pred);	

	* ��������洢;
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

	* �������;
	%if &pred= %then %error(dropPred: Required param is empty! param=&syspbuff);

	* ��Դ�ͷ�;
	%if %refExist(&pred.inSample)=1 %then %dropSample(&&&pred.inSample);
	%if %refExist(&pred.outSample)=1 %then %dropSample(&&&pred.outSample);
	%if %refExist(&pred.model)=1 %then %dropModel(&&&pred.model);
	%if %refExist(&pred.lib)=1 %then %dropLib(&&&pred.lib);
	%recordDrop(&pred);
%mend;
