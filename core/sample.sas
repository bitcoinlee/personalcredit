* ---------------------;
* ---- ������������ ----;
* ---------------------;

* ---- ������� ----;
* �������ɵ�һ�������봦������;
*	start
*	--���������أ�-->level0����;
*	--��������ϴ��-->level1����;
*	--(�趨ʱ��㣬ָ�����)-->rawSample				%genRawSample	���ָ�������;
*	--(��¼����ɸѡ�����)-->filteredSample			%filterSample 	ͨ����������Աid��������ά�ȶԱ������н�һ��ɸѡ�����;
*	--(woeת����logת����-->sample��level2���ݣ�		%genSample		����woe��logת���ȵ�����;


* ---- ���������ļ����� ----;
* ���������ļ�����rawSample��filteredSample��sample��������;
* ���������ļ���				��[sampleId]_data��ʽ�����ı�;
*							����rawSample��sampleId=rawSampleId;
*							����filteredSample,sampleId��filter���裬��ʽ����ΪrawSampleId_filterId_outId��������ʽ;
* ���������ļ�sinfo			��[sampleId]_sinfo���Ʊ�������������ͬ�ļ���;
* �������ݱ�����������dinfo	��{sampleId]_dinfo���Ʊ�������������ͬ�ļ���;
* 
* dinfo,sinfo����dict���ݽṹ;

* ---- ������������ dinfo ----;
* ��������������Ϣ�����ݱ�;
* ��ʽ���£�
* �����ࣺ;
*	sampleId		��������;
* 	var				��������;
*	label			����label;
*	format			������ʽ����;
*	isSasNum		�Ƿ���sas��ֵ����;
*	isSasString		�Ƿ���sas�ַ�����;
*	isClass			�Ƿ��Ƿ������;
*	isMoney			�Ƿ��ǽ������;
*	isWoe			�Ƿ���woe����;
*	woeFmtLib		����woeת����ʹ�õ�format lib����;

* ---- ������������ sinfo ----;
* �����������̵����ݼ�;
* ��ʽ���£�
* ������Ϣ:
*	varList			���������б�;
*   sampleType		�����������ͣ�������raw filtered final;
*	sampleId		����id;

* RawSample�����Ϣ��;
* 	rawSampleId		����id;
*	rawSampleCode	rawSample�����������;
*	rawSampleDate	rasSample����ʱ��;
* 	rawSampleDesc	rasSample��������;
*	baseDate		��׼����;
*	dur1			�۲��ڳ��ȣ���Ϊ��λ;
*	dur2			�����ڳ��ȣ���Ϊ��λ;

* %filterSample������;
*	filterId		����id;
* 	filterCode		���˺�����������;
*	filterDate		����ʱ��;
*	filterDesc		���˷�ʽ��������������������׶Σ�����׶ε����ݴ���������ҿ��ܴ���rawSample�Ĳ�֣����ֶ�������������ķ���;

* %genSample������;
*	allClassToWoe	����class����ת��Ϊwoe��ʽ;		
*	allMoneyToLog	���н������ת��Ϊlog��ʽ;
*	allDurToMonth	����ʱ��������ת��Ϊ�¶ȵ�λ;
*	filterDesc		���˵�������Ϣ;

* --- macro %genRawSample_? ----;
* ���ɷ���Ҫ���raw sample���ݣ�%genRawSample��һϵ���������ɳ����ͳ�ƣ�ͳһ��%genRawSample_[rsCode]��ʽ����;
* input;
* 	date			��׼ʱ�䣬�۲��ڽ���ʱ��/�����ڿ�ʼʱ��;
*	dur1			�۲��ڳ��ȣ����¶ȼ���;
*	dur2			�����ڳ��ȣ����¶ȼ���;
*	varList			�����ָ���б�;
*	outLib			��������ݱ�λ��;
*	sampleId		����id;
* output:;
*	rawSample		raw��������;
*	dinfo			dinfo��;
*	sinfo			sinfo��;

* --- macro %filterSample_? ----;
* ������������ֳ�����Ҫ����������¼ά�ȵĲ��,����cross-validation�ȳ���;
* ���ɷ���Ҫ���filteredSample���ݣ� %filterSample��һϵ���������������ͳ�ƣ�ͳһ��%filterSample_[filterCode]��ʽ����;
* input;
*	inLib			�����lib;
*	inSampleId		�����sampleId;
*	outLib			�����lib;
*	outSampleId		�����sampleId;	
* output:;
*	filteredSample	���������������;
*	dinfo			dinfo��;
*	sinfo			sinfo��;

* --- macro %genSample ----;
* ���ɷ���Ҫ���sample����;
* input;
*	data			��������;
*	dinfo			dinfo��;
*	sinfo			sinfo��;
* output:;

* ------------------------------------;
* ---- ָ������ࣨrawSample���ɣ� ----;
* ------------------------------------;

* ---- ������������ sinfo ----;
* �����������̵����ݼ�;
* ��ʽ���£�
* ������Ϣ:
*	varList			���������б�;
*   sampleType		�����������ͣ�������raw filtered final;
*	sampleId		����id;

* RawSample�����Ϣ��;
* 	rawSampleId		����id;
*	rawSampleCode	rawSample�����������;
*	rawSampleDate	rasSample����ʱ��;
* 	rawSampleDesc	rasSample��������;
*	baseDate		��׼����;
*	dur1			�۲��ڳ��ȣ���Ϊ��λ;
*	dur2			�����ڳ��ȣ���Ϊ��λ;

* ----------------------------------------;
* ---- ���������ࣨfilteredSample���ɣ�----;
* ----------------------------------------;

* ---- randomGroup ----;
* ������麯��,��Ҫ����k-flod cv����;;
* ��ȫ�����������������ÿ��������������ͬ;
* Input:;
*	data		ԭʼ���ݣ�����;
*	out			����Ĳ������ݣ�����;
*	k			��������;
* Output:;
*	�����ļ�		1���ļ�����ͬ��������groupVar����;
*;

* ---- unitGroup ----;
* �Ա�����unit�ۺϽ��з���;
* �������Ҫ������unit���������varList������������k��;
* �������Ҫ�߼��ǣ��Ȼ���obs��������Ԫ���ٸ��ݴ�С����ۺϵĹ��򽫵�Ԫ���䵽��ͬ���У�����յ�Ԫ��۲�Ĺ�ϵ�����۲���䵽����;
* Input:;
*	data			ԭ����;
*	out				�������;
*	varList			��Ԫ��������;
*	k				�������ĸ�����Ĭ��Ϊ10;
*	lowerBound		���������ʱ��obs�������������ǰ��������obs��������Ŀ��ֵ*lowerBound����ֹͣ��䣬Ĭ��Ϊ0.9��90%;
*					����ʱ���Ȱ�obs������С�����unit����Ȼ���ۼ�obs�������з��飬���ڱ�Ե��unit��Ĭ�Ϸֵ�ǰ�棨��С��������;
*					������unit�Ĵ��ڣ�����ÿ����Ĺ۲�������Ȼ������ȫ��ͬ���������յķ�������Ҳ���ܴﲻ��Ŀ�������;
* 	groupVar		�������������;	

* ---- simpleSample ----;
* �����꣬��Ҫ���ڷ�װproc surveyselect��������ʽ��proc surveyselect��ͬ;
* Input:;
*	data			��Ӧoption data;
*	out				��Ӧoption out;
*	where			ԭʼ���ݵĹ�������;
*	method			��Ӧoption method;
*	n				��Ӧoption n;
*	strata			��Ӧstatement strata;
*	reps			��Ӧoption reps;
* ���⹦�ܣ�;
*	method=simple	�������������nС����������ʱʹ��srs��������n��������ʱ���ȳ�ȡ������������ֵ����ʹ��urs��������;
*					ע��ʹ�����ֲ�����ʽʱ��������strata;

* ---- balancedSample ----;
* ƽ�������ʽ���趨bad/good�ı������г���;
* �����趨��response�������г���;
* �����߼�������ָ����ratio���в���;
* Input:;
*	data		ԭʼ���ݣ�����;
*	out			����Ĳ������ݣ�����;
*	num			�ܲ���obs����������;
*	varRsp		response�������ƣ�����;
*	ratio		����Ŀ��event/nonevent����������;
*	event		�¼�����ֵ������;
* 	force		ǿ�Ʋ�������ѡֵ0��1��2��Ĭ��Ϊ0������ָ������������������������������Ҫ��ʱ�Ĵ�����;
*		0				ֹͣ��Ĭ��ֵ;
*		1				ʹ�÷Żز���;
*		2				����������������;

* -------------------------------------------------;
* ---- �������ռӹ������ࣨsample/level�������ɣ�----;
* -------------------------------------------------;

* ---- macro %genWOEFormat ----;
* ���ݱ�����������WOEת����informat;
* ����oddΪ0�����������������͵�ӳ��ֵΪȱʧֵ.��ͬʱ�ڼ��������oddʱ�������͵�����������ᱻ�޳�;
* input;
* 	data			�������ݼ�;
*	var				ת��������;
*	varRsp			response��������;
*	formatName		ָ�����ڱ������woeת����ʽ�����ƣ�Ĭ��Ϊ��������_woe;
*	formatLib		ָ�����ڱ������woeת����ʽ�ļ���formatlib,Ĭ��Ϊ����locallib·��;
* output;
*	format�ļ�		������formatLibָ����lib��;

* ---- macro %trans_woe ----;
* ��ָ������ת��ΪWOE��ʽ��ת����ı�������Ϊ��woe_ԭ������;
* formatת���������½�format��ת�����롰ʹ������formatת��������ģʽ;
* ǰ��ת�����ȵ���%genWOEFormat����format����ʹ��formatת�������߻�ʹ������format����ת��;
* input;
* 	data			�������ݼ�;
*	out				������ݼ���Ϊ��ʱ��data��ͬ;
*	varRsp			response��������;
*	inVarList		ָ����Ҫת��Ϊwoe��ʽ�ı������б�;
*	inFormatLib		ָ������ת����formatlib���˲�����Ϊ��ʱ����Ϊ��ʹ������formatת��;
*	outVarListRef	ָ������ת����ı������б�ĺ������;
*	outFormatLib	ָ�����ڱ������woeת����ʽ�ļ���formatlib;


* ------------------------;
* ---- ͨ������������ ----;
* ------------------------;

* ---- function mergeSample ----;
* �����ϲ���������spin�ϲ�����;
* �ϲ�����ȱʧֵ�������/������;

* input;
*	inSampleIds		���ϲ�����id�б�;
*	outSampleId		�ϲ��������id;
*	baseSampleId	�ϲ���������id;
*	baseData		�ϲ���������;
*	sameBaseDate	�Ƿ�Ҫ��Ϊ��ͬ�Ļ�׼ʱ��,��ѡ��0|1;
*	type			�ϲ���ʽ,��ѡ�����;
*						inner 		innerjoin���۲������������ȱʧ�����޳���ob;
*						left 		leftjoin���ںϲ������У���inSampleIds�еĵ�һ��ds��Ϊ��׼������ȱʧ�����ʹ��Ĭ��ֵ;
*						base 		leftjoin��ʹ��baseSampleId��baseData�е�spin��Ϊ��׼������ȱʧ�����ʹ��Ĭ��ֵ;
* 	force			�Ƿ�ǿ�ƺϲ������ϲ���ĳ��sampleId������ʱ���Ƿ�ϲ������ı�򱨴�,Ĭ��Ϊ0;
*	res				�ϲ��������id�������;

* details;
* 	ȱʧֵ����;
*			������ȱʧֵ��mergeSampleҪ����ϲ������ڲ��ó���ȱʧֵ;
*			�ϲ�ȱʧֵ�����г���ĳ��keyֵ��Ӧ������ȱʧ���������ֺϲ�/��������ͨ��typeָ��;
* 	��׼ʱ������;
*			mergeSampleҪ�����б��ϲ�������������ͬ��baseDate;
*;
* TODO KEY���;
%macro mergeSample(inSampleIds=,outSampleId=,baseSampleId=,baseData=,type=,sameBaseDate=,force=,res=) /parmbuff;
 	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%put &macro: Start! param=&syspbuff;

	%if &inSampleIds= or %refExist(&res)=0 %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &sameBaseDate= %then %let sameBaseDate=1;
	%if &force= %then %let force=0;

	%local i exist;
	%local sample samples;
	%local sampleId realInSampleIds;
	%local newDefaultValue;
	%local sampleData;
	%local baseSample tableData;

	%let sampleData=%str();
	%let samples=%str();

	%createSample(sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
	%let &outSample.genMacro=mergeSample;
	%let &outSample.genParam=&syspbuff;
	%let &outSample.genDate=%timestamp;
	%let &outSample.desc=Merged sample;
	
	%local arrayVars;%let arrayVars=%arrayVars(name=sampleIds,values=&inSampleIds);
	%local &arrayVars;%array(sampleIds,values=&inSampleIds);
	%let newDefaultValue=%str();
	%let realInSampleIds=%str();
	%do i=1 %to &sampleIdsN;
		%let sampleId=&&sampleIds&i;
		%sampleExist(sampleId=&sampleId,res=&tres);%let exist=&&&tres;
		%if &exist=0 %then %put &macro: Sample does not exist! sampleId=&sampleId;
		%if &exist=0 and &force=1 %then %goto CONTINUE;
		%if &exist=0 and &force=0 %then %error(&macro: Sample doesnot exist! sampleId=&sampleId); 
		%loadSample(sampleId=&sampleId,res=&tres);%let sample=&&&tres;
		%put &macro: Load sample &&&sample.sampleId obs=&&&sample.obs;

		* ���������������ļ��嵥;
		%let sampleData=&sampleData &&&sample.data;
		%let samples=&samples &sample;
		%let realInSampleIds=&realInSampleIds &sampleId;
		* �����ϲ�;
		%varsOr(a=&&&outSample.textVars,b=&&&sample.textVars,res=&outSample.textVars);
		%varsOr(a=&&&outSample.enumVars,b=&&&sample.enumVars,res=&outSample.enumVars);
		%varsOr(a=&&&outSample.binaryVars,b=&&&sample.binaryVars,res=&outSample.binaryVars);
		%varsOr(a=&&&outSample.amountVars,b=&&&sample.amountVars,res=&outSample.amountVars);
		%varsOr(a=&&&outSample.moneyVars,b=&&&sample.moneyVars,res=&outSample.moneyVars);
		%varsOr(a=&&&outSample.logMoneyVars,b=&&&sample.logMoneyVars,res=&outSample.logMoneyVars);
		%varsOr(a=&&&outSample.woeEnumVars,b=&&&sample.woeEnumVars,res=&outSample.woeEnumVars);

		* Ĭ��ֵ�ϲ�;
		%let newDefaultValue=%str(&newDefaultValue &&&sample.defaultValue);
	
		* ������׼���ڱȽ�;
		%if &i=1 %then %let &outSample.baseDate=&&&sample.baseDate;
		%if &sameBaseDate=1 %then %do;	
			%if &&&sample.baseDate^=&&&outSample.baseDate %then %error(&macro: All samples must have the same baseDate !);
		%end;
		%else %do;
			%let &outSample.baseDate=%dsLatest(&&&sample.baseDate,&&&outSample.baseDate);
		%end;
		%CONTINUE:
	%end;

	%let &outSample.defaultValue=%str(&newDefaultValue);
	%let &outSample.inSampleIds=&realInSampleIds;

	%put &macro: Samples to be merged: &realInSampleIds;

	* ���ݺϲ�;
	%if %upcase(&type)=%upcase(inner) %then %do;
		%innerJoin(table=&sampleData,key=spin,out=&&&outSample.data);
	%end;
	%else %do;
		* ���ݺϲ�;
		%if %upcase(&type)=%upcase(left) %then %do;
			%varsFirst(vars=&sampleData,res=&tres);%let baseData=&&&tres;
			%varsSub(a=&sampleData,b=&baseData,res=&tres);%let tableData=&&&tres;
		%end;
		%else %if %upcase(&type)=%upcase(base) %then %do;
			%if &baseData ne %then %let baseData=&baseData;
			%else %if &baseSampleId ne %then %do;
				%loadSample(sampleId=&baseSampleId,res=&tres);%let baseSample=&&&tres;
				%let baseData=&&&baseSample.data;
			%end;
			%else %error(&macro: Cannot find base data!);
			%let tableData=&sampleData;
		%end;
		%else %error(&macro: Unsupported merge type! type=&type);
		%leftjoin(base=&baseData,table=&tableData,defaultValue=%str(&&&outSample.defaultValue),key=spin,out=&&&outSample.data);
	%end;

	* �����洢;	
	%saveSample(sample=&outSample);
	%if %refExist(&res)=1 %then %let &res=&&&outSample.sampleId;

	* ��������;
	%do_over(values=&samples,macro=dropSample);
	%if &baseSample ne %then %dropSample(&baseSample);
	%dropSample(&outSample);
%mend;

* ---- function filterSample ----;
* ���������й��ˣ�����keyֵ�ĺ�����/���������ˣ��Լ�where�����Ĺ���;
* input;
*	inSampleId			���ϲ�����id;
*	outSampleId			�������id;
*	filterSampleId		����������id����where������ͬʱΪ��;
* 	isBlackList			keyֵ�Ƿ��Ǻ�������Ĭ��Ϊ��;
*	where				where��������,ע��where���������������ǰ���������;
* 	res					���ر�����;
* output;
*	res					�޳��ļ�¼����;

%macro filterSample(inSampleId=,outSampleId=,filterSampleId=,isBlacklist=,where=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	* ��������;
	%local inSample outSample filterSample;

	* �������;
	%if &inSampleId= or &filterSampleId= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &outSampleId= %then %let outSampleId=&inSampleId;
	%if &isBlacklist= %then %let isBlacklist=1;

	* ��������;
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	%if &inSampleId^=&outSampleId %then %do;
		%createSample(sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
		%recordCopy(from=&inSample,to=&outSample,overwrite=0);	
	%end;
	%loadSample(sampleId=&filterSampleId,res=&tres);%let filterSample=&&&tres;

	%let &outSample.genMacro=&macro;
	%let &outSample.genParam=&syspbuff;
	%let &outSample.genDate=%timestamp;
	%let &outSample.desc=Filterred sample;
	%let &outSample.inSampleIds=&inSampleId;
	
	%if %upcase(&&&inSample.key)^=%upcase(&&&filterSample.key) %then %error(&macro: The index must matched!);

	%filter2(data=&&&inSample.data,filter=&&&filterSample.data,out=&&&outSample.data,indexName=&&&filterSample.key,where=&where,isBlacklist=&isBlacklist);

	%saveSample(sample=&outSample);

	* ��������;
	%dropSample(&inSample);
	%dropSample(&filterSample);
	%if &inSampleId^=&outSampleId %then %dropSample(&outSample);
%mend;

* ---------------------------------------------------;
* --------------------- ���������� -------------------;
* ---------------------------------------------------;

* ---- macro %transLog ----;
* ��ָ������ת��Ϊlog��ʽ��ת����ı�������Ϊ��LOG_ԭ������;
* input;
* 	data			�������ݼ�;
*	out				������ݼ���Ϊ��ʱ��data��ͬ;
*	vars			ָ����Ҫת��Ϊlog��ʽ�ı������б�;
*	res				ָ������ת����ı������б�ĺ������;
*	type			ת�����ͣ���Ҫ���ڸ��ݱ������ͽ���ת����������ѡ����������ʽ���£�;
*		��ֵ			Ĭ�ϣ���0ӳ�䵽missing value������Ϊlog;
*		money		����[0,1]ӳ�䵽0���Ա���0���ʱ�޷����������;

%macro transLog(data=,out=,vars=,type=,res=);
	%local var arrayVars;
	%let arrayVars=%arrayVars(name=varArray,values=&vars);
	%local &arrayVars;
	%array(varArray,values=&vars);
	%if &type=money %then %do;
		data &out;
			set &data;
			%do i=1 %to &varArrayN;
				%let var=&&varArray&i;
				if &var<=1 then LOG_&var=0;
				else LOG_&var=log(&var);
			%end;
		run;
	%end;
	%else %do;
		data &out;
			set &data;
			%do i=1 %to &varArrayN;
				%let var=&&varArray&i;
				if &var<=0 then LOG_&var=.;
				else LOG_&var=log(&var);
			%end;
		run;
	%end;
	%let &res=%upcase(%do_over(values=&vars,phrase=LOG_?));
%mend;


* ---- function gsLogMoney ----;
* ���������д�������money��������logMoney����;
* logMoneyת������С��0��ֵ������ȱʧֵ;
* gsLogMoneyͬʱ��ԭ��ֵ��defaultValue����logMoneyVars��Ĭ��ֵ;
* input;
*	inSampleId;
*	outSampleId			;
*	res					������룬���䣬��ѡ�����;
*						0	����;
*						1	��moneyVars;

%macro gsLogMoney(inSampleId=,outSampleId=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local outSample inSample;
	%local var value  logVar logValue newDefaultValue;

	%put &macro: Start! param=&syspbuff;

	%if &inSampleId= or %refExist(&res)=0 %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &outSampleId= %then %let outSampleId=&inSampleId;

	%let &res=0;
	
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;

	%if &&&inSample.moneyVars= %then %do;
		%dropSample(&inSample);
		%let &res=1;
		%return;
	%end;

	%if &inSampleId^=&outSampleId %then %do;
		%createSample(sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
		%recordCopy(from=&inSample,to=&outSample,overwrite=0);
	%end;
	%else %let outSample=&inSample;

	%let &outSample.genMacro=&macro;
	%let &outSample.genParam=&syspbuff;
	%let &outSample.genDate=%timestamp;
	%let &outSample.desc=add log money vars;
	%let &outSample.inSampleIds=&inSampleId;

	%transLog(data=&&&inSample.data,out=&&&outSample.data,vars=&&&inSample.moneyVars,type=money,res=&outSample.logMoneyVars);

	%let vars=&&&inSample.moneyVars;

	* ������Ĭ��ֵ;
	
	%local arrayVars;%let arrayVars=%arrayVars(name=varList,values=&vars);
	%local &arrayVars;%array(varList,values=&vars);
	%let newDefaultValue=%str();

	%do i=1 %to &varListN;
		%let var=&&varList&i;
		%let logVar=LOG_&var;
		%assignsFind(assigns=%str(&&&inSample.defaultValue),var=&var,res=&tres);%let value=%str(&&&tres);
		%if %length(&value)^=0 %then %do;
			%if &value>=1 %then %let logValue=%log(&value);   
			%else %if &value>0 %then %let logValue=0;   
			%let newDefaultValue=%str(&newDefaultValue &logVar=&logValue);
		%end;
	%end;

	%let &outSample.defaultValue=%str(&&&inSample.defaultValue &newDefaultValue);
	%varsOr(a=&&&inSample.amountVars,b=&&&outSample.logMoneyVars,res=&outSample.amountVars);

	%saveSample(sample=&outSample);
	%dropSample(&inSample);
	%if &inSampleId^=&outSampleId %then %dropSample(&outSample);
%mend;

* ---- macro %genWOEFormat ----;
* ���ݱ�����������WOEת����informat;
* ����oddΪ0�����������������͵�ӳ��ֵΪȱʧֵ.��ͬʱ�ڼ��������oddʱ�������͵�����������ᱻ�޳�;
* input;
* 	data			�������ݼ�;
*	var				ת��������;
*	varRsp			response��������;
*	format		ָ�����ڱ������woeת����ʽ�����ƣ�Ĭ��Ϊ��������_WOE������Ҫ��.;
*	formatLib		ָ�����ڱ������woeת����ʽ�ļ���formatlib,Ĭ��Ϊ����locallib·��;
* output;
*	format�ļ�		������formatLibָ����lib��;

%macro genWOEFormat(data=,var=,varRsp=,format=,formatLib=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put Start genWOEFormat! &syspbuff; 

	%local sorted freq odd cntl;
	%local totalEvent totalNonEvent;
	%local varType;

	* �������;
	%if &data= or &var= or &varRsp= or &formatLib= %then %error(genWOEFormat: Required param is empty! param=&syspbuff);
	%if &format= %then %let format=&var._WOE;
	
	%let sorted=%createTempDs;
	%let freq=%createTempDs;
	%let odd=%createTempDs;
	%let cntl=%createTempDs;

	proc sort data=&data out=&sorted;
		by &var &varRsp;
	run;
	proc freq data=&sorted noprint;
		tables &var*&varRsp /out=&freq;
	run;
	proc sort data=&freq;
		by &var &varRsp;
	run;
	data &odd;
		set &freq;
		by &var &varRsp;
		keep &var odd;
		retain countEvent 0 countNonEvent 0 totalEvent 0 totalNonEvent 0;
		if first.&var and &varRsp=0 then do;
			countNonEvent=count;
		end;
		if last.&var and &varRsp=1 then do;
			countEvent=count;
		end;
		if last.&var then do;
			if countNonEvent>0 and countEvent>0 then do;
				odd=countEvent/countNonEvent;
				totalNonEvent=totalNonEvent+countNonEvent;
				totalEvent=totalEvent+countEvent;
			end;
			else do;
				odd=.;
			end;
			countEvent=0;
			countNonEvent=0;
			output;
		end;
		call symput("totalEvent",totalEvent);
		call symput("totalNonEvent",totalNonEvent);
	run;

	%getDsVarType(ds=&data,var=&var,res=&tres);%let varType=&&&tres;
	%if &varType^=2 and &varType^=1 %then %error(&macro: Var type error! var=&var);
	
	data &cntl;
		set &odd;
		keep fmtname start label type;
		type="I";
		fmtname="&format";
/*		
		%if &varType=2 %then %do;* �ַ�->����,����2;	
			fmtname="&format";
		%end;
		%else %do;* ����->���֣�����4;
			fmtname="&format";
		%end;
		*/
		start=&var;
		label=log((odd)/(&totalEvent/&totalNonEvent));
		
	run;
	proc format cntlin=&cntl library=&formatLib;
	run;

	%dropDs(&sorted);
	%dropDs(&freq);
	%dropDs(&odd);
	%dropDs(&cntl);
%mend;

* ---- macro %gsWoe ----;
* ��enum���������woeת��;
* input;
* 	inSampleId			���䣬�������ݼ�;
*	outSampleId			ѡ�䣬������ݼ���Ĭ��ΪinSampleId;
*	woeSampleId			ѡ�䣬ָ��woeformat������������Ĭ��ΪinSampleId;
*	varRsp				���䣬response var���ñ�����woeSample�б������;

* WOEת���������������;
*	gsWoe���Զ��������ڡ����С���enumVars����woeת��;
*	ת����������������ʽΪ:WOE_[varName]��woe��������Ϊamount��Ĭ��ֵΪת��ǰֵ�Ķ�Ӧת��ֵ����ԭʼֵû��Ĭ��ֵ����WOE�����Ҳû��Ĭ��ֵ;
* WOE Format��ʹ��;
*	gsWoe��ʹ�õ�woeFormatͨ��woeSampleId��ȷ����woeSample�ļ��е�formats�ļ�Ӧ������Ӧ�ĸ�ʽ��Ϣ;
*	woeת����format����ͳһΪ[varName]_WOE�����������βΪ���ֵ����������Ϊinformat;
* WOE Format�����Լ�����Զ���������;
*	1�����inSample��woeSample�Ƿ���ͬ�������ͬ����ΪWOE Format�����ڣ���������;
*	2����inSample��woeSample��ͬ�������Ӧ��woeSample�ļ������Ƿ���������С���Ҫ��WOE Format���硾���С���ʽ��������ֱ��ʹ��;
*	3������ڸ�ʽ�ļ���ȱʧ�������Ӧ��woeSample���Ƿ���С����С������������������С�������������ڣ������´��������С�WOE Format;
*	4�������������ָinSample�е�����enumVars��ָ����varRsp;
*	5������������д���ȱʧ����ֱ�ӱ���;

%macro gsWoe(inSampleId=,outSampleId=,varRsp=,woeSampleId=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	%local inSample outSample woeSample;
	%local var value vars;
	%local format fmt fmtExist allFmtExist;
	%local notAvaliableVars hasVarRsp;
	%local strVarList numVarList isStrVar formatType;
	%local i j;

	* �������;
	%if &inSampleId= or &varRsp= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &outSampleId= %then %let outSampleId=&inSampleId;
	%if &woeSampleId= %then %let woeSampleId=&inSampleId;
	
	* ��������;
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	%if &inSampleId^=&woeSampleId %then %do;
		%loadSample(sampleId=&woeSampleId,res=&tres);%let woeSample=&&&tres;
	%end;
	%else %do;
		%let woeSample=&inSample;
	%end;
	
	* inSample: enum vars ���;
	%let vars=&&&inSample.enumVars;
	%if &vars= %then %error(&macro: No enum variables in the sample!);
	%getDsStrVarList(ds=&&&inSample.data,res=&tres);%let strVarList=&&&tres;
	%getDsNumVarList(ds=&&&inSample.data,res=&tres);%let numVarList=&&&tres;

	* woeSample: woe format�����Լ��;
	%local arrayVars;%let arrayVars=%arrayVars(name=varList,values=&vars);
	%local &arrayVars;%array(varList,values=&vars);
	%let allFmtExist=1;
	%if &inSampleId=&woeSampleId %then %let allFmtExist=0;
	%else %do;
		%do i=1 %to &varListN;
			%let var=&&varList&i;
			%let format=&var._WOE;
			%let fmt=%str(&format..);
			%formatExist(lib=&&&woeSample.lib,fmt=&fmt,isInformat=1,res=&tres);%let fmtExist=&&&tres;
			%if &fmtExist=0 %then %let allFmtExist=0;
		%end;
	%end;
	
	* woe format����;
	%if &allFmtExist=0 %then %do;
		* woeSample��enumVars����Լ��;
		%varsSub(a=&vars,b=&&&woeSample.enumVars,res=&tres);%let notAvaliableVars=&&&tres;
		%if &notAvaliableVars ne %then %error(&macro: Cannot create woe format! Variable &notAvaliableVars doesnot exist!);

		* woeSample��varRsp����Լ��;
		%varsIn(source=&&&woeSample.binaryVars,target=&varRsp,res=&tres);%let hasVarRsp=&&&tres; 
		%if &hasVarRsp=0 %then %error(&macro: Cannot create woe format! Response variable doesnot exist!);

		* woe format����;
		%do i=1 %to &varListN;
			%let var=&&varList&i;
			%let format=&var._WOE;
			%genWOEFormat(data=&&&woeSample.data,var=&var,varRsp=&varRsp,format=&format,formatLib=&&&woeSample.lib);
		%end;
	%end;

	%let fmtPath=&&&woeSample.path;

	* �׶�����Դ�ͷ�;
	%dropSample(&inSample);
	%dropSample(&woeSample);

	* ����gsFormat����woeת��;
	%let sourceSampleId=&inSampleId;
	%do i=1 %to &varListN;
		%let var=&&varList&i;
		%put &macro: Do woe transformation for variable &var! &i/&varListN;
		%let format=&var._WOE;
		%if &i>1 %then %let sourceSampleId=&outSampleId;
		%varsIn(source=&strVarList,target=&var,res=&tres);%let isStrVar=&&&tres;
		%put strVarList=&strVarList var=&var isStrVar=&isStrVar;
		%if &isStrVar=1 %then %let formatType=2;
		%else %let formatType=4;
		%gsFormat(inSampleId=&sourceSampleId,outSampleId=&outSampleId,inVars=&var,outVarType=AMOUNT,outVarPrefix=WOE_,format=&format,formatPath=&fmtPath,formatType=&formatType);
	%end;
%mend;

* ---- macro %gsFormat ----;
* �Ա������л���format�ĸ�ʽת��;
* gsFormatһ�ο��Զ�һ�������еĶ����������ת������ֻ��ʹ��ͬ����format����;
* input;
* 	inSampleId			���䣬�������ݼ�;
*	outSampleId			ѡ�䣬������ݼ���Ϊ��ʱ��inSampleId��ͬ;
*	vars				���䣬�������;
*	outVarPrefix		���䣬�������ǰ׺;
*	outVarType			���䣬���������������;
*	format				���䣬ʹ�õ�format���ƣ������Դ�$��ĩβ�ĵ�;
*	formatPath			���䣬ʹ�õ�format����·��;
*	formatType			����;

* ����format��ʹ�þ�������ԣ�����ǿ�ƹ涨����;
*	ת����ʽ				format���Ƹ�ʽ	��������ת������	dataStepת������		��ӦformatType;
*	�ַ�->�ַ�			$informat		inputc				input				formatType=1;
*	�ַ�->����			informat		inputn				input				formatType=2;
*	����->�ַ�			format			putn				put					formatType=3;
* 	����->����			informat		inputn				input				formatType=4;

%macro gsFormat(inSampleId=,outSampleId=,inVars=,outVarType=,outVarPrefix=,format=,formatPath=,formatType=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	* ����׼��;
	%local fmt isInformat formatLib;
	%local inSample outSample;
	%local vars newVars;
	%local var value newVar newValue newDv newDefaultValue;
	%local found;
	%local i;
 	
	* �������;
	%if &inSampleId= or &inVars= or &outVarType= or &outVarPrefix= or &format= or &formatType= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &outSampleId= %then %let outSampleId=&inSampleId;
	%if %isValidVarType(&outVarType)=0 %then %error(&macro: Invalid OUTVARTYPE! outVarType=&outVarType);
	%if &formatPath= %then %error(&macro: Format path is empty!);
	%if &formatType^=1 and &formatType^=2 and &formatType^=3 and &formatType^=4 %then %error(&macro: FORMAT TYPE ERROR!);
	%if &formatType=1 or &formatType=2 or &formatType=4 %then %let isInformat=1;%else %let isInformat=0;
	* ��ʽ���������;
	%if &formatType=1 %then %let fmt=%str($&format..);
	%else %let fmt=%str(&format..);

	%formatExist(path=&formatPath,fmt=&fmt,isInformat=&isInformat,res=&tres);

	%if &&&tres=0 %then %error(&macro: Target format doesnot exist! format=&format);
	%importFormatLib(path=&formatPath,res=&tres);%let formatLib=&&&tres;

	* ��������;
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	
	%if &inSampleId^=&outSampleId %then %do;
		%createSample(sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
		%recordCopy(from=&inSample,to=&outSample,overwrite=0);
	%end;
	%else %do;
		%let outSample=&inSample;
	%end;

	* ����ȷ�������;
	%varsAnd(a=&&&inSample.vars,b=&inVars,res=&tres);%let vars=&&&tres;
	%if &vars= %then %error(&macro: No matched vars in sample! inVars=&inVars sampleVars=&&&inSample.vars);
	 
	* ����ת��;
	data &&&outSample.data;
		set &&&inSample.data;
		%if &formatType=3 %then %do;
			%do_over(values=&InVars,phrase=%str(
				&outVarPrefix.?=put(?,&fmt);
			));
		%end;
		%else %do;
			%do_over(values=&InVars,phrase=%str(
				&outVarPrefix.?=input(?,&fmt);
			));
		%end;
	run;

	* ���ñ�������;
	%let newVars=%do_over(values=&vars,phrase=&outVarPrefix.?);
	%varsOr(a=&&&inSample.&outVarType.vars,b=&newVars,res=&outSample.&outVarType.vars);
	
	* Ĭ��ֵת��;
	%local arrayVars;%let arrayVars=%arrayVars(name=varList,values=&vars);
	%local &arrayVars;%array(varList,values=&vars);
	%let newDefaultValue=%str();

	%do i=1 %to &varListN;
		%let var=&&varList&i;
		%assignsFind(assigns=%str(&&&inSample.defaultValue),var=&var,res=&tres);%let value=%str(&&&tres);
		%if %length(&value)^=0 %then %do;
			* �ַ��͵�ԭʼ����ȥ����;
			%if &formatType=1 or &formatType=2 %then %let value=%dequote(&value);
			%let newVar=&outVarPrefix.&var;
			%if &formatType=1 %then %do;
				%let newValue=%inputc(&value,&format);
				%let newDv=%str(&newVar=%'&newValue%');
			%end;
			%else %if &formatType=2 %then %do;
				%let newValue=%inputn(&value,&format);
				%let newDv=%str(&newVar=&newValue);
			%end;
			%else %if &formatType=3 %then %do;
				%let newValue=%putn(&value,&format);
				%let newDv=%str(&newVar=%'&newValue%');
			%end;
			%else %if &formatType=4 %then %do;
				%let newValue=%inputn(&value,&format);
				%let newDv=%str(&newVar=&newValue);
			%end;
			%let newDefaultValue=%str(&newDefaultValue &newDv);
		%end;
	%end;

	%let &outSample.defaultValue=%str(&&&inSample.defaultValue &newDefaultValue);

	* ���������Ϣ����;
	%let &outSample.genMacro=&macro;
	%let &outSample.genParam=&syspbuff;
	%let &outSample.genDate=%timestamp;
	%let &outSample.desc=Format transformation;

	* ��������;
	%dropFormatLib(&formatLib);
	%saveSample(sample=&outSample);
	%dropSample(&inSample);
	%if &inSampleId^=&outSampleId %then %dropSample(&outSample);
%mend;


* ---- macro %gsMisDummy ----;
* ��num���������ר�ŵ�dummy�����Ա��ȱʧֵ;
* gsMisDummy��Ŀ�����Ϊ������Ĭ��ֵ��num����;
* ����;
*	����ֵ�仯;
*		���ڱ���annualIncome�����Զ�����һ��MIS_ANNUALINCOME����;
*		��annualIncomeΪȱʧʱ��MIS_ANNUALINCOMEΪ1��ͬʱannualIncome����Ϊָ����Ĭ��ֵ;
*		��annualIncomeΪ��ȱʧֵʱ��MIS_ANNUALINCOMEΪ0;
*	������Ϣ�仯;
*		��������MIS_ANNUALINCOME����������binary��Ĭ��ֵ1;
*		��������annualIncome��Ĭ��ֵ��Ĭ��ֵΪ����ָ����Ĭ��ֵ�������δָ����Ĭ��ֵΪ0);
* input;
* 	inSampleId			���䣬�������ݼ�;
*	outSampleId			ѡ�䣬������ݼ���Ϊ��ʱ��inSampleId��ͬ;
*	vars				ѡ�䣬������ı���������Ĭ��Ϊ����û��Ĭ��ֵ����ֵ����;
*	defaultValue		ѡ�䣬Ĭ��ֵ����,assigns��ʽ��Ĭ��ֵΪ0;
* 	defaultEqMiss		ѡ�䣬Ĭ��ֵΪ1���������ǿ���ΪĬ��ֵʱ���Ƿ񽫽�ȱʧ�����1;

%macro gsMisDummy(inSampleId=,outSampleId=,vars=,defaultValue=,defaultEqMiss=) /parmbuff;

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	* ��������;
	%local inSample outSample;
	%local i;
	%local varsHasDefaultValue varsRaw;
	%local var dummyVar;
	%local dv defaultAssign;
	%local newDefaultValue newBinaryVars;

	* �������;
	%if &inSampleId= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &outSampleId= %then %let outSampleId=&inSampleId;
	%if &defaultEqMiss= %then %let defaultEqMiss=1;

	* ��������;
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	%if &inSampleId^=&outSampleId %then %do;
		%createSample(sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
		%recordCopy(from=&inSample,to=&outSample,overwrite=0);	
	%end;
	%else %do;
		%let outSample=&inSample;
	%end;

	%let &outSample.genMacro=&macro;
	%let &outSample.genParam=&syspbuff;
	%let &outSample.genDate=%timestamp;
	%let &outSample.desc=Add missing dummy;
	%let &outSample.inSampleIds=&inSampleId;


	* ת������ȷ��;
	%assignsVars(assigns=%str(&&&inSample.defaultValue),res=&tres);%let varsHasDefaultValue=&&&tres;
	%varsSub(a=&&&inSample.numVars,b=&varsHasDefaultValue,res=&tres);%let varsRaw=&&&tres;
	%if &vars= %then %let vars=&varsRaw;
	%else %varsAnd(a=&varsRaw,b=&vars,res=&tres);%let vars=&&&tres;
	%if &vars= %then %do;
		%put &macro: Warning! No numVar has no default value! Only sample data copy will be done if the inSampleId^=outSampleId!);
		%if &inSampleId^=&outSampleId %then %do;
			data &&&outSample.data;
				set &&&inSample.data;
			run;
		%end;
	%end;
	%else %do;
		* dummy�������ɣ�ԭ����ֵȱʧֵ�޸�;

		%local arrayVars;%let arrayVars=%arrayVars(name=varList,values=&vars);
		%local &arrayVars;%array(varList,values=&vars);
		%let newDefaultValue=%str();
		%let newBinaryVars=%str();

		data &&&outSample.data;
			set &&&inSample.data;
			%do i=1 %to &varListN;
				* ȷ����Ĭ��ֵ;
				%let var=&&varList&i;
				%let dummyVar=%upcase(MIS_&var);
				%assignsFind(assigns=%str(&defaultValue),var=&var,res=&tres);%let dv=%str(&&&tres);
				%if %length(&dv)=0 %then %do;
					%let dv=0;
					%let defaultAssign=%str(&var=0);
				%end;
				%else %do;
					%let defaultAssign=%str(&var=&dv); 
				%end;
				%let newDefaultValue=%str(&newDefaultValue &defaultAssign &dummyVar=1);
				%let newBinaryVars=%str(&newBinaryVars &dummyVar);

				* dummy����������Ĭ��ֵ��ֵ;
				if missing(&var) then do;
					&dummyVar=1;
					&var=&dv;
				end;
				%if &defaultEqMiss=1 %then %do;
					else if &var=&dv then do;
						&dummyVar=1;
					end;
				%end;
				else do;
					&dummyVar=0;
				end;			
			%end;
		run;

		* ���������������;
		%let &outSample.defaultValue=%str(&&&inSample.defaultValue &newDefaultValue);
		%varsOr(a=&&&inSample.binaryVars,b=&newBinaryVars,res=&outSample.binaryVars);

		%saveSample(sample=&outSample);
	%end;

	* ��������;
	%dropSample(&inSample);
	%if &inSampleId^=&outSampleId %then %dropSample(&outSample);

%mend;

* ---------------------------------------------------;
* --------------------- ������ -----------------------;
* ---------------------------------------------------;
* ---- randomGroup ----;
* ������麯��,��Ҫ����k-flod cv����;;
* ��ȫ�����������������ÿ��������������ͬ;
* Input:;
*	data		ԭʼ���ݣ�����;
*	out			����Ĳ������ݣ�����;
*	k			��������;
* Output:;
*	�����ļ�		1���ļ�����ͬ��������groupVar����;
*;
%macro randomGroup(data=,out=,k=,groupVar=);
	%local obs temp;
	%if &data= %then %error(randomGroup: Required param is empty!);
	%if &out= %then %let out=&data;
	%if &groupVar= %then %let groupVar=groupId;
	%if &k= %then %let k=10;

	%getDsObs(ds=&data,res=obs);
	%if &obs<=0 %then %error(randomGroup: Dataset has no record!);
	%let n=%floor(%sysevalf(&obs/&k));

	%let temp=%createTempDs;
	data &temp;
		set &data;
		_groupRandom=rand('uniform');
	run;
	proc sort data=&temp out=&temp;
		by _groupRandom;
	quit;
	data &out;
		set &temp;
		retain _cumObs 0 &groupVar 1;
		drop _cumObs _groupRandom;
		_cumObs=_cumObs+1;
		if &groupVar>=&k or _cumObs<=&n then return;
		else do;
			_cumObs=1;
			&groupVar=&groupVar+1;
		end;
	run;
	%dropDs(&temp);
%mend;

* ---- unitGroup ----;
* �Ա�����unit�ۺϽ��з���;
* �������Ҫ������unit���������varList������������k��;
* �������Ҫ�߼��ǣ��Ȼ���obs��������Ԫ���ٸ��ݴ�С����ۺϵĹ��򽫵�Ԫ���䵽��ͬ���У�����յ�Ԫ��۲�Ĺ�ϵ�����۲���䵽����;
* Input:;
*	data			ԭ����;
*	out				�������;
*	varList			��Ԫ��������;
*	k				�������ĸ�����Ĭ��Ϊ10;
*	lowerBound		���������ʱ��obs�������������ǰ��������obs��������Ŀ��ֵ*lowerBound����ֹͣ��䣬Ĭ��Ϊ0.9��90%;
*					����ʱ���Ȱ�obs������С�����unit����Ȼ���ۼ�obs�������з��飬���ڱ�Ե��unit��Ĭ�Ϸֵ�ǰ�棨��С��������;
*					������unit�Ĵ��ڣ�����ÿ����Ĺ۲�������Ȼ������ȫ��ͬ���������յķ�������Ҳ���ܴﲻ��Ŀ�������;
* 	groupVar		�������������;	
*;
%macro unitGroup(data=,varList=,k=,lowerBound=,out=,groupVar=);
	%if &data= or &varList= %then %error(simpleSample: Required param is empty!);
	%if &out= %then %let out=&data;
	%if &groupVar= %then %let groupVar=groupId;
	%if &k= %then %let k=10;
	%if &lowerBound= %then %let lowerBound=0.9;

	%getDsObs(ds=&data,res=obs);
	%if &obs<=0 %then %error(randomGroup: Dataset has no record!);
	%let n=%floor(%sysevalf(&obs/&k));

	%let sqlVarList=%sasVarsToSql(&varList);

	%let temp=%createTempDs;
	* �������Ԫ���������Ĺ۲�����;
	proc sql;
		create table &temp as (select &sqlVarList,count(*) as _countObs from &data group by &sqlVarList);
	quit;
	* ���۲�������С��������;
	proc sort data=&temp out=&temp;
		by _countObs;
	quit;
	* ��������;
	data &temp;
		set &temp;
		retain &groupVar 1 _cumObs 0;
		*drop _cumObs _countObs;
		_cumObs=_countObs+_cumObs;
		if &groupVar>=&k or _cumObs<&n*&lowerBound then output;
		else do;
			output;
			_cumObs=0;
			&groupVar=&groupVar+1;
		end;
	run;
	%leftJoin(base=&data,table=&temp,key=&varList,out=&out,tableSelect=);
	%dropDs(&temp);
%mend;


* ---- simpleSample ----;
* �����꣬��Ҫ���ڷ�װproc surveyselect��������ʽ��proc surveyselect��ͬ;
* Input:;
*	data			��Ӧoption data;
*	out				��Ӧoption out;
*	where			ԭʼ���ݵĹ�������;
*	method			��Ӧoption method;
*	n				��Ӧoption n;
*	strata			��Ӧstatement strata;
*	reps			��Ӧoption reps;
* ���⹦�ܣ�;
*	method=simple	�������������nС����������ʱʹ��srs��������n��������ʱ���ȳ�ȡ������������ֵ����ʹ��urs��������;
*					ע��ʹ�����ֲ�����ʽʱ��������strata;
* ;
%macro simpleSample(data=,where=,method=,n=,out=,strata=,reps=);
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local totalObs num source statement_strata option_where option_reps option_outhits sampleSrs sampleUrs temp;
	%if &data= or &n= or &out= %then %error(simpleSample: Required param is empty!);
	%getDsObs(ds=&data,where=&where,res=&tres);%let totalObs=&&&tres;
	%let num=&totalObs;
	%if &num<=0 %then %error(&macro: No valid record in dataset &data!); 
	%if &method= %then %let method=srs;
	%let source=&data;
	%let statement_strata=%str();
	%let option_where=%str();
	%let option_reps=%str();
	%let option_outhits=%str();
	%if &strata ne %then %let statement_strata=strata &strata;
	%if &where ne %then %let option_where=%str((where=(&where)));
	%if &reps ne %then %let option_reps=%str(reps=&reps);
	%if &method=urs %then %let option_outhits=outhits;
	%let temp=%str();

	* starta ����;
	%if &strata ne %then %do;
		%let temp=%createTempDs;
		proc sort data=&data out=&temp;
			by &strata;
		quit;
		%let source=&temp;
	%end;
	
	%if &method=simple %then %do;
		%if &n<=&num %then %do;* ����obsС�ڵ���Դ����obs����;
			proc surveyselect data=&source &option_where method=srs n=&n out=&out &option_reps noprint;
			run;
		%end;
		%else %do;* ����obs����Դ����obs����;
			%let sampleSrs=%createTempDs;
			%let sampleUrs=%createTempDs;
			proc surveyselect data=&source &option_where method=srs n=&num out=&sampleSrs &option_reps noprint;
			run;
			proc surveyselect data=&source &option_where method=urs n=%eval(&n-&num) out=&sampleUrs &option_reps outhits noprint;
			run;
			data &out;
				set &sampleUrs &sampleSrs;
			run;
			%dropDs(&sampleSrs);
			%dropDs(&sampleUrs);
		%end;
	%end;
	%else %do;
		proc surveyselect data=&source &option_where method=&method n=&n out=&out &option_reps &option_outhits noprint;
			&statement_strata;
		run;
	%end;
	%dropDs(&temp);
%mend;

* ---- function gsSimpleSample ----;
* �򵥳�����������;
* input;
*	inSampleId			��������id������;
*	outSampleId			�������id������;
*	n					��������������;
*	method				����������ѡ�䣬��ѡ�����;
*							simple		�����ڲ��Żأ�������Żأ�Ĭ��;
*							urs			�Ż�;
*							srs			���Ż�;
*	strata				���������ѡ��;
*	where				inSample����������ѡ��;
*	res					������룬���䣬��ѡ�����;
*							0			����;
*							1			��moneyVars;

%macro gsSimpleSample(inSampleId=,outSampleId=,n=,method=,where=,strata=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	%if &inSampleId= or &outSampleId= or %refExist(&res)=0 or &n= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if method= %then %let method=simple;
	%let &res=0;
	
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	%createSample(sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
	%recordCopy(from=&inSample,to=&outSample,overwrite=0);

	* ������Ϣ����;
	%let &outSample.genMacro=&macro;
	%let &outSample.genParam=&syspbuff;
	%let &outSample.genDate=%timestamp;
	%let &outSample.desc=simple sampling;
	%let &outSample.inSampleIds=&inSampleId;

	* ��������;
	%simpleSample(data=&&&inSample.data,out=&&&outSample.data,n=&n,method=&method,where=&where,strata=&strata);
	%saveSample(sample=&outSample);
	%dropSample(&outSample);
	%dropSample(&inSample);
%mend;

* ---- balancedSample ----;
* ƽ�������ʽ���趨bad/good�ı������г���;
* �����趨��response�������г���;
* �����߼�������ָ����ratio����
* Input:;
*	data		ԭʼ���ݣ�����;
*	out			����Ĳ������ݣ�����;
*	num			�ܲ���obs����������;
*	varRsp		response�������ƣ�����;
*	ratio		����Ŀ��event/nonevent����������;
*	event		�¼�����ֵ������;
* 	force		ǿ�Ʋ�������ѡֵ0��1��2��Ĭ��Ϊ0������ָ������������������������������Ҫ��ʱ�Ĵ�����;
*		0				ֹͣ��Ĭ��ֵ;
*		1				ʹ�÷Żز���;
*		2				����������������;

%macro balancedSample(data=,out=,num=,varRsp=,ratio=,event=,force=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local type eventSampleMode nonEventSampleMode targetEventNum targetNonEventNum realEventNum realNonEventNum sampleEventNum sampleNonEventNum e ne; 

	* �������;
	%if &data= or &out= &varRsp= or &num= or &ratio= %then %error(&macro: Required param is empty! param=&syspbuff) ;
	%if &event= %then %let event=1;
	%if &force= %then %let force=0;

	* ���response��������;
	%getDsVarType(ds=&data,var=&varRsp,res=&tres);%let type=&&&tres;
	%if &type=1 %then %let ev=&event;
	%else %if &type=2 %then %let ev="&event"; 
	%else %error(&macro: No valid rspVar type! varType=&type);

	* ����Ĭ�ϲ�����ʽ:simple;
	%let eventSampleMode=simple;
	%let nonEventSampleMode=simple;

	* ���㲢���Ŀ����������;
	%let targetEventNum=%floor(%sysevalf(&num*&ratio/(1+&ratio)));
	%let targetNonEventNum=%floor(%sysevalf(&num-&targetEventNum));
	proc sql noprint;
		select count(*) into :realEventNum from &data where &varRsp=&ev;
		select count(*) into :realNonEventNum from &data where &varRsp^=&ev;
	quit;

	* -- ���������������������0;
	%if &realEventNum<0 or &realNonEventNum<0 %then %error(&macro: Real event num error!);
	%if &targetEventNum<0 or &targetNonEventNum<0 %then %error(&macro: Target event num error!);
	%let sampleEventNum=&targetEventNum;
	%let sampleNonEventNum=&targetNonEventNum;

	%if &realEventNum<&targetEventNum or &realNonEventNum<&targetNonEventNum %then %do;
		* -- ǿ�Ʋ�������봦��;
		%if &force=1 %then %do;*ʹ�÷Żز���;
			%let sampleEventNum=&targetEventNum;
			%let sampleNonEventNum=&targetNonEventNum;
		%end;
		%else %if &force=2 %then %do;*����������������;
			%let realRatio=%sysevalf(&realEventNum/&realNonEventNum);
			%if &ratio>&realRatio %then %do;
				%let sampleEventNum=&realEventNum;
				%let sampleNonEventNum=%floor(%sysevalf(&sampleEventNum/&ratio));
			%end;
			%else %do;
				%let sampleNonEventNum=&realNonEventNum;
				%let sampleEventNum=%floor(%sysevalf(&sampleNonEventNum*&ratio));
			%end;
		%end;
		%else %do;
			%error(&macro: Not enough sample!);
		%end;
	%end;
	
	* event/nonevent sampling;
	%let e=%createTempDs;
	%let ne=%createTempDs;
	%simpleSample(data=&data,where=%str(&varRsp=&ev),method=&eventSampleMode,n=&sampleEventNum,out=&e);
	%simpleSample(data=&data,where=%str(&varRsp^=&ev),method=&nonEventSampleMode,n=&sampleNonEventNum,out=&ne);

	data &out;
		set &e &ne;
	run;

	%dropDs(&e);
	%dropDs(&ne);

%mend;

* ---- function gsBalancedSample ----;
* �򵥳�����������;
* input;
*	inSampleId			��������id������;
*	outSampleId			�������id������;
*	n					��������������;
* 	varRsp				response var����;
*	p					rsoponse varΪ1��������ռ����;
* 	event				response �¼�ֵ;
*	force				����n��pҪ����������ʵ���������ì��ʱ�Ĵ���������ѡ�����;
*							0			ֹͣ��Ĭ��ֵ;
*							1			ʹ�÷Żز���;
*							2			����������������;
*	res					������룬���䣬��ѡ�����;
*							0			����;
*							1			��moneyVars;

%macro gsBalancedSample(inSampleId=,outSampleId=,n=,varRsp=,p=,force=,event=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	%if &inSampleId= or &outSampleId= or %refExist(&res)=0 or &n= or &varRsp= or &p= %then %error(&macro: Required param is empty! param=&syspbuff);
	%let &res=0;
	
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	%createSample(sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
	%recordCopy(from=&inSample,to=&outSample,overwrite=0);

	* ������Ϣ����;
	%let &outSample.genMacro=&macro;
	%let &outSample.genParam=&syspbuff;
	%let &outSample.genDate=%timestamp;
	%let &outSample.desc=balanced sampling;
	%let &outSample.inSampleIds=&inSampleId;

	* ��������;
	%balancedSample(data=&&&inSample.data,out=&&&outSample.data,num=&n,force=&force,varRsp=&varRsp,ratio=%sysevalf(&p/(1-&p)),event=&event);

	* �����洢;
	%saveSample(sample=&outSample);

	* ��������;
	%dropSample(&outSample);
	%dropSample(&inSample);
%mend;


* ---- function gs_clean ----;
* һ������������������Ҫ��ɸ��ݶԱ�Ҫ�����б��޳���¼����ȱʧ�Ľ�����������0ֵ���Խ���������logת��;
* input;
*	root;
*	inSampleId;
*	outSampleId;
*	reqVars					����������ñ���Ϊ�յļ�¼����ɾ��;
*	enableLogMoney			����logת�������ñ���Ϊ1ʱ����������moneyVars����logת��;

%macro gs_clean(root=,inSampleId=,outSampleId=,reqVars=) /parmbuff;

	%loadSample(root=&root,sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	%createSample(root=&root,sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
	%recordCopy(from=&inSample,to=&outSample,overwrite=0);
	%let &outSample.genMacro=gs_clean;
	%let &outSample.genDate=%timestamp;
	%let &outSample.genParam=&syspbuff;

	%let cleaned=%createTempDs;
	%clearOutput(path=&&&outSample.path,file=report,noTimestamp=1);
	title '����ֵȱʧ�������';
	proc sql;
		select count(*) as count_record,%do_over(values=&&&outSample.vars,phrase=%str(count(?) as ?),between=comma) from &&&inSample.data;
	quit;
	
	%if  &&&inSample.classVars ne %then %do;
		title '��������ṹ����';
		proc freq data=&&&outSample.data;
			tables &&&inSample.classVars /missing;
		quit;
	%end;

	%if &&&inSample.numVars ne %then %do;
		title '��ֵ�ͱ����������';
		proc means data=&&&outSample.data;
			var &&&inSample.numVars ;
		quit;
	%end;

	* ����missingֵ�������;
	data &cleaned;
		set &&&inSample.data;

		* �������б�����Ϊ�յļ�¼;
		%if &reqVars ne %then %do;
			%do_over(values=&reqVars,phrase=%str(if missing(?) then delete),between=%str(;));
		%end;

		* ���ý�������Ĭ��ֵ;
		%if &&&outSample.moneyVars ne %then %do;
			%do_over(values=&&&inSample.moneyVars,phrase=%str(if missing(?) then ?=0),between=%str(;));
		%end;
	run;

	title '����ֵȱʧ�������(�����';
	proc sql;
		select count(*) as count_record,
			%do_over(values=&&&inSample.vars,phrase=%str(count(?) as ?),between=comma)
		from &cleaned;
	quit;
	
	%if &&&inSample.classVars ne %then %do;
		title '��������ṹ����(�����';
		proc freq data=&cleaned;
			tables &&&inSample.classVars /missing;
		quit;
	%end;

	%if &&&inSample.numVars ne %then %do;
		title '��ֵ�ͱ�����������������';
		proc means data=&cleaned;
			var &&&inSample.numVars ;
		quit;
	%end;

	* -- ���logת�� --;
 	%trans_log(data=&cleaned,out=&cleaned,inVarList=&&&inSample.moneyVars,type=money,outVarListRef=&outSample.logMoneyVars);

	* -- �������� --;
	data &&&outSample.data;
		set &cleaned;
	run;

	%saveSample(root=&root,sample=&outSample);

	%dropSample(&inSample);
	%dropSample(&outSample);
	%dropDs(&cleaned);
%mend;


* ---------------------------------------------------------;
* --------------------- ������������ -----------------------;
* ---------------------------------------------------------;

* -- ȫ����������֤�������� --;
* �ɸ�����������k-fold cross validation����;
* input;
*	root;
*	inSampleId			��������id;
*	outSampleIdPrefix	�������idǰ׺����Ϊ������Զ���������id;
*	k					k����;
*	method				���ɽ�����֤�����ķ���;

%macro genCVSample(root=,inSampleId=,outSampleIdPrefix=,k=,method=) /parmbuff;
	%put genCVSample: start! param=&syspbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local groupVar;
	%local currDate genId;
	%local outRecord;
	%local inSample outSample;
	%local groupedSample tvSample tempSplited;
	%local trainId validId trainPath validPath trainLib validLib;

	* �������;
	%if &root= or &inSampleId= %then %error(genCVSample: Required param is empty! param=&syspbuff);
	%if &outSampleIdPrefix= %then %let outSampleIdPrefix=%genId(prefix=GS,len=10);
	%if &k= %then %let k=10;
	%if &k<2 %then %error(genCVSample: The param k must be larger than 1 ! param=&syspbuff);
	%if &method= %then %let method=random;

	* ������������;
	%let groupedSample=%createTempDs;
	%let tvSample=%createTempDs;
	%let splitedSamplePrefix=work.%genId(prefix=T,len=12);

	%let groupVar=%createTempVar;
	%let splitTag=%createTempVar;
	
	* ����������ȡ;
	%loadSample(root=&root,sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;

	* �����������;
	%let outSample=%newSample;
	%let &outSample.genDate=&timestamp;
	%let &outSample.genMacro=genCVSample;
	%let &outSample.genParam=%str(&syspbuff);
	%let &outSample.inSampleId=&inSampleId;
	%let &outSample.groupId=&outSampleIdPrefix;
	%let &outSample.data=%createTempDs;
	%randomGroup(data=&&&inSample.data,out=&groupedSample,k=&k,groupVar=&groupVar);
	%do i=1 %to &k;
		%put genCVSample: generate sub sample &i;
		* �������ݲ��;
		%let trainId=&outSampleIdPrefix._&i._train;
		%let validId=&outSampleIdPrefix._&i._valid;
		data &tvSample;
			set &groupedSample;
			if &groupVar=&i then &splitTag='valid';
			else &splitTag='train';
		run;
		%split(data=&tvSample,by=&splitTag,outPrefix=&splitedSamplePrefix);

		* ѵ���������ݱ���;
		data &&&outSample.data;
			drop &splitTag &groupVar;
			set &splitedSamplePrefix._train;
		run;
		%let &outSample.groupSeq=&i._train;
		%let &outSample.sampleId=&trainId;
		%let &outSample.use=train;
		%saveSample(root=&root,record=&outSample);
	
		* ȷ���������ݱ���;
		data &&&outSample.data;
			drop &splitTag &groupVar;
			set &splitedSamplePrefix._valid;
		run;
		%let &outSample.groupSeq=&i._valid;
		%let &outSample.sampleId=&validId;
		%let &outSample.use=train;
		%saveSample(root=&root,sample=&outSample);
	%end;
	* ��������;
	%dropSample(sample=&inSample);
	%dropDs(&tvSample);
	%dropDs(&splitedSamplePrefix._train);
	%dropDs(&splitedSamplePrefix._valid);
	%dropDs(&groupedSample);
	%dropLib(&inSampleLib);
%mend;


