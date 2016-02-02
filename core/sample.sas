* ---------------------;
* ---- 样本数据生成 ----;
* ---------------------;

* ---- 总体介绍 ----;
* 数据生成的一般流程与处理函数：;
*	start
*	--（数据下载）-->level0数据;
*	--（数据清洗）-->level1数据;
*	--(设定时间点，指标计算)-->rawSample				%genRawSample	完成指标计算与;
*	--(记录层面筛选、拆分)-->filteredSample			%filterSample 	通过机构、人员id、变量等维度对变量进行进一步筛选、拆分;
*	--(woe转换、log转换）-->sample（level2数据）		%genSample		进行woe与log转换等单表处理;


* ---- 样本数据文件管理 ----;
* 样本数据文件包含rawSample、filteredSample、sample三种数据;
* 样本数据文件名				以[sampleId]_data形式命名的表;
*							对于rawSample，sampleId=rawSampleId;
*							对于filteredSample,sampleId由filter赋予，格式可以为rawSampleId_filterId_outId或其他格式;
* 样本表述文件sinfo			以[sampleId]_sinfo名称保存在样本数据同文件夹;
* 样本数据变量描述数据dinfo	以{sampleId]_dinfo名称保存在样本数据同文件夹;
* 
* dinfo,sinfo采用dict数据结构;

* ---- 变量描述数据 dinfo ----;
* 描述变量类型信息的数据表;
* 格式如下：
* 基础类：;
*	sampleId		样本名称;
* 	var				变量名称;
*	label			变量label;
*	format			变量格式名称;
*	isSasNum		是否是sas数值类型;
*	isSasString		是否是sas字符类型;
*	isClass			是否是分类变量;
*	isMoney			是否是金额类型;
*	isWoe			是否是woe变量;
*	woeFmtLib		进行woe转换所使用的format lib名称;

* ---- 样本描述数据 sinfo ----;
* 描述采样过程的数据集;
* 格式如下：
* 基本信息:
*	varList			样本数据列表;
*   sampleType		样本数据类型，包含：raw filtered final;
*	sampleId		样本id;

* RawSample相关信息：;
* 	rawSampleId		操作id;
*	rawSampleCode	rawSample处理代码名称;
*	rawSampleDate	rasSample处理时间;
* 	rawSampleDesc	rasSample处理描述;
*	baseDate		基准日期;
*	dur1			观察期长度，月为单位;
*	dur2			表现期长度，月为单位;

* %filterSample参数：;
*	filterId		操作id;
* 	filterCode		过滤函数代码名称;
*	filterDate		操作时间;
*	filterDesc		过滤方式描述。相较于另外两个阶段，这个阶段的数据处理更加灵活，且可能存在rawSample的拆分，该字段用于描述处理的方法;

* %genSample参数：;
*	allClassToWoe	所有class数据转换为woe形式;		
*	allMoneyToLog	所有金额数据转换为log形式;
*	allDurToMonth	所有时间间隔数据转换为月度单位;
*	filterDesc		过滤的描述信息;

* --- macro %genRawSample_? ----;
* 生成符合要求的raw sample数据，%genRawSample是一系列样本生成程序的统称，统一以%genRawSample_[rsCode]方式命名;
* input;
* 	date			基准时间，观察期结束时间/表现期开始时间;
*	dur1			观察期长度，以月度计算;
*	dur2			表现期长度，以月度计算;
*	varList			计算的指标列表;
*	outLib			输出的数据表位置;
*	sampleId		样本id;
* output:;
*	rawSample		raw样本数据;
*	dinfo			dinfo表;
*	sinfo			sinfo表;

* --- macro %filterSample_? ----;
* 样本抽样、拆分程序，主要进行样本记录维度的拆分,用于cross-validation等场景;
* 生成符合要求的filteredSample数据， %filterSample是一系列样本抽样程序的统称，统一以%filterSample_[filterCode]方式命名;
* input;
*	inLib			输入的lib;
*	inSampleId		输入的sampleId;
*	outLib			输出的lib;
*	outSampleId		输出的sampleId;	
* output:;
*	filteredSample	抽样后的样本数据;
*	dinfo			dinfo表;
*	sinfo			sinfo表;

* --- macro %genSample ----;
* 生成符合要求的sample数据;
* input;
*	data			输入数据;
*	dinfo			dinfo表;
*	sinfo			sinfo表;
* output:;

* ------------------------------------;
* ---- 指标计算类（rawSample生成） ----;
* ------------------------------------;

* ---- 样本描述数据 sinfo ----;
* 描述采样过程的数据集;
* 格式如下：
* 基本信息:
*	varList			样本数据列表;
*   sampleType		样本数据类型，包含：raw filtered final;
*	sampleId		样本id;

* RawSample相关信息：;
* 	rawSampleId		操作id;
*	rawSampleCode	rawSample处理代码名称;
*	rawSampleDate	rasSample处理时间;
* 	rawSampleDesc	rasSample处理描述;
*	baseDate		基准日期;
*	dur1			观察期长度，月为单位;
*	dur2			表现期长度，月为单位;

* ----------------------------------------;
* ---- 采样函数类（filteredSample生成）----;
* ----------------------------------------;

* ---- randomGroup ----;
* 随机分组函数,主要用于k-flod cv等用途;
* 完全随机划分所有样本，每个子样本数量相同;
* Input:;
*	data		原始数据，必输;
*	out			输出的采样数据，必输;
*	k			分组数量;
* Output:;
*	样本文件		1个文件，不同样本组以groupVar区分;
*;

* ---- unitGroup ----;
* 对变量按unit聚合进行分组;
* 分组的主要参数是unit定义变量（varList）与分组个数（k）;
* 分组的主要逻辑是：先划分obs所归属单元，再根据从小到大聚合的规则将单元分配到不同组中，最后按照单元与观测的关系，将观测分配到组中;
* Input:;
*	data			原数据;
*	out				输出数据;
*	varList			单元主键变量;
*	k				子样本的个数，默认为10;
*	lowerBound		填充子样本时的obs满足比例，若当前子样本中obs数量超过目标值*lowerBound，即停止填充，默认为0.9即90%;
*					分组时，先按obs数量从小到大对unit排序，然后按累加obs数量进行分组，对于边缘的unit，默认分到前面（较小）的组中;
*					由于有unit的存在，所以每个组的观测数量必然不会完全相同，而且最终的分组数量也可能达不到目标的数量;
* 	groupVar		分组变量的名称;	

* ---- simpleSample ----;
* 抽样宏，主要用于封装proc surveyselect，参数格式与proc surveyselect相同;
* Input:;
*	data			对应option data;
*	out				对应option out;
*	where			原始数据的过滤条件;
*	method			对应option method;
*	n				对应option n;
*	strata			对应statement strata;
*	reps			对应option reps;
* 特殊功能：;
*	method=simple	简单随机采样，当n小于总体数量时使用srs采样，当n大于总体时，先抽取所有样本，差值部分使用urs抽样补足;
*					注意使用这种采样方式时不能设置strata;

* ---- balancedSample ----;
* 平衡采样方式，设定bad/good的比例进行抽样;
* 根据设定的response比例进行抽样;
* 采样逻辑：根据指定的ratio进行采样;
* Input:;
*	data		原始数据，必输;
*	out			输出的采样数据，必输;
*	num			总采样obs数量，必输;
*	varRsp		response变量名称，必输;
*	ratio		采样目标event/nonevent比例，必输;
*	event		事件类型值，必输;
* 	force		强制采样，可选值0、1、2，默认为0，用于指定当总体各类型数量不满足采样参数要求时的处理方法;
*		0				停止，默认值;
*		1				使用放回采样;
*		2				按最大可用样本采样;

* -------------------------------------------------;
* ---- 数据最终加工处理类（sample/level数据生成）----;
* -------------------------------------------------;

* ---- macro %genWOEFormat ----;
* 根据变量数据生成WOE转换的informat;
* 对于odd为0或无穷的情况，该类型的映射值为缺失值.，同时在计算总体的odd时，该类型的所有情况均会被剔除;
* input;
* 	data			输入数据集;
*	var				转换变量名;
*	varRsp			response变量名称;
*	formatName		指定用于保存变量woe转换格式的名称，默认为：变量名_woe;
*	formatLib		指定用于保存变量woe转换格式文件的formatlib,默认为本地locallib路径;
* output;
*	format文件		保存于formatLib指定的lib内;

* ---- macro %trans_woe ----;
* 将指定变量转换为WOE形式，转换后的变量名称为：woe_原变量名;
* format转换包含“新建format并转换”与“使用已有format转换”两种模式;
* 前者转换会先调用%genWOEFormat创建format，再使用format转换，后者会使用已有format进行转换;
* input;
* 	data			输入数据集;
*	out				输出数据集，为空时与data相同;
*	varRsp			response变量名称;
*	inVarList		指定需要转换为woe形式的变量名列表;
*	inFormatLib		指定用于转换的formatlib，此参数不为空时，认为是使用已有format转换;
*	outVarListRef	指定保存转换后的变量名列表的宏变量名;
*	outFormatLib	指定用于保存变量woe转换格式文件的formatlib;


* ------------------------;
* ---- 通用样本处理类 ----;
* ------------------------;

* ---- function mergeSample ----;
* 样本合并函数，按spin合并样本;
* 合并样本缺失值处理规则/方法：;

* input;
*	inSampleIds		待合并样本id列表;
*	outSampleId		合并输出样本id;
*	baseSampleId	合并基础样本id;
*	baseData		合并基础数据;
*	sameBaseDate	是否要求为相同的基准时间,可选项0|1;
*	type			合并方式,可选项包含;
*						inner 		innerjoin，观测任意变量存在缺失，则剔除该ob;
*						left 		leftjoin，在合并样本中，将inSampleIds中的第一个ds作为基准，对于缺失的情况使用默认值;
*						base 		leftjoin，使用baseSampleId或baseData中的spin作为基准，对于缺失的情况使用默认值;
* 	force			是否强制合并，当合并中某个sampleId不存在时，是否合并其他的表或报错,默认为0;
*	res				合并输出样本id保存变量;

* details;
* 	缺失值问题;
*			样本内缺失值：mergeSample要求待合并样本内不得出现缺失值;
*			合并缺失值：如中出现某个key值对应的样本缺失，则有三种合并/处理方法，通过type指定;
* 	基准时间问题;
*			mergeSample要求所有被合并样本必须有相同的baseDate;
*;
* TODO KEY检查;
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

		* 样本与样本数据文件清单;
		%let sampleData=&sampleData &&&sample.data;
		%let samples=&samples &sample;
		%let realInSampleIds=&realInSampleIds &sampleId;
		* 变量合并;
		%varsOr(a=&&&outSample.textVars,b=&&&sample.textVars,res=&outSample.textVars);
		%varsOr(a=&&&outSample.enumVars,b=&&&sample.enumVars,res=&outSample.enumVars);
		%varsOr(a=&&&outSample.binaryVars,b=&&&sample.binaryVars,res=&outSample.binaryVars);
		%varsOr(a=&&&outSample.amountVars,b=&&&sample.amountVars,res=&outSample.amountVars);
		%varsOr(a=&&&outSample.moneyVars,b=&&&sample.moneyVars,res=&outSample.moneyVars);
		%varsOr(a=&&&outSample.logMoneyVars,b=&&&sample.logMoneyVars,res=&outSample.logMoneyVars);
		%varsOr(a=&&&outSample.woeEnumVars,b=&&&sample.woeEnumVars,res=&outSample.woeEnumVars);

		* 默认值合并;
		%let newDefaultValue=%str(&newDefaultValue &&&sample.defaultValue);
	
		* 样本基准日期比较;
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

	* 数据合并;
	%if %upcase(&type)=%upcase(inner) %then %do;
		%innerJoin(table=&sampleData,key=spin,out=&&&outSample.data);
	%end;
	%else %do;
		* 数据合并;
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

	* 样本存储;	
	%saveSample(sample=&outSample);
	%if %refExist(&res)=1 %then %let &res=&&&outSample.sampleId;

	* 环境清理;
	%do_over(values=&samples,macro=dropSample);
	%if &baseSample ne %then %dropSample(&baseSample);
	%dropSample(&outSample);
%mend;

* ---- function filterSample ----;
* 对样本进行过滤，包含key值的黑名单/白名单过滤，以及where条件的过滤;
* input;
*	inSampleId			待合并样本id;
*	outSampleId			输出样本id;
*	filterSampleId		过滤器样本id，与where不可以同时为空;
* 	isBlackList			key值是否是黑名单，默认为是;
*	where				where过滤条件,注意where过滤条件表名的是白名单条件;
* 	res					返回变量名;
* output;
*	res					剔除的记录条数;

%macro filterSample(inSampleId=,outSampleId=,filterSampleId=,isBlacklist=,where=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	* 变量声明;
	%local inSample outSample filterSample;

	* 参数检查;
	%if &inSampleId= or &filterSampleId= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &outSampleId= %then %let outSampleId=&inSampleId;
	%if &isBlacklist= %then %let isBlacklist=1;

	* 样本载入;
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

	* 环境清理;
	%dropSample(&inSample);
	%dropSample(&filterSample);
	%if &inSampleId^=&outSampleId %then %dropSample(&outSample);
%mend;

* ---------------------------------------------------;
* --------------------- 变量处理类 -------------------;
* ---------------------------------------------------;

* ---- macro %transLog ----;
* 将指定变量转换为log形式，转换后的变量名称为：LOG_原变量名;
* input;
* 	data			输入数据集;
*	out				输出数据集，为空时与data相同;
*	vars			指定需要转换为log形式的变量名列表;
*	res				指定保存转换后的变量名列表的宏变量名;
*	type			转换类型，主要用于根据变量类型进行转换修正，可选项与修正方式如下：;
*		空值			默认，将0映射到missing value，其余为log;
*		money		金额，将[0,1]映射到0，以避免0金额时无法处理的问题;

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
* 对样本进行处理，根据money变量生成logMoney变量;
* logMoney转换对于小于0的值将产生缺失值;
* gsLogMoney同时按原有值的defaultValue生成logMoneyVars的默认值;
* input;
*	inSampleId;
*	outSampleId			;
*	res					错误代码，必输，可选项包含;
*						0	正常;
*						1	无moneyVars;

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

	* 设置新默认值;
	
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
* 根据变量数据生成WOE转换的informat;
* 对于odd为0或无穷的情况，该类型的映射值为缺失值.，同时在计算总体的odd时，该类型的所有情况均会被剔除;
* input;
* 	data			输入数据集;
*	var				转换变量名;
*	varRsp			response变量名称;
*	format		指定用于保存变量woe转换格式的名称，默认为：变量名_WOE，不需要带.;
*	formatLib		指定用于保存变量woe转换格式文件的formatlib,默认为本地locallib路径;
* output;
*	format文件		保存于formatLib指定的lib内;

%macro genWOEFormat(data=,var=,varRsp=,format=,formatLib=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put Start genWOEFormat! &syspbuff; 

	%local sorted freq odd cntl;
	%local totalEvent totalNonEvent;
	%local varType;

	* 参数检查;
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
		%if &varType=2 %then %do;* 字符->数字,类型2;	
			fmtname="&format";
		%end;
		%else %do;* 数字->数字，类型4;
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
* 对enum类变量进行woe转换;
* input;
* 	inSampleId			必输，输入数据集;
*	outSampleId			选输，输出数据集，默认为inSampleId;
*	woeSampleId			选输，指定woeformat的生成样本，默认为inSampleId;
*	varRsp				必输，response var，该变量在woeSample中必须存在;

* WOE转换变量的输入输出;
*	gsWoe会自动对样本内【所有】的enumVars进行woe转换;
*	转换后的输出变量名格式为:WOE_[varName]，woe变量类型为amount，默认值为转换前值的对应转换值，如原始值没有默认值，则WOE的输出也没有默认值;
* WOE Format的使用;
*	gsWoe所使用的woeFormat通过woeSampleId的确定，woeSample文件夹的formats文件应保存相应的格式信息;
*	woe转换的format名称统一为[varName]_WOE（避免变量名尾为数字的情况），均为informat;
* WOE Format存在性检查与自动创建规则;
*	1、检查inSample与woeSample是否相同，如果相同则认为WOE Format不存在，重新生成;
*	2、如inSample与woeSample不同，则检查对应的woeSample文件夹中是否包含【所有】需要的WOE Format，如【所有】格式均存在则直接使用;
*	3、如存在格式文件的缺失，则检查对应的woeSample中是否具有【所有】所需变量，如果【所有】所需变量均存在，则重新创建【所有】WOE Format;
*	4、【所需变量】指inSample中的所有enumVars和指定的varRsp;
*	5、如所需变量中存在缺失，则直接报错;

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

	* 参数检查;
	%if &inSampleId= or &varRsp= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &outSampleId= %then %let outSampleId=&inSampleId;
	%if &woeSampleId= %then %let woeSampleId=&inSampleId;
	
	* 样本载入;
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	%if &inSampleId^=&woeSampleId %then %do;
		%loadSample(sampleId=&woeSampleId,res=&tres);%let woeSample=&&&tres;
	%end;
	%else %do;
		%let woeSample=&inSample;
	%end;
	
	* inSample: enum vars 检查;
	%let vars=&&&inSample.enumVars;
	%if &vars= %then %error(&macro: No enum variables in the sample!);
	%getDsStrVarList(ds=&&&inSample.data,res=&tres);%let strVarList=&&&tres;
	%getDsNumVarList(ds=&&&inSample.data,res=&tres);%let numVarList=&&&tres;

	* woeSample: woe format存在性检查;
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
	
	* woe format创建;
	%if &allFmtExist=0 %then %do;
		* woeSample中enumVars充分性检查;
		%varsSub(a=&vars,b=&&&woeSample.enumVars,res=&tres);%let notAvaliableVars=&&&tres;
		%if &notAvaliableVars ne %then %error(&macro: Cannot create woe format! Variable &notAvaliableVars doesnot exist!);

		* woeSample中varRsp充分性检查;
		%varsIn(source=&&&woeSample.binaryVars,target=&varRsp,res=&tres);%let hasVarRsp=&&&tres; 
		%if &hasVarRsp=0 %then %error(&macro: Cannot create woe format! Response variable doesnot exist!);

		* woe format生成;
		%do i=1 %to &varListN;
			%let var=&&varList&i;
			%let format=&var._WOE;
			%genWOEFormat(data=&&&woeSample.data,var=&var,varRsp=&varRsp,format=&format,formatLib=&&&woeSample.lib);
		%end;
	%end;

	%let fmtPath=&&&woeSample.path;

	* 阶段性资源释放;
	%dropSample(&inSample);
	%dropSample(&woeSample);

	* 调用gsFormat进行woe转换;
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
* 对变量进行基于format的格式转换;
* gsFormat一次可以对一个样本中的多个变量进行转换，但只能使用同样的format函数;
* input;
* 	inSampleId			必输，输入数据集;
*	outSampleId			选输，输出数据集，为空时与inSampleId相同;
*	vars				必输，输入变量;
*	outVarPrefix		必输，输出变量前缀;
*	outVarType			必输，输出样本变量类型;
*	format				必输，使用的format名称，不可以带$与末尾的点;
*	formatPath			必输，使用的format所在路径;
*	formatType			必输;

* 由于format的使用具有灵活性，这里强制规定如下;
*	转换方式				format名称格式	宏层面调用转换函数	dataStep转换函数		对应formatType;
*	字符->字符			$informat		inputc				input				formatType=1;
*	字符->数字			informat		inputn				input				formatType=2;
*	数字->字符			format			putn				put					formatType=3;
* 	数字->数字			informat		inputn				input				formatType=4;

%macro gsFormat(inSampleId=,outSampleId=,inVars=,outVarType=,outVarPrefix=,format=,formatPath=,formatType=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	* 环境准备;
	%local fmt isInformat formatLib;
	%local inSample outSample;
	%local vars newVars;
	%local var value newVar newValue newDv newDefaultValue;
	%local found;
	%local i;
 	
	* 参数检查;
	%if &inSampleId= or &inVars= or &outVarType= or &outVarPrefix= or &format= or &formatType= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &outSampleId= %then %let outSampleId=&inSampleId;
	%if %isValidVarType(&outVarType)=0 %then %error(&macro: Invalid OUTVARTYPE! outVarType=&outVarType);
	%if &formatPath= %then %error(&macro: Format path is empty!);
	%if &formatType^=1 and &formatType^=2 and &formatType^=3 and &formatType^=4 %then %error(&macro: FORMAT TYPE ERROR!);
	%if &formatType=1 or &formatType=2 or &formatType=4 %then %let isInformat=1;%else %let isInformat=0;
	* 格式检查与载入;
	%if &formatType=1 %then %let fmt=%str($&format..);
	%else %let fmt=%str(&format..);

	%formatExist(path=&formatPath,fmt=&fmt,isInformat=&isInformat,res=&tres);

	%if &&&tres=0 %then %error(&macro: Target format doesnot exist! format=&format);
	%importFormatLib(path=&formatPath,res=&tres);%let formatLib=&&&tres;

	* 样本载入;
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	
	%if &inSampleId^=&outSampleId %then %do;
		%createSample(sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
		%recordCopy(from=&inSample,to=&outSample,overwrite=0);
	%end;
	%else %do;
		%let outSample=&inSample;
	%end;

	* 变量确认与过滤;
	%varsAnd(a=&&&inSample.vars,b=&inVars,res=&tres);%let vars=&&&tres;
	%if &vars= %then %error(&macro: No matched vars in sample! inVars=&inVars sampleVars=&&&inSample.vars);
	 
	* 数据转换;
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

	* 设置变量类型;
	%let newVars=%do_over(values=&vars,phrase=&outVarPrefix.?);
	%varsOr(a=&&&inSample.&outVarType.vars,b=&newVars,res=&outSample.&outVarType.vars);
	
	* 默认值转换;
	%local arrayVars;%let arrayVars=%arrayVars(name=varList,values=&vars);
	%local &arrayVars;%array(varList,values=&vars);
	%let newDefaultValue=%str();

	%do i=1 %to &varListN;
		%let var=&&varList&i;
		%assignsFind(assigns=%str(&&&inSample.defaultValue),var=&var,res=&tres);%let value=%str(&&&tres);
		%if %length(&value)^=0 %then %do;
			* 字符型的原始变量去引号;
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

	* 输出样本信息更新;
	%let &outSample.genMacro=&macro;
	%let &outSample.genParam=&syspbuff;
	%let &outSample.genDate=%timestamp;
	%let &outSample.desc=Format transformation;

	* 环境清理;
	%dropFormatLib(&formatLib);
	%saveSample(sample=&outSample);
	%dropSample(&inSample);
	%if &inSampleId^=&outSampleId %then %dropSample(&outSample);
%mend;


* ---- macro %gsMisDummy ----;
* 对num类变量建立专门的dummy变量以标记缺失值;
* gsMisDummy的目标变量为所有无默认值的num变量;
* 例如;
*	样本值变化;
*		对于变量annualIncome，将自动生成一个MIS_ANNUALINCOME变量;
*		当annualIncome为缺失时，MIS_ANNUALINCOME为1，同时annualIncome被设为指定的默认值;
*		当annualIncome为非缺失值时，MIS_ANNUALINCOME为0;
*	样本信息变化;
*		新增变量MIS_ANNUALINCOME，变量类型binary，默认值1;
*		新增变量annualIncome的默认值，默认值为参数指定的默认值（如参数未指定则默认值为0);
* input;
* 	inSampleId			必输，输入数据集;
*	outSampleId			选输，输出数据集，为空时与inSampleId相同;
*	vars				选输，待处理的变量名单，默认为所有没有默认值的数值变量;
*	defaultValue		选输，默认值变量,assigns格式，默认值为0;
* 	defaultEqMiss		选输，默认值为1，当变量非空且为默认值时，是否将将缺失标记置1;

%macro gsMisDummy(inSampleId=,outSampleId=,vars=,defaultValue=,defaultEqMiss=) /parmbuff;

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	* 变量声明;
	%local inSample outSample;
	%local i;
	%local varsHasDefaultValue varsRaw;
	%local var dummyVar;
	%local dv defaultAssign;
	%local newDefaultValue newBinaryVars;

	* 参数检查;
	%if &inSampleId= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &outSampleId= %then %let outSampleId=&inSampleId;
	%if &defaultEqMiss= %then %let defaultEqMiss=1;

	* 样本载入;
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


	* 转换变量确定;
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
		* dummy变量生成，原变量值缺失值修复;

		%local arrayVars;%let arrayVars=%arrayVars(name=varList,values=&vars);
		%local &arrayVars;%array(varList,values=&vars);
		%let newDefaultValue=%str();
		%let newBinaryVars=%str();

		data &&&outSample.data;
			set &&&inSample.data;
			%do i=1 %to &varListN;
				* 确定新默认值;
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

				* dummy变量生成与默认值赋值;
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

		* 输出样本变量设置;
		%let &outSample.defaultValue=%str(&&&inSample.defaultValue &newDefaultValue);
		%varsOr(a=&&&inSample.binaryVars,b=&newBinaryVars,res=&outSample.binaryVars);

		%saveSample(sample=&outSample);
	%end;

	* 环境清理;
	%dropSample(&inSample);
	%if &inSampleId^=&outSampleId %then %dropSample(&outSample);

%mend;

* ---------------------------------------------------;
* --------------------- 采样类 -----------------------;
* ---------------------------------------------------;
* ---- randomGroup ----;
* 随机分组函数,主要用于k-flod cv等用途;
* 完全随机划分所有样本，每个子样本数量相同;
* Input:;
*	data		原始数据，必输;
*	out			输出的采样数据，必输;
*	k			分组数量;
* Output:;
*	样本文件		1个文件，不同样本组以groupVar区分;
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
* 对变量按unit聚合进行分组;
* 分组的主要参数是unit定义变量（varList）与分组个数（k）;
* 分组的主要逻辑是：先划分obs所归属单元，再根据从小到大聚合的规则将单元分配到不同组中，最后按照单元与观测的关系，将观测分配到组中;
* Input:;
*	data			原数据;
*	out				输出数据;
*	varList			单元主键变量;
*	k				子样本的个数，默认为10;
*	lowerBound		填充子样本时的obs满足比例，若当前子样本中obs数量超过目标值*lowerBound，即停止填充，默认为0.9即90%;
*					分组时，先按obs数量从小到大对unit排序，然后按累加obs数量进行分组，对于边缘的unit，默认分到前面（较小）的组中;
*					由于有unit的存在，所以每个组的观测数量必然不会完全相同，而且最终的分组数量也可能达不到目标的数量;
* 	groupVar		分组变量的名称;	
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
	* 计算各单元（机构）的观测数量;
	proc sql;
		create table &temp as (select &sqlVarList,count(*) as _countObs from &data group by &sqlVarList);
	quit;
	* 按观测数量从小到大排序;
	proc sort data=&temp out=&temp;
		by _countObs;
	quit;
	* 机构分组;
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
* 抽样宏，主要用于封装proc surveyselect，参数格式与proc surveyselect相同;
* Input:;
*	data			对应option data;
*	out				对应option out;
*	where			原始数据的过滤条件;
*	method			对应option method;
*	n				对应option n;
*	strata			对应statement strata;
*	reps			对应option reps;
* 特殊功能：;
*	method=simple	简单随机采样，当n小于总体数量时使用srs采样，当n大于总体时，先抽取所有样本，差值部分使用urs抽样补足;
*					注意使用这种采样方式时不能设置strata;
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

	* starta 排序;
	%if &strata ne %then %do;
		%let temp=%createTempDs;
		proc sort data=&data out=&temp;
			by &strata;
		quit;
		%let source=&temp;
	%end;
	
	%if &method=simple %then %do;
		%if &n<=&num %then %do;* 采样obs小于等于源数据obs个数;
			proc surveyselect data=&source &option_where method=srs n=&n out=&out &option_reps noprint;
			run;
		%end;
		%else %do;* 采样obs大于源数据obs个数;
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
* 简单抽样样本生成;
* input;
*	inSampleId			输入样本id，必输;
*	outSampleId			输出样本id，必输;
*	n					抽样数量，必输;
*	method				抽样方法，选输，可选项包含;
*							simple		数量内不放回，数量外放回，默认;
*							urs			放回;
*							srs			不放回;
*	strata				分组变量，选输;
*	where				inSample过滤条件，选输;
*	res					错误代码，必输，可选项包含;
*							0			正常;
*							1			无moneyVars;

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

	* 样本信息更新;
	%let &outSample.genMacro=&macro;
	%let &outSample.genParam=&syspbuff;
	%let &outSample.genDate=%timestamp;
	%let &outSample.desc=simple sampling;
	%let &outSample.inSampleIds=&inSampleId;

	* 采样过程;
	%simpleSample(data=&&&inSample.data,out=&&&outSample.data,n=&n,method=&method,where=&where,strata=&strata);
	%saveSample(sample=&outSample);
	%dropSample(&outSample);
	%dropSample(&inSample);
%mend;

* ---- balancedSample ----;
* 平衡采样方式，设定bad/good的比例进行抽样;
* 根据设定的response比例进行抽样;
* 采样逻辑：根据指定的ratio计算
* Input:;
*	data		原始数据，必输;
*	out			输出的采样数据，必输;
*	num			总采样obs数量，必输;
*	varRsp		response变量名称，必输;
*	ratio		采样目标event/nonevent比例，必输;
*	event		事件类型值，必输;
* 	force		强制采样，可选值0、1、2，默认为0，用于指定当总体各类型数量不满足采样参数要求时的处理方法;
*		0				停止，默认值;
*		1				使用放回采样;
*		2				按最大可用样本采样;

%macro balancedSample(data=,out=,num=,varRsp=,ratio=,event=,force=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local type eventSampleMode nonEventSampleMode targetEventNum targetNonEventNum realEventNum realNonEventNum sampleEventNum sampleNonEventNum e ne; 

	* 参数检查;
	%if &data= or &out= &varRsp= or &num= or &ratio= %then %error(&macro: Required param is empty! param=&syspbuff) ;
	%if &event= %then %let event=1;
	%if &force= %then %let force=0;

	* 检查response变量类型;
	%getDsVarType(ds=&data,var=&varRsp,res=&tres);%let type=&&&tres;
	%if &type=1 %then %let ev=&event;
	%else %if &type=2 %then %let ev="&event"; 
	%else %error(&macro: No valid rspVar type! varType=&type);

	* 设置默认采样方式:simple;
	%let eventSampleMode=simple;
	%let nonEventSampleMode=simple;

	* 计算并检查目标样本数量;
	%let targetEventNum=%floor(%sysevalf(&num*&ratio/(1+&ratio)));
	%let targetNonEventNum=%floor(%sysevalf(&num-&targetEventNum));
	proc sql noprint;
		select count(*) into :realEventNum from &data where &varRsp=&ev;
		select count(*) into :realNonEventNum from &data where &varRsp^=&ev;
	quit;

	* -- 各类型样本数量必须大于0;
	%if &realEventNum<0 or &realNonEventNum<0 %then %error(&macro: Real event num error!);
	%if &targetEventNum<0 or &targetNonEventNum<0 %then %error(&macro: Target event num error!);
	%let sampleEventNum=&targetEventNum;
	%let sampleNonEventNum=&targetNonEventNum;

	%if &realEventNum<&targetEventNum or &realNonEventNum<&targetNonEventNum %then %do;
		* -- 强制采样检查与处理;
		%if &force=1 %then %do;*使用放回采样;
			%let sampleEventNum=&targetEventNum;
			%let sampleNonEventNum=&targetNonEventNum;
		%end;
		%else %if &force=2 %then %do;*按最大可用样本采样;
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
* 简单抽样样本生成;
* input;
*	inSampleId			输入样本id，必输;
*	outSampleId			输出样本id，必输;
*	n					抽样数量，必输;
* 	varRsp				response var名称;
*	p					rsoponse var为1的样本所占比例;
* 	event				response 事件值;
*	force				采样n、p要求与样本中实际情况发生矛盾时的处理方法，可选项包含;
*							0			停止，默认值;
*							1			使用放回采样;
*							2			按最大可用样本采样;
*	res					错误代码，必输，可选项包含;
*							0			正常;
*							1			无moneyVars;

%macro gsBalancedSample(inSampleId=,outSampleId=,n=,varRsp=,p=,force=,event=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	%if &inSampleId= or &outSampleId= or %refExist(&res)=0 or &n= or &varRsp= or &p= %then %error(&macro: Required param is empty! param=&syspbuff);
	%let &res=0;
	
	%loadSample(sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	%createSample(sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
	%recordCopy(from=&inSample,to=&outSample,overwrite=0);

	* 样本信息更新;
	%let &outSample.genMacro=&macro;
	%let &outSample.genParam=&syspbuff;
	%let &outSample.genDate=%timestamp;
	%let &outSample.desc=balanced sampling;
	%let &outSample.inSampleIds=&inSampleId;

	* 采样过程;
	%balancedSample(data=&&&inSample.data,out=&&&outSample.data,num=&n,force=&force,varRsp=&varRsp,ratio=%sysevalf(&p/(1-&p)),event=&event);

	* 样本存储;
	%saveSample(sample=&outSample);

	* 环境清理;
	%dropSample(&outSample);
	%dropSample(&inSample);
%mend;


* ---- function gs_clean ----;
* 一般性样本清理函数，主要完成根据对必要变量列表剔除记录、对缺失的金额类变量设置0值、对金额变量增加log转换;
* input;
*	root;
*	inSampleId;
*	outSampleId;
*	reqVars					必输变量，该变量为空的记录将被删除;
*	enableLogMoney			启用log转换，当该变量为1时，将对所有moneyVars进行log转换;

%macro gs_clean(root=,inSampleId=,outSampleId=,reqVars=) /parmbuff;

	%loadSample(root=&root,sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;
	%createSample(root=&root,sampleId=&outSampleId,res=&tres);%let outSample=&&&tres;
	%recordCopy(from=&inSample,to=&outSample,overwrite=0);
	%let &outSample.genMacro=gs_clean;
	%let &outSample.genDate=%timestamp;
	%let &outSample.genParam=&syspbuff;

	%let cleaned=%createTempDs;
	%clearOutput(path=&&&outSample.path,file=report,noTimestamp=1);
	title '变量值缺失情况分析';
	proc sql;
		select count(*) as count_record,%do_over(values=&&&outSample.vars,phrase=%str(count(?) as ?),between=comma) from &&&inSample.data;
	quit;
	
	%if  &&&inSample.classVars ne %then %do;
		title '分类变量结构分析';
		proc freq data=&&&outSample.data;
			tables &&&inSample.classVars /missing;
		quit;
	%end;

	%if &&&inSample.numVars ne %then %do;
		title '数值型变量基本情况';
		proc means data=&&&outSample.data;
			var &&&inSample.numVars ;
		quit;
	%end;

	* 变量missing值处理过滤;
	data &cleaned;
		set &&&inSample.data;

		* 过滤所有必填项为空的记录;
		%if &reqVars ne %then %do;
			%do_over(values=&reqVars,phrase=%str(if missing(?) then delete),between=%str(;));
		%end;

		* 设置金额变量的默认值;
		%if &&&outSample.moneyVars ne %then %do;
			%do_over(values=&&&inSample.moneyVars,phrase=%str(if missing(?) then ?=0),between=%str(;));
		%end;
	run;

	title '变量值缺失情况分析(处理后）';
	proc sql;
		select count(*) as count_record,
			%do_over(values=&&&inSample.vars,phrase=%str(count(?) as ?),between=comma)
		from &cleaned;
	quit;
	
	%if &&&inSample.classVars ne %then %do;
		title '分类变量结构分析(处理后）';
		proc freq data=&cleaned;
			tables &&&inSample.classVars /missing;
		quit;
	%end;

	%if &&&inSample.numVars ne %then %do;
		title '数值型变量基本情况（处理后）';
		proc means data=&cleaned;
			var &&&inSample.numVars ;
		quit;
	%end;

	* -- 金额log转换 --;
 	%trans_log(data=&cleaned,out=&cleaned,inVarList=&&&inSample.moneyVars,type=money,outVarListRef=&outSample.logMoneyVars);

	* -- 样本保存 --;
	data &&&outSample.data;
		set &cleaned;
	run;

	%saveSample(root=&root,sample=&outSample);

	%dropSample(&inSample);
	%dropSample(&outSample);
	%dropDs(&cleaned);
%mend;


* ---------------------------------------------------------;
* --------------------- 样本集生成类 -----------------------;
* ---------------------------------------------------------;

* -- 全样本交叉验证样本生成 --;
* 由给定样本生成k-fold cross validation样本;
* input;
*	root;
*	inSampleId			输入样本id;
*	outSampleIdPrefix	输出样本id前缀，如为空则会自动生成样本id;
*	k					k个数;
*	method				生成交叉验证样本的方法;

%macro genCVSample(root=,inSampleId=,outSampleIdPrefix=,k=,method=) /parmbuff;
	%put genCVSample: start! param=&syspbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local groupVar;
	%local currDate genId;
	%local outRecord;
	%local inSample outSample;
	%local groupedSample tvSample tempSplited;
	%local trainId validId trainPath validPath trainLib validLib;

	* 参数检查;
	%if &root= or &inSampleId= %then %error(genCVSample: Required param is empty! param=&syspbuff);
	%if &outSampleIdPrefix= %then %let outSampleIdPrefix=%genId(prefix=GS,len=10);
	%if &k= %then %let k=10;
	%if &k<2 %then %error(genCVSample: The param k must be larger than 1 ! param=&syspbuff);
	%if &method= %then %let method=random;

	* 工作环境创建;
	%let groupedSample=%createTempDs;
	%let tvSample=%createTempDs;
	%let splitedSamplePrefix=work.%genId(prefix=T,len=12);

	%let groupVar=%createTempVar;
	%let splitTag=%createTempVar;
	
	* 输入样本读取;
	%loadSample(root=&root,sampleId=&inSampleId,res=&tres);%let inSample=&&&tres;

	* 输出样本设置;
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
		* 样本数据拆分;
		%let trainId=&outSampleIdPrefix._&i._train;
		%let validId=&outSampleIdPrefix._&i._valid;
		data &tvSample;
			set &groupedSample;
			if &groupVar=&i then &splitTag='valid';
			else &splitTag='train';
		run;
		%split(data=&tvSample,by=&splitTag,outPrefix=&splitedSamplePrefix);

		* 训练样本数据保存;
		data &&&outSample.data;
			drop &splitTag &groupVar;
			set &splitedSamplePrefix._train;
		run;
		%let &outSample.groupSeq=&i._train;
		%let &outSample.sampleId=&trainId;
		%let &outSample.use=train;
		%saveSample(root=&root,record=&outSample);
	
		* 确认样本数据保存;
		data &&&outSample.data;
			drop &splitTag &groupVar;
			set &splitedSamplePrefix._valid;
		run;
		%let &outSample.groupSeq=&i._valid;
		%let &outSample.sampleId=&validId;
		%let &outSample.use=train;
		%saveSample(root=&root,sample=&outSample);
	%end;
	* 环境清理;
	%dropSample(sample=&inSample);
	%dropDs(&tvSample);
	%dropDs(&splitedSamplePrefix._train);
	%dropDs(&splitedSamplePrefix._valid);
	%dropDs(&groupedSample);
	%dropLib(&inSampleLib);
%mend;


