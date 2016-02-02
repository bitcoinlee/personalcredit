
* ---------------------------;
* ---- Raw sample -----------;
* ---------------------------;
* raw sample指由Level1数据计算而来的样本,在数据格式、操作方式上与sample相同;
* raw sample sampleId与变量名规则;
*	raw sample的sampleId、变量名均不能指定，按统一规则生成;
* 	sampleId生成规则：
*		B[基准日期]_[基础样本名称]_[生成宏参数];
*		其中基础样本名称是对该样本中所包含的各个变量的含义的概括;
*		例：B20150101_DEMOGRAPHICS;
* 	变量命名规则：
*		[基础变量名]_[后缀]，其中后缀是以下划线区分的多个字段;
* 		对于较简单的变量，一般以其全名作为变量名，如SEX;
*		对于名称较长的或修饰较多的变量，一般以其全名的缩写作为基础变量名，并附加修饰符，如：LC_NORMAL;
* raw sample生成;
* 	grs自L1数据库提取数据，L1数据lib的路径通过全局宏变量L1PATH获取;
* 	raw sample由grs系列宏生成，所有该系列宏至少包含baseDate、res两个参数，其中res用于返回生成的样本id;
* 	1个grs宏可以生成多个样本，每个样本可以包含多个变量;
* 	grs说明中必须列示所有该宏可以生成的所有样本、变量;

* 基准时间与获取时间;
*	待补充;
* 时间范围定义;
*	所有时间范围按“算头不算尾原则”表示，即baseDate为20150101,对应时间范围为(-inf,20150101);
*	如baseDate为20150101，间隔为6个月，则时间范围为[20150701,20150101);


* ---- macro grsDemographics ----;
* 生成demographics相关数据;
* 基础样本名称：DEMOGRAPHICS;
* sampleId列表：B20150101_DEMOGRAHPICS;
* 变量列表：sex marriage highestEducationLevel highestEducationDegreeObtained age;
* input:
*	baseDate	基准时间;
*	res			错误代码变量名，暂不启用;
%macro grsDemographics(baseDate=,res=) /parmbuff;
	%put grsDemographics: start to generate demographics! param=&syspbuff;
	%if &baseDate= %then %error(grsDemographics: Required param is empty! param=&syspbuff);

	* -- grs标准工作环境准备 --;
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

	* -- 数据生成代码 --;
	
	* 自nfcs.person表提取记录，生成年龄信息;	
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

	* 按dgetdate获取最新的一条记录;
	proc sort data=&t;
		by spin dgetdate;
	quit;

	%clearLabel(&t);

	* 数据过滤与生成;
	data &&&sample.data;
		set &t;
		if age<14 or age>70 then delete;
		keep spin sex marriage eduLevel eduDegree age;
		by spin dgetdate;
		if last.spin then output;
	run;

	* -- grs标准工作环境清理 --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);

%mend;
* ---- macro grsRecordNum ----;
* 生成RecordNum相关数据;
* 基础样本名称：RECORDNUM;
* sampleId列表：B20150101_RECORDNUM;
* 变量列表：spin getnum allnum lastdate;
* input:
*	baseDate	基准时间;
%macro grsRecordNum(baseDate=,endDate=) /parmbuff;
	%put grsRecordNum: start to generate RecordNum! param=&syspbuff;
	%if &baseDate= %then %error(grsRecordNum: Required param is empty! param=&syspbuff);

	* -- grs标准工作环境准备 --;
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

	* -- 数据生成代码 --;
	
	* 自nfcs.sino_credit_record表提取记录;	
    data &t.;
     set &nfcs..sino_credit_record(keep=sname SCERTTYPE SCERTNO DREQUESTTIME IREQUESTTYPE
                                  where=(&dtvBaseDate. ge drequesttime le &dtvEndDate.));
     
	run;
/*	生成查询量 getnum查得量 allnum查询量*/
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
/*	自person_certification中取spin*/
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
/*	合并两表，创建spin为主键*/
	data &&&sample.data(drop=sname SCERTTYPE SCERTNO IREQUESTTYPE dgetdate);
     merge &t.(in=ina) &t2.(in=inb);
	 by sname scerttype scertno;
	 if ina;
	 if inb;
	run;

/*	%if &SQLOBS<1 %then %error(grsDemographics: No matched record!);*/


	%clearLabel(&t);

	* 数据过滤与生成;

	* -- grs标准工作环境清理 --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);

%mend;

* ---- macro grsEmployment ----;
* 生成employment相关数据;
* 基础样本名称：EMPLOYMENT;
* sampleId列表：B20150101_EMPLOYMENT;
* 变量列表：occupation title technicalTitleLevel annualIncome;
* input:
*	baseDate	基准时间;
*	res			错误代码变量名，暂不启用;

%macro grsEmployment(baseDate=,res=) /parmbuff;
	%put grsEmployment: start to generate employment! param=&syspbuff;
	%if &baseDate= %then %error(grsEmployment: Required param is empty! param=&syspbuff);

	* -- grs标准工作环境准备 --;
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

	* -- 数据生成代码 --;
	
	* 自person_employment表提取记录;
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

	* 剔除重复记录,按dgetdate取最新的一条记录;
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

	* -- grs标准工作环境清理 --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro grsLoanCount ----;
* 指标名称：时点正常贷款笔数;
* 指标说明：计算指定时点，正常仍在偿还的贷款笔数;
* 指标类型：debt profile;
* 基础样本名称：LC;
* sampleId列表：B20150101_LOANCOUNT;
* 变量列表：
*	LC					时点有效贷款笔数;
*	LC_UNSECURED		时点有效信用贷款笔数;
*	LC_SECURED			时点有效抵押贷款笔数;
* input:
*	baseDate			基准时间;
*	res					错误代码变量名，暂不启用;

%macro grsLoanCount(baseDate=,res=) /parmbuff;
	%put grsLoanCount: start to generate loanCount! param=&syspbuff;
	%if &baseDate= %then %error(grsLoanCount: Required param is empty! param=&syspbuff);

	* -- grs标准工作环境准备 --;
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

	* -- 数据生成代码 --;

	* 第1步 筛选;
	* 有效贷款：贷款期限必须框住基准日期;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,dgetdate,iaccountstat,iguaranteeway 
			from &nfcs..loan
			where ddateopened<&dtvBaseDate and ddateclosed>=&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(grsLoanCount: No matched record!);

	* 第2步 剔除;
	* 信息更新：选择每笔贷款的距离基准日期最近的记录;
	* 贷款状态正常：选择状态为正常的贷款;
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

	* 第3步 计算各贷款数量指标;
	
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

	* -- grs标准工作环境清理 --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro grsNewLoanCount ----;
* 指标名称：新增贷款笔数;
* 指标说明：计算指定时间范围内，新增的贷款笔数;
* 指标类型：debt profile;
* 基础样本名称：NLC;
* sampleId列表：B20150101_NEWLOANCOUNT;
* 变量列表：
*	NLC					新增贷款笔数;
*	NLC_UNSECURED		新增信用贷款笔数;
*	NLC_SECURED			新增非信用贷款笔数;
* 变量后缀：
*	_T[inc][int标识符]	回溯期间后缀，例如：NLC_UNSECURED_T6M表示自基准日期6个月内新增;
* input:
*	baseDate	基准时间;
*	int			期间时间单位;	
*	inc			期间长度;
*	res			错误代码变量名，暂不启用;

%macro grsNewLoanCount(baseDate=,int=,inc=,res=) /parmbuff;
	%put grsLoanCount: start to generate loanCount! param=&syspbuff;
	%if &baseDate= or &int= or &inc= %then %error(grsLoanCount: Required param is empty! param=&syspbuff);

	* -- grs标准工作环境准备 --;
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

	* -- 数据生成代码 --;
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

	* Step1 期间筛选;
	proc sql;	
		create table &t as (
			select spin,sorgcode,saccount,iguaranteeway,ddateopened,dgetdate
			from &nfcs..loan
			where ddateopened<&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(grsNewLoanCount: No matched record!);

	* Step2 对每笔贷款取最新的一条记录;
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

	* Step3 指标计算;
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

	* -- grs标准工作环境清理 --;

	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro genLoanList ----;
* 生成指定时间范围内的贷款清单;
* 贷款;
* intput:
*	baseDate			基准时间;
* output:
*	LL贷款清单数据集	包含spin,sorgcode,saccount,iguaranteeway，如返回数据为空则不向目标写入数据;

%macro genLoanList(baseDate=,startDate=,out=) /parmbuff;
	%put genLoanList: start to generate LoanDefaultList! param=&syspbuff;
	%if &baseDate= or &startDate= or &out= %then %error(genLoanList: Required param is empty! param=&syspbuff);

	* -- 工作环境准备 --;
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
	
	* Step 1 日期筛选;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,iguaranteeway,dgetdate,iamountpastdue
			from &nfcs..loan
			where ddateopened<&dtvBaseDate and ddateclosed>=&dtvStartDate
		);
	quit;

	%if &SQLOBS=0 %then %goto CLEANUP;
	
	* Step 2 筛选每笔贷款的最新一条记录;
	proc sort data=&t;
		by spin sorgcode saccount dgetdate;
	quit;

	data &out(index=(sss=(spin sorgcode saccount) /unique));
		set &t;
		keep spin sorgcode saccount iguaranteeway;
		by spin sorgcode saccount dgetdate;
		if last.saccount then output;
	run;

	* 环境清理;
	%CLEANUP:
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro grsHasLoan ----;
* 指标名称：有贷款业务标记;
* 指标说明：用户在指定的时间段内，有贷款业务存续;
* 指标类型：debt profile;
* 基础样本名称：HASHLOAN;
* sampleId列表：B20150101_HASLOAN;
* 变量列表：
*	SLC_T6M					区间内贷款数量;
*	HL_T6M					区间内有贷款;
* input:
*	baseDate				基准时间;
*	res;

%macro grsHasLoan(baseDate=,int=,inc=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%let baseName=HasLoan;
	%let abbrName=HL;

	%put &macro: start to generate HasLoan! param=&syspbuff;
	%if &baseDate= or &int= or &inc= %then %error(&macro: Required param is empty! param=&syspbuff);

	* -- grs标准工作环境准备 --;
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

	* -- 数据生成代码 --;
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


	* inc参数检查;
	%do i=1 %to &incsN;
		%if &&incs&i<=0 %then %error(&macro: All INC must be larger then 0! inc=&inc);
	%end;
	
	* Step1 生成基准时间的逾期贷款名单baseDLL;
	%let SLCs=%str();
	%let suffixes=%str();
	%do i=1 %to &incsN;

		%let interval=&&incs&i;			* 考察期长度;
		%let tempLL=%createTempDs;		* 贷款清单;
		%let tempSLC=%createTempDs; 	* 贷款统计数据表;
		%let startDate=%dsIntnx(&int,&baseDate,-&interval);
		%let scopeSuffix=_T&interval.&intTag;
		%put &macro: Sample &&&sample.sampleId: start to generated var: SLC&scopeSuffix scope=&startDate.-&baseDate;

		* Step 1 生成观察期间开始时间点tempDLL;
		%genLoanList(baseDate=&baseDate,startDate=&startDate,out=&tempLL);
	
		* Step 2 计算贷款数量统计;
		* tempSLC格式为：spin,SLC_T6M;
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
		%let SLCs=&SLCs &tempSLC; * 非空HL数据集清单;
		%let suffixes=&suffixes &scopeSuffix;

		%CONTINUE:
		%let totalSLCs=totalSLCs &tempSLC; * 全量NDC数据集清单;
	%end;

	%if &suffixes= %then %error(&macro: All INC return 0 record!);

	* Step5 合并非空的违约数量数据集、填充缺失值 --;
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

	* -- 环境清理 --;
	%dropDs(&totalSLCs);

	* -- grs标准工作环境清理 --;

	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;


* --- macro grsLoanBalance ----;
* 指标名称：时点待偿还贷款余额;
* 指标说明：计算指定时间点，用户的所有未偿还贷款的总额;
* 指标类型：debt profile;
* 基础样本名称：LOANBALANCE;
* sampleId列表：B20150101_LOANBALANCE;
* 变量列表：
*	LB					时点待偿还贷款余额;
*	LB_UNSECURED		时点待偿还信用贷款余额;
*	LB_SECURED			时点待偿还非信用贷款余额;
* input:
*	baseDate			基准时间;
*	res;

%macro grsLoanBalance(baseDate=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%let baseName=LoanBalance;
	%let abbrName=LB;

	%put &macro: Start to generate &baseName! param=&syspbuff;
	%if &baseDate= %then %error(&macro: Required param is empty! param=&syspbuff);

	
	* -- grs标准工作环境准备 --;
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

	* -- 数据生成代码 --;

	* 第1步 贷款筛选;
	* 贷款开立时间在基准日期前;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,dgetdate,iaccountstat,iguaranteeway,ibalance
			from &nfcs..sino_loan
			where ddateopened<&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(&macro: No matched record!);

	* 第2步 筛选;
	* 信息更新：选择每笔贷款的距离基准日期最近的记录;
	* 非结清贷款：剔除所有贷款状态为结清的贷款;
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

	* 第3步 计算各贷款余额指标;
	
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

	* -- grs标准工作环境清理 --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro genDefaultLoanList ----;
* 生成指定时间点的逾期贷款名单;
* 辅助函数，用于辅助生成新增逾期贷款数量;
* intput:
*	baseDate			基准时间;
* output:
*	DLL逾期清单数据集	包含spin,sorgcode,saccount,iguaranteeway，如返回数据为空则不向目标写入数据;
%macro genDefaultLoanList(baseDate=,out=) /parmbuff;
	%put genLoanDefaultList: start to generate LoanDefaultList! param=&syspbuff;
	%if &baseDate= or &out= %then %error(genLoanDefaultList: Required param is empty! param=&syspbuff);

	* -- 工作环境准备 --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate t t2 t3 t4;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;
	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	* Step 1 日期筛选;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,iguaranteeway,dgetdate,iamountpastdue
			from &nfcs..loan
			where ddateopened<&dtvBaseDate
		);
	quit;

	%if &SQLOBS=0 %then %goto CLEANUP;
	
	* Step 2 筛选每笔贷款的最新一条记录，如逾期金额大于0则视同该账户当前状态为逾期;
	proc sort data=&t;
		by spin sorgcode saccount dgetdate;
	quit;

	data &out(index=(sss=(spin sorgcode saccount) /unique));
		set &t;
		keep spin sorgcode saccount iguaranteeway;
		by spin sorgcode saccount dgetdate;
		if last.saccount and iamountpastdue>0 then output;
	run;

	* 环境清理;
	%CLEANUP:
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro grsNewDefaultCount ----;
* 指标名称：新增违约贷款笔数;
* 指标说明：计算指定时间范围内，新增的违约贷款笔数;
* 指标类型：bad profile;
* 指标逻辑：
*	分别计算主体在两个时点的违约贷款清单，当较晚时点清单L0中出现了较早时点清单L-t中未出现的违约贷款，则认为是新增;
*	主体未出现在L-t中包含两种情况：未违约、无数据，这两种情况都被视同相同;
* 基础样本名称：NDC;
* sampleId列表：B20150101_NEWDEFAULTCOUNT;
* 变量列表：
*	NDC_T6M				回溯期为6个月的新增违约贷款笔数;
*	HD_T6M				新增违约贷款笔数大于1;

* 变量后缀：
*	_T[inc][int标识符]	回溯期间后缀，例如：NLC_T6M表示自基准日期回溯6个月内新增违约贷款笔数;
* input:
*	baseDate	基准时间;
*	int			期间时间单位;	
*	inc			期间长度;
*	res			错误代码变量名，暂不启用;

%macro grsNewDefaultCount(baseDate=,int=,inc=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%put &macro: start to generate loanCount! param=&syspbuff;
	%if &baseDate= or &int= or &inc= %then %error(&macro: Required param is empty! param=&syspbuff);

	* -- 基本参数设置 --;

	%let baseName=NewDefaultCount;
	%let abbrName=NDC;

	* -- grs标准工作环境准备 --;
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

	* -- 数据生成代码 --;
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

	* inc参数检查;
	%do i=1 %to &incsN;
		%if &&incs&i<=0 %then %error(&macro: All INC must be larger then 0! inc=&inc);
	%end;

	* Step1 生成基准时间的逾期贷款名单baseDLL;
	%let baseDLL=%createTempDs;
	%genDefaultLoanList(baseDate=&baseDate,out=&baseDLL);
	%getDsObs(ds=&baseDLL,res=&tres);%put baseDLL &baseDLL obs=&&&tres;
	%let NDCs=%str();
	%let suffixes=%str();
	%do i=1 %to &incsN;
		%let interval=&&incs&i;		* 开始时点的考察期长度;
		%let tempDLL=%createTempDs; * 开始时点的逾期贷款名单;
		%let tempNDL=%createTempDs; * 期间内新增逾期贷款名单;
		%let tempNDC=%createTempDs; * 期间内新增逾期贷款数量;
		%let startDate=%dsIntnx(&int,&baseDate,-&interval);
		%let scopeSuffix=_T&interval.&intTag;
		%let varName=&abbrName.&scopeSuffix;
		%put &macro: Sample &&&sample.sampleId: start to generated var: &varName scope=&startDate.-&baseDate;

		* Step2 生成观察期间开始时间点tempDLL;
		%genDefaultLoanList(baseDate=&startDate,out=&tempDLL);

		* Step3 计算新增违约贷款清单;
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
		* Step4 计算新增违约贷款数量;
		proc sql;
			create table &tempNDC as (
				select spin,count(*) as &varName from &tempNDL group by spin
			);
			create index spin on &tempNDC;
		quit;
		%let NDCs=&NDCs &tempNDC; * 非空NDC数据集清单;
		%let suffixes=&suffixes &scopeSuffix;

		%CONTINUE:
		%let totalNDCs=totalNDCs &tempNDC; * 全量NDC数据集清单;
		%dropDs(&tempDLL);
		%dropDs(&tempNDL);
	%end;
	%put &macro: Sample &&&sample.sampleId: vars=&vars;
	%if &suffixes= %then %error(&macro: All INC return 0 record!);

	* Step5 合并非空的违约数量数据集、填充缺失值 --;
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

	* -- 环境清理 --;
	%dropDs(&totalNDCs);
	%dropDs(&baseDLL);

	* -- grs标准工作环境清理 --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
%mend;

* --- macro  grsPastDueStatus ----;
* 指标名称：逾期贷款总额;
* 指标说明：计算指定时间点，主体的所有逾期贷款数量、逾期贷款的平均逾期额;
* 指标时间类型：SCOPE;
* 指标信息类型：bad profile;
* 样本名称：PastDueStatus;
* 样本缩写：PDA;
* sampleId列表：B20150101_PASTDUEHISTORY;
* 变量列表：
*	PDC					当前逾期贷款数量;
*	PDAA				当前逾期的平均金额;

* input:
*	baseDate			基准时间;

%macro grsPastDueStatus(baseDate=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%let baseName=PastDueStatus;
	%let abbrName=%str();

	%put &macro: Start to generate &baseName! param=&syspbuff;
	%if &baseDate= %then %error(&macro: Required param is empty! param=&syspbuff);

	* -- grs标准工作环境准备 --;
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
	
	* -- 样本信息设置 --;
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
	
	* -- 数据生成代码 --;

	* 第1步 贷款筛选;
	* 贷款开立时间在基准日期前;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,dgetdate,iamountpastdue
			from &nfcs..loan
			where ddateopened<&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(&macro: No matched record!);

	* 第2步 筛选;
	* 信息更新：选择每笔贷款的距离基准日期最近的记录;
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

	* 第3步 计算各贷款违约数量;
	
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

	* -- grs标准工作环境清理 --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

* --- macro grsPastDueAmount ----;
* 指标名称：逾期贷款总额;
* 指标说明：计算指定时间点，总逾期贷款金额;
* 指标类型：bad profile;
* 样本名称：PastDueAmount;
* 样本缩写：PDA;
* sampleId列表：B20150101_PASTDUEAMOUNT;
* 变量列表：
*	PDA					逾期贷款金额;
*	PDA_30D				逾期30天贷款金额;
*	PDA_60D				逾期60天贷款金额;
*	PDA_90D				逾期90天贷款金额;
*	PDA_180D			逾期180天贷款金额;

* input:
*	baseDate			基准时间;

%macro grsPastDueAmount(baseDate=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%let baseName=PastDueAmount;
	%let abbrName=PDA;

	%put &macro: Start to generate &baseName! param=&syspbuff;
	%if &baseDate= %then %error(&macro: Required param is empty! param=&syspbuff);

	* -- grs标准工作环境准备 --;
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

	* -- 数据生成代码 --;

	* 第1步 贷款筛选;
	* 贷款开立时间在基准日期前;
	proc sql;
		create table &t as (
			select spin,sorgcode,saccount,dgetdate,iamountpastdue,iamountpastdue30,iamountpastdue60,iamountpastdue90,iamountpastdue180
			from &nfcs..loan
			where ddateopened<&dtvBaseDate
		);
	quit;

	%if &SQLOBS<1 %then %error(&macro: No matched record!);

	* 第2步 筛选;
	* 信息更新：选择每笔贷款的距离基准日期最近的记录;
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

	* 第3步 计算各贷款余额指标;
	
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

	* -- grs标准工作环境清理 --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;


* --- macro genDefaultLoanList ----;
* 生成指定时间点的逾期贷款名单;
* 辅助函数，用于辅助生成新增逾期贷款数量;
* intput:
*	baseDate			基准时间;
* output:
*	DLL逾期清单数据集	包含spin,sorgcode,saccount,iguaranteeway，如返回数据为空则不向目标写入数据;
%macro genLoanPastDueStatus(baseDate=,startDate=,out=) /parmbuff;
	%put genLoanDefaultList: start to generate LoanDefaultList! param=&syspbuff;
	%if &baseDate= or &out= %then %error(genLoanDefaultList: Required param is empty! param=&syspbuff);

	* -- 工作环境准备 --;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local nfcs;
	%importTempLib(&L1_PATH,&tres);%let nfcs=&&&tres;

	%local dtvBaseDate t t2 t3 t4;
	%let t=%createTempDs;
	%let t2=%createTempDs;
	%let t3=%createTempDs;
	%let t4=%createTempDs;

	%let dtvBaseDate=%dsToDtv(&baseDate);
	
	* Step 1 日期筛选;
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
	
	* Step 2 筛选每笔贷款的最新一条记录，如逾期金额大于0则视同该账户当前状态为逾期;
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

	* 环境清理;
	%CLEANUP:
	%dropLib(&nfcs);
	%dropDs(&t);
	%dropDs(&t2);
	%dropDs(&t3);
	%dropDs(&t4);
%mend;

/*
* --- macro grsHasPastDue ----;
* 指标名称：新增违约贷款笔数;
* 指标说明：计算指定时间范围内，新增的违约贷款笔数;
* 指标类型：bad profile;
* 指标逻辑：
*	分别计算主体在两个时点的违约贷款清单，当较晚时点清单L0中出现了较早时点清单L-t中未出现的违约贷款，则认为是新增;
*	主体未出现在L-t中包含两种情况：未违约、无数据，这两种情况都被视同相同;
* 基础样本名称：NDC;
* sampleId列表：B20150101_NEWDEFAULTCOUNT;
* 变量列表：
*	NDC					新增违约贷款笔数;
* 变量列表：
*	HPD_30D_T6M				逾期超过30天，回溯期6个月;
*	HPD_30D_T12M			逾期超过30天，回溯期12个月;
*	HPD_60D_T6M				逾期超过60天，回溯期6个月;
*	HPD_60D_T12M			逾期超过60天，回溯期12个月;
* input:
*	baseDate				基准时间;
*	outcomePeriod			表现期长度;	
*	good					认定为0的条件的逾期金额变量，可选项0 30 60 90 180，例如，当值为30时，表示当“逾期大于30天金额”为0时，认定为良好，对应0
*	bad						认定为1的条件的逾期金额变量，可选项0 30 60 90 180，例如，当值为60时，表示当“逾期大于60天金额”大于0时，认定为逾期，对应1;
* 	remove
*	res						错误代码变量名，暂不启用;



%macro grsHasPastDue(baseDate=,outcomPeriod=,good=,bad=,removeOldPastDue=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%put &macro: start to generate HasPastDue! param=&syspbuff;
	%if &baseDate= or &int= or &inc= %then %error(&macro: Required param is empty! param=&syspbuff);

	* -- 基本参数设置 --;

	%let baseName=NewDefaultCount;
	%let abbrName=NDC;

	* -- grs标准工作环境准备 --;
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

	* -- 数据生成代码 --;
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

	* inc参数检查;
	%do i=1 %to &incsN;
		%if &&incs&i<=0 %then %error(&macro: All INC must be larger then 0! inc=&inc);
	%end;

	* Step1 生成基准时间的逾期贷款状态baseLPDS;
	%let baseDLL=%createTempDs;
	%genLoanPastDueStatus(baseDate=,startDate=,out=);

	%do i=1 %to &incsN;
		%let interval=&&incs&i;		* 开始时点的考察期长度;
		%let tempDLL=%createTempDs; * 开始时点的逾期贷款名单;
		%let tempNDL=%createTempDs; * 期间内新增逾期贷款名单;
		%let tempNDC=%createTempDs; * 期间内新增逾期贷款数量;
		%let startDate=%dsIntnx(&int,&baseDate,-&interval);
		%let varName=&abbrName._T&interval.&intTag;
		%put &macro: Sample &&&sample.sampleId: start to generated var: &varName scope=&startDate.-&baseDate;

		* 生成基准/结束时点的逾期贷款信息表;
		%genLoanPastDueStatus(baseDate=&baseDate,startDate=&startDate,out=&baseLPDS);

		%if &removeOldPastDueLoan=0 %then %do;
			* 计算汇总的逾期信息;
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
			* 生成开始时点的逾期贷款信息;
			%genLoanPastDueStatus(baseDate=&startDate,out=&startLPDS);

			* 修改列名称并重新建立索引;
			data &startLPDS(index=(SSS=(spin sorgcode saccount) /unique));
				keep spin sorgcode saccount START_LHPD_GT30D START_LHPD_GT60D START_LHPD_GT90D START_LHPD_GT180D;
				set &startLPDS;
				START_LHPD_GT30D=LHPD_GT30D;
				START_LHPD_GT60D=LHPD_GT60D;
				START_LHPD_GT90D=LHPD_GT90D;
				START_LHPD_GT180D=LHPD_GT180D;
			run;

			* 剔除开始时已经达到违约标准的逾期贷款;
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

	* Step5 合并非空的违约数量数据集、填充缺失值 --;
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

	* -- 环境清理 --;
	%dropDs(&totalNDCs);
	%dropDs(&baseDLL);

	* -- grs标准工作环境清理 --;
	%saveSample(sample=&sample);
	%dropSample(&sample);
	%dropLib(&nfcs);
%mend;

*/
