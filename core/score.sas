
* -------------------;
* ---- 评分转换类 ----;
* -------------------;


* ---- macro %genScore ----;
* 预测概率-评分转换函数;
* input;
*	A			50%概率评分,默认值650;
*	B			logOdd倍数,默认值100，实际使用负值;
*	K			评分最大倍数，生成的评分将控制在[A-B*K,A+B*K]，默认值3;
*				A，B，K全部使用默认值时，分值区间[350,950]，覆盖违约概率[0.001,0.999];
* 	data		输入数据;
*	out			输出数据集;
*	probVar		概率变量名;
*	xbVar		线性预测结果XB的变量名;
*				probVar与xbVar不可以全为空，当同时存在时，默认优先使用probVar;
*	scoreVar	评分输出变量名;

%macro genScore(data=,out=,probVar=,xbVar=,scoreVar=,A=,B=,K=) /parmbuff;

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%if &data= %then %error(&macro: Required param is empty!);

	%local useProbVar maxScore minScore;

	%if &probVar ne %then %do;
		%let useProbVar=1;
		%if %dsVarExist(&data,&probVar)=0 %then %error(&macro: probVar doesnot exist! probVar=&probVar);
	%end;
	%else %if &xbVar ne %then %do;
		%let useProbVar=0;
		%if %dsVarExist(&data,&probVar)=0 %then %error(&macro: xbVar doesnot exist! xbVar=&xbVar);
	%end;
	%else %error(&macro: PROBVAR and XBVAR cannot be empty at the same time!);

	%if &out= %then %let out=&data;
	%if &A= %then %let A=650;
	%if &B= %then %let B=100;
	%if &K= %then %let K=3;
	%if &scoreVar= %then %let scoreVar=_SCORE_;

	%let maxScore=%sysevalf(&A+&B*&K);
	%let minScore=%sysevalf(&A-&B*&K);

	
	data &out;
		set &data;
		%if &useProbVar=1 %then %do;
			if not missing(&probVar) then &scoreVar=&A-&B*log(&probVar/(1-&probVar));
		%end;
		%else %do;
			if not missing(&xbVar) then &scoreVar=&A-&B*&xbVar;
		%end;
		if not missing(&scoreVar) then do;
			if &scoreVar>&maxScore then &scoreVar=&maxScore;
			if &scoreVar<&minScore then &scoreVar=&minScore;
		end;
	run;
%mend;

* ---- macro %gsScore ----;
* 生成评分样本;
* input;
*	A					50%概率评分,默认值650;
*	B					logOdd倍数，默认值100，实际使用负值;
*	K					评分最大倍数，生成的评分将控制在[A-B*K,A+B*K]，默认值3;
*						A，B，K全部使用默认值时，分值区间[350,950]，覆盖违约概率[0.001,0.999];
* 	inSampleId			输入样本id;
*	outSampleId			输出样本id，选输，可以为空，默认为inSampleId;
*	probVar				概率变量名;
*	scoreVar			评分变量名;
* output;
*	包含评分数据的样本;
* 	分值变量默认名为 _SCORE_，类型为amount，无默认值;

%macro gsScore(inSampleId=,outSampleId=,probVar=,scoreVar=,A=,B=,K=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	* 参数检查;
	%if &inSampleId= or &probVar= %then %error(&macro: Required param is empty!);
	%if &outSampleId= %then %let outSampleId=&inSampleId;
	
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
	%let &outSample.desc=Add score &scoreVar;
	%let &outSample.inSampleIds=&inSampleId;

	%genScore(data=&&&inSample.data,out=&&&outSample.data,A=&A,B=&B,K=&K,probVar=&probVar,scoreVar=&scoreVar);

	%varsOr(a=&&&inSample.amountVars,b=&scoreVar,res=&outSample.amountVars);
	
	* 样本存储于环境清理;
	%saveSample(sample=&outSample);
	%dropSample(&inSample);
	%if &inSampleId^=&outSampleId %then %dropSample(&outSample);
%mend;


* ---- macro %gsGroupScore ----;
* 生成分组子评分，即部分变量所计算的评分;
* input;
*	A					50%概率评分,默认值500;
*	B					logOdd倍数，默认值125;
* 	inSampleId			输入样本id;
*	outSampleId			输出样本id，选输，可以为空，默认为inSampleId;
*	probVar				概率变量名;
*	scoreVar			评分变量名;
*	groups				变量分组定义，定义样例如下;
*							demographics=sex|age credit=NLC_UNSECURED|NLC_SECURED ability=income|assets;
*						其中=左侧为组名称（同时作为输出的组评分变量名称），右侧为组变量的构成;
* output;
*	包含分组评分的样本;

%macro gsGroupScore(inSampleId=,outSampleId=,modelId=,probVar=,scoreVar=,A=,B=) /parmbuff;
%mend;


* ---- macro showScoreDist ----;
* 评分分布展现;
* input;
*	sampleId					样本id;
*	scoreVar					分值变量名;
*	classVars					分类变量;
*	where						比较变量，样本筛选条件;
*	minScore					展现的最大评分;
*	maxScore					展现的最小评分;
* output;
* 	评分分布图;
*;

%macro showScoreDist(sampleId=,scoreVar=,where=,classVars=,maxScore=,minScore=,overlay=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	%local sample;
	%local classStatement whereStatement scopeStr overlayStr scoreInt; 

	%if &sampleId= %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &scoreVar= %then %let scoreVar=_SCORE_;
	%if &overlay= %then %let overlay=0;

	%loadSample(sampleId=&SampleId,res=&tres);%let sample=&&&tres;

	%if %dsVarExist(&&&sample.data,&scoreVar)=0 %then %error(&macro: The scoreVar does not exist! scoreVar=&scoreVar);

	%let classStatement=%str();
	%let whereStatement=%str();
	%let scopeStr=%str();
	%let overlayStr=%str();

	%if &classVars ne %then %let classStatement=%str(class &classVars);
	%if &where ne %then %let whereStatement=%str((where=(&where)));
	%if &overlay=1 %then %let overlayStr=%str(overlay);
	%if &maxScore ne and &minScore ne %then %do;
		%let scoreInt=%floor(%sysevalf((&maxScore-&minScore)/100));
		%let scopeStr=%str(midpoints=&minScore to &maxScore by &scoreInt);
	%end;

	%clearOutput(path=&&&sample.path,file=scoreDist);
	ods html select Histogram BasicMeasures MissingValues;
	title "Scores Distribution sampleId=&sampleId scoreVar=&scoreVar";
	proc univariate data=&&&sample.data &whereStatement;
		&classStatement;
		var &scoreVar;
		histogram /&scopeStr &overlayStr;
	run;
	%dropSample(&sample);
%mend;

