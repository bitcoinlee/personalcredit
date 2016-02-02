
* -------------------;
* ---- ����ת���� ----;
* -------------------;


* ---- macro %genScore ----;
* Ԥ�����-����ת������;
* input;
*	A			50%��������,Ĭ��ֵ650;
*	B			logOdd����,Ĭ��ֵ100��ʵ��ʹ�ø�ֵ;
*	K			��������������ɵ����ֽ�������[A-B*K,A+B*K]��Ĭ��ֵ3;
*				A��B��Kȫ��ʹ��Ĭ��ֵʱ����ֵ����[350,950]������ΥԼ����[0.001,0.999];
* 	data		��������;
*	out			������ݼ�;
*	probVar		���ʱ�����;
*	xbVar		����Ԥ����XB�ı�����;
*				probVar��xbVar������ȫΪ�գ���ͬʱ����ʱ��Ĭ������ʹ��probVar;
*	scoreVar	�������������;

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
* ������������;
* input;
*	A					50%��������,Ĭ��ֵ650;
*	B					logOdd������Ĭ��ֵ100��ʵ��ʹ�ø�ֵ;
*	K					��������������ɵ����ֽ�������[A-B*K,A+B*K]��Ĭ��ֵ3;
*						A��B��Kȫ��ʹ��Ĭ��ֵʱ����ֵ����[350,950]������ΥԼ����[0.001,0.999];
* 	inSampleId			��������id;
*	outSampleId			�������id��ѡ�䣬����Ϊ�գ�Ĭ��ΪinSampleId;
*	probVar				���ʱ�����;
*	scoreVar			���ֱ�����;
* output;
*	�����������ݵ�����;
* 	��ֵ����Ĭ����Ϊ _SCORE_������Ϊamount����Ĭ��ֵ;

%macro gsScore(inSampleId=,outSampleId=,probVar=,scoreVar=,A=,B=,K=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;
	%put &macro: Start! param=&syspbuff;

	* �������;
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
	
	* �����洢�ڻ�������;
	%saveSample(sample=&outSample);
	%dropSample(&inSample);
	%if &inSampleId^=&outSampleId %then %dropSample(&outSample);
%mend;


* ---- macro %gsGroupScore ----;
* ���ɷ��������֣������ֱ��������������;
* input;
*	A					50%��������,Ĭ��ֵ500;
*	B					logOdd������Ĭ��ֵ125;
* 	inSampleId			��������id;
*	outSampleId			�������id��ѡ�䣬����Ϊ�գ�Ĭ��ΪinSampleId;
*	probVar				���ʱ�����;
*	scoreVar			���ֱ�����;
*	groups				�������鶨�壬������������;
*							demographics=sex|age credit=NLC_UNSECURED|NLC_SECURED ability=income|assets;
*						����=���Ϊ�����ƣ�ͬʱ��Ϊ����������ֱ������ƣ����Ҳ�Ϊ������Ĺ���;
* output;
*	�����������ֵ�����;

%macro gsGroupScore(inSampleId=,outSampleId=,modelId=,probVar=,scoreVar=,A=,B=) /parmbuff;
%mend;


* ---- macro showScoreDist ----;
* ���ֲַ�չ��;
* input;
*	sampleId					����id;
*	scoreVar					��ֵ������;
*	classVars					�������;
*	where						�Ƚϱ���������ɸѡ����;
*	minScore					չ�ֵ��������;
*	maxScore					չ�ֵ���С����;
* output;
* 	���ֲַ�ͼ;
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

