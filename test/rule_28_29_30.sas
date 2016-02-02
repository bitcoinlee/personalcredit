options compress=yes mprint mlogic noxwait noxsync;
libname mydata "&L1_PATH.";
libname mypath "&ROOT_PATH.";
proc sort 
 data=mydata.sino_loan(keep=sorgcode sloancompactcode saccount dgetdate 
                       drecentpaydate ddateopened ibalance imaxdebt
                       Iamountpastdue30 Iamountpastdue60 Iamountpastdue90 Iamountpastdue180)
 out=sino_loan;
 by sorgcode sloancompactcode saccount dgetdate;
run;
/*ȡ���һ��saccount*/
data sino_loan_last;
 set sino_loan(where=(datepart(dgetdate) le mdy(12,31,2015)));
 by sorgcode sloancompactcode saccount dgetdate;
 if last.saccount;
run;
/**************************************************************************************************/
/*rule 28*/
/* �����һ��ʵ�ʻ������ڡ�>=���������ڡ�*/
/*ȡ�������¼*/
data err_rule_28;
 set sino_loan_last;
 if drecentpaydate lt ddateopened;
run;
/*��¼��������*/
data sum_err_28(keep=num);
 num=_n_;
 set err_rule_28 end=eof;
 if eof;
run;
/*���㱣�������¼��������*/
data sum_all_28(keep=num);
 set sino_loan_last end=eof;
 num=_n_;
 if eof;
run;
/*���������¼*/
data sino_loan_last_re_28;
 set sino_loan_last;
 if drecentpaydate ge ddateopened;
run;
/*ȡ��������Ĵ����¼��*/
data err_rule_re_28;
 set sino_loan_last_re_28;
 if drecentpaydate lt ddateopened;
run;
/*��¼�������������*/
data sum_err_re_28(keep=num);
 num=_n_;
 set err_rule_re_28 end=eof;
 if eof;
run;
/*�����������������¼��������*/
data sum_all_re_28(keep=num);
 set sino_loan_last_re_28 end=eof;
 num=_n_;
 if eof;
run;

/**************************************************************************************************/
/*rule 29*/
/*����<=�����ծ�*/
/*ȡ�������¼*/
data err_rule_29;
 set sino_loan_last;
 if ibalance gt imaxdebt;
run;
/*��¼��������*/
proc means data=err_rule_29 noprint;
 var ibalance imaxdebt;
 output out=sum_err_29(rename=(_freq_=num))
        sum(ibalance imaxdebt)=ibalance imaxdebt;
run;
/*���㱣�������¼��������*/
proc means data=sino_loan_last noprint;
 var ibalance imaxdebt;
 output out=sum_all_29(rename=(_freq_=num))
        sum(ibalance imaxdebt)=ibalance imaxdebt;
run;
/*���������¼*/
data sino_loan_last_re_29;
 set sino_loan_last;
 if ibalance le imaxdebt;
run;
/*ȡ��������Ĵ����¼��*/
data err_rule_re_29;
 set sino_loan_last_re_29;
 if ibalance gt imaxdebt;
run;
/*��¼�������������*/
proc means data=err_rule_re_29 noprint;
 var ibalance imaxdebt;
 output out=sum_err_re_29(rename=(_freq_=num))
        sum(ibalance imaxdebt)=ibalance imaxdebt;
run;
/*�����������������¼��������*/
proc means data=sino_loan_last_re_29 noprint;
 var ibalance imaxdebt;
 output out=sum_all_re_29(rename=(_freq_=num))
        sum(ibalance imaxdebt)=ibalance imaxdebt;
run;

/**************************************************************************************************/
/*rule 29*/
/*�������С�����31-60��δ�黹����𡱣�������61-90��δ�黹����𡱣�������91-180��δ�黹����𡱣�
������180������δ�黹�����<=������2*/
/*�Ӻ�*/
data sino_loan_persum;
 set sino_loan_last;
 over=Iamountpastdue30+Iamountpastdue60+Iamountpastdue90+Iamountpastdue180;
run;
/*ȡ�������¼*/
data err_rule_30;
 set sino_loan_persum;
 if over gt ibalance+2;
run;
/*��¼��������*/
proc means data=err_rule_30 noprint;
 var over ibalance;
 output out=sum_err_30(rename=(_freq_=num))
        sum(over ibalance)=over ibalance;
run;
/*���㱣�������¼��������*/
proc means data=sino_loan_persum noprint;
 var over ibalance;
 output out=sum_all_30(rename=(_freq_=num))
        sum(over ibalance)=over ibalance;
run;
/*���������¼*/
data sino_loan_last_re_30;
 set sino_loan_persum;
 if over le ibalance+2;
run;
/*ȡ��������Ĵ����¼��*/
data err_rule_re_30;
 set sino_loan_last_re_30;
 if over gt ibalance+2;
run;
/*��¼�������������*/
proc means data=err_rule_re_30 noprint;
 var over ibalance;
 output out=sum_err_re_30(rename=(_freq_=num))
        sum(over ibalance)=over ibalance;
run;
/*�����������������¼��������*/
proc means data=sino_loan_last_re_30 noprint;
 var over ibalance;
 output out=sum_all_re_30(rename=(_freq_=num))
        sum(over ibalance)=over ibalance;
run;
