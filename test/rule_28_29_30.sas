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
/*取最后一条saccount*/
data sino_loan_last;
 set sino_loan(where=(datepart(dgetdate) le mdy(12,31,2015)));
 by sorgcode sloancompactcode saccount dgetdate;
 if last.saccount;
run;
/**************************************************************************************************/
/*rule 28*/
/* “最近一次实际还款日期”>=“开户日期”*/
/*取出错误记录*/
data err_rule_28;
 set sino_loan_last;
 if drecentpaydate lt ddateopened;
run;
/*记录错误总数*/
data sum_err_28(keep=num);
 num=_n_;
 set err_rule_28 end=eof;
 if eof;
run;
/*计算保留错误记录的总条数*/
data sum_all_28(keep=num);
 set sino_loan_last end=eof;
 num=_n_;
 if eof;
run;
/*修正错误记录*/
data sino_loan_last_re_28;
 set sino_loan_last;
 if drecentpaydate ge ddateopened;
run;
/*取出修正后的错误记录数*/
data err_rule_re_28;
 set sino_loan_last_re_28;
 if drecentpaydate lt ddateopened;
run;
/*记录修正后错误总数*/
data sum_err_re_28(keep=num);
 num=_n_;
 set err_rule_re_28 end=eof;
 if eof;
run;
/*计算修正后保留错误记录的总条数*/
data sum_all_re_28(keep=num);
 set sino_loan_last_re_28 end=eof;
 num=_n_;
 if eof;
run;

/**************************************************************************************************/
/*rule 29*/
/*“余额”<=“最大负债额”*/
/*取出错误记录*/
data err_rule_29;
 set sino_loan_last;
 if ibalance gt imaxdebt;
run;
/*记录错误总数*/
proc means data=err_rule_29 noprint;
 var ibalance imaxdebt;
 output out=sum_err_29(rename=(_freq_=num))
        sum(ibalance imaxdebt)=ibalance imaxdebt;
run;
/*计算保留错误记录的总条数*/
proc means data=sino_loan_last noprint;
 var ibalance imaxdebt;
 output out=sum_all_29(rename=(_freq_=num))
        sum(ibalance imaxdebt)=ibalance imaxdebt;
run;
/*修正错误记录*/
data sino_loan_last_re_29;
 set sino_loan_last;
 if ibalance le imaxdebt;
run;
/*取出修正后的错误记录数*/
data err_rule_re_29;
 set sino_loan_last_re_29;
 if ibalance gt imaxdebt;
run;
/*记录修正后错误总数*/
proc means data=err_rule_re_29 noprint;
 var ibalance imaxdebt;
 output out=sum_err_re_29(rename=(_freq_=num))
        sum(ibalance imaxdebt)=ibalance imaxdebt;
run;
/*计算修正后保留错误记录的总条数*/
proc means data=sino_loan_last_re_29 noprint;
 var ibalance imaxdebt;
 output out=sum_all_re_29(rename=(_freq_=num))
        sum(ibalance imaxdebt)=ibalance imaxdebt;
run;

/**************************************************************************************************/
/*rule 29*/
/*基础段中“逾期31-60天未归还贷款本金”＋“逾期61-90天未归还贷款本金”＋“逾期91-180天未归还贷款本金”＋
“逾期180天以上未归还贷款本金”<=“余额”＋2*/
/*加和*/
data sino_loan_persum;
 set sino_loan_last;
 over=Iamountpastdue30+Iamountpastdue60+Iamountpastdue90+Iamountpastdue180;
run;
/*取出错误记录*/
data err_rule_30;
 set sino_loan_persum;
 if over gt ibalance+2;
run;
/*记录错误总数*/
proc means data=err_rule_30 noprint;
 var over ibalance;
 output out=sum_err_30(rename=(_freq_=num))
        sum(over ibalance)=over ibalance;
run;
/*计算保留错误记录的总条数*/
proc means data=sino_loan_persum noprint;
 var over ibalance;
 output out=sum_all_30(rename=(_freq_=num))
        sum(over ibalance)=over ibalance;
run;
/*修正错误记录*/
data sino_loan_last_re_30;
 set sino_loan_persum;
 if over le ibalance+2;
run;
/*取出修正后的错误记录数*/
data err_rule_re_30;
 set sino_loan_last_re_30;
 if over gt ibalance+2;
run;
/*记录修正后错误总数*/
proc means data=err_rule_re_30 noprint;
 var over ibalance;
 output out=sum_err_re_30(rename=(_freq_=num))
        sum(over ibalance)=over ibalance;
run;
/*计算修正后保留错误记录的总条数*/
proc means data=sino_loan_last_re_30 noprint;
 var over ibalance;
 output out=sum_all_re_30(rename=(_freq_=num))
        sum(over ibalance)=over ibalance;
run;
