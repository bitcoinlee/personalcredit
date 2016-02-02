%MACRO DO_OVER(arraypos, array=, 
               values=, delim=%STR( ),
               phrase=?, escape=?, between=, 
               macro=, keyword=);

 /*  Last modified: 8/4/2006
                                                           72nd col -->|
  Function: Loop over one or more arrays of macro variables 
           substituting values into a phrase or macro.

  Authors: Ted Clay, M.S.  
              Clay Software & Statistics
              tclay@ashlandhome.net  (541) 482-6435
           David Katz, M.S. www.davidkatzconsulting.com
         "Please keep, use and pass on the ARRAY and DO_OVER macros with
               this authorship note.  -Thanks "
          Send any improvements, fixes or comments to Ted Clay.

  Full documentation with examples appears in 
     "Tight Looping with Macro Arrays".SUGI Proceedings 2006, 
       The keyword parameter was added after the SUGI article was written.

  REQUIRED OTHER MACROS:
        NUMLIST -- if using numbered lists in VALUES parameter.
        ARRAY   -- if using macro arrays.

  Parameters:

     ARRAYPOS and 
     ARRAY are equivalent parameters.  One or the other, but not both, 
             is required.  ARRAYPOS is the only position parameter. 
           = Identifier(s) for the macro array(s) to iterate over. 
             Up to 9 array names are allowed. If multiple macro arrays
             are given, they must have the same length, that is, 
             contain the same number of macro variables.

     VALUES = An explicit list of character strings to put in an 
             internal macro array, VALUES may be a numbered lists of 
             the form 3-15, 03-15, xx3-xx15, etc.

     DELIM = Character used to separate values in VALUES parameter.  
             Blank is default.

     PHRASE = SAS code into which to substitute the values of the 
             macro variable array, replacing the ESCAPE
             character with each value in turn.  The default
             value of PHRASE is a single <?> which is equivalent to
             simply the values of the macro variable array.
             The PHRASE parameter may contain semicolons and extend to
             multiple lines.
             NOTE: The text "?_I_", where ? is the ESCAPE character, 
                   will be replaced with the value of the index variable
                   values, e.g. 1, 2, 3, etc. 
             Note: Any portion of the PHRASE parameter enclosed in 
               single quotes will not be scanned for the ESCAPE.
               So, use double quotes within the PHRASE parameter. 

             If more than one array name is given in the ARRAY= or 
             ARRAYPOS parameter, in the PHRASE parameter the ESCAPE 
             character must be immediately followed by the name of one 
             of the macro arrays, using the same case.

     ESCAPE = A single character to be replaced by macro array values.
             Default is "?".  

     BETWEEN = code to generate between iterations of the main 
             phrase or macro.  The most frequent need for this is to
             place a comma between elements of an array, so the special
             argument COMMA is provided for programming convenience.
             BETWEEN=COMMA is equivalent to BETWEEN=%STR(,).

     MACRO = Name of an externally-defined macro to execute on each 
             value of the array. It overrides the PHRASE parameter.  
             The parameters of this macro may be a combination of 
             positional or keyword parameters, but keyword parameters
             on the external macro require the use of the KEYWORD=
             parameter in DO_OVER.  Normally, the macro would have 
             only positional parameters and these would be defined in
             in the same order and meaning as the macro arrays specified
             in the ARRAY or ARRAYPOS parameter. 
             For example, to execute the macro DOIT with one positional
             parameter, separately define
                      %MACRO DOIT(STRING1); 
                          <statements>
                      %MEND;
             and give the parameter MACRO=DOIT.  The values of AAA1, 
             AAA2, etc. would be substituted for STRING.
             MACRO=DOIT is equivalent to PHRASE=%NRQUOTE(%DOIT(?)).
             Note: Within an externally defined macro, the value of the 
             macro index variable would be coded as "&I".  This is 
             comparable to "?_I_" within the PHRASE parameter.

    KEYWORD = Name(s) of keyword parameters used in the definition of 
             the macro refered to in the MACRO= parameter. Optional.  
             This parameter controls how DO_OVER passes macro array 
             values to specific keyword parameters on the macro.
             This allows DO_OVER to execute a legacy or standard macro.
             The number of keywords listed in the KEYWORD= parameter
             must be less than or equal to the number of macro arrays 
             listed in the ARRAYPOS or ARRAY parameter.  Macro array 
             names are matched with keywords proceeding from right 
             to left.  If there are fewer keywords than macro array 
             names, the remaining array names are passed as positional 
             parameters to the external macro.  See Example 6.

  Rules:
      Exactly one of ARRAYPOS or ARRAY or VALUES is required.
      PHRASE or MACRO is required.  MACRO overrides PHRASE.
      ESCAPE is used when PHRASE is used, but is ignored with MACRO.
      If ARRAY or ARRAYPOS have multiple array names, these must exist 
          and have the same length.  If used with externally defined 
          MACRO, the macro must have positional parameters that 
          correspond 1-for-1 with the array names.  Alternatively, one 
          can specify keywords which tell DO_OVER the names of keyword 
          parameters of the external macro.
 
  Examples:
     Assume macro array AAA has been created with 
             %ARRAY(AAA,VALUES=x y z)
      (1) %DO_OVER(AAA) generates: x y z;
      (2) %DO_OVER(AAA,phrase="?",between=comma) generates: "x","y","z"
      (3) %DO_OVER(AAA,phrase=if L="?" then ?=1;,between=else) generates:
                    if L="x" then x=1;
               else if L="y" then y=1;
               else if L="z" then z=1;
 
      (4) %DO_OVER(AAA,macro=DOIT) generates:
                %DOIT(x) 
                %DOIT(y)
                %DOIT(z)
          which assumes %DOIT has a single positional parameter.
          It is equivalent to:
          %DO_OVER(AAA,PHRASE=%NRSTR(%DOIT(?)))

      (5) %DO_OVER(AAA,phrase=?pct=?/tot*100; format ?pct 4.1;) 
            generates: 
                xpct=x/tot*100; format xpct 4.1;
                ypct=y/tot*100; format ypct 4.1;
                zpct=z/tot*100; format zpct 4.1;
      (6) %DO_OVER(aa bb cc,MACRO=doit,KEYWORD=borders columns)
         is equivalent to %DO_OVER(aa,bb,cc,
                  PHRASE=%NRSTR(%doit(?aa,borders=?bb,columns=?cc)))
         Either example would generate the following internal do-loop:
         %DO I=1 %to &AAN;
           %doit(&&aa&I,borders=&&bb&I,columns=&&cc&I)
         %END;
         Because we are giving three macro array names, the macro DOIT 
         must have three parameters.  Since there are only two keyword
         parameters listed, the third parameter is assumed to be 
         positional.  Positional parameters always preceed keyword
         parameters in SAS macro definitions, so the first parameter
         a positional parameter, which is given the values of first 
         macro array "aa".  The second is keyword parameter "borders=" 
         which is fed the values of the second array "bb".  The third 
         is a keyword parameter "columns=" which is fed the values of
         the third array "cc".  

  History
    7/15/05 changed %str(&VAL) to %quote(&VAL).          
    4/1/06 added KEYWORD parameter
    4/9/06 declared "_Intrnl" array variables local to remove problems
            with nesting with VALUES=.
    8/4/06 made lines 72 characters or less to be mainframe compatible
*/

%LOCAL 
  _IntrnlN
  _Intrnl1  _Intrnl2  _Intrnl3  _Intrnl4  _Intrnl5  
  _Intrnl6  _Intrnl7  _Intrnl8  _Intrnl9  _Intrnl10
  _Intrnl11 _Intrnl12 _Intrnl13 _Intrnl14 _Intrnl15 
  _Intrnl16 _Intrnl17 _Intrnl18 _Intrnl19 _Intrnl20
  _Intrnl21 _Intrnl22 _Intrnl23 _Intrnl24 _Intrnl25
  _Intrnl26 _Intrnl27 _Intrnl28 _Intrnl29 _Intrnl30
  _Intrnl31 _Intrnl32 _Intrnl33 _Intrnl34 _Intrnl35
  _Intrnl36 _Intrnl37 _Intrnl38 _Intrnl39 _Intrnl40
  _Intrnl41 _Intrnl42 _Intrnl43 _Intrnl44 _Intrnl45
  _Intrnl46 _Intrnl47 _Intrnl48 _Intrnl49 _Intrnl50
  _Intrnl51 _Intrnl52 _Intrnl53 _Intrnl54 _Intrnl55
  _Intrnl56 _Intrnl57 _Intrnl58 _Intrnl59 _Intrnl60
  _Intrnl61 _Intrnl62 _Intrnl63 _Intrnl64 _Intrnl65
  _Intrnl66 _Intrnl67 _Intrnl68 _Intrnl69 _Intrnl70
  _Intrnl71 _Intrnl72 _Intrnl73 _Intrnl74 _Intrnl75
  _Intrnl76 _Intrnl77 _Intrnl78 _Intrnl79 _Intrnl80
  _Intrnl81 _Intrnl82 _Intrnl83 _Intrnl84 _Intrnl85
  _Intrnl86 _Intrnl87 _Intrnl88 _Intrnl89 _Intrnl90
  _Intrnl91 _Intrnl92 _Intrnl93 _Intrnl94 _Intrnl95
  _Intrnl96 _Intrnl97 _Intrnl98 _Intrnl99 _Intrnl100
 _KEYWRDN _KEYWRD1 _KEYWRD2 _KEYWRD3 _KEYWRD4 _KEYWRD5 
 _KEYWRD6 _KEYWRD7 _KEYWRD8 _KEYWRD9
 _KWRDI
 ARRAYNOTFOUND CRC CURRPREFIX DELIMI DID FRC I ITER J KWRDINDEX MANUM
 PREFIXES PREFIXN PREFIX1 PREFIX2 PREFIX3 PREFIX4 PREFIX5 
 PREFIX6 PREFIX7 PREFIX8 PREFIX9
 SOMETHINGTODO TP VAL VALUESGIVEN
 ;

%let somethingtodo=Y;

%* Get macro array name(s) from either keyword or positional parameter;
%if %str(&arraypos) ne %then %let prefixes=&arraypos;
%else %if %str(&array)    ne %then %let prefixes=&array;
%else %if %quote(&values) ne %then %let prefixes=_Intrnl;
%else %let Somethingtodo=N;
%if &somethingtodo=Y %then %do;
	%* Parse the macro array names;
	%let PREFIXN=0;
	%do MAnum = 1 %to 999; 
	 	%let prefix&MANUM=%scan(&prefixes,&MAnum,' ');
	 	%if &&prefix&MAnum ne %then %let PREFIXN=&MAnum;
	 	%else %goto out1;
	%end; 
	%out1:

	%* Parse the keywords;
	%let _KEYWRDN=0;
	%do _KWRDI = 1 %to 999; 
	 	%let _KEYWRD&_KWRDI=%scan(&KEYWORD,&_KWRDI,' ');
	 	%if &&_KEYWRD&_KWRDI ne %then %let _KEYWRDN=&_KWRDI;
	 	%else %goto out2;
	%end; 

	%out2:
	%* Load the VALUES into macro array 1 (only one is permitted);
	%if %length(%str(&VALUES)) >0 %then %let VALUESGIVEN=1;
	%else %let VALUESGIVEN=0;
		%if &VALUESGIVEN=1 %THEN %do;
	     	%* Check for numbered list of form xxx-xxx and expand it  using NUMLIST macro.;
	     	%IF (%INDEX(%STR(&VALUES),-) GT 0) and 
	         	(%SCAN(%str(&VALUES),2,-) NE ) and 
	         	(%SCAN(%str(&VALUES),3,-) EQ ) 
	       	%THEN %LET VALUES=%NUMLIST(&VALUES);

			%do iter=1 %TO 9999;  
		  		%let val=%scan(%str(&VALUES),&iter,%str(&DELIM));
		 		 %if %quote(&VAL) ne %then %do;
		      		%let &PREFIX1&ITER=&VAL;
		      		%let &PREFIX1.N=&ITER;
		    	%end;
		  		%else %goto out3;
			%end; 
			%out3:
		%end;
		
		%let ArrayNotFound=0;
		%do j=1 %to &PREFIXN;
  			%*put prefix &j is &&prefix&j;
  			%LET did=%sysfunc(open(sashelp.vmacro 
                    (where=(name eq "%upcase(&&PREFIX&J..N)")) ));
  			%LET frc=%sysfunc(fetchobs(&did,1));
	  		%LET crc=%sysfunc(close(&did));
  			%IF &FRC ne 0 %then %do;
       			%PUT Macro Array with Prefix &&PREFIX&J does not exist;
       			%let ArrayNotFound=1;
    		%end;
		%end;
		
		%if &ArrayNotFound=0 %then %do;

		%if %quote(%upcase(&BETWEEN))=COMMA %then %let BETWEEN=%str(,);

		%if %length(%str(&MACRO)) ne 0 %then %do;
     	%let TP = %nrstr(%&MACRO)(;
     	%do J=1 %to &PREFIXN;
         	%let currprefix=&&prefix&J;
         	%IF &J>1 %then %let TP=&TP%str(,);
           	 %* Write out macro keywords followed by equals. 
               If fewer keywords than macro arrays, assume parameter 
               is positional and do not write keyword=;
           	%let kwrdindex=%eval(&_KEYWRDN-&PREFIXN+&J);
			%IF &KWRDINDEX>0 %then %let TP=&TP&&_KEYWRD&KWRDINDEX=;
        	%LET TP=&TP%nrstr(&&)&currprefix%nrstr(&I);
     	%end;
     	%let TP=&TP);  %* close parenthesis on external macro call;
	%end; 
	%else %do;
     	%let TP=&PHRASE;
     	%let TP = %qsysfunc(tranwrd(&TP,&ESCAPE._I_,%nrstr(&I.)));
     	%let TP = %qsysfunc(tranwrd(&TP,&ESCAPE._i_,%nrstr(&I.)));
     	%do J=1 %to &PREFIXN;
         	%let currprefix=&&prefix&J;
         	%LET TP = %qsysfunc(tranwrd(&TP,&ESCAPE&currprefix,%nrstr(&&)&currprefix%nrstr(&I..))); 
         	%if &PREFIXN=1 %then %let TP = %qsysfunc(tranwrd(&TP,&ESCAPE,%nrstr(&&)&currprefix%nrstr(&I..)));
     	%end;
	%end;

	%* resolve TP (the translated phrase) and perform the looping;
	%do I=1 %to &&&prefix1.n;
		%if &I>1 and %length(%str(&between))>0 %then &BETWEEN;
		%unquote(&TP)
	%end; 
%end;
%end;

%MEND;

%MACRO ARRAY(arraypos, array=, data=, var=, values=,
                       delim=%STR( ), debug=N, numlist=Y);

 /* last modified 8/4/2006                    a.k.a. MACARRAY( ).  
                                                           72nd col -->|
 Function: Define one or more Macro Arrays
     This macro creates one or more macro arrays, and stores in them 
     character values from a SAS dataset or view, or an explicit list 
     of values.

     A macro array is a list of macro variables sharing the same prefix
     and a numerical suffix.  The suffix numbers run from 1 up to a 
     highest number.  The value of this highest number, or the length 
     of the array, is stored in an additional macro variable with the 
     same prefix, plus the letter �N?  The prefix is also referred to
     as the name of the macro array. For example, "AA1", "AA2", "AA3", 
     etc., plus "AAN".  All such variables are declared GLOBAL.

 Authors: Ted Clay, M.S.   tclay@ashlandhome.net  (541) 482-6435
          David Katz, M.S. www.davidkatzconsulting.com
      "Please keep, use and pass on the ARRAY and DO_OVER macros with
          this authorship note.  -Thanks "

 Full documentation with examples appears in SUGI Proceedings, 2006, 
     "Tight Looping With Macro Arrays" by Ted Clay
 Please send improvements, fixes or comments to Ted Clay.

 Parameters: 
    ARRAYPOS and 
    ARRAY are equivalent parameters.  One or the other, but not both, 
             is required.  ARRAYPOS is the only position parameter. 
           = Identifier(s) for the macro array(s) to be defined. 
    DATA = Dataset containing values to load into the array(s).  Can be
              a view, and dataset options such as WHERE= are OK.
    VAR  = Variable(s) containing values to put in list. If multiple 
              array names are specified in ARRAYPOS or ARRAY then the 
              same number of variables must be listed.  
    VALUES  = An explicit list of character strings to put in the list 
              or lists.  If present, VALUES are used rather than DATA 
              and VAR.  VALUES can be a numbered list, eg 1-10, a01-A20, 
              a feature which can be turned of with NUMLIST=N.
              The VALUES can be used with one or more array names 
              specified in the ARRAYPOS or ARRAY parameters.  If more 
              than one array name is given, the values are assigned to
              each array in turn.  For example, if arrays AA and BB 
              are being assigned values, the values are assigned to 
              AA1, BB1, AA2, BB2, AA3, BB3, etc.  Therefore the number
              of values must be a multiple of the number of arrays. 

    DELIM = Character used to separate values in VALUES parameter.  
              Blank is default.

    DEBUG = N/Y. Default=N.  If Y, debugging statements are activated.

    NUMLIST = Y/N.  Default=Y.  If Y, VALUES may be a number list.

 REQUIRED OTHER MACRO: Requires NUMLIST if using numbered lists are used
              in the VALUES parameter.

 How the program works.
    When the VALUES parameter is used, it is parsed into individual 
    words using the scan function. With the DATA parameter, each 
    observation of data to be loaded into one or more macro
    arrays, _n_ determines the numeric suffix.  Each one is declared
    GLOBAL using "call execute" which is acted upon by the SAS macro 
    processor immediately. (Without this "global" setting, "Call symput" 
    would by default put the new macro variables in the local symbol 
    table, which would not be accessible outside this macro.)  Because 
    "call execute" only is handling macro statements, the following 
    statement will normally appear on the SAS log: "NOTE: CALL EXECUTE 
    routine executed successfully, but no SAS statements were generated."

 History
  7/14/05 handle char variable value containing single quote
  1/19/06 VALUES can be a a numbered list with dash, e.g. AA1-AA20 
  4/1/06 simplified process of making variables global.
  4/12/06 allow VALUES= when creating more than one macro array.

    */

%LOCAL prefixes PREFIXN manum _VAR_N iter i J val VAR WHICH MINLENG
   PREFIX1 PREFIX2 PREFIX3 PREFIX4 PREFIX5 PREFIX6 PREFIX7 PREFIX8 
   PREFIX9 PREFIX10 PREFIX11
   var1 var2 var3 var4 var5 var6 var7 var8 var9 var10 var11 ;

%* Get array names from either the keyword or positional parameter;
%if &ARRAY= %then %let PREFIXES=&ARRAYPOS;
%else %let PREFIXES=&ARRAY;

%* Parse the list of macro array names;
%do MANUM = 1 %to 999; 
 %let prefix&MANUM=%scan(&prefixes,&MAnum,' ');
 %if &&prefix&MANUM ne %then 
   %DO;
    %let PREFIXN=&MAnum;
   	/*%global &&prefix&MANUM..N; * delete!;*/
    %* initialize length to zero;
    %let &&prefix&MANUM..N=0;
   %END;
  %else %goto out1;
%end; 
%out1:

%if &DEBUG=Y %then %put PREFIXN is &PREFIXN;

%* Parse the VAR parameter;
%let _VAR_N=0;
%do MANUM = 1 %to 999; 
 %let _var_&MANUM=%scan(&VAR,&MAnum,' ');
 %if %str(&&_var_&MANUM) ne %then %let _VAR_N=&MAnum;
 %else %goto out2;
%end; 
%out2:

%IF &PREFIXN=0 %THEN 
    %PUT ERROR: No macro array names are given;
%ELSE %IF %LENGTH(%STR(&DATA)) >0 and &_VAR_N=0 %THEN
    %PUT ERROR: DATA parameter is used but VAR parameter is blank;
%ELSE %IF %LENGTH(%STR(&DATA)) >0 and &_VAR_N ne &PREFIXN %THEN
    %PUT ERROR: The number of variables in the VAR parameter is not 
 equal to the number of arrays;
%ELSE %DO;

%*------------------------------------------------------;
%*  CASE 1: VALUES parameter is used
%*------------------------------------------------------;

%IF %LENGTH(%STR(&VALUES)) >0 %THEN 
%DO;
     %IF &NUMLIST=Y %then
     %DO;
         %* Check for numbered list of form xxx-xxx and expand it using
             the NUMLIST macro.;
         %IF (%INDEX(%quote(&VALUES),-) GT 0) and 
             (%length(%SCAN(%quote(&VALUES),1,-))>0) and 
             (%length(%SCAN(%quote(&VALUES),2,-))>0) and 
             (%length(%SCAN(%quote(&VALUES),3,-))=0) 
           %THEN %LET VALUES=%NUMLIST(&VALUES);
     %END;

%LET MINLENG=99999;
%DO J=1 %TO &PREFIXN;
%DO ITER=1 %TO 9999;  
  %LET WHICH=%EVAL((&ITER-1)*&PREFIXN +&J); 
  %LET VAL=%SCAN(%STR(&VALUES),&WHICH,%STR(&DELIM));
  %IF %QUOTE(&VAL) NE %THEN
    %DO;
      /*%GLOBAL &&&&PREFIX&J..&ITER;* delete!;*/
      %LET &&&&PREFIX&J..&ITER=&VAL;
      %LET &&&&PREFIX&J..N=&ITER;
    %END;
  %ELSE %goto out3;
%END; 
%out3: %IF &&&&&&PREFIX&J..N LT &MINLENG
          %THEN %LET MINLENG=&&&&&&PREFIX&J..N;
%END;

%if &PREFIXN >1 %THEN 
%DO J=1 %TO &PREFIXN;
    %IF &&&&&&PREFIX&J..N NE &MINLENG %THEN 
%PUT ERROR: Number of values must be a multiple of the number of arrays;
%END;

%END;
%ELSE %DO;

%*------------------------------------------------------;
%*  CASE 2: DATA and VAR parameters used
%*------------------------------------------------------;

%* Get values from one or more variables in a dataset or view;
  data _null_;
  set &DATA end = lastobs;
%DO J=1 %to &PREFIXN; 
  /*call execute('%GLOBAL '||"&&PREFIX&J.."||left(put(_n_,5.)) );* delete!;*/
  call symput(compress("&&prefix&J"||left(put(_n_,5.))), 
              trim(left(&&_VAR_&J)));
  if lastobs then 
   call symput(compress("&&prefix&J"||"N"), trim(left(put(_n_,5.))));
%END;
  run ;

%* Write message to the log;
%IF &DEBUG=Y %then
%DO J=1 %to &PREFIXN;
 %PUT &&&&PREFIX&J..N is &&&&&&PREFIX&J..N;
%END;

%END;
%END;

%MEND;

* ---- OPTIONS ----;

options dlcreatedir;
options nosource;
options nosource2;
options noxwait;
options xsync;


* -------------------------------------------------;
* ------------------ Format ���� ------------------;
* -------------------------------------------------;

* ����base32 format;
* base32 ����0-9ʮ�����֣�A-Z���޳�I��O��M��V,�ܼ�5bit��32���ַ�;
%macro genFormat_Base32x;
	proc format;
  		invalue $base32x
			'00000'='0'
			'00001'='1'
			'00010'='2'
			'00011'='3'
			'00100'='4'
			'00101'='5'
			'00110'='6'
			'00111'='7'
			'01000'='8'
			'01001'='9'
			'01010'='A'
			'01011'='B'
			'01100'='C'
			'01101'='D'
			'01110'='E'
			'01111'='F'
			'10000'='G'
			'10001'='H'
			'10010'='J'
			'10011'='K'
			'10100'='L'
			'10101'='N'
			'10110'='P'
			'10111'='Q'
			'11000'='R'
			'11001'='S'
			'11010'='T'
			'11011'='U'
			'11100'='W'
			'11101'='X'
			'11110'='Y'
			'11111'='X';
	run;
%mend;


%genFormat_Base32x;

* -------------------------------------------------;
* ------------------ Macros -----------------------;
* -------------------------------------------------;

* -------------------------------------------------;
* ---------------- Debug functions ----------------;
* -------------------------------------------------;

* ---- function error ----;
%macro error(err);
	%put ERROR: &err;
	%abort;
%mend;


* -------------------------------------------------;
* ---------------- Basic functions ---------------;
* -------------------------------------------------;

* ----- function isBlank -----;
* �ж������Ƿ�Ϊ��;

%macro isBlank(param);
 	%sysevalf(%superq(param)=,boolean)
%mend;

%macro arrayVars(name=,values=);
	%local res;
	%if values= %then %let res=%str();
	%else %let res=&name.N %do_over(values=&values,phrase=&name.?_i_);
	&res.
%mend;

* -------------------------------------------------;
* ---------------- Math functions -----------------;
* -------------------------------------------------;

%macro log(n);
 	%sysfunc(log(&n))
%mend;

%macro floor(n);
 	%sysfunc(floor(&n))
%mend;

%macro ceil(n);
 	%sysfunc(ceil(&n))
%mend;

%macro round(n,unit);
	%local u;
	%if &unit= %then %let u=1;
	%else %let u=&unit;
 	%sysfunc(round(&n,&u))
%mend;

%macro random;
	%sysfunc(rand(UNIFORM))
%mend;

%macro max/parmbuff;
	%local s res;
	%let res=%str();
	%let s=%substring(&syspbuff,2,-2);
	%if &s ne %then %let res=%sysfunc(max(&s));
	&res.
%mend;

* -------------------------------------------------;
* ---------------- String functions ---------------;
* -------------------------------------------------;

%macro anyAlpha(s);
	%sysfunc(anyAlpha(&s))
%mend;

%macro anyCntrl(s);
	%sysfunc(anyCntrl(&s))
%mend;

%macro anyDigit(s);
	%sysfunc(anyDigit(&s))
%mend;

%macro anyPunct(s);
	%sysfunc(anyPunct(&s))
%mend;

* ----- function findStr -----;
* ��ָ��string������Ŀ���ַ����������������г��ֵ�λ��;
%macro findStr(source,target,pos);
	%local src ind r len_s len_t finded_pos next_pos;
	%if %length(&source)=0 or %length(&target)=0 %then %let r=%str();
	%else %do;
		%if &pos= %then %let pos=1;
		%let src=%substr(%str(&source),&pos);
		%let ind=%index(&src,&target);
		%if &ind=0 %then %let r=%str();
		%else %do;
			%let len_s=%length(&source);
			%let len_t=%length(&target);
			%let finded_pos=%eval(&pos+&ind-1);
			%let next_pos=%eval(&pos+&ind+&len_t-1);
			%if &next_pos>&len_s %then %let r=&finded_pos;
			%else %do;
				%let r=&finded_pos %findStr(%quote(&source),%quote(&target),&next_pos);
			%end;
		%end;
	%end;
	&r.
%mend;

* ----- function countStr -----;
* ��ָ��string������Ŀ���ַ����������������г��ֵ�λ��;
%macro countStr(source,target,pos);
	%local src ind r len_s len_t finded_pos next_pos;
	%if %length(&source)=0 or %length(&target)=0 %then %let r=0;
	%else %do;
		%if &pos= %then %let pos=1;
		%let src=%quote(%substr(&source,&pos));
		%let ind=%index(&src,&target);
		%if &ind=0 %then %let r=0;
		%else %do;
			%let len_s=%length(&source);
			%let len_t=%length(&target);
			%let finded_pos=%eval(&pos+&ind-1);
			%let next_pos=%eval(&pos+&ind+&len_t-1);
			%if &next_pos>&len_s %then %let r=1;
			%else %do;
				%let r=%countStr(%quote(&source),%quote(&target),&next_pos);
				%let r=%eval(1+&r);
			%end;
		%end;
	%end;
	&r.
%mend;

%macro tranwrd(source,target,replacement);
	%local res;
	%let res=%sysfunc(tranwrd(&source,&target,&replacement));
	&res.
%mend;

* ---- function strip ----;
* ȥ����β�ո�;
%macro strip(s);
	%local res;
	%if %length(&s)=0 %then %let res=%str();
	%else %let res=%sysfunc(strip(&s));
	&res.
%mend; 

* ---- function compress ----;
* ȥ�����пո�;
%macro compress(s);
	%local res;
	%if %length(&s)=0 %then %let res=%str();
	%else %let res=%sysfunc(compress(&s));
	&res.
%mend; 

* ---- function reverse ----;
* ��ת�ַ���;
%macro reverse(s);
	%local res;
	%if %length(&s)=0 %then %let res=%str();
	%else %let res=%sysfunc(reverse(&s));
	&res.
%mend;

* ---- function repeat ----;
* ��ת�ַ���;
%macro repeat(s,n);
	%local res;
	%let res=%sysfunc(repeat(&s,%eval(&n-1)));
	&res.
%mend;

* ---- function cats ----;
* �����ַ�;
%macro cats /parmbuff;
	%local s res;
	%let s=%substring(&syspbuff,2,-2);
	%if &s= %then %let res=%str();
	%else %let res=%sysfunc(cats(&s));
	&res.
%mend;

* ---- function isDigit ----;
* �ж��Ƿ�������;
%macro isDigit(s);
	%local res str l i c;
	%let res=1;
	%if %length(&s)=0 %then %let res=0;
	%else %if %anyAlpha(&s)>0 %then %let res=0;
	%else %if %countStr(&s,%str(.))>1 %then %let res=0;
	%else %if %countStr(&s,%str(-))>1 %then %let res=0;
	%else %if %findStr(&s,%str(-))^=1 and %findStr(&s,%str(-)) ne %then %let res=0;
	%else %do;
		%let s=%tranwrd(&s,%str(.),1);
		%let s=%tranwrd(&s,%str(-),1);
		%let l=%length(&s);
		%do i=1 %to &l;
			%let c=%substr(&s,&i,1);
			%if %anyDigit(&c)^=1 %then %let res=0; 
		%end;
	%end;
	&res.
%mend;

* ---- function subString ----;
* ��λ����ȡ���ַ���;
*	start	��ʼλ��	����������Ĭ��ֵ1;
*	end		����λ�ã���������Ĭ��ֵ-1;
*	����������ֵ��ֵ����ֵ��ʾ�����λ�ã�-1��ʾ���һλ����0ʱʹ��Ĭ��ֵ;
*	��������λ�ñ�ʾ��start<=endʱ�����������ַ�������start>endʱ����������ַ���;
%macro subString(s,start,end);
	%local startPos endPos res;
	%if &start= or &start=0 %then %let start=1;
	%if &end= or &end=0 %then %let end=-1;
	%if &start<0 %then %let startPos=%eval(%length(%str(&s))+&start+1);
	%else %let startPos=&start;
	%if &end<0 %then %let endPos=%eval(%length(%str(&s))+&end+1);
	%else %let endPos=&end;
	%if &startPos<=&endPos %then %let res=%substr(%str(&s),&startPos,%eval(&endPos-&startPos+1));
	%else %let res=%reverse(%substr(%str(&s),&endPos,%eval(&startPos-&endPos+1)));
	&res.
%mend; 


* ---- function binStrToBase32 ----;
* ���������ַ���ת��Ϊbase32��ʽ;
%macro binStrToBase32(binStr);
	%local l k p i start end b c base32;
	%let l=%length(&binStr);
	%let k=%ceil(%sysevalf(&l/5));
	%let p=%eval(&k*5-&l);
	%if &p>0 %then %let binStr=&binStr.%repeat(0,&p);
	%let base32=%str();
	%do i=1 %to &k;
		%let start=%eval((&i-1)*5+1);
		%let end=%eval(&start+4);
		%let b=%subString(&binStr,&start,&end);
		%let c=%sysfunc(inputc(&b,$base32x.));
		%let base32=&base32.&c;
	%end;
	&base32.
%mend;

* ---- function md5base32 ----;
* ���ַ�������md5 hashȻ����base32��ʽ���;
%macro md5base32(str);
	%local a;
	%let a=%binStrToBase32(%sysfunc(md5(&str),$binary128.));
	%str(&a)
%mend;

* ---- function hash ----;
* ���ַ�������md5 hashȻ����base32��ʽ���;
%macro hash(str,len);
	%local a;
	%if &str= %then %let str=%str();
	%if &len= %then %let len=20;
	%let a=%binStrToBase32(%sysfunc(md5(&str),$binary128.));
	%let a=%substr(&a,1,&len);
	&a.
%mend;

%macro logBreak(info);
	%local w l padLen pad;
	%let w=80;
	%let l=%length(&info);
	%let padLen=%floor(%sysevalf((&w-&l-2)/2));
	%let pad=%repeat(%str(-),&padLen);
	%put &pad &info &pad;
%mend;

%macro unicodecParen(v);
	%sysfunc(unicodec(&v,PAREN))
%mend;

* �ж�s�Ƿ��ǰ�����������;
%macro inBracket(s);
	%local ns res l r res;
	%let ns=%str(%strip(&s));
	%let ns=%unicodecParen(&ns);
	%let l=%subString(&ns,2,6);
	%let r=%subString(&ns,-6,-2);
	%let res=0;
	%if &l=u0028 and &r=u0029 %then %let res=1;
	&res.
%mend;

* ȥ��������һ�������;
* ��������һ��Ϊ��ƥ������ʱ����;
%macro dequote(s);
	%local len ss ns l r res;
	%let ss=%str(%strip(&s));
	%let len=%length(%str(&ss));
	%let ns=%unicodecParen(&ss); 
	%let l=%subString(&ns,2,6);
	%let r=%subString(&ns,-6,-2);
	%let res=%str();
	%if (&l=u0022 and &r=u0022) or (&l=u0027 and &r=u0027) %then %do;
		%if &len>2 %then %let res=%subString(&ss,2,-2);
		%else %if &len=2 %then %let res=%str();
		%else %error(dequote: Unmatched quote!);
	%end;
	%else %if &l^=u0022 and &r^=u0022 and &l^=u0027 and &r^=u0027 %then %let res=&ss;
	%else %error(dequote: Unmatched quote!);
	&res.
%mend;

* ȥ��������һ�������;
* ��������һ��Ϊ��ƥ������ʱ����;
%macro debracket(s);
	%local len ss ns l r res;
	%let ss=%str(%strip(&s));
	%let len=%length(%str(&ss));
	%let ns=%unicodecParen(&ss);
	%let l=%subString(%str(&ns),2,6);
	%let r=%subString(%str(&ns),-6,-2);
	%let res=%str();
	%if &l=u0028 and &r=u0029 %then %do;
		%if &len>2 %then %let res=%subString(&ss,2,-2);
		%else %if &len=2 %then %let res=%str();
		%else %error(debracket: Unmatched bracket!);
	%end;
	%else %if &l^=u0028 and &r^=u0029 %then %let res=&ss;
	%else %error(debracket: Unmatched debracket!);
	&res.
%mend;
* -------------------------------------------------;
* ---------------- Vars functions -----------------;
* -------------------------------------------------;

* ---- function %sasVarsToSql ----;
* ��sas��ʽ�ı����б�ת��Ϊsql��ʽ;
* a b c -> a,b,c;
%macro sasVarsToSql(vars);
	%local res;
	%if &vars= %then %let res=%str();
	%else %let res=%compress(%do_over(values=&vars,phrase=?,between=comma));
	&res.
%mend;

* ---- function %sqlVarsToSas ----;
* ��sql��ʽ�ı����б�ת��Ϊsas��ʽ;
* a,b,c -> a b c;
%macro sqlVarsToSas/parmbuff;
	%local s res;
	%let s=%substring(&syspbuff,2,-2);
	%if &s= %then %let res=%str();
	%else %let res=%do_over(values=%quote(&s),phrase=?,delim=%str(,));
	&res.
%mend;

%macro varsCount(vars=,res=);
	%if &res= %then %error(varsCount: The result macro variable is empty!);
	%let &res=0;
	%if &vars= %then %return;
	%local arrayVars varsA;
	%let arrayVars=%arrayVars(name=varsA,values=&vars);
	%local &arrayVars;
	%array(varsA,values=&vars);
	%let &res=&varsAN;
%mend;

%macro varsFirst(vars=,res=);
	%if &res= %then %error(varsCount: The result macro variable is empty!);
	%let &res=%str();
	%if &vars= %then %return;
	%local arrayVars varsA;
	%let arrayVars=%arrayVars(name=varsA,values=&vars);
	%local &arrayVars;%array(varsA,values=&vars);
	%let &res=&varsA1;
%mend;

%macro varsAnd(a=,b=,caseSensitive=,res=);
	%if &res= %then %error(varsAnd: The result macro variable is empty!);
	%let &res=%str();
	%if &a= or &b= %then %return;
	%if &caseSensitive= or &caseSensitive=0 %then %do;
		%let a=%upcase(&a);
		%let b=%upcase(&b);
	%end;
	%local arrayVars resVars i;
	%let resVars=%str(); 
	%let arrayVars=%arrayVars(name=varsA,values=&a);
	%local &arrayVars;
	%array(varsA,values=&a);
	%do i=1 %to &varsAN;
		%if %index(%str( &b ),%str( &&varsA&i ))>0 %then %let resVars=%str(&resVars &&varsA&i);
	%end;
	%let &res=%strip(%str(&resVars));
%mend;
%macro varsOr(a=,b=,caseSensitive=,res=);
	%if &res= %then %error(varsOr: The result macro variable is empty!);
	%let &res=%str();
	%if &a= or &b= %then %do;
		%let &res=%strip(%str(&a &b));
		%return;
	%end;
	
	%if &caseSensitive= or &caseSensitive=0 %then %do;
		%let a=%upcase(&a);
		%let b=%upcase(&b);
	%end;
	
	%local arrayVars resVars i;
	%let resVars=&a; 
	%let arrayVars=%arrayVars(name=varsB,values=%str(&b));
	%local &arrayVars;%array(varsB,values=%str(&b));
	
	%do i=1 %to &varsBN;
		%if %index(%str( &a ),%str( &&varsB&i ))=0 %then %let resVars=%str(&resVars &&varsB&i);
	%end;
	%let &res=%strip(%str(&resVars));
%mend;

%macro varsSub(a=,b=,caseSensitive=,res=);
	%if &res= %then %error(varsSub: The result macro variable is empty!);
	%let &res=%str();
	%if &a= %then %do;
		%let &res=%str();
		%return;
	%end;
	%if &b= %then %do;
		%let &res=%strip(&a);
		%return;
	%end;
	%if &caseSensitive= or &caseSensitive=0 %then %do;
		%let a=%upcase(&a);
		%let b=%upcase(&b);
	%end;
	%local arrayVars resVars i;
	%let resVars=%str(); 
	%let arrayVars=%arrayVars(name=varsA,values=&a);
	%local &arrayVars;
	%array(varsA,values=&a);
	%do i=1 %to &varsAN;
		%if %index(%str( &b ),%str( &&varsA&i ))=0 %then %let resVars=%str(&resVars &&varsA&i);
	%end;
	%let &res=%strip(%str(&resVars));
%mend;


%macro varsIn(source=,target=,res=,caseSensitive=);
	%if &res= %then %error(varsIn: The result macro variable is empty!);
	%let &res=0;
	%if &target= or &source= %then %return;
	%if &caseSensitive= or &caseSensitive=0 %then %do;
		%let source=%upcase(&source);
		%let target=%upcase(&target);
	%end;
	%if %index(%str( &source ),%str( &target ))=0 %then %return;
	%let &res=1;
%mend;

%macro varsEqual(a=,b=,caseSensitive=,res=);
	%local r1 r2;
	%let &res=0;
	%if &caseSensitive= or &caseSensitive=0 %then %do;
		%let a=%upcase(&a);
		%let b=%upcase(&b);
	%end;
	%varsSub(a=&a,b=&b,res=r1);
	%varsSub(a=&b,b=&a,res=r2);
	%if &r1= and &r2= %then %let &res=1;
%mend;

%macro varsUnique(vars=,caseSensitive=,res=);
	%local count var uniqueVars resultVars;

	%if &caseSensitive= or &caseSensitive=0 %then %do;
		%let vars=%upcase(&vars);
	%end;

	%local arrayVars;%let arrayVars=%arrayVars(name=arrayVars,values=&vars);
	%local &arrayVars;%array(arrayVars,values=&vars);

	%let uniqueVars=%str();
	%let resultVars=%str();
	%do i=1 %to &arrayVarsN;
		%let var=%strip(&&arrayVars&i);
		%let count=%countStr(&uniqueVars,%str( &var ));
		%if &count=0 %then %do;
			%let uniqueVars=%str(&uniqueVars &var );
			%let resultVars=%str(&resultVars &var);
		%end;
	%end;
	%let &res=%strip(&resultVars);
%mend;

* -------------------------------------------------;
* ---------------- Vars2 functions --------------;
* -------------------------------------------------;

%macro removeDupBlanks(s);
	%local l1 l2;
	%let l1=0;
	%let l2=1;
	%if %length(%str(&s))=0 %then %let res=%str();
	%do %while (&l1^=&l2);
		%let l1=%length(%str(&s));
		%let s=%tranwrd(%str(&s),%str(  ),%str( ));
		%let l2=%length(%str(&s));
	%end;
	&s.
%mend;

* -------------------------------------------------;
* ---------------- Assigns functions --------------;
* -------------------------------------------------;
%macro getAssignVar(a);
	%local i res;
	%let i=%index(&a,%str(=));
	%if &i=0 %then %error(getAssignVar: No assign!);
	%let res=%subString(&a,1,%eval(&i-1));
	&res.
%mend;

%macro getAssignValue(a);
	%local i res;
	%let i=%index(&a,%str(=));
	%if &i=0 %then %error(getAssignValue: No assign!);
	%let res=%subString(&a,%eval(&i+1));
	&res.
%mend;

* ����assigns�е�vars;
* input;
*	assings			��ֵ��������%str������;
*	caseSensitive	�Ƿ�����vars�Ĵ�Сд;

%macro assignsVars(assigns=,caseSensitive=,res=);
	%local eqPos var substr nextBlankPos nextSubstr;
	%local tres;%let tres=%createTempVar;%local &tres;
	%if %length(&assigns)=0 or %refExist(&res)=0 %then %error(assignsVars: Required param is empty!);
	%if &caseSensitive= %then %let caseSensitive=0;
	%let &res=%str();
	%if %length(&assigns)=0 %then %return;
	%let assigns=%removeDupBlanks(&assigns);
	%let eqPos=%index(%str(&assigns),%str(=));%if &eqPos=0 %then %return;
	%let var=%subString(%str(&assigns),1,%eval(&eqPos-1));%if %length(&var)=0 %then %return;
	%let &res=%str(&var);
	%let substr=%subString(%str(&assigns),%eval(&eqPos+1));%if %length(&substr)=0 %then %return;
	%let nextBlankPos=%index(%str(&substr),%str( ));%if &nextBlankPos=0 %then %return;
	%let nextSubstr=%subString(%str(&substr),%eval(&nextBlankPos+1));%if %length(&nextSubstr)=0 %then %return;
	%assignsVars(assigns=%str(&nextSubstr),caseSensitive=&caseSensitive,res=&tres);
	%varsUnique(vars=%str(&var &&&tres),caseSensitive=&caseSensitive,res=&tres);
	%let &res=&&&tres;
%mend;

* ����assigns��ָ��var��ֵ;
*	assings			��ֵ��������%str������;
*	var				ָ��var����;
*	caseSensitive	�Ƿ�����vars�Ĵ�Сд;

%macro assignsFind(assigns=,var=,caseSensitive=,res=);
	%local startPos endPos assign substr a;
	%if &var= or %refExist(&res)=0 %then %error(assignsFind: Required param is empty!);
	%let &res=%str();
	%if &caseSensitive= %then %let caseSensitive=0;
	%if %length(&assigns)=0 %then %return;
	%let assigns=%removeDupBlanks(&assigns);
	%if &caseSensitive=0 %then %let a=%upcase(%str(&assigns));
	%else %let a=%str(&assigns);
	%let startPos=%index(%str( &a),%str( &var=));%if &startPos=0 %then %return;
	%let substr=%substr(%str(&assigns),&startPos);%if %length(%str(&substr))=0 %then %return;
	%let endPos=%index(%str(&substr ),%str( ));
	%let assign=%subString(%str(&substr ),1,%eval(&endPos-1));
	%let &res=%getAssignValue(%str(&assign));

%mend;


* -------------------------------------------------;
* ---------------- Time&Date functions ------------;
* -------------------------------------------------;

* ----- function timestamp -----;
* ��ȡ��ǰʱ�������ʽΪ��yyyymmddThhmmss;
%macro timestamp;
	%strip(%sysfunc(strip(%sysfunc(datetime(),B8601DT.))))
%mend;

* ----- function htimestamp -----;
* ��ȡ��ǰʱ�������ʽΪ��yyyymmddThhmmss;
%macro htimestamp;
	%strip(%sysfunc(strip(%sysfunc(datetime(),B8601DT.3))))
%mend;

* ---- function dsToDv ----;
* ��yyyymmdd��ʽ��ʱ��ת��Ϊdatetime����ֵ;
%macro dsToDtv(ds);
	%strip(%sysfunc(inputn(&ds.000000,B8601DJ18.)))
%mend;

* ---- function dtvToDs ----;
* ��datetime����ֵת��Ϊyyyymmdd��ʽ��ʱ��;
%macro dtvToDs(dtv);
	%strip(%sysfunc(putn(&dtv,B8601DN.)))
%mend;

* ----- function intnx -----;
* ʱ������������װintnx;
%macro intnx(interval,dtv,inc,align);
	%local res;
	%if &align= %then %let align=same;
	%let res=%strip(%sysfunc(intnx(&interval,&dtv,&inc,&align)));
	&res.
%mend;

* ----- function dsIntnx -----;
* ʱ����������yyyymmdd��ʽ�����������;
%macro dsIntnx(interval,ds,inc,align);
	%local res;
	%if &align= %then %let align=same;
	%let res=%dtvToDs(%intnx(&interval,%dsToDtv(&ds),&inc,&align));
	&res.
%mend;


%macro intck(interval,dtv1,dtv2,method);
	%local res;
	%let res=%strip(%sysfunc(intck(&interval,&dtv1,&dtv2,&method)));
	&res.
%mend;

%macro dsIntck(interval,ds1,ds2,method);
	%local res dtv1 dtv2;
	%if &method= %then %let method=d;
	%let dtv1=%dsToDtv(&ds1);
	%let dtv2=%dsToDtv(&ds2);
	%put &dtv1 &dtv2;
	%intck(&interval,&dtv1,&dtv2,&method);
%mend;

* ----- function dsLatest -----;
* ����������;
%macro dsLatest(ds1,ds2);
	%local res dtv1 dtv2 dtvLatest;
	%if &ds1= and &ds2= %then %error(dsLatest: Required param is empty!);
	%if &ds1= %then %let res=&ds2;
	%else %if &ds2= %then %let res=&ds1;
	%else %do;
		%let dtv1=%dsToDtv(&ds1);
		%let dtv2=%dsToDtv(&ds2);
		%let dtvLatest=%max(&dtv1,&dtv2);
		%let res=%dtvToDs(&dtvLatest);
	%end;
	&res.
%mend;


* -------------------------------------------------;
* ---------------- fileIO functions ---------------;
* -------------------------------------------------;

* ----- function getPath -----;
* ���ش��뵱ǰ����·���ַ������磺 C:\a\b\c\;
* ���ṩ����filename���򷵻ذ����ļ���filename������·�����磺C:\a\b\c\abc.txt;
%macro getPath;
	%local res;
	%let res=%sysfunc(tranwrd(%sysget(SAS_EXECFILEPATH),%sysget(SAS_EXECFILENAME),));
	&res.
%mend;

* ----- function checkFolder -----;
* ���ָ��·���Ƿ���ڣ�����������������ļ���;
%macro checkFolder(dir) ; 
   	%LOCAL rc fileref; 
	%if &dir= %then %let rc=1;
   	%let rc = %sysfunc(filename(fileref,&dir)); 
   	%if %sysfunc(fexist(&fileref)) %then %let rc=1;    
  	%else %do;
		X md &dir ; 
   	%end;
%mend;

* ----- function folderExist -----;
* ���ָ��·���Ƿ����;
%macro folderExist(dir) ; 
   	%local rc fileref;
	%let rc=0;
	%if &dir= %then %let rc=0;
	%else %do;
		%let rc = %sysfunc(filename(fileref,&dir)) ; 
   		%let rc = %sysfunc(fexist(&fileref));	
	%end; 
   	&rc.
%mend;

* ---- function removeFolder ----;
* ɾ��ָ���ļ��м����е���������;
%macro removeFolder(path);
	X rd /s /q "&path";
%mend;


* -------------------------------------------------;
* ---------------- Source functions ---------------;
* -------------------------------------------------;

* ----- function includeLocal -----;
* include����sas�ļ�,file�����.sas��չ��;
%macro includeLocal(file);
	%local p;
	%let p=%getPath;
	%include "&p.&file";
%mend;


* -------------------------------------------------;
* ---------------- Format functions ----------------;
* -------------------------------------------------;

%macro inputn(v,fmt);
	%sysfunc(inputn(&v,&fmt))
%mend;

%macro inputc(v,fmt);
	%sysfunc(inputc(&v,&fmt))
%mend;

%macro putn(v,fmt);
	%sysfunc(putn(&v,&fmt))
%mend;

%macro putc(v,fmt);
	%sysfunc(putc(&v,&fmt))
%mend;

%macro isStrFormat(fmt);
	%local res;
	%let res=0;
	%if %subString(&fmt,1,1)=%str($) %then %let res=1;
	&res.
%mend;
* -- macro formatExist --;
* input;
*	lib;
*	path;
*	fmt;
*	isInformat;
*	res;
%macro formatExist(lib=,path=,fmt=,isInformat=,res=);
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local fmtSuffix fmtName fmtType tempLib;

	%let tempLib=0;
	%let fmtSuffix=%str();
	%let fmtName=%tranwrd(&fmt,%str(.),%str());
	%let fmtType=FORMAT;
	%let &res=0;
	%if &isInformat= %then %let isInformat=0;
	%if &path ne %then %do;
		%if %folderExist(&path)=0 %then %return;
		%importTempLib(&path,&tres);%let lib=&&&tres;
		%let tempLib=1;
	%end;
	%else %if &lib= %then %error(&macro: PATH and LIB cannot be empty at the same time!);
	
	%if %subString(&fmt,1,1)=%str($) %then %do;
		%let fmtSuffix=C;
		%let fmtName=%subString(&fmt,2);
	%end;
	%if &isInformat=1 %then %let fmtType=INFMT;
	%else %let fmtType=FORMAT;
	%if %sysfunc(cexist(&lib..formats.&fmtName..&fmtType.&fmtSuffix.))=1 %then %let &res=1;
	%if &tempLib=1 %then %dropLib(&lib);
%mend;

%macro importFormatLib(path=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%if &path= or %refExist(&res)=0 %then %error(&macro: Required param is empty! param=&syspbuff);
	%importTempLib(&path,&tres);%let lib=&&&tres;
	
	%addOption(fmtsearch,&lib);
	%let &res=&lib;
%mend;

%macro dropFormatLib(lib);
	%deleteOption(fmtsearch,&lib);
	%dropLib(&lib);
%mend;



* -------------------------------------------------;
* ---------------- ds basic functions -------------;
* -------------------------------------------------;

* ----- function getTablename -----;
* ��lib.table��ʽ����ȡtable����;

%macro getTableName(f);
	%local i;
	%local res;
	%let i= %index(&f,.);
	%if &i>0 %then %let res=%substr(&f,&i+1);
	%else %let res=&f;
	%let res=%upcase(&res);
	&res.
%mend;

* ----- function getLibname -----;
* ��lib.table��ʽ����ȡlib����;

%macro getLibName(f);
	%local i;
	%local res;
	%let i= %index(&f,.);
	%if &i>0 %then %let res=%substr(&f,1,&i-1);
	%else %let res=work;
	%let res=%upcase(&res);
	&res.
%mend getLibName;

* ----- function createTempDs -----;
* ������ʱ��ds���ƣ�Ĭ��libΪwork;
%macro createTempDs(lib);
	%local ds;
	%if &lib= %then %let lib=work;
	%let ds=&lib..%genId(prefix=DS,len=16);
	&ds.
%mend;

* ----- function createTempVar -----;
* ������ʱ�ı�������;
%macro createTempVar(prefix);
	%if &prefix= %then %let prefix=T;
	%genId(prefix=&prefix,len=16)
%mend;

* ---- function hashLib ----;
* ����ָ��·��/�ַ����Ĺ�ϣֵ��Ϊtable����;
%macro hashLib(path);
	L%substr(%sysfunc(md5(&path),hex32.),1,7)
%mend;

* ---- function hashTable ----;
* ����ָ��·��/�ַ����Ĺ�ϣֵ��Ϊtable����;
%macro hashTable(path);
	T%substr(%sysfunc(md5(&path),hex32.),1,30)
%mend;

* -------------------------------------------------;
* ---------------- ds info functions -------------;
* -------------------------------------------------;

* ----- function dsExist -----;
* ����ds�Ƿ����,����Ϊ1������Ϊ0;
%macro dsExist(ds);
	%local res;
	%if &ds= %then %let res=0;
	%else %let res=%sysfunc(exist(&ds));
	&res.
%mend;

* ---- function getLibPath -----;
* ����lib��·��;
%macro getLibPath(lib);
	%str(%sysfunc(pathname(&lib)))
%mend;

* ----- function dsEqual -----;
* �ж�����ds�Ƿ��Ӧ��ͬ���ļ�;
%macro dsEqual(a,b) /parmbuff;
	%local res la lb ta tb pa pb;
	%let res=1;
	%if &a= or &b= %then %error(dsEqual: The required param is empty! param=&syspbuff);
	%let la=%getLibName(&a);
	%let lb=%getLibName(&b);
	%let ta=%getTableName(&a);
	%let tb=%getTableName(&b);
	%let pa=%getLibPath(&la);
	%let pb=%getLibPath(&lb);
	%if &pa= or &pb= %then %error(dsEqual: The lib path is empty! param=&syspbuff);
	%if &pa ne &pb %then %let res=0;
	%else %if &ta ne &tb %then %let res=0;
	&res.
%mend;

* ----- function dsVarExist -----;
* ����ds��ָ�������Ƿ����,����Ϊ1������Ϊ0;
%macro dsVarExist(ds,var);
	%local res dsid varnum rc;
	%if %dsExist(&ds)=0 %then %let res=0;
	%else %do;
		%let dsid=%sysfunc(open(&ds,i));
		%if &dsid=0 %then %let res=0;
		%else %do;
			%let varnum=%sysfunc(varnum(&dsid,&var));
			%if	&varnum=0 %then %let res=0;
			%else %let res=1;
			%let rc=%sysfunc(close(&dsid));
		%end;
	%end;
	&res.
%mend;

* ----- function getDsObs -----;
* ����ds�ı�������;
%macro getDsObs(ds=,res=,where=);
	%local whereStr dsid anobs whstmt err;
	%let whereStr=%str();
	%if %sysfunc(exist(&ds))=0 %then %do;
		%let &res=0;
		%return;
	%end;
	%if &where= %then %do;
		%let dsid = %sysfunc(open(&ds., IS));
		%let anobs = %sysfunc(attrn(&dsid, ANOBS));
		%let whstmt = %sysfunc(attrn(&dsid, WHSTMT));
		%if &anobs=1 and &whstmt=0 %then %do;
			%let &res=%sysfunc(attrn(&dsid, NLOBS));
			%let err=%sysfunc(close(&dsid));
			%return;
		%end;
		%else %do;
			%let err=%sysfunc(close(&dsid));
		%end;
	%end;
	%else %let whereStr=where &where;
	proc sql noprint;
		select count(*) into :&res from &ds &whereStr;
	quit;
%mend;

* ----- function getDsVarLen -----;
* ����ds�ı�������;
%macro getDsVarLen(ds=,var=,res=);
	%local dsid varnum rc;
	%let &res=0;
	%let dsid=%sysfunc(open(&ds,i));
	%if &dsid=0 %then %do;
		%return;
	%end;
	%let varnum=%sysfunc(varnum(&dsid,&var));
	%if	&varnum=0 %then %do;
		%let rc=%sysfunc(close(&dsid));
		%return;
	%end;
	%let &res=%strip(%sysfunc(varlen(&dsid,&varnum)));
    %let rc=%sysfunc(close(&dsid));
%mend;

* ----- function getDsVarCount -----;
* ����ds�ı�����var�����Ĺ۲�����;
%macro getDsVarCount(ds=,var=,res=,noNull=);
	%let &res=%str();
	%if %dsVarExist(&ds,&var)=0 %then %error(getDsVarCount: No dataset!);
	%if &noNull= or &noNull=1 %then %do;
		proc sql noprint;
			select count(&var) into :&res from &ds;
		quit; 
	%end;
	%else %do;
		%getDsObs(ds=&ds,var=&var,res=&res);
	%end;
%mend;

* ----- function getDsVarMax -----;
* ����ds�ı�����var���������ֵ��missing value���������;
%macro getDsVarMax(ds=,var=,res=);
	%let &res=%str();
	%if %dsVarExist(&ds,&var)=0 %then %error(getDsVarCount: No dataset! ds=&data);
	proc sql noprint;
		select max(&var) into :&res from &ds;
	quit; 
%mend;

* ----- function getDsVarMin -----;
* ����ds�ı�����var��������Сֵ��missing value���������;
* ע�⣺��Ҫʹ��var=.��ʽ���ַ���������ȱʧֵ���ᵼ��minֵΪ.;
%macro getDsVarMin(ds=,var=,res=);
	%let &res=%str();
	%if %dsVarExist(&ds,&var)=0 %then %error(getDsVarCount: No dataset! ds=&data);
	proc sql noprint;
		select min(&var) into :&res from &ds;
	quit; 
%mend;

* ----- function getDsVarList -----;
* ����ds�ı����б�,������data step�е�_all_;
%macro getDsVarList(ds=,res=);
	%local temp;
	%let &res=%str();
	%if %sysfunc(exist(&ds))=0 %then %return;
	%let temp=%createTempDs;
	proc contents data=&ds out=&temp noprint varnum;
	run;
	proc sql noprint;
		select name into :&res separated by ' ' from &temp order by varnum;
	quit; 
	%dropDs(&temp);
%mend;

* ----- function getDsNumVarList -----;
* ����ds��num�����б�,������data step�е�_numeric_;
%macro getDsNumVarList(ds=,res=);
	%local temp;
	%let &res=%str();
	%if %sysfunc(exist(&ds))=0 %then %return;
	%let temp=%createTempDs;
	
	proc contents data=&ds out=&temp noprint varnum;
	run;
	proc sql noprint;
		select name into :&res separated by ' ' from &temp where type=1 order by varnum;
	quit; 
	%dropDs(&temp);
	%let &res=%strip(&&&res);
%mend;

* ----- function getDsStrVarList -----;
* ����ds��string�����б�,������data step�е�_string_;
%macro getDsStrVarList(ds=,res=);
	%local temp;
	%let &res=%str();
	%if %sysfunc(exist(&ds))=0 %then %return;
	%let temp=%createTempDs;
	proc contents data=&ds out=&temp noprint varnum;
	run;
	proc sql noprint;
		select name into :&res separated by ' ' from &temp where type=2 order by varnum ;
	quit;
	%dropDs(&temp);
	%let &res=%strip(&&&res);
%mend;

* ----- function getDsVarNum -----;
* ����ds��ָ����������ţ�0��ʾvar������;
%macro getDsVarNum(ds=,var=,res=);
	%local dsid rc;
	%let &res=0;
	%if not %dsExist(&ds) %then %return;
	%let dsid=%sysfunc(open(&ds,i));
	%if &dsid=0 %then %do;
		%let rc=%sysfunc(close(&dsid));
		%return;
	%end;
	%let &res=%sysfunc(varnum(&dsid,&var));
	%let rc=%sysfunc(close(&dsid));
%mend;

* ----- function getDsVarType -----;
* ����ds��ָ���������ͣ����1��ʾ��ֵ��2��ʾ�ַ�,0��ʾdata������;
%macro getDsVarType(ds=,var=,res=);
	%local temp varnum;
	%let &res=0;
	%getDsVarNum(ds=&ds,var=&var,res=varnum);
	%if &varnum=0 %then %return;
	%let temp=%createTempDs;
	proc contents data=&ds out=&temp varnum noprint;
	run;
	proc sql noprint;
		select type into :&res from &temp where varnum=&varnum;
	quit;
	%dropDs(&temp);
	%let &res=%strip(&&&res);
%mend;

%macro getDsList(lib=,res=);
%mend;

* -------------------------------------------------;
* ---------------- ds operation functions ---------;
* -------------------------------------------------;

* ----- function importLocalLib -----;
* ���뵱ǰ�����ĵ������ļ��У������ļ��У������������ݼ�
* libname�������ļ���ӳ�䵽�����ƣ�����ӳ�䵽Ĭ�ϵ�����local;
* subdir��ӳ�䵽�������ļ���;
%macro importLocalLib(libname,subdir);
	%local path fullpath;
	%if &libname= %then %let libname=local;
	%let path=%getPath;
	%if &subdir= %then %let fullpath=&path;
	%else %let fullpath=%sysfunc(cats(&path,&subdir));
	options dlcreatedir;
	libname &libname "&fullpath";
%mend;

* ----- function importTempLib -----;
* ����ָ���ļ��������������ݼ�;
* path��lib·��;
* name������lib���Ƶĺ��������;
%macro importTempLib(path,name);
	%local srcLib;
	%let &name=%str();
	%if not %folderExist(&path) %then %error(importTempLib: Path does not exist! path=&path);
	%let srcLib=%genId(prefix=L,len=8);
	libname &srcLib "&path";
	%let &name=&srcLib;
%mend;

* ----- function createTempLib -----;
* ����һ����ʱlib;
* path��lib·��;
* name������lib���Ƶĺ��������;
%macro createTempLib(res) /parmbuff;
	%local tempFolder path tempPath;
	%let tempFolder=%genId(prefix=TEMPLIB,len=30);
	%let path=%getPath;
	%let tempPath=&path.&tempFolder.\;
	%checkFolder(&tempPath);
	%importTempLib(&tempPath,&res);
%mend;

* ----- function dropLocalLib -----;
* ɾ��local�ֶ��뵱ǰ�ļ��еĹ�����ϵ;
%macro dropLocalLib;
	libname local clear;
%mend;

* ----- function dropLib -----;
* ɾ��ָ��lib������;
%macro dropLib(libname);
	%if &libname= %then %return;
	libname &libname clear;
%mend;

* ----- function clearLib -----;
* ɾ��ָ��lib���������ݣ�����Ϊ��ʱ������ʱlib��work��;
* ע�⣺ֻɾ��lib���ݣ��ļ��С�lib���ñ���;
%macro clearLib(libname);
	%if &libname= %then %do; 
		proc datasets lib=work kill memtype=all nolist nowarn;run;quit;
	%end;
	%else %do;
		proc datasets lib=&libname kill memtype=all nolist nowarn;run;quit;
	%end;
%mend;

* ----- function dropTempLib -----;
* ɾ��ָ��lib���������ݣ�ɾ�����á�ɾ���ļ���;
%macro dropTempLib(lib) /parmbuff;
	%local path;
	%if &lib= %then %error(dropTempLib: Required param is empty! param=&syspbuff);
	%clearLib(&lib);
	%let path=%getLibPath(&lib);
	%dropLib(&lib);
	X rd /s /q "&path"; 
%mend;


* ----- function dropDs/clearTable/dropTable -----;
* ɾ��ָ��dataset;
%macro dropDs(ds);
	%local i d lib table;
	%if &ds= %then %return;
	%local arrayVars;%let arrayVars=%arrayVars(name=dss,values=&ds);%local &arrayVars;%array(dss,values=&ds);
	%do i=1 %to &dssN;
		%let d=&&dss&i;
		%if %dsExist(&d)=1 %then %do;
			%let lib=%getLibName(&d);
			%let table=%getTableName(&d);
			proc datasets lib=&lib memtype=data nolist nowarn;
				delete &table;
			run;quit;
		%end;
	%end;
%mend;


%macro clearTable(ds);
	%dropDs(&ds);	
%mend;

%macro dropTable(ds);
	%dropDs(&ds);	
%mend;

%macro dropTempDsByPrefix(prefix);
	%let lib=%getLibName(&prefix);
	%if &lib ne work %then %error(dropTempDsByPrefix: Only temp ds in library work can be dropped!);
	%getDsList(lib=work,prefix=&prefix,res=&tres);%let dsList=&&&tres;
	%dropDs(&dsList);
%mend;

* ----- function dropView/clearView -----;
* ɾ��ָ��view;

%macro dropView(ds);
	%local i d lib table;
	%if &ds= %then %return;
	%local arrayVars;%let arrayVars=%arrayVars(name=dss,values=&ds);%local &arrayVars;%array(dss,values=&ds);
	%do i=1 %to &dssN;
		%let d=&&dss&i;
		%if %dsExist(&d)=1 %then %do;
			%let lib=%getLibName(&d);
			%let table=%getTableName(&d);
			proc datasets lib=&lib memtype=view nolist nowarn;
				delete &table;
			run;quit;
		%end;
	%end;
%mend;


%macro clearView(ds);
	%dropView(&ds);
%mend;


* -------------------------------------------------;
* ---------------- Random functions -------------;
* -------------------------------------------------;

* ����ID;
* 	prefix 	idǰ׺;
*	len		id�ܳ���;
* id������ɣ���·�����������ʱ�乲ͬ�����ַ���������md5��ת��Ϊbase32��ʽ;
%macro genId(prefix=,len=);
	%local t p r s id l rLen;
	%if &len= %then %let len=30;
	%let rLen=%eval(&len-%length(&prefix));
	%if &rLen<0 %then %error(genId: Prefix is too long! prefix=&prefix);
	%let id=%str();
	%let l=0;
	%do %while(&l<&rLen);
		%let t=%htimestamp;
		%let p=%getPath;
		%let r=%random;
		%let s=&t.&p.&r;
		%let id=&id.%md5base32(&s);
		%let l=%length(&id);
	%end;
	%let id=%substr(&id,1,&rLen);
	&prefix.&id.
%mend;


* -------------------------------------------------;
* ---------------- FCMP functions -----------------;
* -------------------------------------------------;

* ----- function importFuncs -----;
* ����ָ���ļ���λ�õĺ���;
* ����ָ���ļ���·����dataset�������ļ���·�����䣬Ĭ��dataset=funcs
* ���ڵ���ĺ���������lib���ƣ��������ͳһʹ��md5(dirpath+dataset����Ϊlib����;
%macro importFuncs(path=,table=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%if %refExist(&res)=0 %then %error(&macro: Required param is empty! param=&syspbuff);
	%if &path= %then %let path=%getPath;
	%if &table= %then %let table=funcs;
	%importTempLib(&path,&tres);%let lib=&&&tres;
	%addOption(cmplib,&lib..&table);
	%let &res=&lib..&table;
%mend;

%macro dropFuncs(ds);
	%local lib;
	%deleteOption(cmplib,&ds);
	%let lib=%getLibName(&ds);
	%if %upcase(&lib)^=WORK %then %dropLib(&lib);
%mend;

* ----- function clearFuncs -----;
* ������ĺ���/����cmplib·��;
* sas��fcmp������cmplib�������ܴ�������·������cmplib�У���ʵ���ϸ�lib�Ѿ�ʧЧ����������������ϵͳ�ᱨ������Ӱ�����������������;
%macro clearFuncs;
	options cmplib=();
%mend;

* ----- function clearCmplib -----;
* ��system options��cmplib�ÿ�;
%macro clearCmplib;
	options cmplib=();
%mend;


* -------------------------------------------------;
* ---------------- Options functions --------------;
* -------------------------------------------------;

* option��ʽ;

%macro displayOption(op);
	proc options option=&op define value lognumberformat;
	run;
%mend;

* ��ȡָ��option;
* ���������ŵ�option���Զ�������ȥ��;
%macro getOption(op);
	%local opValue res;
	%let opValue=%str(%sysfunc(getoption(&op)));
	%if %length(&opValue)=0 %then %let res=%str();
	%else %if %inBracket(&opValue)=1 %then %let res=%debracket(&opValue);
	%else %let res=&opValue;
	&res.
%mend;

* ����ָ��option;
%macro setOption(op,opValue);
	option &op=&opValue;
%mend;

* ��ָ��option������һ��value���ʺ�������ֵ�ģ���ʽΪ(opv1 opv2 ...)��ʽ��option;
* �����ӵ�ֵ��������ĩβ;
%macro addOption(op,value);
	%local tres;%let tres=%createTempVar;%local &tres;
	%local oldOpValue newOpValue;
	%let oldOpValue=%getOption(&op);	
	%varsOr(a=&oldOpValue,b=&value,res=&tres);%let newOpValue=&&&tres;
	%setOption(&op,%str((&newOpValue)));
%mend;

* ��ָ��option��ɾ��һ��value���ʺ�������ֵ��option;
%macro deleteOption(op,value);
	%local tres;%let tres=%createTempVar;%local &tres;
	%local oldOpValue newOpValue;
	%let oldOpValue=%getOption(&op);
	%varsSub(a=&oldOpValue,b=&value,res=&tres);%let newOpValue=&&&tres;
	%*put old=&oldOpValue new=&newOpValue value=&value;
	%setOption(&op,%str((&newOpValue)));
%mend;

* -------------------------------------------------;
* ---------------- ODS functions ------------------;
* -------------------------------------------------;

* ----- function clearOutput -----;
* ��������ҳ��,ָ�����html�ļ���ODS HTML��;
%macro clearOutput(path=,file=,noTimestamp=);
	%local p t f;
	ods html close;
	ods graphics off;
	%let p=%getPath;
	%let t=_%timestamp;
	%let f=result;
	%if &noTimestamp ne %then %let t=%str();
	%if &file ne %then %let f=&file;
	%if &path ne %then %let p=&path;
	%checkFolder(&p);
	%checkFolder(&path.img\);
	ods graphics on;
	ods html body="&f.&t..html" path="&p" gpath = "&path.img\" (url="img/");
%mend;

* -------------------------------------------------;
* ---------------- Macro variable functions -------;
* -------------------------------------------------;

* ---- function getInvoker ----;
* ���ص��ú������;
%macro getInvoker;
	%sysmexecname(%sysmexecdepth - 2)
%mend;

* ---- function getSelf ----;
* �������������;
%macro getSelf;
	%sysmexecname(%sysmexecdepth - 1)
%mend;

* ----- function getMacroVars -----;
* ��ȡ������б�;
*	prefix			ǰ׺;
*	res				���ر���;
* 	scope			�����scope,��ѡ�������;
*						GLOBAL	����ȫ�ֱ���������SYS_��ͷ��;
*						LOCAL		���б��ر��������ر���ָ���ô�macro�ĺ��е�local����;
*						USER		�����û����������з��Զ�����ϵͳ�������Ǳ����������getMacroVars�ж����local������;
*	noDuplicate		�޳��������е��ظ����������Ҫ����USER�У����ܴ��ڶ༶����ͬ�������;
*					ʹ��noDuplicate+USER ���صı����б������û��ڵ��ú��У���ǰ�ɷ���
*	noPrefix		���ر������в�����ǰ׺;

%macro getMacroVars(prefix=,scope=,noPrefix=,noDuplicate=,res=);

	%local prefixStr scopeStr invokerScope l;
	%if &res= %then %error(getMacroVars: No result macro variable!);
	%if &noPrefix= %then %let noPrefix=0;
	%if &noDuplicate= %then %let noDuplicate=1;
	%if &scope= %then %let scope=USER;
	%let scope=%upcase(&scope);
	%if &prefix= %then %do;
		%let prefixStr=%str();
	%end;
	%else %do;
		%let prefix=%upcase(&prefix);
		%let prefixStr=%str(and name like %'&prefix.%%%');
	%end;
	%if &scope=GLOBAL %then %do;
		%let scopeStr=%str(and scope=%'GLOBAL%');
	%end;
	%else %if &scope=USER %then %do;
		%let scopeStr=%str(and scope^=%'AUTOMATIC%');
	%end;
	%else %if &scope=LOCAL %then %do;
		%let invokerScope=%getInvoker;	
		%let scopeStr=%str(and scope=%'&invokerScope.%');
	%end;
	proc sql noprint;
		select name into :&res separated by ' ' from sashelp.vmacro
		where name not like 'SYS_%' and name not like 'SQL%' and scope^='GETMACROVARS' %unquote(&prefixStr) %unquote(&scopeStr);
	quit;
	%*put inGetMacroVariable: &&&res;
	%if &&&res ne %then %do;
		%if &noPrefix %then %do;
			%let l=%length(&prefix);
			%let &res=%tranwrd(%str( &&&res ),%str( &prefix),%str());
		%end;
		%*put inGetMacroVariable noprefix: &&&res;
		%if &noDuplicate %then %do;
			%varsUnique(vars=&&&res,res=&res);
		%end;
		%*put inGetMacroVariable final: &&&res;
	%end;
%mend;

* ----- function createTempGlobal -----;
* ������ʱ��global����;
%macro createTempGlobal(prefix);
	%local var;
	%if &prefix= %then %let prefix=T;
	%let var=%upcase(%genId(prefix=&prefix,len=32));
	%global &var;
	&var.
%mend;


* ----- function dropGlobal/clearGlobal -----;
* ɾ��ָ�����Ƶĺ����;

%macro dropGlobal(name);
	%local temp;
	%if &name= %then %return;
	%let name=%upcase(&name);
	%let temp=%createTempDs;
	proc sql;
		create table &temp as (
			select name 
			from sashelp.vmacro
			where scope="GLOBAL" and name not like 'SYS_' and name="&name"
		);
	quit;
	%if &SQLOBS=0 %then %do;
		%dropDs(&temp);
		%return;
	%end;
	data _null_ ;
  		set &temp;
  		call symdel(name);
	run;
	%dropDs(&temp);
%mend;

%macro clearGlobal(name);
	%dropGlobal(&name);
%mend;

* ----- function dropMacroVars -----;
* ɾ��ָ��ǰ׺�ĺ����;
* ��ǰ׺Ϊ��ʱ��Ϊ���к����;

%macro dropGlobals(prefix);
	%let prefix=%upcase(&prefix);
	%local temp;
	%let temp=%createTempDs;
	proc sql;
		create table &temp as (
			select name 
			from sashelp.vmacro
			where name like "&prefix.%" and scope='GLOBAL'
		);
	quit;
	%if &SQLOBS=0 %then %do;
		%dropDs(&temp);
		%return;
	%end;
	data _null_ ;
  		set &temp;
  		call symdel(name);
	run;
	%dropDs(&temp);
%mend;

%macro clearGlobals(prefix);
	%dropGlobals(&prefix);
%mend;


* -------------------------------------------------;
* ---------------- Workspace functions --------------;
* -------------------------------------------------;

* ----- function clearAll -----;
* ɾ������ȫ�ֺ��������ʱ������;

%macro clearAll;
	%clearGlobals;
	%clearLib;
%mend;


* -------------------------------------------------------------------;
* ---------------- Dataset complex operation functions --------------;
* -------------------------------------------------------------------;

* ----- function quickMap -----;
* ����������֮�������;
* quickMap(A,B,key);
%macro quickMap(T,A,B,keyA,keyB,valueB);
	proc sql;
		create table &T as (
			select x.*,y.&valueB.
			from &A. as x
			left join &B. as y
			on x.&keyA.=y.&keyB.
		);
	quit;
%mend;

* ----- function quickMap -----;
* ����dict�����enum������������Ϣ;
%macro dictMap(T,tableName,colName,enumName);
	proc sql;
		create table &T as 
		(
			select a.*,b.codeDesc as &colName.Desc from &tableName as a
			left join nfcs_con.dict as b
			on (a.&colName.=b.code and b.enum="&enumName")
		);
	quit;
%mend dictMap;

* ----- function addColFix -----;
* Add prefix or suffix to each column name of the target table;
* fixType = 0 | empty : prefix;
* fixType = other : suffix;

%macro addColFix(fixType=,fix=,delimiter=,lib=,table=);
	%local u_lib u_table prefixStr;
	%if %isBlank(&table) %then %let u_lib=WORK;%else %let u_lib = %qupcase(&lib); 
	%let u_table = %qupcase(&table);
	%put &u_lib &u_table;
	%if %isBlank(&fixType) | &fixType=0 %then %do;
		proc sql noprint;
			select cats(name,'=',"&fix&delimiter",name)
			into :prefixStr separated by ' '
			from dictionary.columns
			where libname="&u_lib" and memname = "&u_table";
		quit;
	%end;
	%else %do;
		proc sql noprint;
				select cats(name,'=',name,"&delimiter&fix")
				into :prefixStr separated by ' '
				from dictionary.columns
				where libname="&u_lib" and memname = "&u_table";
			quit;
	%end;
	proc datasets library=&u_lib nolist;
		modify &u_table;
		rename &prefixStr;
	quit;
%mend;



* ---- function join ----;
* ���ƴ�Ӻ��������ڽ����ָ����Ի�׼������Ϊ���ݣ�����ƴ�ӣ�left join��;
* Ҫ�����б��������ϱ��뽨��������;
%macro join(base=,tables=,key=,out=);
	data &out(index=(&key));
		merge &base &tables;
		by &key; 
	run;
%mend;

* ---- function leftjoin ----;
* input;
*	base;
*	table;
*	key;
*	out;
*	defaultValue		Ĭ��ֵ����ʽΪassings;

%macro leftjoin(base=,table=,key=,out=,defaultValue=)	/parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local macro;%let macro=%getSelf;

	%if &base= or &table= or &key= or &out= %then %error(&macro: Required param is empty! param=&syspbuff);
	
	%local i vars var notExistKeyVars sqlFrom;	

	* Ŀ����������;
	%local arrayVars;%let arrayVars=%arrayVars(name=tableList,values=&table);
	%local &arrayVars;%array(tableList,values=&table);
	
	* Ŀ�������Լ�顢key���������Լ��;
	%do i=1 %to &tableListN;
		%getDsVarList(ds=&&tableList&i,res=&tres);%let vars=&&&tres;
		%if &vars= %then %error(&macro: Target table doesnot exist! table=&&tableList&i);
		%varsSub(a=&key,b=&vars,res=&tres);%let notExistKeyVars=&&&tres;
		%if &notExistKeyVars ne %then %error(&macro: Target key does not exist in table &&tableList&i ! key=&key ,vars in &&tableList&i=&vars);
	%end;
	%put base=&base;

	* �������Ӳ���;
	%let sqlFrom=%str(from &base as table0);
	%do i=1 %to &tableListN;
		%let sqlFrom=&sqlFrom left join &&tableList&i as table&i on (%do_over(values=&key,phrase=%str(table&i..?=table0.?),between=%str( and )) );
	%end;
	proc sql;
		create table &out as (select * &sqlFrom);
	quit;

	%if &defaultValue ne %then %do;
		%assignsVars(assigns=&defaultValue,res=&tres);%let vars=&&&tres;
		%let arrayVars=%arrayVars(name=varList,values=&vars);
		%local &arrayVars;%array(varList,values=&vars);
		data &out;
			set &out;
			%do i=1 %to &varListN;
				%let var=&&varList&i;
				%assignsFind(assigns=&defaultValue,var=&var,res=&tres);%let varDefaultValue=&&&tres;
				if missing(&var) then &var=&varDefaultValue;
			%end;
		run;
	%end;
%mend;

%macro innerJoin(table=,key=,out=) /parmbuff;
	%if &table= or &key= or &out= %then %error(innerJoin: Required param is empty! param=&syspbuff);

	%local tres;%let tres=%createTempVar;%local &tres;

	%local i vars notExistKeyVars sqlFrom;	
	%local arrayVars;%let arrayVars=%arrayVars(name=tableList,values=&table);
	%local &arrayVars;%array(tableList,values=&table);

	* Ŀ����������;
	%if &tableListN<2 %then %error(innerJoin: There must be more than 2 tables!);

	* Ŀ�������Լ�顢key���������Լ��;
	%do i=1 %to &tableListN;
		%getDsVarList(ds=&&tableList&i,res=&tres);%let vars=&&&tres;
		%if &vars= %then %error(innerJoin: Target table doesnot exist! table=&&tableList&i);
		%varsSub(a=&key,b=&vars,res=&tres);%let notExistKeyVars=&&&tres;
		%if &notExistKeyVars ne %then %error(innerJoin: Target key does not exist in table &&tableList&i ! key=&key ,vars in &&tableList&i=&vars);
	%end;

	* �������Ӳ���;
	%let sqlFrom=%str(from &tableList1 as table1);
	%do i=2 %to &tableListN;
		%let sqlFrom=&sqlFrom inner join &&tableList&i as table&i on (%do_over(values=&key,phrase=%str(table&i..?=table1.?),between=%str( and )) );
	%end;
	proc sql;
		create table &out as (select * &sqlFrom);
	quit;
%mend;


* ---- function mleftjoin ----;
* ���leftjoin�����������������ϱ��ƴ��;

%macro mleftjoin(base=,tables=,key=,out=,baseSelect=,tableSelect=);
	%local baseHashName tableHashName tableName baseSelectStr tableSelectStr sqlSelect sqlFrom arrayVars;
	%let baseHashName=%hashTable(&base);
	%if &baseSelect= %then %let baseSelectStr=%str(&baseHashName..*);
	%else %let baseSelectStr=%do_over(values=&baseSelect,phrase=&baseHashName..?,between=comma);
	%let sqlFrom=%str(from &base as &baseHashName );
	%let sqlSelect=%str(select &baseSelectStr);
	%let arrayVars=%arrayVars(name=tableList,values=&tables);
	%local &arrayVars;
	%array(tableList,values=&tables);
	%do i=1 %to &tableListN;
		%let tableHashName=%hashTable(&&tableList&i);
		%let tableName=&&tableList&i;
		%let sqlFrom=&sqlFrom left join &tableName as &tableHashName on ( %do_over(values=&key,phrase=%str(&baseHashName..?=&tableHashName..?),between=%str(and)) );
		%if &tableSelect= %then %let tableSelectStr=%str(&tableHashName..*);
		%else %let tableSelectStr=%do_over(values=&tableSelect,phrase=&tableHashName..?,between=comma);
		%let sqlSelect=%str(&sqlSelect,&tableSelectStr);
	%end;
	%put &sqlSelect &sqlFrom;
	%put &out;
	proc sql;
		create table &out as (&sqlSelect &sqlFrom);
	quit;
%mend;

* ---- function split ----;
* �����ָ���������Ϊ�Ա�����ӱ��ԡ��趨ǰ׺_�������ݱ���ֵ���ķ�ʽ��������:output_A;
*	data			���Ŀ��;	
*	by				������ݱ�����ֻ���ǵ�һ����;
*	outPrefix		�����ǰ׺,�磺temp.scores;
*	outlib			�����lib;
*	outTablePrefix	����ı��ǰ׺;
*		��������������ʹ�����ȼ�Ϊ��outPrefix>outLib.outTablePrefix;

%macro split(data=,by=,outPrefix=,outLib=,outTablePrefix=) /parmbuff;
	%local prefix outLib outTablePrefix varType valueList;
	* �趨�������;
	%if &data= or &by= %then %error(split:	Required param is empty! param=&syspbuff);
	%if &outPrefix ne %then %let prefix=&outPrefix;
	%else %do;
		%if &outLib= %then %let outLib=%getLibName(&data);
		%if &outTablePrefix= %then %let outTablePrefix=%getTableName(&data);
		%let prefix=&outLib..&outTablePrefix;
	%end;
	%getDsVarType(ds=&data,var=&by,res=varType);
	%if &varType^=1 and &varType^=2 %then %error(split: Data byVar type error! param=&syspbuff); 
	proc sql noprint;
		select distinct &by into :valueList separated by ' ' from &data;
	quit;
	%if &SQLOBS<1 %then %error(split: Dataset byVar has no record!);
	%if &varType=1 %then %do;* split by numeric var; 
		proc sql;
			%do_over(values=&valueList,phrase=%str(create table &prefix._? as (select * from &data where &by=?)),between=%str(;));		
		quit;
	%end;
	%if &varType=2 %then %do;* split by string var; 
		proc sql;
			%do_over(values=&valueList,phrase=%str(create table &prefix._? as (select * from &data where &by="?")),between=%str(;));		
		quit;
	%end;
%mend;

* ---- function createIndex ----;
%macro createIndex(tables=,key=);
	proc sql;
		%do_over(values=&tables,phrase=%str(create index &key on ?;))
	quit;
%mend;

* -------------------------------------------------;
* ---------------- Record functions ---------------;
* -------------------------------------------------;
* record��������;
* recordָһ������,����������Ϊ��prefix+attrName��ʽ����prefixΪTEMP��һ�������TEMPA��TEMPB��TEMPC�����Խ�record���Ϊһ�����������Ա�Ķ���;
* �������Ժ������ʽ�����record��Ϊ��record����Ӧ��ds�е�һ����¼Ϊds record;
* record������Ҫʵ�ֺ�record��ds record��Ļ�����;
* getDsRecord 	ds record->��record;
* setDsRecord	��record->ds record;

* -- ��Ա���� --;
* Ϊ�˱�֤�������Ψһ�ԣ������ǰ׺һ��Ϊ���id;
* ���ڱ������32λ������ע��ǰ׺�ĳ��Ȳ����Թ����������Ӱ������洢��Ĭ�ϵ�ǰ׺ΪR��11λ����ַ����ܼ�12λ�������ĳ�Ա����������Ϊ20λ;

* -- ���ʷ�ʽ --;
* ��Ա��ֵ %let &record.type=abc;
* ��Ա���� %let temp=&&&record.type
* ����record�Ǳ����¼���Ƶĺ����,type��һ����Ա��;

* -- ��record��scope --;
* 	��ʹ��getDsRecord������recordʱ�����к�record��Ա��Ϊȫ�ֱ���,�ױ����ڵ���ǰԤ�������������鷳����ע���ڽ���ʹ�ú�ʹ��clearGlobals�ķ�ʽ��������ȫ�ֺ����;
* 	�����ֹ�������record��ʹ��setDsRecordд��ʱ����Ҫ��record��ԱΪȫ�ֱ���������Ҫ�ٽ�������;

* ----- function createTempRecord -----;
* ������ʱ��recordǰ׺;
%macro createTempRecord(prefix);
	%local var;
	%if &prefix= %then %let prefix=R;
	%let var=%genId(prefix=&prefix,len=12);
	&var.
%mend;

* ----- function recordNew -----;
* ����member�б���һ��record,����������ָ�������д���;
%macro recordNew(members=,record=,res=) /parmbuff;
	%if &record= and %refExist(&res)=0 %then %error(recordNew: RES is not a valid macro variable! param=&syspbuff);
	%if &record= %then %let record=%createTempRecord;
	%global &record;
	%if &members ne %then %do;
		%global %do_over(values=&members,phrase=&record.?);
	%end;
	%if &res ne %then %let &res=&record;
%mend;

* ----- function recordClone -----;
* ��һ�����󴴽�һ����¡;
%macro recordClone(record=,res=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local members newRecord arrayVars i member;

	%if &record= or &res= %then %error(recordClone: No required param ! param=&syspbuff);
	%if not %symexist(&res) %then %error(recordClone: RES is not a valid macro variable ! param=&syspbuff);

	%getMacroVars(prefix=&record,noPrefix=1,noDuplicate=1,res=&tres);%let members=&&&tres;
	%recordNew(members=&members,res=&tres);%let newRecord=&&&tres;

	%let arrayVars=%arrayVars(name=arrayVars,values=&members);
	%local &arrayVars;%array(arrayVars,values=&members);

	%do i=1 %to &arrayVarsN;
		%let member=&&arrayVars&i;
		%let &newRecord.&member=&&&record.&member;
	%end;
	%let &res=&newRecord; 
%mend;


* ----- function recordCopy -----;
* ��һ������ĳ�Ա���Ƶ���һ������;
* input;
*	from		Դ����;
*	to			Ŀ�Ķ���;
*	members		�����ĳ�Ա��Χ����Ϊ������Ϊ����ȫ��;
*	prefix		������Ŀ�Ķ���ĳ�Ա������prefix;
*	suffix		������Ŀ�Ķ���ĳ�Ա������suffix;
*	overrite	�Ƿ񸲸�;
%macro recordCopy(from=,to=,members=,prefix=,suffix=,overwrite=) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local allMembers i member fromMember toMember;
	
	%if &from= or &to= %then %error(recordCopy: No required param ! param=&syspbuff);
	%if &overwrite= %then %let overwrite=1;

	%getMacroVars(prefix=&from,noPrefix=1,noDuplicate=1,res=&tres);%let allMembers=&&&tres;

	%if &members ne %then %do;
		%varsAnd(a=&allMembers,b=&members,res=&tres);
		%let members=&&&tres;
	%end;
	%else %let members=&allMembers;
	%if &members= %then %return;

	%local arrayVars;%let arrayVars=%arrayVars(name=arrayVars,values=&members);
	%local &arrayVars;%array(arrayVars,values=&members);
	%do i=1 %to &arrayVarsN;
		%let member=&&arrayVars&i;
		%let fromMember=&from.&member;
		%let toMember=&to.&prefix.&member.&suffix;
		%if %symexist(&toMember)=0 %then %do;
			%global &toMember;
			%let  &toMember=&&&fromMember;
		%end;
		%else %if &overwrite=1 or %length(&&&toMember)=0 %then %let &toMember=&&&fromMember;
	%end;
%mend;

* ----- function recordShow -----;
* ��ʾ�����Ա;
%macro recordShow(record) /parmbuff;
	%local tres;%let tres=%createTempVar;%local &tres;
	%local members arrayVars i member;

	%if &record= %then %error(recordShow: No required param ! param=&syspbuff);

	%if %symexist(&record) %then %put ---------- Record &record=&&&record ----------;
	%else %put &record=;
	%getMacroVars(prefix=&record,noPrefix=1,noDuplicate=1,res=&tres);%let members=&&&tres;
	%let arrayVars=%arrayVars(name=arrayVars,values=&members);
	%local &arrayVars;
	%array(arrayVars,values=&members);
	%do i=1 %to &arrayVarsN;
		%let member=&&arrayVars&i;
		%put &record..&member=&&&record.&member;
	%end;
	%put ---------- Record End ----------;
%mend;

* ----- function recordDrop -----;
* ��ʾ�����Ա;
%macro recordDrop(record) /parmbuff;
	%dropGlobals(prefix=&record);
%mend;

%macro dropRecord(record) /parmbuff;
	%dropGlobals(prefix=&record);
%mend;


* -- getDsRecord --;
* input:
*	ds			ds;
*	where		where;
*	record		ָ����record����;
*	res			ָ�����ڱ����record���Ƶı���;
*				record��res������ͬʱΪ��,��recordΪ��ʱ��Ĭ���Զ�����һ����record������ͨ��res����;
* output;
*	[record][member]��ʽ��ȫ�ֱ�������Ӧ���غ��¼��Ա;
*	[record]ȫ�ֱ������洢�ż�¼����״̬��Ϣ����Ϊ0���ʾ������Ϊ����ֵ��ʾ����ʧ��;

%macro getDsRecord(ds=,where=,res=,record=) /parmbuff;
	%local l globalPrefix whereStr vars obs vars_sql vars_global_sas vars_global_macro; 
	* �������;
	%if &ds= %then %error(getRecord: The required param is empty! param=&syspbuff);
	%let isNewRecord=0;
	%if &record= %then %do;
		%if &res= %then %error(getRecord: Param RES and RECORD cannot be empty at the same time! param=&syspbuff);
		%let record=%createTempRecord;
		%global &reocrd;
		%let isNewRecord=1;
	%end;
	%else %do;
		%let l=%length(&record);
		%if &l>12 %then %error(getDsRecord: The record name is too long! record=&record!);
		%if not %symexist(&record) %then %error(getDsRecord: The macro variable RECORD does not exist! record=&record!);
	%end;
	%if &res ne %then %let &res=&record;
	%let &record=0;
	
	%let globalPrefix=&record;
	
	%let whereStr=%str();
	%if &where ne %then %let whereStr=where &where;
	
	%getDsVarList(ds=&ds,res=vars);
	%if &vars= %then %do;
		%let &record=1;
		%return;
	%end;
	%getDsObs(ds=&ds,res=obs);
	%if &obs= %then %do;
		%let &record=2;
		%return;
	%end;

	%let vars_sql=%sasVarsToSql(&vars);
	%let vars_global_sas=%do_over(values=&vars,phrase=&globalPrefix.?);
	%let vars_global_macro=%do_over(values=&vars_global_sas,phrase=%str(:?),between=comma);
	%global &vars_global_sas;
	%*put %str(select &vars_sql into &vars_global_macro from &ds &whereStr);
	proc sql outobs=1 noprint;
		select &vars_sql into &vars_global_macro from &ds &whereStr;
	quit;
	%if &SQLOBS<1 %then %do;
		%let &record=3;
		%return;
	%end;
%mend;

* -- setDsRecord --;
* input:
*	ds			ds;
*	record		ָ����record����;
*	key			ָ������key�ı���������keyΪ��ʱ����Ϊ��ֱ�ӵ�д�����;

%macro setDsRecord(ds=,key=,record=);
	%local tres;%let tres=%createTempVar;%local &tres;

	* ������ʼ��;
	%local vars strVars numVars dsVars dsNumVars dsStrVars recordVars recordNullVars recordNumVars recordStrVars;
	%local recordVarsArray varsArray;
	%local varName varValue varType;
	%local inRecordVars inRecordNullVars inRecordNumVars inRecordStrVars inDsVar inDsNumVars inDsStrVars;
	%local recordVarLen dsVarLen;
	%local keyVars foundKeyVars inKeyVars firstKey allKeyMatched;
	%local setStatement assignStatement lengthStatement keyStatement mainStatement dropStatement;
	%local isLast isFound;
	%local i;
	%local obs;

	%let assignStatement=%str();
	%let lengthStatement=%str();
	%let mainStatement=%str();
	%let dropStatement=%str();
	%let isLast=%createTempVar;
	%let isFound=%createTempVar;
	%let firstKey=1;
	%let keyVars=&key;
	%let foundKeyVars=%str();

	* ��ȡds�еı����嵥��������;
	%getDsNumVarList(ds=&ds,res=&tres);%let dsNumVars=&&&tres;
	%getDsStrVarList(ds=&ds,res=&tres);%let dsStrVars=&&&tres;
	%let dsVars=&dsNumVars &dsStrVars;
	
	* ��ȡȫ�ֺ����/��¼�ı����嵥��������;
	%getMacroVars(prefix=&record,res=&tres,noPrefix=1,noDuplicate=1);%let recordVars=&&&tres;
	%let recordVarsArray=%arrayVars(name=recordVarsArray,values=&recordVars);
	%local &recordVarsArray;
	%array(recordVarsArray,values=&recordVars);
	
	%do i=1 %to &recordVarsArrayN;
		%let varName=&&recordVarsArray&i;
		%let varValue=&&&record.&varName;
		%if %length(&varValue)=0 %then %let recordNullVars=&recordNullVars &varName;
		%else %if %isDigit(&varValue)=1 %then %let recordNumVars=&recordNumVars &varName;
		%else %let recordStrVars=&recordStrVars &varName;
	%end;

	%varsOr(a=&recordStrVars,b=&dsStrVars,res=&tres);%let strVars=&&&tres;
	%if &strVars= %then %let lengthStatement=%str();
	%else %let lengthStatement=%str(length);

	%varsOr(a=&recordVars,b=&dsVars,res=&tres);%let vars=&&&tres;
	
	%let varsArray=%arrayVars(name=varsArray,values=&vars);
	%local &varsArray;
	%array(varsArray,values=&vars);
	%do i=1 %to &varsArrayN;

		%let varName=&&varsArray&i;

		* �ж�var����;
		%let inRecordVars=0;
		%let inRecordNumVars=0;
		%let inRecordStrVars=0;
		%let inDsVars=0;
		%let inDsNumVars=0;
		%let inDsStrVars=0;
		%varsIn(source=&recordVars,target=&varName,res=&tres);%let inRecordVars=&&&tres;
		%varsIn(source=&recordNumVars,target=&varName,res=&tres);%let inRecordNumVars=&&&tres;
		%varsIn(source=&recordStrVars,target=&varName,res=&tres);%let inRecordStrVars=&&&tres;
		%varsIn(source=&recordNullVars,target=&varName,res=&tres);%let inRecordNullVars=&&&tres;
		%varsIn(source=&dsVars,target=&varName,res=&tres);%let inDsVars=&&&tres;
		%varsIn(source=&dsNumVars,target=&varName,res=&tres);%let inDsNumVars=&&&tres;
		%varsIn(source=&dsStrVars,target=&varName,res=&tres);%let inDsStrVars=&&&tres;

		%if &inRecordVars and &inDsVars %then %do;
			%if &inRecordNumVars and &inDsNumVars %then %let varType=1;
			%else %if &inRecordNullVars and &inDsNumVars %then %let varType=1;
			%else %if &inRecordStrVars and &inDsNumVars %then %do;
				%error(setDsRecord: Variable type not matched! var=&varName);
			%end;
			%else %let varType=2;
		%end;	
		%else %do;
			%if &inRecordVars %then %do;
				%if &inRecordNumVars %then %let varType=1;
				%else %let varType=2;
			%end;
			%else %do;
				%if &inDsNumVars %then %let varType=1;
				%else %let varType=2;
			%end;
		%end;

		* ��ֵ��䡪����ֵ����;
		%if &varType=1 %then %do;
			%if &inRecordVars %then %do;
				%if %length(&&&record.&varName)=0 %then %let assignStatement=%str(&assignStatement &varName=%str(.););
				%else %let assignStatement=%str(&assignStatement &varName=&&&record.&varName;);
			%end;
			%else %do;
				%let assignStatement=%str(&assignStatement &varName=%str(.););
			%end;
		%end;

		%if &varType=2 %then %do;
			* ��ֵ��䡪���ַ�����;
			%if &inRecordVars %then %do;
				%if %length(&&&record.&varName)=0 %then %let assignStatement=%str(&assignStatement &varName=' ';);
				%else %let assignStatement=%str(&assignStatement &varName="&&&record.&varName";);
			%end;
			%else %do;
				%let assignStatement=%str(&assignStatement &varName=' ';);
			%end;
			* length��䡪���ַ�����;
			%let recordVarLen=0;
			%let dsVarLen=0;
			%let varLen=0;
			%if &inRecordVars %then %let recordVarLen=%length(&&&record.&varName);
			%if &inDsVars %then %do;
				%getDsVarLen(ds=&ds,var=&varName,res=dsVarLen);
			%end;
			%let varLen=%max(&recordVarLen,&dsVarLen,20);
			%let lengthStatement=%str(&lengthStatement &varName $ &varLen);
		%end;

		* key statement;
		%if &keyVars ne %then %do;
			%varsIn(source=&keyVars,target=&varName,res=inKeyVars);
			%*put inKeyVars=&inKeyVars;
			%if &inKeyVars %then %do;
				%if not &inRecordVars %then %error(setDsRecord: The key var doesnot exist! keyVars=&keyVars var=&varName );
				%let varValue=&&&record.&varName;
				%if &firstKey %then %do;
					%if &varType=1 %then %let keyStatement=%str(&varName=&varValue);
					%else %let keyStatement=%str(&varName="&varValue");
				%end;
				%else %do;
					%if &varType=1 %then %let keyStatement=%str(&keyStatement and &varName=&varValue);
					%else %let keyStatement=%str(&keyStatement and &varName="&varValue");
				%end;
				%let firstKey=0;
				%let foundKeyVars=&foundKeyVars &varName;
			%end;
		%end;
		%*%put varName=&varName varType=&varType;
		%*%put inRecordVars=&inRecordVars inRecordNumVars=&inRecordNumVars inRecordStrVars=&inRecordStrVars;
		%*%put inDsVars=&inDsVars inDsNumVars=&inDsNumVars inDsStrVars=&inDsStrVars;

	%end;
	%if &keyVars ne %then %do;
		%varsEqual(a=&keyVars,b=&foundKeyVars,res=&tres);%let allKeyMatched=&&&tres;
		%if not &allKeyMatched %then %error(setDsRecord: Not enogth key variable in record! key=&keyVars foundKey=&foundKeyVars);
		%* %put keyStatement=&keyStatement;
	%end;

	%*%put assignStatement=&assignStatement;
	%*%put lengthStatement=&lengthStatement;

	* �������;
	%getDsObs(ds=&ds,res=&tres);%let obs=&&&tres;
	%if &obs=0 %then %do;
		%let setStatement=%str();
		%let mainStatement=&assignStatement;
	%end;
	%else %if &keyVars= %then %do;
		%let setStatement=%str(set &ds end=&isLast);
		%let dropStatement=%str(drop &isLast);
		%let mainStatement=%str(
								output;
								if &isLast then do;
									&assignStatement;
									output;	
								end;
							);
	%end;
	%else %do;
		%let setStatement=%str(set &ds end=&isLast);
		%let dropStatement=%str(drop &isLast &isFound;retain &isFound 0;);
		%let mainStatement=%str(
								if &keyStatement then do;
									&assignStatement;
									&isFound=1;
								end;
								output;
								if &isLast and not &isFound then do;
									&assignStatement;
									output;
								end;
							);
	%end;
	%*%put mainStatement=&mainStatement;
	%*%put setStatement=&setStatement;
	
	data &ds;
		&lengthStatement;
		&dropStatement;
		&setStatement;
		&mainStatement;
	run;

%mend;


* -------------------------------------------------;
* ---------------- DICT functions -----------------;
* -------------------------------------------------;

* dict�ǻ���dataset���ֵ䣬dataset��ʽΪ��keyVar����������1����������2����������3 ...;
* keyVarĬ��Ϊkey;
* dict�����б��������ַ���;
* ע�⣺;
*	dictGet��dictCopy����Դ������������ͨ��ds���������Զ��ж�key����������;
*	��keyVar��valueVar�����ַ��͵�����£�dictSet��dictCopy��Ŀ�������������ͨ��ds;
*	dict��֧����ֵ����Ҫԭ������Ӧ��д���Ϊ��ͬһ�п��ܻ��в�ͬ���͵�ֵ����������ַ�д����ֵ�����;

* ---- function %dictGet ----;
* ��ȡ����;
* ע�⣺��keyΪ��ʱ�����ȡ��һ����¼;

%macro dictGet(data=,key=,var=,keyVar=,res=,where=);

	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local whereStr obs len type;
	%let whereStr=%str();
	%if &data= or &var= %then %error(dictGet: Required param is empty! data=&data);
	%let &res=%str();

	%getDsObs(ds=&data,res=&tres);%let obs=&&&tres;
	%if &obs=0 %then %error(dictGet: Target dataset is empty! data=&data);
	%getDsVarLen(ds=&data,var=&var,res=&tres);%let len=&&&tres;
	%if &len=0 %then %error(dictGet: Target var does not exisit! var=&var);

	%if &key= %then %do;
		%if &where ne %then %let whereStr=%str(where &where);
	%end;
	%else %do;
		%if &keyVar= %then %let keyVar=key;
		%if &where ne %then %let whereStr=%str(where &where and );
		%else %let whereStr=%str(where );
		%getDsVarType(ds=&data,var=&keyVar,res=type);
		%if &type=1 %then %let whereStr=%str(&whereStr &keyVar=&key);
		%else %if &type=2 %then %let whereStr=%str(&whereStr &keyVar="&key");
		%else %error(dictGet: Target key var type error! keyVar=&keyVar varType=&type);
	%end;
	proc sql outobs=1 noprint;
		select &var into :&res from &data &whereStr;
	quit;
%mend;

* ---- function %dictSet ----;
* д�뺯��;
* ���ָ����dict����������Զ�����;
%macro dictSet(data=,key=,var=,value=,keyVar=);
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local obs oldLen newLen varLen varList;
	%if &data= or &key= or &var= %then %error(dictSet: Required param is empty! data=&data);
	%if &keyVar= %then %let keyVar=key;

	%getDsObs(ds=&data,res=&tres);%let obs=&&&tres;
	%getDsVarLen(ds=&data,var=&var,res=&tres);%let oldLen=&&&tres;

	%let newLen=%length(&value);
	%let varLen=&oldLen;
	%if &newLen>&oldLen %then %let varLen=&newLen;
	%if &varLen<20 %then %let varLen=20; 
	%if &obs ne 0 %then %do;
		%getDsVarList(ds=&data,res=&tres);%let varList=&&&tres;
	 	data &data;
			length &var $ &varLen;
			drop found;
			set &data end=last;
			retain found 0;
			if &keyVar="&key" then do;
				&var="&value";
				found=1;
			end;
			output;
			if last and not found then do;
				%do_over(values=&varList,phrase=%str(?=%str(' ')),between=%str(;));* clear pdv;
				&keyVar="&key";
				&var="&value";
				output;
			end;
		run;
	 %end;
	 %else %do;
		data &data;
			length &keyVar $ 100 &var $ &varLen;
			&keyVar="&key";
			&var="&value";
			output;
		run;
	 %end;
%mend;

* ---- function %dictCopy ----;
* ֵcopy����;
%macro dictCopy(out=,outKey=,outVar=,outKeyVar=,in=,inKey=,inKeyVar=,inVar=,inWhere=);
	%local temp;
	%dictGet(data=&in,key=&inKey,var=&inVar,keyVar=&inKeyVar,res=temp,where=&inWhere);
	%dictSet(data=&out,key=&outKey,var=&outVar,keyVar=&outKeyVar,value=&temp);
%mend;


* -------------------------------------------------;
* ---------------- STAT functions -----------------;
* -------------------------------------------------;

%macro categoryToDummy(data=,var=,out=) /parmbuff;
	%if &data= or &var= or &out= %then %error(categoryToDummy: Required param is empty! param=&syspbuff);
	
	%local tres;%let tres=%createTempVar;%local &tres;
	%local varType valueList isEqual;
	%getDsVarType(ds=&data,var=&var,res=&tres);%let varType=&&&tres;
	
	%let isEqual=%dsEqual(&data,&tres);
	
	%if &varType=0 %then %error(categoryToDummy: VAR doesnot exist in dataset! var=&var);
	proc sql noprint;
		select distinct &var. into :valueList separated by ' ' from &data;
	quit;
	%if &valueList= %then %error(categoryToDummy: No distinct value for target VAR! var=&var);
	%if &varType=1 %then %do;
		data &out;
			set &data;
			%do_over(values=&valueList,phrase=%str(if &var.=? then &var._?=1;else &var._?=0),between=%str(;));
		run;
	%end;
	%else %do;
		data &out;
			set &data;
			%do_over(values=&valueList,phrase=%str(if &var.="?" then &var._?=1;else &var._?=0),between=%str(;));
		run;
	%end;
%mend;

* ---- function filter ----;
* ����˺�����ʹ�ð��������������Ŀ�����й���;
* TODELTE;
%macro filter(in=,filter=,out=,keyIn=,keyFilter=,where=,isBlacklist=);
	%local tableStr notStr temp;
	%if &keyFilter= %then %do;
		%if &keyIn= %then %error(no key);
		%let keyFilter=&keyIn;
	%end;
	%if &where= %then %let tableStr=%str((select distinct &keyFilter from &filter));
	%else %let tableStr=%str((select distinct &keyFilter from &filter where &where));
	%if &isBlacklist=1 %then %let notStr=%str(not);
	%else %let notStr=%str();
	%let temp=%createTempDs;
	proc sql;
		create table &temp as (select * from &in as a where a.&keyIn &notStr in &tableStr);
		create table &out as (select * from &temp);
	quit;
	%dropDs(&temp);
%mend;

%macro filter2(data=,filter=,out=,indexVar=,indexName=,where=,isBlacklist=,res=) /parmbuff;
	%local macro;%let macro=%getSelf;
	%local tres;%let tres=%createTempVar;%local &tres;

	%local whereStr dataObas filterObs outObs err;

	%if &data= or &filter= %then %error(filter: Required param is empty!);
	%if &indexVar= and &indexName= %then %error(filter: INDEXNAME and INDEXVAR cannot be empty at the same time!);
	%if &indexVar= %then %let indexVar=&indexName;
	%if &indexName= %then %let indexName=&indexVar;
 
	%if &out= %then %let out=&data;
	%if &isBlacklist= %then %let isBlacklist=1;
	%let whereStr=%str();
	%if &where ne %then %let whereStr=%str((where=(&where)));

	%getDsObs(ds=&data,res=&tres);%let dataObs=&&&tres;
	%*if &dataObs=0 %then %error(filter: Data is empty!);

	%getDsObs(ds=&filter,res=&tres);%let filterObs=&&&tres;
	%*if &filterObs=0 %then %error(filter: Filter is empty!);

	%let err=%createTempVar;
	data &out;
		set &data.&whereStr;
		drop &err;
		%if &dataObs^=0 and &filterObs^=0 %then %do;
			set &filter(keep=&indexVar) key=&indexName /unique;
			%if &isBlacklist=1 %then %do;
				* ����������;
				select(_iorc_);
					when(%sysrc(_sok)) do;
						delete;
					end;
					when(%sysrc(_dsenom)) do;
						_error_=0;
					end;
					otherwise do;
						&err=dosubl('%error(Unexpected _IORC_ CODE!)');
        			end;
				end;
			%end;
			%else %do;
				* ����������;
				select(_iorc_);
					when(%sysrc(_sok)) do;
						output;
					end;
					when(%sysrc(_dsenom)) do;
						_error_=0;
						delete;
					end;
					otherwise do;
						&err=dosubl('%error(Unexpected _IORC_ CODE!)');
        			end;
				end;
			%end;
		%end;
	run;

	%getDsObs(ds=&out,res=&tres);%let outObs=&&&tres;
	%if &res ne %then %let &res=%eval(&dataObs-&outObs);
%mend;

* ---- fuction verifyKey ----;
* ��������飬�����趨��������������ϳɣ������Ψһ��;
%macro verifyKey(in=,key=,);
	%local k v;
	%let k=%sysfunc(tranwrd(%sysfunc(strip(&key)),%str( ),%str(,)));
	%let v=%hashTable(&in.&key.);
	proc sql;
		create view &v as (select &k,count(*) as countRecord from &in group by &k);
	quit;
	title "verify primary key: table=&in , pk=&key";
	proc freq data=&v;
		tablse countRecord;
	run;quit;
	%clearView(&v);
%mend;

%macro refExist(ref);
	%local res;
	%let res=1;
	%if &ref= %then %let res=0;
	%else %if not %symexist(&ref) %then %let res=0;
	&res.
%mend;


%macro ifEmptyThenGlobal(v,g);
	%local res;
	%global &g;
	%let res=&&&g;
	%if &v ne %then %let res=&v;
	&res.
%mend;


%macro clearLabel(ds);
	%local table lib;
	%let table=%getTableName(&ds);
	%let lib=%getLibName(&ds);
	proc datasets lib=&lib memtype=data noprint;
	   	modify &table; 
	    attrib _all_ label=' '; 
	run;
%mend;

%macro clearFormat(ds);
	%local table lib;
	%let table=%getTableName(&ds);
	%let lib=%getLibName(&ds);
	proc datasets lib=&lib memtype=data noprint;
	   	modify &table; 
	    attrib _all_ format=;
	run;
%mend;

