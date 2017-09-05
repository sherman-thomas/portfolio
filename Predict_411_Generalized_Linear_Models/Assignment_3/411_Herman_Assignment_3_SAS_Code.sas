libname mydata '/sscc/home/s/sth932/WINE';

proc contents data=mydata.wine;
run;

proc means data=mydata.wine p10 mean median p90;
run;

proc sgplot data=mydata.wine;
	vbox FixedAcidity;
run;

proc sgplot data=mydata.wine;
	vbox FixedAcidity/group=target;
run;

proc sgplot data=mydata.wine;
	vbox Density;
run;

proc sgplot data=mydata.wine;
	vbox Density/group=target;
run;

proc sgplot data=mydata.wine;
	vbox ALCOHOL;
run;

proc sgplot data=mydata.wine;
	vbox ALCOHOL/group=target;
run;

proc sgplot data=mydata.wine;
	vbox TOTALSULFURDIOXIDE;
run;

proc sgplot data=mydata.wine;
	vbox TOTALSULFURDIOXIDE/group=target;
run;

proc sgplot data=mydata.wine;
	vbox labelappeal;
run;

proc sgplot data=mydata.wine;
	vbox labelappeal/group=target;
run;
 

data one (DROP=INDEX);
set mydata.wine;

if fixedacidity > 15.6 then do;
	fixedacidity = 15.6;
end;

if fixedacidity < -1.2 then do;
	fixedacidity = -1.2;
end;

if VolatileAcidity < -0.72  then do;
	 VolatileAcidity= -0.72;
end;

if VolatileAcidity > 1.35 then do;
	 VolatileAcidity= 1.35;
end;

if  ResidualSugar>.481 then do;
	ResidualSugar= .481;
end;

if ResidualSugar <-39.7 then do;
	ResidualSugar=-39.7 ;
end;

if  Chlorides< -.372 then do;
	Chlorides= -.372;
end;

if Chlorides > .481 then do;
	 Chlorides=.481;
end;


if  FreeSulfurDioxide< -171.0 then do;
	 FreeSulfurDioxide=-171.0;
end;

if FreeSulfurDioxide > 230 then do;
	 FreeSulfurDioxide= 230;
end;


if totalsulfurdioxide <-185 then do;
	 totalsulfurdioxide=-185;
end;

if totalsulfurdioxide > 422 then do;
	 totalsulfurdioxide=422;
end;

if   density< .9587then do;
	  density=.9587;
end;

if  density > 1.0295then do;
	  density=1.0295;
end;

if  pH<2.31 then do;
	 pH=2.31;
end;

if  pH> 4.1 then do;
	 pH=4.1;
end;

if   sulphates<-0.7 then do;
	  sulphates=-0.7;
end;

if  sulphates > 1.77then do;
	  sulphates=1.77;
end;

if alcohol < 5.7 then do;
	 alcohol=5.7;
end;

if alcohol >15.2 then do;
	 alcohol=15.2;
end;

proc contents data=one;
run;

proc means data=one n nmiss mean median max std stderr var qrange;
run;

DATA two;
	set one;

IMP_RES=RESIDUALSUGAR;
M_RES=0;
if RESIDUALSUGAR="." then do;
		IMP_RES=5.4187331;
		M_RES=1;
end;

IMP_CHLORIDES=CHLORIDES;
M_CHLORIDES=0;
if CHLORIDES="." then do;
		IMP_CHLORIDES=0.0548225;
		M_CHLORIDES=1;
end;

IMP_FREE_SD=FREESULFURDIOXIDE;
M_FREE_SD=0;
if FREESULFURDIOXIDE="." then do;
		IMP_FREE_SD=30.8455713;
		M_FREE_SD=1;
end;


IMP_TOTAL_SD=TOTALSULFURDIOXIDE;
M_TOTAL_SD=0;
if TOTALSULFURDIOXIDE="." then do;
		IMP_TOTAL_SD=120.7142326;
		M_TOTAL_SD=1;
end;

IMP_pH=pH;
M_pH=0;
if pH="." then do;
		IMP_pH=3.2076282;
		M_pH=1;
end;

IMP_SULPHATES=SULPHATES;
M_SULPHATES=0;
if SULPHATES="." then do;
		IMP_SULPHATES=0.5271118;
		M_SULPHATES=1;
end;

IMP_Alcohol=Alcohol;
M_Alcohol=0;
if Alcohol="." then do;
		IMP_Alcohol=10.4892363;
		M_Alcohol=1;
end;

IMP_STARS = STARS;
M_STARS = 0;
if STARS='.' then do;
		IMP_STARS=2;
		M_STARS=1;
end;


proc means data=two n nmiss mean median var std ndec=4;
run;

proc sgplot data=two;
	vbox FixedAcidity;
run;

proc sgplot data=two;
	vbox FixedAcidity/group=target;
run;

proc sgplot data=two;
	vbox Density;
run;

proc sgplot data=two;
	vbox Density/group=target;
run;

proc sgplot data=two;
	vbox ALCOHOL;
run;

proc sgplot data=two;
	vbox ALCOHOL/group=target;
run;

proc sgplot data=two;
	vbox TOTALSULFURDIOXIDE;
run;

proc sgplot data=two;
	vbox TOTALSULFURDIOXIDE/group=target;
run;

proc sgplot data=two;
	vbox labelappeal;
run;

proc sgplot data=two;
	vbox labelappeal/group=target;
run;
 

proc corr data=two rank;
var FixedAcidity Density LabelAppeal AcidIndex IMP_TOTAL_SD IMP_Alcohol IMP_STARS M_STARS;
	with TARGET;
run;


proc genmod data=two;
	class labelappeal imp_stars M_STARS;
	model target = LabelAppeal AcidIndex IMP_Alcohol IMP_STARS M_STARS/ link=log dist=poi;
	output out=two p=pr1;

proc genmod data=two;
	class labelappeal imp_stars M_STARS;
	model target = LabelAppeal AcidIndex IMP_Alcohol IMP_STARS M_STARS/ link=log dist=nb;
	output out=two p=nbr1;

proc genmod data=two;
	class labelappeal imp_stars M_STARS;
	model target = LabelAppeal AcidIndex IMP_Alcohol IMP_STARS M_STARS/ link=log dist=ZIP;
	zeromodel acidindex m_stars/link=logit;
	output out=two p=zip1;

proc genmod data=two;
	class labelappeal imp_stars M_STARS;
	model target = LabelAppeal AcidIndex IMP_Alcohol IMP_STARS M_STARS/ link=log dist=ZIP;
	zeromodel acidindex m_stars/link=logit;
	output out=two p=zip1 pzero=zzip1;

proc genmod data=two;
	class labelappeal imp_stars M_STARS;
	model target = LabelAppeal AcidIndex IMP_Alcohol IMP_STARS M_STARS/ link=log dist=ZINB;
	zeromodel acidindex m_stars/link=logit;
	output out=two p=zinb1 pzero=zzinb1;

proc reg data=two;
	model target =LabelAppeal AcidIndex IMP_Alcohol IMP_STARS M_STARS;
	output out=two p=yhat;
run;

proc genmod data=two;
	class labelappeal imp_stars m_stars;
	model target =  LabelAppeal AcidIndex IMP_Alcohol IMP_STARS M_STARS/ link=identity dist=normal;
	output out=two p=ols1;
	
proc print data=two (obs=20);
	var target pr1 nbr1 zip1 zinb1 yhat ols1;
run;


**************************************************
**proc means data=TEMPFILE noprint;
*output 		out 	= MEANFILE 
			P1(LABELAPPEAL) 	= P01x 
			P5(LABELAPPEAL)		= P05x
			P95(LABELAPPEAL)	= P95x
			P99(LABELAPPEAL)	= P99x;
*run;

*proc print data=MEANFILE;
*run;

*data;
*set MEANFILE;
*call symput("P1_LABELAPPEAL",P01x);
*call symput("P5_LABELAPPEAL",P05x);
*call symput("P95_LABELAPPEAL",P95x);
*call symput("P99_LABELAPPEAL",P99x);
*run;

*data XFORM;
*set TEMPFILE;
*T95_LABELAPPEAL		= max(min(X,&P95x.),&P05x.); 
*T99_LABELAPPEAL		= max(min(X,&P99x.),&P01x.); 
*run;
