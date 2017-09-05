libname mydata '/sscc/home/s/sth932/Moneyball' access=readonly;

proc contents data=mydata.moneyball;
run;

proc means data=mydata.moneyball(drop=INDEX)nmiss mean median max min stddev ndec=0;
run;

proc sgplot data=mydata.moneyball;
	vbox target_wins;
	xaxis label="Wins";
	keylegend/title="Boxplot";
	run;

proc sgplot data=mydata.moneyball;
histogram target_wins / SHOWBINS;
run;

proc univariate data=mydata.moneyball;
VAR TARGET_WINS;
run;

proc sgplot data=mydata.moneyball;
histogram TEAM_BASERUN_SB / SHOWBINS;
run;

proc sgplot data=mydata.moneyball;
vbox TEAM_BASERUN_SB;
run;

proc sgplot data=mydata.moneyball;
histogram TEAM_BATTING_BB/ SHOWBINS;
run;

proc sgplot data=mydata.moneyball;
vbox TEAM_BATTING_BB;
run;

proc sgplot data=mydata.moneyball;
histogram TEAM_BATTING_H / SHOWBINS;
run;

proc sgplot data=mydata.moneyball;
vbox TEAM_BATTING_H;
run;

proc sgplot data=mydata.moneyball;
histogram TEAM_BATTING_3B / SHOWBINS;
run;

proc sgplot data=mydata.moneyball;
vbox TEAM_BATTING_3B;
run;

proc sgplot data=mydata.moneyball;
histogram TEAM_FIELDING_DP / SHOWBINS;
run;

proc sgplot data=mydata.moneyball;
vbox TEAM_FIELDING_DP;
run;

proc sgplot data=mydata.moneyball;
histogram TEAM_FIELDING_E / SHOWBINS;
run;

proc sgplot data=mydata.moneyball;
vbox TEAM_FIELDING_E;
run;

proc sgplot data=mydata.moneyball;
histogram TEAM_PITCHING_SO / SHOWBINS;
run;

proc sgplot data=mydata.moneyball;
vbox TEAM_PITCHING_SO;
run;

proc sgplot data=mydata.moneyball;
histogram TEAM_PITCHING_H / SHOWBINS;
run;

proc sgplot data=mydata.moneyball;
vbox TEAM_PITCHING_H;
run;

proc sgplot data=mydata.moneyball;
histogram TEAM_PITCHING_BB / SHOWBINS;
run;

proc sgplot data=mydata.moneyball;
vbox TEAM_PITCHING_BB;
run;

proc sgplot data=mydata.moneyball;
histogram TEAM_BATTING_HBP / SHOWBINS;
run;

proc sgplot data=mydata.moneyball;
vbox TEAM_BATTING_HBP;
run;

proc corr data=mydata.moneyball(drop=INDEX)rank;
var TEAM_BATTING_H TEAM_BATTING_2B TEAM_BATTING_3B TEAM_BATTING_HR TEAM_BASERUN_SB 
TEAM_BATTING_BB TEAM_BATTING_HBP TEAM_PITCHING_SO TEAM_FIELDING_DP TEAM_BATTING_SO 
TEAM_BASERUN_CS TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_BB TEAM_FIELDING_E;
with target_wins;
Title "Predictor Variables Correlation to Wins";
run;

data one;
	set mydata.moneyball;
if (TARGET_WINS < 20) THEN DELETE;
if (TARGET_WINS > 116) THEN DELETE;
run;

proc contents data=one;
run;

data two;
 set one (DROP= TEAM_BATTING_HBP INDEX);
 BASERUN_CS=TEAM_BASERUN_CS;
 MISSING_BASERUN_CS= 0;
 if baserun_cs= '.' then DO;
 baserun_cs=53;
 	missing_baserun_cs= 1;
 	end;
LABEL BASERUN_CS= 'Caught Stealing (fixed)';

FIELDING_DP = TEAM_FIELDING_DP;
MISSING_FIELDING_DP= 0;
	if fielding_dp='.' then DO;
		fielding_dp= 146;
	missing_fielding_dp= 1;
	end;
LABEL FIELDING_DP = 'Double Plays (fixed)';

 BASERUN_SB=TEAM_BASERUN_SB;
 MISSING_BASERUN_SB= 0;
 if baserun_sb= '.' then DO;
 baserun_sb=125;
 	missing_baserun_sb= 1;
 	end;
LABEL BASERUN_SB= 'Stolen Bases (fixed)';

 BATTING_SO=TEAM_BATTING_SO;
 MISSING_BATTING_SO= 0;
 if batting_so= '.' then DO;
 batting_so=736;
 	missing_batting_so= 1;
 	end;
LABEL BATTING_SO= 'Strikeouts by Batters(fixed)';

 PITCHING_SO=TEAM_BATTING_SO;
 MISSING_PITCHING_SO= 0;
 if pitching_so= '.' then DO;
 pitching_so=818;
 	missing_pitching_so= 1;
 	end;
LABEL PITCHING_SO= 'Strikeouts by Pitchers(fixed)';
run;

proc contents data=two;
run;

proc means data=two nmiss mean median max min stddev ndec=0;
run;

proc univariate data=two;
VAR PITCHING_SO;
run;

proc univariate data=two;
VAR TEAM_PITCHING_H;
run;

data three;
	set two(DROP = TEAM_BASERUN_CS TEAM_FIELDING_DP TEAM_BASERUN_SB TEAM_BATTING_SO TEAM_PITCHING_SO);
if (PITCHING_SO > 1300) THEN DELETE;
if (TEAM_PITCHING_H > 25000) THEN DELETE;

TEAM_EXTRA_BASE = (TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR);
run;

proc contents data=three;
run;

proc means data=three nmiss mean median max min stddev ndec=0;
run;

proc corr data=three rank;
var TEAM_BATTING_H TEAM_BATTING_BB BASERUN_SB TEAM_BATTING_BB PITCHING_SO 
FIELDING_DP BATTING_SO BASERUN_CS TEAM_PITCHING_H TEAM_PITCHING_HR 
TEAM_PITCHING_BB TEAM_FIELDING_E TEAM_EXTRA_BASE;
with target_wins;
Title "New Predictor Variables Correlation to Wins";
run;
 

proc reg data= three PLOTS(ONLY)= (AIC BIC CP ADJRSQ);
model Target_Wins = TEAM_BATTING_H BASERUN_SB TEAM_BATTING_BB PITCHING_SO FIELDING_DP 
BATTING_SO BASERUN_CS TEAM_PITCHING_H TEAM_PITCHING_HR 
TEAM_PITCHING_BB TEAM_FIELDING_E TEAM_EXTRA_BASE/ selection = forward
slentry=0.5;
Title "Multiple Regression Model using Forward Selection";
run;


proc reg data= three PLOTS(ONLY)= (AIC BIC CP ADJRSQ);
model Target_Wins = TEAM_BATTING_H BASERUN_SB TEAM_BATTING_BB PITCHING_SO FIELDING_DP
BATTING_SO BASERUN_CS TEAM_PITCHING_H TEAM_PITCHING_HR TEAM_PITCHING_BB 
TEAM_FIELDING_E TEAM_EXTRA_BASE/ selection = stepwise
slentry=0.5;
Title "Multiple Regression Model using Forward Selection";
run;

proc reg data = three PLOTS= fit(stats=(DEFAULT AIC BIC CP));
model Target_Wins = TEAM_BATTING_H TEAM_EXTRA_BASE BASERUN_SB TEAM_BATTING_BB TEAM_FIELDING_E;
Title "Multiple Regression Model via Manual Selection";
run; 


data moneyball_test;
	set mydata.moneyball; 		
							
		P_TARGET_WINS = 11.39410 + 
						0.04079*TEAM_BATTING_H  + 
						0.01305*TEAM_EXTRA_BASE + 
						0.03744*BASERUN_SB + 
						0.00884*TEAM_BATTING_BB +  
					   -0.02035*TEAM_FIELDING_E;
									

		if p_target_wins='.' then p_target_wins=81; 
		P_TARGET_WINS = min( P_TARGET_WINS, 0 ); 
		P_TARGET_WINS = max( P_TARGET_WINS, 162 ); 
		P_TARGET_WINS = round(P_TARGET_WINS,1); 
		

keep INDEX P_TARGET_WINS; 
run;

proc print data=moneyball_test(obs=100);
run;

proc means data=moneyball_test N NMISS MIN MAX;
var P_TARGET_WINS;
run;