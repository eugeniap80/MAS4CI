clear all

set more off

cap log close

******************************************************************************
cd "C:\Users\andrea\Dropbox\MAS4CI - Experiments\Data_analysis"
******************************************************************************

*-------------------------------------------------------------------------------------------
log using "preliminary_results/prelim_results_MAB", replace
*-------------------------------------------------------------------------------------------

use "Stata/data/MAB_data_indiv", clear

*-------*ANALYSES MAB* DATA UNIT=INDIVIDUALS (69, 127, 125)*-------*

drop if tot_rounds_played<40 // ****drop all those with less than 40 rounds ******

*SHOW PAYOFF AND EXPLORATION COST IN EACH TREATMENT
table treatment, c(mean payoff mean costly) // These are averages of individual values

*CHECK IF SOCIAL TREATMENTS ARE MORE EFFICIENT IN TERMS OF EXPLORATION COSTS (NON PARAMETRIC TTESTS)

ranksum costly if  treat==0|treat==1, by(treatment) //  solo > choice

ranksum costly if  treat==0|treat==2, by(treatment) // solo > rating

ranksum costly if  treat==1|treat==2, by(treatment) // no diff bw choice and rating

*CHECK IF SOCIAL TREATMENTS ARE MORE EFFICIENT IN TERMS OF PAYOFF (NON PARAMETRIC TTESTS)

ranksum payoff if  treat==0|treat==1, by(treatment) // payoff choice > solo

ranksum payoff if  treat==0|treat==2, by(treatment) // payoff rating = solo

ranksum payoff if  treat==1|treat==2, by(treatment) // no diff bw choice and rating

*make bar graphs on exploration cost and payoff
qui log off
collapse (mean) meanpayoff=payoff (sd) sdpayoff=payoff (mean) meancostly=costly (sd) sdcostly=costly (count) n=costly, by(treat)
generate highp = meanpayoff + invttail(n-1,0.025)*(sdpayoff / sqrt(n))
generate lowp = meanpayoff - invttail(n-1,0.025)*(sdpayoff / sqrt(n))
generate highc = meancost + invttail(n-1,0.025)*(sdcost / sqrt(n))
generate lowc = meancost - invttail(n-1,0.025)*(sdcost / sqrt(n))
qui log on
twoway (bar meanpayoff treat if treat==0)(bar meanpayoff treat if treat ==1)(bar meanpayoff treat if treat ==2)(bar meanpayoff treat if treat ==3)(rcap highp lowp treat), ytitle (payoff)  xlabel (none) xtitle("") legend(order (1 "Solo" 2 "Choice" 3 "Rating" 4 "Mas")) legend (row(1)) graphregion(color(white)) note("")
graph export "Stata/graphs/graph_payoff_mab.pdf", replace
twoway (bar meancostly treat if treat==0)(bar meancostly treat if treat ==1)(bar meancostly treat if treat ==2)(bar meancostly treat if treat ==3)(rcap highc lowc treat), ytitle (exploration cost (n rounds))  xlabel (none) xtitle("") legend(order (1 "Solo" 2 "Choice" 3 "Rating" 4 "Mas")) legend (row(1)) graphregion(color(white)) note("")    
graph export "Stata/graphs/graph_exploration_cost_mab.pdf", replace


*-------*ANALYSES MAB* DATA UNIT=GROUPS (69, 27, 26)*-------*

use "Stata/data/MAB_data_group", clear

///// for now, take out MAS data/////////
drop if treatment==3
/////////////////////////////////////////

drop if tot_rounds_played<40 // ****drop all those with less than 40 rounds ******
drop if group_size>=3 & group_size<4 // drop groups of only 3 subjects..

*ANALYSIS: PROB OF GETTING CORRECT AT 40 ROUND (regression models using both measures of group performance, Correct_at_40 and Group_correct_at_40 )

*Correct_at_40= average % of group members being correct at 40 round (when this is equal or above 0.75 it corresponds to group_correct_at_40=1)

table treatment , c( mean correct_at_40 semean correct_at_40) // 

tobit correct_at_40 i.treatment, ll(0) ul(1) // choice is higher than solo, no diff bw rating and solo

tobit correct_at_40 ib1.treatment, ll(0) ul(1) // to test the difference bw choice (1) and rating (2) (no diff)

*including controlling for overconfidence values
tobit correct_at_40 i.treatment confidence_level, ll(0) ul(1) // average confidence level of all

*Group_correct_at_40 is the percentage of groups being correct at 40st round

table treatment , c( mean group_correct_at_40 semean group_correct_at_40) // 

logit group_correct_at_40 i.treatment // makes less sense to run analysis on this variable because data are less informative than those of correct_at_40 (1=values higher than 0.75 threshold (group is correct); 0=otherwise)

logit group_correct_at_40 ib1.treatment // to test the difference bw choice and rating. No diff

*including controlling for overconfidence values
logit group_correct_at_40 i.treatment confidence_level

qui log off
*make bar graphs on group performance
use "Stata/data/MAB_data_group", clear
drop if tot_rounds_played<40 // ****drop all those with less than 40 rounds ******
drop if group_size>=3 & group_size<4 // drop groups of only 3 subjects..
collapse (mean) meangroupcorr= group_correct_at_40 (sd) sdgroupc= group_correct_at_40 (mean) meancorrect=correct_at_40 (sd) sdcorrect= correct_at_40 (count) n=group_id, by(treat)
generate highg = meangroup + invttail(n-1,0.025)*(sdgroupc/ sqrt(n))
generate lowg = meangroup - invttail(n-1,0.025)*(sdgroupc / sqrt(n))
generate highc = meancorr + invttail(n-1,0.025)*(sdcorr/ sqrt(n))
generate lowc = meancorr - invttail(n-1,0.025)*(sdcorr / sqrt(n))

qui log on
twoway (bar meangroup treat if treat==0)(bar meangroup treat if treat ==1)(bar meangroup treat if treat ==2)(bar meangroup treat if treat ==3)(rcap highg lowg treat), ytitle (group performance (% of winning groups))  xlabel (none) xtitle("") legend(order (1 "Solo" 2 "Choice" 3 "Rating" 4 "Mas")) legend (row(1)) graphregion(color(white)) note("")
graph export "Stata/graphs/graph_group_performance_mab.pdf", replace
twoway (bar meancorr treat if treat==0)(bar meancorr treat if treat ==1)(bar meancorr treat if treat ==2)(bar meancorr treat if treat ==3)(rcap highc lowc treat), ytitle (group performance (% of correct players))  xlabel (none) xtitle("") legend(order (1 "Solo" 2 "Choice" 3 "Rating" 4 "Mas")) legend (row(1)) graphregion(color(white)) note("") 
graph export "Stata/graphs/graph_group_performance_1_mab.pdf", replace


*********ANALYSIS ON OVERCONFIDENCE*************

use "Stata/data/MAB_data_indiv", clear

*CHECK THE CONFIDENCE PROFILE AMONG TREATMENTS (non parametric tests)

table treatment overconfidence, c(mean confidence_level) // -1: underconfident +1 overconfident

kwallis confidence_level, by(treatment)

*CHECK IF OVERCONFIDENTS STATE THEIR CB SOONER:

table treat qconf, c(mean first_cb_round sd first_cb_round) row // using 4 quartiles. first_cb_round= first round at which players state their current best

table treat overconfidence, c(mean first_cb_round sd first_cb_round) row // same as above, but using -1,0,1

*CHECK CORRELATIONS BW TIME OF FIRST CB STATEMENT AND OVERCONFIDENCE FOR EACH TREATMENT SEPARATELY:I expect a neg correlation, the more they are overconfident, the lower the n of the round at which they choose their cb for the forst time . 
// It seems that most subjects rush to state their cb, so probably we have a kind of "floor" effect

pwcorr first_cb_round confidence, sig // overall

by treat, sort :pwcorr first_cb_round confidence, sig // it could make sense to look only on solo treatment (cleaner..)

*CHECK DIFFERENCE IN DECISION TIME BW OVER AND UNDERCONFIDENT OVERALL AND WITHIN EACH TREATMENT (non parametric ttests) 

ranksum  first_cb_round if overconfidence!=0, by(overconfidence) // across all treatments

ranksum  first_cb_round if treat==0 & overconfidence!=0, by(overconfidence) // solo

ranksum  first_cb_round if treat==1 & overconfidence!=0, by(overconfidence) // choice

ranksum  first_cb_round if treat==2 & overconfidence!=0, by(overconfidence) // rating

ranksum  first_cb_round if treat==3 & overconfidence!=0, by(overconfidence) // MAS

*make graph on decision time (round number) and overconfidence
twoway (scatter first_cb_round confidence if overconfidence==-1)(scatter first_cb_round confidence if overconfidence==1) if confidence<2.5 , by(treatment, note("") legend (row(1)) graphregion(color(white))) ytitle("decision time -n rounds-") xtitle("confidence ratio") legend(order(2 "overconfident" 1 "underconfident")) legend (row(1) region(lstyle(none))) graphregion(color(white))
graph export "Stata/graphs/graph_decision_time_mab.pdf", replace

* CHECK IF OVERCONFIDENTS ASSIGN MORE EXTREME RATING VALUES // we use rating_diff as the absolute difference from the average rating value 3
// I expect a positive correlation, the more they are overconfident, the more extreme the rating

pwcorr rating_diff confidence, sig // overall

by treat, sort :pwcorr rating_diff confidence, sig // it could make sense to look only on solo treatment (cleaner..)

*CHECK DIFFERENCE IN RATING DIFF BW OVER AND UNDERCONFIDENT OVERALL AND WITHIN EACH TREATMENT

ranksum  rating_diff if overconfidence!=0, by(overconfidence) // across all treatments

ranksum  rating_diff if treat==0 & overconfidence!=0, by(overconfidence) // solo

ranksum  rating_diff if treat==1 & overconfidence!=0, by(overconfidence) // choice

ranksum  rating_diff if treat==2 & overconfidence!=0, by(overconfidence) // rating

ranksum  rating_diff if treat==3 & overconfidence!=0, by(overconfidence) // MAS

*make graph 
twoway (scatter rating_diff confidence if overconfidence==-1)(scatter rating_diff confidence if overconfidence==1) if confidence<2.5, by(treat, note("") legend (row(1)) graphregion(color(white))) ytitle("rating skewedness") xtitle("confidence ratio") legend(order(2 "overconfident" 1 "underconfident")) legend (row(1) region(lstyle(none))) graphregion(color(white))
graph export "Stata/graphs/graph_rating_mab.pdf", replace

**********MAS system*************

use "Stata/data/MAB_data_indiv", clear

*SHOW PAYOFF AND EXPLORATION COST IN EACH TREATMENT
table treatment, c(mean payoff mean costly) // These are averages of individual values

*CHECK IF MAS ALLOWS TO REDUCE EXPLORATION EFFORT (non parametric ttests)

ranksum costly if  treat==4|treat==0, by(treatment) // MAS<solo

ranksum costly if  treat==4|treat==1, by(treatment) // MAS<synth

ranksum costly if  treat==4|treat==2, by(treatment) // MAS=choice

ranksum costly if  treat==4|treat==3, by(treatment) // MAS<rating trend

*reg costly ib3.treatment

*CHECK IF MAS ALLOWS TO INCREASE INDIVIDUAL PAYOFF (non parametric ttests)

ranksum payoff if  treat==4|treat==0, by(treatment) // MAS>solo

ranksum payoff if  treat==4|treat==1, by(treatment) // MAS>synth

ranksum payoff if  treat==4|treat==2, by(treatment) // MAS=choice

ranksum payoff if  treat==4|treat==3, by(treatment) // MAS>rating

*reg payoff ib3.treatment

use "Stata/data/MAB_data_group", clear

table treatment , c( mean correct_at_40 semean correct_at_40)
table treatment , c( mean group_correct_at_40 semean group_correct_at_40)

*CHECK IF GROUPS IN MAS ARE BETTER IN GUESSING THE CORRECT OPTION (regression models)
// Correct_at_40= average % of group members being correct at 40 round (when this is equal or above 0.75 it corresponds to group_correct_at_40=1)

tobit correct_at_40 ib4.treatment, ll(0) ul(1) robust // average % of subjects choosing the correct option within groups at 40 round
tobit correct_at_40 ib4.treatment if treatment != 0, ll(0) ul(1) robust
tobit correct_at_40 ib4.treatment confidence_level, ll(0) ul(1) robust // including confidence

//Group_correct_at_40 is the percentage of groups being correct at 40st round

logit group_correct_at_40 ib4.treatment ,robust // % of groups chosing the correct option at 40 round
logit group_correct_at_40 ib4.treatment if treatment != 0 ,robust // % of groups chosing the correct option at 40 round
logit group_correct_at_40 ib4.treatment confidence_level ,robust // including confidence

*translate file into pdf
graphlog using "preliminary_results/prelim_results_MAB.smcl",  lspacing(0.8) fwidth(0.6) enumerate replace // to include graphs in pdf!
