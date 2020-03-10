clear all

set more off

cap log close

******************************************************************************
cd "C:\Users\andrea\Dropbox\MAS4CI - Experiments"
******************************************************************************


*import raw data
insheet using "mas4ci_data_joined_risk.txt", comma

unab xvars: round_valid individual_is_active individual_exploration individual_exploration_prev individual_is_correct group_quorum group_correct

foreach x of local xvars {
replace `x'="1" if `x'=="True"
replace `x'="0" if `x'=="False"
destring `x', replace
}

rename subject_id subject_id_string
encode subject_id_string, generate(subject_id)
drop subject_id_string

preserve

*******************************prepare data MAB********************************
drop if game=="PD"

label define treatment 0 "Solo" 1 "Solo_Synt" 2 "Choice" 3 "Rating" 4 "MAS"
label values treatment treatment

*CLEAN DATA
drop if round_number>40 // delete all rounds above 40  !!--------------------!!

*check for initial dropouts over treatments
egen tot_rounds_played = sum(individual_is_active), by (subject_id)
table treatment if tot_rounds_played==0 & round_number==1, c(count tot_rounds_played)
drop if tot_rounds_played==0 // drop individuals that are TOTALLY inactive (e.g., inactive in all trials)

*check for individuals that NEVER explored
egen tot_expl = sum(individual_exploration), by (subject_id)
table treat if tot_expl==0 & round_number==1, c(count tot_expl) 
drop if tot_expl==0 // drop those that NEVER explored (not even in the free explorations)

*check how much subjects are left (includes those that do not reach the 40st round, that may be useful to keep in round-data analyses..)
table treat if round_number==1, c(count subject) 

*generate payoff value *at 40ST ROUND*
gen temp=1 if individual_exploration==1 & individual_is_active==1 & round_number>10 // identify costly explorations (rounds above 10)
bysort subject_id: egen costly_expl=sum(temp)
gen leftover= 50 - costly // generate what would be left from costly exploration 

* gen exploration var for all rounds (from 1 to 40)
gen temp2=1 if individual_exploration==1 & individual_is_active==1 // identify costly explorations
bysort subject_id: egen fullexploration=sum(temp2)

gen temp0=1 if round_number==40 & individual_is_correct==1 // generate a var that identifies if individuals are correct at round 40, regardless of their treatment
bysort subject_id: egen correct_at_40=mean(temp0)
replace correct_at_40=0 if correct_at_40==.

gen temp1=1 if round_number==40 & group_quorum==1 & group_correct==1 & treatment!=0 // generate a var that identifies for those playing in social treatments, if the group is correct AND has reached the quorum at round 40 (may include subjects who are however NOT correct in their single choice!)
bysort subject_id: egen group_correct_at_40=mean(temp1)
replace group_correct_at_40=0 if group_correct_at_40==. & treatment!=0
replace group_correct_at_40=correct_at_40 if treatment==0 // in the individual treatment group_correct_at_40 is equal to correct_at_40


drop temp*

gen payoff=leftover if correct_at_40==1 & treatment==0 // assigns payoff value for subjects in solo treatment
replace payoff= leftover if payoff==. &  group_correct_at_40==1 // assigns payoff for subjects in social treatment (depends on group being correct and reached quorum, regardless of individual being correct)
replace payoff=0 if payoff==. // assigns 0 otherwise

*add "fake" individual groups for solo treat
replace group_id=1000+subject_id if group_id==. & treatment==0

*gen var indicating the n of the first round at which subject decide to state their cb and its rating

sort subject_id round_numb
gen temp_round=round_numb if individual_current_best!=0 
bysort subject_id: egen first_cb_round = min(temp_round)
gen temp_rat=individual_rating_current_best if first_cb_round==temp_round
bysort subject_id: egen first_cb_rating = mean(temp_rat)
replace first_cb_rating=. if first_cb_rating==0
drop temp*

**add confidence related variables**

*generate overconfidence level
*gen confidence_level=(training_avg_confidence_valid/training_num_success_valid)/100
///////////////////////////////////////////////////////////////////////////////
*generate overconfidence level using data CORRECTED for what subjects actually see..(see mail from Vito)
gen confidence_level=(training_avg_confidence_correcte/training_num_success_valid)/100
///////////////////////////////////////////////////////////////////////////////

gen overconfidence=1 if confidence>1 // overconfident	
replace overconfidence=-1 if confidence<1 //underconfident
replace overconfidence=0 if confidence==1 // balanced subjects

xi I.overconfidence, prefix(_I) noomit // create 3 dummy variables for under (1), equal(2) and overconfident(3)

gen overconfidence_value=confidence_level if overconfidence==1 // show values of those considered overconfident (above 1)

gen rating_diff=abs(3-individual_rating_current_best) // gen the absolute difference from the average rating value 3

*generate leader confidence and rating: it shows the confidence value of the first subject/s in a group that state its current best (if tie, then average or show max) (and the rating of its current best)
bysort subject_id: gen temp=1 if individual_current_best!=0 // create a var that is 1 if current best is chosen, null otherwise
by subject_id (round_n), sort: gen byte temp1 = sum(temp) == 1  & sum(temp[_n - 1]) == 0 // tells to put 1 only to the first occurrence --for each subject, it identifies the round at which the cb is chosen
replace temp1 =round_n if temp1 ==1 // overwrite with the round number
replace temp1 =. if temp1 ==0
bysort group_id: egen temp2=min(temp1) // identifies at group level the min round number among group members
gen temp3=confidence_level if temp2 == temp1 // here we can add more information from the leader (its rating value etc)
gen temp4=individual_rating_current_best if temp2 == temp1
bysort group_id: egen leader_confidence_av=mean(temp3) // in case of ties inside a group pick the average value
bysort group_id: egen leader_confidence_max=max(temp3) // in case of ties inside a group, pick the maximum value
bysort group_id: egen leader_rating_av=mean(temp4) // in case of ties inside a group pick the average value
bysort group_id: egen leader_rating_max=max(temp4) // in case of ties inside a group, pick the maximum value
drop temp*


save "Data_analysis/Stata/data/MAB_data_long", replace // long version of dataset, includes data per rounds

*drop those that do not reach the 40st round
egen max_round = max(round_number) if individual_is_active==1, by (subject_id)
egen last_round = mean(max_round), by (subject_id)
drop max_round
drop if last_round<40

collapse rating_diff first* leader* confidence_level overconfidence* _I* group_size costly_exp fullexploration leftover payoff correct_at_40 tot_rounds_played tot_expl group_correct_at_40, by(subject_id treatment group_id)

*list outliers for confidence values
list subject_id treat confidence if confidence>4 // several outliers appear if we use the uncorrected confidence data 

*gen variable that splits subj in quartiles based on confidence value (4 is extremely overconfident)
xtile qconf = confidence_level if confidence_level<=4, nq(4) // decision: let's drop those with conf_level above 4, as they skew the quartile creation

xi I.qconf, prefix(dum_) noomit

gen extr_overconfidence=confidence_level if qconf==4

*gen max group numerosity var for social treats // group size could be less if some subjects do not finish all 40 rounds
bysort treat group_id: egen n_players=count(_N)

save "Data_analysis/Stata/data/MAB_data_indiv", replace // collapsed version of dataset, includes only data per subject

collapse leader* confidence overconfidence_value extr_ dum* _I* group_size costly_expl fullexploration leftover payoff correct_at_40 tot_rounds_played tot_expl group_correct_at_40 n_players, by(treatment group_id)

drop if tot_rounds_played<40 // ****drop all those with less than 40 rounds ******
drop if group_size>=3 & group_size<4 // drop groups of only 3 subjects..

save "Data_analysis/Stata/data/MAB_data_group", replace

restore

*******************************prepare data PD********************************
drop if game=="MAB"

label define treatment 0 "Solo" 1 "Solo_Synt" 2 "Choice" 3 "Rating" 4 "MAS"
label values treatment treatment

*CLEAN DATA
drop if round_number>40 // delete all rounds above 40  !!--------------------!!

*check for initial dropouts over treatments
egen tot_rounds_played = sum(individual_is_active), by (subject_id)
table treatment if tot_rounds_played==0 & round_number==1, c(count tot_rounds_played)
drop if tot_rounds_played==0 // drop individuals that are TOTALLY inactive (e.g., inactive in all trials)

*check for individuals that NEVER explored
egen tot_expl = sum(individual_exploration), by (subject_id)
table treat if tot_expl==0 & round_number==1, c(count tot_expl) 
drop if tot_expl==0 // drop those that NEVER explored (not even in the free explorations)

*check how much subjects are left (includes those that do not reach the 40st round, that may be useful to keep in round-data analyses..)
table treat if round_number==1, c(count subject) 

*generate payoff value *at 40ST ROUND*
gen temp=1 if individual_exploration==1 & individual_is_active==1 & round_number>10 // identify costly explorations (rounds above 10)
bysort subject_id: egen costly_expl=sum(temp)
gen leftover= 50 - costly // generate what would be left from costly exploration 

* gen exploration var for all rounds (from 1 to 40)
gen temp2=1 if individual_exploration==1 & individual_is_active==1 // identify costly explorations
bysort subject_id: egen fullexploration=sum(temp2)

gen temp0=1 if round_number==40 & individual_is_correct==1 // generate a var that identifies if individuals are correct at round 40, regardless of their treatment
bysort subject_id: egen correct_at_40=mean(temp0)
replace correct_at_40=0 if correct_at_40==.

gen temp1=1 if round_number==40 & group_quorum==1 & group_correct==1 & treatment!=0 // generate a var that identifies for those playing in social treatments, if the group is correct AND has reached the quorum at round 40 (may include subjects who are however NOT correct in their single choice!)
bysort subject_id: egen group_correct_at_40=mean(temp1)
replace group_correct_at_40=0 if group_correct_at_40==. & treatment!=0
replace group_correct_at_40=correct_at_40 if treatment==0 // in the individual treatment group_correct_at_40 is equal to correct_at_40

drop temp*

gen payoff=leftover if correct_at_40==1 & treatment==0 // assigns payoff value for subjects in solo treatment
replace payoff= leftover if payoff==. &  group_correct_at_40==1 // assigns payoff for subjects in social treatment (depends on group being correct and reached quorum, regardless of individual being correct)
replace payoff=0 if payoff==. // assigns 0 otherwise

*add "fake" individual groups for solo treat
replace group_id=1000+subject_id if group_id==. & treatment==0

*gen var indicating the n of the first round at which subject decide to state their cb and its rating

sort subject_id round_numb
gen temp_round=round_numb if individual_current_best!=0 
bysort subject_id: egen first_cb_round = min(temp_round)
gen temp_rat=individual_rating_current_best if first_cb_round==temp_round
bysort subject_id: egen first_cb_rating = mean(temp_rat)
replace first_cb_rating=. if first_cb_rating==0
drop temp*

**add confidence related variables**

*generate overconfidence level
gen confidence_level=(training_avg_confidence_valid/training_num_success_valid)/100

gen overconfidence=1 if confidence>1 // overconfident	
replace overconfidence=-1 if confidence<1 //underconfident
replace overconfidence=0 if confidence==1 // balanced subjects

xi I.overconfidence, prefix(_I) noomit // create 3 dummy variables for under (1), equal(2) and overconfident(3)

gen rating_diff=abs(3-individual_rating_current_best) // gen the absolute difference from the average rating value 3

gen overconfidence_value=confidence_level if overconfidence==1 // show values of those considered overconfident (above 1)

*generate leader confidence: it shows the confidence value of the first subject/s in a group that state its current best (if tie, then average or show max)
bysort subject_id: gen temp=1 if individual_current_best!=0 // create a var that is 1 if current best is chosen, null otherwise
by subject_id (round_n), sort: gen byte temp1 = sum(temp) == 1  & sum(temp[_n - 1]) == 0 // tells to put 1 only to the first occurrence --here we have to say to do it only on group level
replace temp1 =round_n if temp1 ==1
replace temp1 =. if temp1 ==0
bysort group_id: egen temp2=min(temp1)
gen temp3=confidence_level if temp2 == temp1 // here we can add more information from the leader (its rating value etc)
gen temp4=individual_rating_current_best if temp2 == temp1
bysort group_id: egen leader_confidence_av=mean(temp3) // in caxe of ties inside a group pick the average value
bysort group_id: egen leader_confidence_max=max(temp3) // in case of ties inside a group, pick the maximum value
bysort group_id: egen leader_rating_av=mean(temp4) // in case of ties inside a group pick the average value
bysort group_id: egen leader_rating_max=max(temp4) // in case of ties inside a group, pick the maximum value
drop temp*


save "Data_analysis/Stata/data/PD_data_long", replace // long version of dataset, includes data per rounds

*drop those that do not reach the 40st round
egen max_round = max(round_number) if individual_is_active==1, by (subject_id)
egen last_round = mean(max_round), by (subject_id)
drop max_round
drop if last_round<40

collapse rating_diff first* leader* confidence_level overconfidence* _I* group_size costly_exp fullexploration leftover payoff correct_at_40 tot_rounds_played tot_expl group_correct_at_40, by(subject_id treatment group_id)

*list outliers for confidence values
list subject_id treat confidence if confidence>4 // some outliers??

*gen variable that splits subj in quartiles based on confidence value (4 is extremely overconfident)
xtile qconf = confidence_level if confidence_level<=4, nq(4) // decision: let's drop those with conf_level above 4, as they skew the quartile creation

xi I.qconf, prefix(dum_) noomit

gen extr_overconfidence=confidence_level if qconf==4

*gen max group numerosity var for social treats // group size could be less if some subjects do not finish all 40 rounds
bysort treat group_id: egen n_players=count(_N)

save "Data_analysis/Stata/data/PD_data_indiv", replace // collapsed version of dataset, includes only data per subject

collapse leader* confidence overconfidence_value extr dum* _I* group_size costly_expl fullexploration leftover payoff correct_at_40 tot_rounds_played tot_expl group_correct_at_40 n_players, by(treatment group_id)

drop if tot_rounds_played<40 // ****drop all those with less than 40 rounds ******
drop if group_size>=3 & group_size<4 // drop groups of only 3 subjects..

*AG: DROP IF n_players is below 4
drop if n_players > 1 & group_size <4  
save "Data_analysis/Stata/data/PD_data_group", replace

