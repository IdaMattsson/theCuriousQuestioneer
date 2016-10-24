% The Curious Questioneer is based off of the natural language processor given in the class example code. 
% These parts are under the Copyright (c) of David Poole and Alan Mackworth 2010. This program
% is released under GPL, version 3 or later; see http://www.gnu.org/licenses/gpl.html

% Note: the pronoun 'you' is interpreted both as singular second person 'you' and plural third person 'you'.
%REMOVE THE ABOVE COMMENT if we remove the plural 'you'


% generate(Questions) returns true if we read a valid input from the user, and Q is the question(s) returned from input(Sentence, Question).
generate(Q) :- readln(S), input(S, Q).

% input(Sentence, Question_Type) returns true if the Question is a question returned of type T. 
% output question is formatted to a string.
input(S, Q_T) :- tag_question(S, Q_T).
input(S, Q_R) :- recip_question(S, Q_R).


/* Question Types */

% --- Tag question ---
% tag question is true if Q1 is the sentence S with a tag question added at the end. 
% if we do not succeed in making a tag, we output an error string.
tag_question(S, T) :- tag(S, T1), formatting(S, T1, T).
tag_question(S, Q1) :- \+ tag(S, _), error(Q1).

% tag(S, T) returns true if T is the corresponding tag to the sentence S
% We assume the first word to be the subject and
% the second word to be either an auxiliary verb or a verb.

% Input contains Subject, Auxiliary verb, Verb 
tag([Sub, Aux, Verb|_], T1) :- check_aux_input(Sub, Aux, Verb), prop(Aux, inv_aux, IAux), append([IAux], [Sub], T1).

% Input contains Subject, Verb 
tag([Sub, Verb|_], T1) :- check_verb_input(Sub, Verb, VT), prop(VT, vt_find_aux, IAux), append([IAux], [Sub], T1).

% Input contains Subject, ToBe_Verb, Verb in -ing form
tag([Sub, Verb_tb, Verb_ing|_], T1) :- check_verb_ing_input(Sub, Verb_tb, Verb_ing), prop(Verb_tb, inv_be, IVerb_tb), append([IVerb_tb], [Sub], T1).


% --- Reciprocal question ---
% Reciprocal Question is true if Q2 is the sentence s with a Reciprocal question attached at the end 
% if we do not succeed in making a reciprocal question, we output an error string.
% since the check for the recip question relies on the tag question generator, we do not need to add the error check here. 
recip_question(S, R) :- recip(S, R1), formatting(S, R1, R).

% recip(S, Q) returns true if R1 is the reciprocal question to the input statement S 

% Input contains Subject, Auxiliary verb, Verb
recip([Sub, Aux, Verb|_], T1) :- check_aux_input(Sub, Aux, Verb), tag([Sub, Aux, Verb|_], [T_Aux, T_Sub | _]), prop(T_Sub, resp_sub, R_Sub), prop(T_Aux, resp_aux, R_Aux), append([R_Aux], [R_Sub], T1).

% Input contains Subject, Verb 
recip([Sub, Verb|_], R1) :- check_verb_input(Sub, Verb, _), tag([Sub, Verb|_], [T_Aux, T_Sub|_]), prop(T_Sub, resp_sub, R_Sub), prop(T_Aux, resp_aux, R_Aux), append([R_Aux], [R_Sub], R1).

% Input contains Subject, ToBe_Verb, Verb in -ing form
recip([Sub, Verb_tb, Verb_ing|_], R1) :- check_verb_ing_input(Sub, Verb_tb, Verb_ing), tag([Sub, Verb_tb, Verb_ing|_], [T_Verb_tb, T_Sub|_]), prop(T_Sub, resp_sub, R_Sub), prop(T_Verb_tb, resp_be, R_Verb_tb), append([R_Verb_tb], [R_Sub], R1).


% grammar check functions return true if Sub is a subject, Aux is an auxiliary verb and Verb is a verb
check_aux_input(Sub, Aux, Verb) :-  prop(Sub, subject, ST), prop(Aux, aux, AT), prop(Verb, verb, VT), sav_agree(ST, AT, VT).

% grammar check functions return true if Sub is a subject and Verb is a verb
check_verb_input(Sub, Verb, VT) :-  prop(Sub, subject, ST), prop(Verb, verb, VT), sav_agree(ST, n, VT).

% grammar check functions return true if Sub is a subject, Verb_tb is a 'to be' verb and Verb is a verb_ing
check_verb_ing_input(Sub, Verb_tb, Verb_ing) :-  prop(Sub, subject, ST), prop(Verb_tb, verb_tb, VT), prop(Verb_ing, verb_ing, true), sav_agree(ST, n, VT).

% formatting(Sentence, Addition, Output) returns true if Output is the string of the form 'sentence S, addition A?'
formatting(S, A, O) :- atomics_to_string(S, " ", S1), append([,], A, A1), atomics_to_string(A1, " ", A2), string_concat(S1, A2, I), string_concat(I, '?', O).

% output error message
error("The Curious Questioneer is confused.").

/* Grammar */

% sav_agree(ST, AT, VT) is true when the auxiliary verb agrees with the subject and the verb agrees with the auxiliary verb
% if AT is n, there is no auxuliary verb in the input
% for subjects with verb only
% s_fs is first person singular subject 	'I'
% s_ss is second person singular subject 	'You'
% s_ts is third person singular subject 	'He/She/It'
% s_fp is first person plural subject 		'We'
% s_sp is second person plural subject 		'You'
% s_tp is third person plural subject 		'They'
% root_v is a root verb, root_v_s is a root verb with an added -s, consider for example 'listen' and 'listens'
sav_agree(s_fs, n, root_v).         
sav_agree(s_fs, n, past).
sav_agree(s_ss, n, root_v).
sav_agree(s_ss, n, past).
sav_agree(s_ts, n, root_v_s).      
sav_agree(s_ts, n, past).
sav_agree(s_fp, n, root_v).
sav_agree(s_fp, n, past).
sav_agree(s_sp, n, root_v).
sav_agree(s_sp, n, past).
sav_agree(s_tp, n, root_v).
sav_agree(s_tp, n, past).
 
% for subject, aux, verb
% true if the first part of the sentence is of form 'subject, auxiliary verb, verb (in root form)'
sav_agree(s_fs, true, root_v).
sav_agree(s_ss, true, root_v).
sav_agree(s_sp, true, root_v).
sav_agree(s_ts, true, root_v).
sav_agree(s_fp, true, root_v).
sav_agree(s_tp, true, root_v).

% for subject, verb 'be'
% No auxuliary verb
% vb_fs_pr is verb_firstPersonSingular_presentTense 	'I am'
% vb_fs_pa is verb_firstPersonSingular_pastTense		'I was'
% vb_ss_pr is verb_secondPersonaSingular_presentTense	'You are'
% vb_ss_pa is verb_secondPersonSingular_pastTense		'You were'
% vb_ts_pr is verb_thirdPersonSingular_presentTense		'He/She/It is'
% vb_ts_pa is verb_thirdPersonSingular_pastTense		'He/She/It was'
% vb_fp_pr is verb_firstPersonPlural_presentTense		'We are'
% vb_fp_pa is verb_firstPersonPlural_pastTense			'We were'
% vb_sp_pr is verb_secondPersonPlural_presentTense		'You are'
% vb_sp_pa is verb_secondPersonPlural_pastTense			'You were'
% vb_tp_pr is verb_thirdPersonPlural_presentTense		'They are'
% vb_tp_pa is verb_thirdPersonPlural_pastTense			'They were'
sav_agree(s_fs, n, vb_fs_pr).
sav_agree(s_fs, n, vb_fs_pa).
sav_agree(s_ss, n, vb_ss_pr).
sav_agree(s_ss, n, vb_ss_pa).
sav_agree(s_ts, n, vb_ts_pr).
sav_agree(s_ts, n, vb_ts_pa).
sav_agree(s_fp, n, vb_fp_pr).
sav_agree(s_fp, n, vb_fp_pa).
sav_agree(s_sp, n, vb_sp_pr).
sav_agree(s_sp, n, vb_sp_pa).
sav_agree(s_tp, n, vb_tp_pr).
sav_agree(s_tp, n, vb_tp_pa).



% ======== DATABASE =========

% Detemine the subjects and what kind of subjects (First+second or third person)
% The form of the auxiliary verb does not change between first and second person, so they are treated as the same.

% s_fs is first person singular subject 	'I'
% s_ss is second person singular subject 	'You'
% s_ts is third person singular subject 	'He/She/It'
% s_fp is first person plural subject 		'We'
% s_sp is second person plural subject 		'You'
% s_tp is third person plural subject 		'They'
prop(i, subject, s_fs).           % I do
prop(you, subject, s_ss).         % you do
prop(he, subject, s_ts).          % he does
prop(she, subject, s_ts).         % she does
prop(it, subject, s_ts).          % it does
prop(we, subject, s_fp).          % we do
prop(they, subject, s_tp).        % they do


% A database of verbs and their categorization
% root_v means the root of the verb
prop(like, verb, root_v).
prop(make, verb, root_v).
prop(love, verb, root_v).
prop(dance, verb, root_v).

% root_v_s is root verb plus "s" (for third person sigular subject)
prop(likes, verb, root_v_s).
prop(makes, verb, root_v_s).
prop(loves, verb, root_v_s).
prop(dances, verb, root_v_s).

% past is past tense
prop(liked, verb, past).
prop(made, verb, past).
prop(loved, verb, past).
prop(danced, verb, past).


% Detemine the verbs with -ing endings
% liking and loving are not gramatically correct, but colloquially used
prop(liking, verb_ing, true).
prop(making, verb_ing, true).
prop(loving, verb_ing, true).
prop(dancing, verb_ing, true).


% Detemine the verb "to be" and what kind of verb "to be" it is(first/second/third person + sigular/plural + present/past tense)
% vb_fs_pr is first person singular verb "to be" in present tense
% vb_ss_pr is second person singular verb "to be" in present tense
% vb_ts_pr is third person singular verb "to be" in present tense
% vb_fp_pr is first person plural verb "to be" in present tense
% vb_sp_pr is second person plural verb "to be" in present tense
% vb_tp_pr is third person plural verb "to be" in present tense
prop(am, verb_tb, vb_fs_pr).
prop(are, verb_tb, vb_ss_pr).
prop(is, verb_tb, vb_ts_pr).
prop(are, verb_tb, vb_fp_pr).
prop(are, verb_tb, vb_sp_pr).
prop(are, verb_tb, vb_tp_pr).

prop(ain_t, verb_tb, vb_fs_pr).
prop(aren_t, verb_tb, vb_ss_pr).
prop(isn_t, verb_tb, vb_ts_pr).
prop(aren_t, verb_tb, vb_fp_pr).
prop(aren_t, verb_tb, vb_sp_pr).
prop(aren_t, verb_tb, vb_tp_pr).

% vb_fs_pa is first person singular verb "to be" in past tense
% vb_ss_pa is second person singular verb "to be" in past tense
% vb_ts_pa is third person singular verb "to be" in past tense
% vb_fp_pa is first person plural verb "to be" in past tense
% vb_sp_pa is second person plural verb "to be" in past tense
% vb_tp_pa is third person plural verb "to be" in past tense
prop(was, verb_tb, vb_fs_pa).
prop(were, verb_tb, vb_ss_pa).
prop(was, verb_tb, vb_ts_pa).
prop(were, verb_tb, vb_fp_pa).
prop(were, verb_tb, vb_sp_pa).
prop(were, verb_tb, vb_tp_pa).

prop(wasn_t, verb_tb, vb_fs_pa).
prop(weren_t, verb_tb, vb_ss_pa).
prop(wasn_t, verb_tb, vb_ts_pa).
prop(wasn_t, verb_tb, vb_fp_pa).
prop(wasn_t, verb_tb, vb_sp_pa).
prop(wasn_t, verb_tb, vb_tp_pa).

% Auxiliary verbs
% a_ts means auxiliary verb, third person singular
prop(can, aux, true).
prop(could, aux, true).
prop(do, aux, true).
prop(does, aux, a_ts).
prop(did, aux, true).
prop(have, aux, true).
prop(had, aux, true).
prop(has, aux, a_ts).
prop(will, aux, true).
prop(would, aux, true).

% Negative Auxiliary verbs
% a_ts means auxiliary verb, third person singular
prop(can_t, aux, true).
prop(couldn_t, aux, true).
prop(don_t, aux, true).
prop(doesn_t, aux, a_ts).
prop(didn_t, aux, true).
prop(haven_t, aux, true).
prop(hadn_t, aux, true).
prop(hasn_t, aux, a_ts).
prop(won_t, aux, true).
prop(wouldn_t, aux, true).

% Inverse Auxiliary relation (from negative to positive or the other way around)
prop(can, inv_aux, can_t).
prop(can_t, inv_aux, can).
prop(could, inv_aux, couldn_t).
prop(couldn_t, inv_aux, could).
prop(do, inv_aux, don_t).
prop(don_t, inv_aux, do).
prop(does, inv_aux, doesn_t).
prop(doesn_t, inv_aux, does).
prop(did, inv_aux, didn_t).
prop(didn_t, inv_aux, did).
prop(have, inv_aux, haven_t).
prop(haven_t, inv_aux, have).
prop(had, inv_aux, hadn_t).
prop(hadn_t, inv_aux, had).
prop(has, inv_aux, hasn_t).
prop(hasn_t, inv_aux, has).
prop(will, inv_aux, won_t).
prop(won_t, inv_aux, will).
prop(would, inv_aux, wouldn_t).
prop(wouldn_t, inv_aux, would).

% Inverse "be" verbs (from negative to positive or the other way around)
prop(am, inv_be, ain_t).
prop(ain_t, inv_be, am).
prop(are, inv_be, aren_t).
prop(aren_t, inv_be, are).
prop(is, inv_be, isn_t).
prop(isn_t, inv_be, is).
prop(was, inv_be, wasn_t).
prop(wasn_t, inv_be, was).
prop(were, inv_be, weren_t).
prop(weren_t, inv_be, were).


% Response subject according to the mapping relation from tag to reciprocal question
% prop(Tag_Sub, response_sub, Reciprocal_Sub)
% Tag_Sub is the subject produced in tag in tag_question
% Reciprocal_Sub is the subject we want to output for recip
% For example:
% if the input is "i like apples"
% tag will be "don_t 'i'?"
% recip should be "do 'you'?"
% the relation mapping is built on all the possible input domain and output range

prop(i, resp_sub, you).
prop(you, resp_sub, i).
prop(he, resp_sub, you).
prop(she, resp_sub, you).
prop(it, resp_sub, you).
prop(we, resp_sub, you).
prop(they, resp_sub, you).

% Response aux for recip question according to the mapping relation from tag to recip
% prop(Tag_Aux, resp_aux, Reciprocal_Aux)
% Tag_Aux is the auxiliary verb produced in tag in tag_question
% Reciprocal_Aux is the auxiliary verb we want to output for recip
% For example:
% if the input is "i like apples"	
% tag will be "'don_t' i?"			
% recip should be "'do' you?"
% the relation mapping is built on all the possible input domain and output range
prop(don_t, resp_aux, do).
prop(didn_t, resp_aux, didn_t).
prop(doesn_t, resp_aux, do).
prop(do, resp_aux, don_t).
prop(does, resp_aux, don_t).
prop(did, resp_aux, didn_t).
prop(can, resp_aux, can_t).
prop(can_t, resp_aux, can).
prop(couldn_t, resp_aux, could).
prop(could, resp_aux, couldn_t).
prop(have, resp_aux, haven_t).
prop(haven_t, resp_aux, have).
prop(had, resp_aux, hadn_t).
prop(hadn_t, resp_aux, had).
prop(has, resp_aux, hasn_t).
prop(hasn_t, resp_aux, has).
prop(will, resp_aux, won_t).
prop(won_t, resp_aux, will).
prop(would, resp_aux, wouldn_t).
prop(wouldn_t, resp_aux, would).

% Respone be-verb for recip question according to the mapping relation from tag to recip
% prop(T_Verb_tb, resp_be, R_Verb_tb)
% T_Verb_tb is the verb "to be" produced in tag in tag_question
% R_Verb_tb is the verb "to be" we want to output for recip
% For example:
% if the input is "i am making buns"
% tag will be "'ain_t' i?"
% recip should be "'are' you?"  
% the relation mapping is built on all the possible input domain and output range
prop(ain_t, resp_be, are).
prop(isn_t, resp_be, are).
prop(aren_t, resp_be, are).	
prop(wasn_t, resp_be, were).
prop(weren_t, resp_be, were).
prop(am, resp_be, aren_t).
prop(is, resp_be, aren_t).
prop(are, resp_be, aren_t).	
prop(was, resp_be, weren_t).
prop(were, resp_be, weren_t).


% Use verb type to find auxiliary verb if the input does not contain any auxiliary verb
% this finds hidden auxiliary verb
% prop(Verb Type, vt_find_aux, inverse auxiliary verb)
prop(root_v, vt_find_aux, don_t).
prop(root_v_s, vt_find_aux, doesn_t).
prop(past, vt_find_aux, didn_t).



