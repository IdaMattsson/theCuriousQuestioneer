% The Curious Questioneer is based off of the natural language processor given in the class example code. 
% Prolog representation of a grammar to build a query for a database
% This is not meant to be polished or lingustically reasonable, but purely to show what can be done

% This is slightly expanded code of Figures 12.10 and 12.11 in Section 12.6.6 of
% Poole and Mackworth, Artificial Intelligence: foundations of
% computational agents, Cambridge, 2010.

% Copyright (c) David Poole and Alan Mackworth 2010. This program
% is released under GPL, version 3 or later; see http://www.gnu.org/licenses/gpl.html

% noun_phrase(T0,T4,Ind,C0,C4) is true if
%  T0 and T4 are list of words, such that
%        T4 is an ending of T0
%        the words in T0 before T4 (written T0-T4) form a noun phrase
%  Ind is the individual that the noun phrase is referring to
%  C0 and C4 are lists of relations such that
%        C0 is an ending of C4 and
%        the relations in C4-C0 give the constraints on Ind implied by the noun phrase
% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:


% ================= WIP =======================


% S is input sentence and Q is the returned question(s)
% this version is good for keeping only the basic shape, no variable assignment. Use for final version
%input(S) :- tag_question(S,Q1), write(Q1).

% working version, expand according to number of avilable questions
input(S, T) :- tag_question(S, T).


/* Question Types */

% tag question is true if Q is the sentence S with a tag question added at the end.

% tag_question(S, Q1) :- tag(S, T1), atomics_to_string(S, " ", S1), string_concat(S1, T1, Q1).
tag_question(S, Q1) :- tag(S, T), append(S, T, Q1).
tag_question(S, Q1) :- \+ tag(S, _), error(Q1).

error([the, curious, questioneer, is, confused]).

% tag analyze the first two words in the sentence and produce the tag question.

% We assume the first word to be the subject and
% the second word to be either an auxiliary verb or a verb.
% NOTE: add the atomics_to_string!!! TODO
tag([Sub, Aux, Verb|_], T1) :- prop(Sub, subject, ST), prop(Aux, aux, AT), prop(Verb, verb, VT), sav_agree(ST, AT, VT), prop(Aux, inv_aux, IAux), append([,, IAux], [Sub,?], T1).

tag([Sub, Verb|_], T1) :- prop(Sub, subject, ST), prop(Verb, verb, VT), sav_agree(ST, n, VT), prop(VT, find_aux, IAux), append([,, IAux], [Sub,?], T1).

tag([Sub, Verb_tb, Verb_ing|_], T1) :- prop(Sub, subject, ST), prop(Verb_tb, verb_tb, VT), prop(Verb_ing, verb_ing, true), sav_agree(ST, n, VT), prop(Verb_tb, inv_be, IVerb_tb), append([,, IVerb_tb], [Sub,?], T1).



/* Grammar */

% sav_agree(ST, AT, VT) is true if the auxiliary verb agrees with the subject and the verb agrees with the aux (that the verb is a root verb)
% if AT is n, there is no auxuliary verb in the input

%for subject with verb only
sav_agree(s_fs, n, root_v).         % subject_firstSingular maps to root verb
sav_agree(s_fs, n, past).
sav_agree(s_ss, n, root_v).
sav_agree(s_ss, n, past).
sav_agree(s_ts, n, root_v_s).       % subject_third singular maps to root verb with added s
sav_agree(s_ts, n, past).
sav_agree(s_fp, n, root_v).
sav_agree(s_fp, n, past).
sav_agree(s_sp, n, root_v).
sav_agree(s_sp, n, past).
sav_agree(s_tp, n, root_v).
sav_agree(s_tp, n, past).

% for subject, aux, verb
sav_agree(s_fs, true, root_v).
sav_agree(s_ss, true, root_v).
sav_agree(s_sp, true, root_v).
sav_agree(s_ts, a_ts, root_v).
sav_agree(s_fp, true, root_v).
sav_agree(s_tp, true, root_v).

% for subject, verb 'be'
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



% check if type of subject and verb match
% (This greatly reduce the amount of data in the database we need to input)
% sv_agree(ST, VT)
% ST is subject type
% VT is verb type
/*
sv_agree(fst_snd,fst_snd).
sv_agree(fst_snd,past).
sv_agree(t_rd,t_rd).
sv_agree(t_rd,past).
*/



% use verb type to find auxiliry verb
% this finds hidden auxiliry verb
% prop(Verb Type, find_aux, inverse auxiliry verb)
prop(root_v, find_aux, don_t).
prop(root_v_s, find_aux, doesn_t).
prop(past, find_aux, didn_t).


/*
noun_phrase(T0,T4,Ind,C0,C4) :-
det(T0,T1,Ind,C0,C1),
adjectives(T1,T2,Ind,C1,C2),
noun(T2,T3,Ind,C2,C3),
mp(T3,T4,Ind,C3,C4).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constaints.
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det(T,T,_,C,C).

% Adjectives consist of a sequence of adjectives.
% The meaning of the arguments is the same as for noun_phrase
adjectives(T0,T2,Ind,C0,C2) :-
adj(T0,T1,Ind,C0,C1),
adjectives(T1,T2,Ind,C1,C2).
adjectives(T,T,_,C,C).


% extract subject from sentence (assumes the first subject )
% subject(T0,T1) is true if T0-T1 is a subject
subject([X|_], X) :- prop(X, subject, true).
subject([_, Y|R], S) :- subject([Y|R], S).


% extract auxiliry verb from the sentence
% aux(T0, T1) is true if T0-T1 is a auxiliary verb
aux([X|_], X) :- prop(X, aux, true).
aux([_, Y|R], A) :- aux([Y|R], A).
*/

% relation "like"
% Assume everyone likes everything in the input
% reln([like|T], T, _, _, C, [prop(whoever, like, whatever)|C]).
% reln([likes|T], T, _, _, C, [prop(whoever, likes, whatever)|C]).

/*

% question(Question,QR,Indect,Q0,Query) is true if Query-Q0 provides an answer about Indect to Question-QR
question([is | T0],T2,Ind,C0,C2) :-
noun_phrase(T0,T1,Ind,C0,C1),
mp(T1,T2,Ind,C1,C2).
question([who,is | T0],T1,Ind,C0,C1) :-
mp(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
noun_phrase(T0,T1,Ind,C0,C1).
question([who,is | T0],T1,Ind,C0,C1) :-
adjectives(T0,T1,Ind,C0,C1).
question([what | T0],T2,Ind,C0,C2) :-      % allows for a "what ... is ..."
noun_phrase(T0,[is|T1],Ind,C0,C1),
mp(T1,T2,Ind,C1,C2).
question([what | T0],T2,Ind,C0,C2) :-
noun_phrase(T0,T1,Ind,C0,C1),
mp(T1,T2,Ind,C1,C2).

*/

% ======== DATABASE =========

% Detemine the subjects and what kind of subjects (First+second or third person)
% The form of the auxiliary verb does not change between first and second person, so they are treated as the same.


prop(i, subject, s_fs).           % I do
prop(you, subject, s_ss).         % you do
prop(he, subject, s_ts).          % he does
prop(she, subject, s_ts).         % she does
prop(it, subject, s_ts).          % it does
prop(we, subject, s_fp).           % we do
prop(you, subject, s_sp).          % you do
prop(they, subject, s_tp).         % they do


% Things categorized as fruit
prop(orange, fruit, true).
prop(oranges, fruit, true).
prop(apple, fruit, true).
prop(apples, fruit, true).

% Detemine the verbs and what kind of verbs (First+second or third person or past tense)

prop(like, verb, root_v).
prop(make, verb, root_v).
prop(love, verb, root_v).
prop(dance, verb, root_v).

prop(likes, verb, root_v_s).
prop(makes, verb, root_v_s).
prop(loves, verb, root_v_s).
prop(dances, verb, root_v_s).

prop(liked, verb, past).
prop(made, verb, past).
prop(loved, verb, past).
prop(danced, verb, past).

%liking and loving are not gramatically correct, but colloquially used
prop(liking, verb_ing, true).
prop(making, verb_ing, true).
prop(loving, verb_ing, true).
prop(dancing, verb_ing, true).
prop(making, verb_ing, true).

prop(am, verb_tb, vb_fs_pr).
prop(are, verb_tb, vb_ss_pr).
prop(is, verb_tb, vb_ts_pr).
prop(are, verb_tb, vb_fp_pr).
prop(are, verb_tb, vb_sp_pr).
prop(are, verb_tb, vb_tp_pr).

prop(was, verb_tb, vb_fs_pa).
prop(were, verb_tb, vb_ss_pa).
prop(was, verb_tb, vb_ts_pa).
prop(were, verb_tb, vb_fp_pa).
prop(were, verb_tb, vb_sp_pa).
prop(were, verb_tb, vb_tp_pa).

prop(ain_t, verb_tb, vb_fs_pr).
prop(aren_t, verb_tb, vb_ss_pr).
prop(isn_t, verb_tb, vb_ts_pr).
prop(aren_t, verb_tb, vb_fp_pr).
prop(aren_t, verb_tb, vb_sp_pr).
prop(aren_t, verb_tb, vb_tp_pr).

prop(wasn_t, verb_tb, vb_fs_pa).
prop(weren_t, verb_tb, vb_ss_pa).
prop(wasn_t, verb_tb, vb_ts_pa).
prop(wasn_t, verb_tb, vb_fp_pa).
prop(wasn_t, verb_tb, vb_sp_pa).
prop(wasn_t, verb_tb, vb_tp_pa).

% Auxiliary verbs
prop(can, aux, true).
prop(do, aux, true).
prop(does, aux, a_ts).
prop(did, aux, true).
prop(have, aux, true).
prop(had, aux, true).
prop(has, aux, a_ts).
prop(will, aux, true).
prop(would, aux, true).

% Negative Auxiliary verbs
prop(can_t, aux, true).
prop(don_t, aux, true).
prop(doesn_t, aux, a_ts).
prop(didn_t, aux, true).
prop(haven_t, aux, true).
prop(hadn_t, aux, true).
prop(hasn_t, aux, a_ts).
prop(won_t, aux, true).
prop(wouldn_t, aux, true).

% Inverse Auxiliary relation
prop(can, inv_aux, can_t).
prop(can_t, inv_aux, can).
prop(do, aux, don_t).
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

% Inverse "be" verbs
prop(am, inv_be, ain_t).
prop(ain_t, inv_be, am).
prop(are, inv_be, aren_t).
prop(aren_t, inv_be, aren_t).
prop(is, inv_be, isn_t).
prop(isn_t, inv_be, is).

prop(was, inv_be, wasn_t).
prop(wasn_t, inv_be, was).
prop(were, inv_be, weren_t).
prop(weren_t, inv_be, were).


% Inverse Question Words
prop(i, inv_q_w, you).
prop(you, inv_q_w, i).


