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

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(T0,T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp([that|T0],T2,I1,C0,C2) :-
    reln(T0,T1,I1,I2,C0,C1),
    noun_phrase(T1,T2,I2,C1,C2).
mp(T,T,_,C,C).

% DICTIONARY

% adj(T0,T1,Ind,C0,C1) is true if T0-T1 is an adjective that provides properties C1-C0 to Ind
adj([computer, science | T],T,Ind,C,[dept(Ind,comp_sci)|C]).
adj([math | T],T,Ind,C,[dept(Ind,math)|C]).
adj([female | T],T,Ind,C,[female(Ind)|C]).
adj([male | T],T,Ind,C,[male(Ind)|C]).
adj([tall | T],T,Ind,C,[tall(Ind)|C]).

% noun(T0,T1,Ind,C0,C1) is true if T0-T1 is a noun that provides properties C1-C0 to Ind
noun([course | T],T,Ind,C,[course(Ind)|C]).
noun([student | T],T,Ind,C,[student(Ind)|C]).
noun([building | T],T,Ind,R,[building(Ind)|R]).
% The following are for proper nouns:
noun([Ind | T],T,Ind,C,C) :- course(Ind).
noun([Ind | T],T,Ind,C,C) :- student(Ind).

% reln(T0,T1,I1,I2,R0,R1) is true if T0-T1 is a relation
%   that provides relations R1-R0 on individuals I1 and I2
reln([enrolled, in | T],T,I1,I2,C,[enrolled_in(I1,I2)|C]).
reln([passed | T],T,I1,I2,C,[passed(I1,I2)|C]).


% extract subject from sentence (assumes the first subject )
% subject(T0,T1) is true if T0-T1 is a subject
subject([X|_], X) :- prop(X, subject, true).
subject([_, Y|R], S) :- subject([Y|R], S).

% extract auxiliry verb from the sentence
% aux(T0, T1) is true if T0-T1 is a auxiliary verb
aux([X|_], X) :- prop(X, aux, true).
aux([_, Y|R], A) :- aux([Y|R], A).


% relation "like"
% Assume everyone likes everything in the input
% reln([like|T], T, _, _, C, [prop(whoever, like, whatever)|C]).
% reln([likes|T], T, _, _, C, [prop(whoever, likes, whatever)|C]).


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


% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A,[],C),
    prove_all(C).

% prove_all(L) proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
     H,
    prove_all(T).

% ================= WIP =======================

% master function
% generate(Q) returns true if the

% S is input sentence and Q is the returned question(s)

% this version is good for keeping only the basic shape, no variable assignment. Use for final version
%input(S) :- tag_question(S,Q1), write(Q1).
% working version, expand according to number of avilable questions
%input(S, T) :- \+ check_incorrect(S), tag_question(S, T). assumes the first element is the subject.
input(S, T) :- tag_question(S, T).


/* Question Types */

% check_incorrect returns true if the first element of the input is not a subject. Expand on this later.
check_incorrect([A|_]) :- prop(A, subject, false).

% tag question is true if Q is the sentence S with a tag question added at the end.
% tag question is one question type.

tag_question(S, Q1) :- tag(S, T1), atomics_to_string(S, " ", S1), string_concat(S1, T1, Q1).

% tag analyze the first two words in the sentence and produce the tag question.
% We assume the first word to be the subject and
% the second word to be either an auxiliary verb or a verb.
tag([Sub, Aux|_], T1) :- prop(Sub, subject, true), prop(Aux, aux, true), prop(Aux, inv_aux, IAux), append([,, IAux], [Sub,?], T), atomics_to_string(T, " ", T1).

% input(S,Q) :- sentence(), produce_all();

% subject and verb agreement
%(Sub, Verb, Aux) :- prop();

% ======== DATABASE =========

% Detemine the subjects
prop(i, subject, true).
prop(you, subject, true).
prop(he, subject, true).
prop(she, subject, true).
prop(it, subject, true).
prop(we, subject, true).
prop(they, subject, true).


% Things categorized as fruit
prop(orange, fruit, true).
prop(oranges, fruit, true).
prop(apple, fruit, true).
prop(apples, fruit, true).

% Assume everyone likes everything from the input
prop(whoever, like, whatever).
prop(whoever, likes, whatever).

% Auxiliary verbs
prop(can, aux, true).
prop(do, aux, true).
prop(does, aux, true).
prop(did, aux, true).
prop(have, aux, true).
prop(had, aux, true).
prop(has, aux, true).
prop(will, aux, true).
prop(would, aux, true).


% Negative Auxiliary verbs
prop(can_t, aux, true).
prop(don_t, aux, true).
prop(doesn_t, aux, true).
prop(didn_t, aux, true).
prop(haven_t, aux, true).
prop(hadn_t, aux, true).
prop(hasn_t, aux, true).
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

% Inverse Question Words
prop(i, inv_q_w, you).
prop(you, inv_q_w, i).

% Subject Aux_Verb agreement
prop(i, sv_a, can).
prop(i, sv_a, can_t).
prop(i, sv_a, do).
prop(i, sv_a, don_t).
prop(i, sv_a, have).
prop(i, sv_a, haven_t).
prop(i, sv_a, had).
prop(i, sv_a, hadn_t).
prop(i, sv_a, will).
prop(i, sv_a, won_t).
prop(i, sv_a, would).
prop(i, sv_a, wouldn_t).

prop(you, sv_a, can).
prop(you, sv_a, can_t).
prop(you, sv_a, do).
prop(you, sv_a, don_t).
prop(you, sv_a, have).
prop(you, sv_a, haven_t).
prop(you, sv_a, had).
prop(you, sv_a, hadn_t).
prop(you, sv_a, will).
prop(you, sv_a, won_t).
prop(you, sv_a, would).
prop(you, sv_a, wouldn_t).

prop(he, sv_a, can).
prop(he, sv_a, can_t).
prop(he, sv_a, does).
prop(he, sv_a, doesn_t).
prop(he, sv_a, has).
prop(he, sv_a, hasn_t).
prop(he, sv_a, had).
prop(he, sv_a, hadn_t).
prop(he, sv_a, will).
prop(he, sv_a, won_t).
prop(he, sv_a, would).
prop(he, sv_a, wouldn_t).

prop(she, sv_a, can).
prop(she, sv_a, can_t).
prop(she, sv_a, does).
prop(she, sv_a, doesn_t).
prop(she, sv_a, has).
prop(she, sv_a, hasn_t).
prop(she, sv_a, had).
prop(she, sv_a, hadn_t).
prop(she, sv_a, will).
prop(she, sv_a, won_t).
prop(she, sv_a, would).
prop(she, sv_a, wouldn_t).

prop(it, sv_a, can).
prop(it, sv_a, can_t).
prop(it, sv_a, does).
prop(it, sv_a, doesn_t).
prop(it, sv_a, has).
prop(it, sv_a, hasn_t).
prop(it, sv_a, had).
prop(it, sv_a, hadn_t).
prop(it, sv_a, will).
prop(it, sv_a, won_t).
prop(it, sv_a, would).
prop(it, sv_a, wouldn_t).




