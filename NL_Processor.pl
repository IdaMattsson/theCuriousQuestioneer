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

% Try:
%?- noun_phrase([a,tall,student],T1,I1,[],C1).
%?- noun_phrase([a,math,course],T2,I2,[],C2).
%?- noun_phrase([a,tall,student,enrolled,in,a,math,course],T3,I3,[],C3).

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



% ================= WIP Justin start =======================
% extract subject
% subject(T0,T1) is true if T0-T1 is a subject
subject([X|T], T) :- prop(X, subject, true).

% extract auxiliry verb
% aux(T0, T1) is true if T0-T1 is a auxiliry verb
aux([X|T], T) :- prop(X, aux, true).


% relation "like"
% Assume everyone likes everything in the input
% reln([like|T], T, _, _, C, [prop(whoever, like, whatever)|C]).
% reln([likes|T], T, _, _, C, [prop(whoever, likes, whatever)|C]).

% ================= WIP Justin ending =======================

% Some Example Queries
% ask noun_phrase([a,computer,science,course],R,Ind,[],C).
% ask noun_phrase([a,tall,student,enrolled,in,a,computer,science,course],R,Ind,[],C).

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

% ================= WIP Justin start =======================

% S is input sentence and Q is the returned question(s)

input(S) :- tag_question(S,Q1), write(Q1).

% tag question is true if Q is the sentence S with a tag question added at the end.
% tag question is one question type.

tag_question(S, Q1) :- tag(S, T1), atomics_to_string(S, " ", S1), string_concat(S1, T1, Q1).

% tag analyze the first two words in the sentence and produce the tag question.
% We assume the first word to be the subject and
% the second word to be either an auxiliary verb or a verb.
tag([Sub, Aux|_], T1) :- prop(Sub, subject, true), prop(Aux, aux, true), prop(Aux, inv_aux, IAux), append([,, IAux], [Sub,?], T), atomics_to_string(T, " ", T1).    % WORKED HERE

% input(S,Q) :- sentence(), produce_all();

% subject and verb agreement
%(Sub, Verb, Aux) :- prop();


% ================= WIP Justin ending =======================

%  The Database of Facts to be Queried

% course(C) is true if C is a course
course(cs312).
course(cs322).
course(math315).

dept(cs312,comp_sci).
dept(cs322,comp_sci).
dept(math315,math).

enrolled_in(john,cs312).
enrolled_in(mary,cs312).
enrolled_in(jane,math315).
enrolled_in(sally,cs322).
enrolled_in(sam,math315).

passed(S,C):-
    grade(S,C,G),
    G >= 50.

grade(sam,cs312,93).
grade(chris,cs312,82).

female(mary).
female(jane).
female(sally).
male(john).

tall(mary).
tall(jane).
tall(john).
tall(jordan).

student(mary).
student(jane).
student(sally).
student(john).
student(sam).
student(chris).


% Property triples

% ================= WIP Justin start =======================


% Detemine the subjects
prop(i, subject, true).
prop(you, subject, true).
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
prop(will, aux, true).
prop(would, aux, true).


% Negative Auxiliary verbs
prop(can_t, aux, true).
prop(don_t, aux, true).
prop(doesn_t, aux, true).
prop(didn_t, aux, true).
prop(haven_t, aux, true).
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
prop(will, inv_aux, won_t).
prop(won_t, inv_aux, will).
prop(would, inv_aux, wouldn_t).
prop(wouldn_t, inv_aux, would).




% ================= WIP Justin ending =======================


