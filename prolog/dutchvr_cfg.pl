% ============================================================
% dutchvr_cfg.pl
% ============================================================

/* Provides predicates parse/2, generate/2, generate_sample/1, parse_to_tex/1.

Use: 

?- parse([hij,zal,iets,willen,zeggen],Term).
Term = r0@(g0@(f1@d5@d1)@d2) ;
false.

?- generate([r0,g0,f1,d5,d1,d2],String).
String = [hij, zal, iets, willen, zeggen] ;
false.

?- generate_sample(6).
r0@(f0@d0),[hij,zal,vertrekken]
r0@(f1@d5@d1),[hij,zal,iets,zeggen]
r0@(g0@(f0@d0)@d2),[hij,zal,willen,vertrekken]
r0@(g0@(f1@d5@d1)@d2),[hij,zal,iets,willen,zeggen]
r0@(g1@d4@(f0@d0)@d3),[hij,zal,haar,laten,vertrekken]
false.

?- parse_to_tex([hij,zal,iets,willen,zeggen]).
true.

parse_to_tex/1 ships out ast.tex for typesetting.

sample_tex/1: sample_tex(K) generates samples of max size K;
output: sample.tex with pairs (string,semantic lambda term) for typesetting

*/

:-[inputcat,lambda_aux].
 
% ============================================================
% Abstract Syntax DCG
% ============================================================

s(r0@INFP) --> [r0],infp(INFP). % intro: 'hij zal ...'

s(x3@INFPX@TIP) --> [x3],infpx(INFPX),tip(TIP). % extraposed te-infinitive
s(x4@INFPX@TIP) --> [x4],infpx(INFPX),tip(TIP). % extraposition introduced by 'om'

% basic (in)transitive infinitives:

infp(f0@INF0) --> [f0],inf0(INF0).
infp(f1@OBJ@INF1) --> [f1],obj(OBJ),inf1(INF1).
infp(f2@OBJ@INF1) --> [f2],obj_(OBJ),inf1a(INF1).

% verb raising: bare infinitives:

infp(g0@INFP@IVR0) --> [g0],infp(INFP),ivr0(IVR0).
infp(g1@OBJ@INFP@IVR1) --> [g1],obj_(OBJ),infp(INFP),ivr1(IVR1).

% verb raising: te-infinitives:

infp(h0@TIP@IVR2) --> [h0],tip(TIP),ivr2(IVR2).
%tip(h1@INFP) --> [h1],infp(INFP).
tip(h1@INFP@CMP) --> [h1],infp(INFP),cmp(CMP).

% extraposition: the extraposed te-infinitive phrase percolates to the top s level

infpx(x0@INF2) --> [x0],inf2(INF2).
infpx(x5@OBJ1@INF3) --> [x5],obj1(OBJ1),inf3(INF3). % verzoeken
infpx(x1@INFPX@IVR0) --> [x1],infpx(INFPX),ivr0(IVR0).
infpx(x2@OBJ@INFPX@IVR1) --> [x2],obj_(OBJ),infpx(INFPX),ivr1(IVR1).

% terminals, to be populated from lexicon

inf0(d0) --> [d0]. % vertrekken
%inf0(W) --> {lex(W,inf0)},[W]. % expanded with lexicon lookup

inf1(d1) --> [d1]. % zeggen, inanimate object
%inf1(W) --> {lex(W,inf1)},[W].

inf1a(d11) --> [d11]. % ontmoeten
%inf1a(W) --> {lex(W,inf1a)},[W]. % tv, animate object

ivr0(d2) --> [d2]. % willen
%ivr0(W) --> {lex(W,ivr0)},[W].

ivr1(d3) --> [d3]. % laten
%ivr1(W) --> {lex(W,ivr1)},[W].

obj_(d4) --> [d4]. % haar: object of laten-type verb
%obj_(W) --> {lex(W,obj_)},[W].

obj(d5) --> [d5]. % iets: object of transitive verb
%obj(W) --> {lex(W,obj)},[W].

obj1(d10) --> [d10]. % hun, indirect object

ivr2(d6) --> [d6]. % proberen, vr
%ivr2(W) --> {lex(W,ivr2)},[W].

inf2(d7) --> [d7]. % proberen, extraposition
%inf2(W) --> {lex(W,inf2)},[W].

inf3(d9) --> [d9]. % verzoeken, indirect object, extraposition
%inf3(W) --> {lex(W,inf3)},[W].

cmp(d8) --> [d8]. % te

% lex/2: lex(Word,Type)

% sample lexicon entries

lex(slapen,inf0).
lex(dromen,inf0).
lex(eten,inf1).
lex(ontmoeten,inf1a).
lex(genezen,inf1a).
lex(raadplegen,inf1a).
lex(moeten,ivr0).
lex(kunnen,ivr0).
lex(doen,ivr1).
lex(zien,ivr1).
lex(hem,obj_).
lex(iemand,obj_).
lex(trachten,ivr2).
lex(pogen,ivr2).
lex(trachten,inf2).
lex(pogen,inf2).


% ============================================================
% Source --> Target: string semantics
% ============================================================

theta(F@A,appl(F1,A1)) :- theta(F,F1),theta(A,A1).
theta(Atom,Term) :- theta_(Atom,Term0),t2p(Term0,Term).

theta_(r0,Q^(Q@(X^Y^I^(hij@(zal@(X@(Y@I))))))).
theta_(f0,X^F^(F@(I^I)@X)).
theta_(f1,X^Y^F^(F@X@Y)). % inanimate object
theta_(f2,X^Y^F^(F@X@Y)). % animate object
theta_(g0,Q^X^F^(Q@(Y^Z^(F@Y@(I^(X@(Z@I))))))).
theta_(g1,X^Q^Y^F^(Q@(Z^W^(F@(I^(X@(Z@I)))@(J^(Y@(W@J))))))).
theta_(h0,Q^X^F^(Q@(Y^Z^(F@Y@(I^(X@(Z@I))))))).
%theta_(h1,Q^F^(Q@(X^Y^(F@X@(I^(te@(Y@I))))))).
theta_(h1,Q^T^F^(Q@(X^Y^(F@X@(I^(T@(Y@I))))))).
theta_(x0,X^F^(F@(I^I)@X)).
theta_(x5,X^Y^F^(F@X@Y)).
theta_(x1,Q^X^F^(Q@(Y^Z^(F@Y@(I^(X@(Z@I))))))).
theta_(x2,X^Q^Y^F^(Q@(Z^W^(F@(I^(X@(Z@I)))@(J^(Y@(W@J))))))).
theta_(x3,P^Q^(P@(X^Y^(Q@(Z^W^I^(hij@(zal@(X@(Y@(Z@(W@I))))))))))).
theta_(x4,P^Q^(P@(X^Y^(Q@(Z^W^I^(hij@(zal@(X@(Y@(om@(Z@(W@I)))))))))))).
	
theta_(d0,vertrekken).
theta_(d1,zeggen).
theta_(d2,willen).
theta_(d3,laten).
theta_(d4,haar).
theta_(d5,iets).
theta_(d6,proberen).
theta_(d7,proberen).
theta_(d8,te).
theta_(d9,verzoeken).
theta_(d10,hun).
theta_(d11,ontmoeten).

% Cf expand with lexicon

%theta_(W,W) :- once(lex(W,_)).

% ============================================================
% Source --> Target: meaning
% ============================================================

theta1(F@A,appl(F1,A1)) :- theta1(F,F1),theta1(A,A1).

theta1(r0,lambda(INFP,appl(appl(zal,condia(vc,INFP)),condia(subj,hij)))).

theta1(f0,lambda(INF0,INF0)).
theta1(f1,lambda(OBJ,lambda(INF1,appl(INF1,condia(obj,OBJ))))). % inanimate object
theta1(f2,lambda(OBJ,lambda(INF1,appl(INF1,condia(obj,OBJ))))). % animate object

theta1(g0,lambda(INFP,lambda(IVR0,appl(IVR0,condia(vc,INFP))))).
theta1(g1,lambda(OBJ,lambda(INFP,lambda(IVR1,appl(appl(IVR1,condia(vc,INFP)),condia(obj,OBJ)))))).

theta1(h0,lambda(TIP,lambda(IVR2,appl(IVR2,condia(vc,TIP))))).
theta1(h1,lambda(INFP,lambda(CMP,appl(CMP,condia(vc,INFP))))).

theta1(x0,lambda(INF2,lambda(TIP,appl(INF2,condia(vc,TIP))))).
theta1(x5,lambda(OBJ1,lambda(INF3,lambda(TIP,appl(appl(INF3,condia(vc,TIP)),condia(obj1,OBJ1)))))).
theta1(x1,lambda(INFPX,lambda(IVR0,
	lambda(X,(appl(IVR0,condia(vc,appl(INFPX,X)))))))).
theta1(x2,lambda(OBJ,lambda(INFPX,lambda(IVR1,
	lambda(X,appl(appl(IVR1,condia(vc,appl(INFPX,X))),condia(obj,OBJ))))))).

theta1(x3,lambda(INFPX,lambda(TIP,appl(appl(zal,condia(vc,appl(INFPX,TIP))),condia(subj,hij))))).
theta1(x4,lambda(INFPX,lambda(TIP,appl(appl(zal,condia(vc,appl(INFPX,TIP))),condia(subj,hij))))).
	
theta1(d0,vertrekken).
theta1(d1,zeggen).
theta1(d2,willen).
theta1(d3,laten).
theta1(d4,haar).
theta1(d5,iets).
theta1(d6,proberen).
theta1(d7,proberen).
theta1(d8,te).
theta1(d9,verzoeken).
theta1(d10,hun).
theta1(d11,ontmoeten).

%theta1(W,W) :- once(lex(W,_)).

%%%%%%%%%% Auxiaries %%%%%%%%%%%%%

string2term([Last],appl(Last,_)).
string2term([H|T],appl(H,Rest)) :- string2term(T,Rest).

term2string('$VAR'(_),[]).
term2string(appl(F,A),[F|Rest]) :-
	term2string(A,Rest).

% parse/2
% In: String, a list of words
% Out: Term, the abstract syntax term underlying the derivation of String

parse(String,Term) :-
	string2term(String,StringTerm),
	length(String,K),
	M0 is 2*K-1,
	between(K,M0,M),
	length(L,M),
	s(Term,L,[]),
	theta(Term,Sem),
	numbervars(Sem,0,_),
	reduce_sem(Sem,lambda(_,StringTerm)).

% parse_sem/3
% In: String, a list of words
% Out: Term, the abstract syntax term underlying the derivation of String

parse_sem(String,Term,Meaning) :-
	string2term(String,StringTerm),
	length(String,K),
	M0 is 2*K-1,
	between(K,M0,M),
	length(L,M),
	s(Term,L,[]),
	theta(Term,Sem),
	numbervars(Sem,0,_),
	reduce_sem(Sem,lambda(_,StringTerm)),
	theta1(Term,Sem1),
	numbervars(Sem1,0,_),
	reduce_sem(Sem1,Meaning).


% generate/2
% In: RuleConstants, a list of rule constants
% Out: String, the string obtained by the theta/2 translation
% s/3 produces Term, the derivation tree for RuleConstants

generate(RuleConstants,String) :-
	s(Term,RuleConstants,[]),
	theta(Term,Sem),
	numbervars(Sem,0,_),
	reduce_sem(Sem,lambda(_,StringTerm)),
	term2string(StringTerm,String).

% generate_sample/2 
% In: length bound K
% Out: pairs (Term,String) for abstract syntax terms of length up to K

generate_sample(K) :-
	between(1,K,N),
	length(L,N),
	s(Term,L,[]),
	sort(L,L0),length(L0,N), % filter out duplicates
	generate(L,String),
	writeln((Term,String)),
	fail.

generate_sem_sample(K) :-
	between(1,K,N),
	length(L,N),
	s(Term,L,[]),
%	member(x2,L),
	sort(L,L0),length(L0,N), % filter out duplicates
	generate(L,Words),
	atomics_to_string(Words,' ',String),
	theta1(Term,Sem),
	numbervars(Sem,0,_),
	reduce_sem(Sem,Reduced),
	writeln(Term),
	writeln(String),
	writeln(Reduced),nl,
	fail.
		
%%%%%%%%%% LaTeX %%%%%%%%%%%%%

tree(r0@INFP) --> "[.{$S:r_0$} ", tree(INFP), " ]".
tree(x3@INFPX@TIP) --> "[.{$S:x_3$} ", tree(INFPX), " ", tree(TIP), " ]".
tree(x4@INFPX@TIP) --> "[.{$S:x_4$} ", tree(INFPX), " ", tree(TIP), " ]".

tree(f0@INF0) --> "[.{$\\T{INFP}:f_0$} ", tree(INF0), " ]".
tree(f1@OBJ@INF1) --> "[.{$\\T{INFP}:f_1$} ", tree(OBJ), " ", tree(INF1), " ]".
tree(g0@INFP@IVR0) --> "[.{$\\T{INFP}:g_0$} ", tree(INFP), " ", tree(IVR0), " ]".
tree(g1@OBJ@INFP@IVR1) --> "[.{$\\T{INFP}:g_1$} ", tree(OBJ), " ", tree(INFP), " ", tree(IVR1), " ]".
tree(h0@TIP@IVR2) --> "[.{$\\T{INFP}:h_0$} ", tree(TIP), " ", tree(IVR2), " ]".
%tree(h1@INFP) --> "[.{$\\T{TIP}:h_1$} ", tree(INFP), " ]".
tree(h1@INFP@CMP) --> "[.{$\\T{TIP}:h_1$} ", tree(INFP), tree(CMP), " ]".

tree(x0@INF2) --> "[.{$\\T{INFP}_{x}:x_0$} ", tree(INF2), " ]".
tree(x1@INFPX@IVR0) --> "[.{$\\T{INFP}_{x}:x_1$} ", tree(INFPX), " ", tree(IVR0), " ]".
tree(x2@OBJ@INFPX@IVR1) --> "[.{$\\T{INFP}_{x}:x_2$} ", tree(OBJ), " ", tree(INFPX), " ", tree(IVR1), " ]".

tree(d0) --> "[.{$\\T{INF}_{0}:d_0$} ", "\\W{vertrekken}", " ]".
tree(d1) --> "[.{$\\T{INF}_{1}:d_1$} ", "\\W{zeggen}", " ]".
tree(d2) --> "[.{$\\T{IVR}_{0}:d_2$} ", "\\W{willen}", " ]".
tree(d3) --> "[.{$\\T{IVR}_{1}:d_3$} ", "\\W{laten}", " ]".
tree(d4) --> "[.{$\\T{OBJ}:d_4$} ", "\\W{haar}", " ]".
tree(d5) --> "[.{$\\T{OBJ}:d_5$} ", "\\W{iets}", " ]".
tree(d6) --> "[.{$\\T{IVR}_{2}:d_6$} ", "\\W{proberen}", " ]".
tree(d7) --> "[.{$\\T{INF}_{2}:d_7$} ", "\\W{proberen}", " ]".

tree(d8) --> "[.{$\\T{CMP}:d_8$} ", "\\W{te}", " ]".

parse_to_tex(WordList) :- 
	tell('ast.tex'),
	writeln("\\documentclass{article}"),
	writeln("\\usepackage{tikz-qtree}"),
	writeln("\\usepackage[active,tightpage]{preview}"),
	writeln("\\PreviewEnvironment{tikzpicture}"),
	writeln("\\setlength\\PreviewBorder{5pt}"),
	writeln("\\newcommand{\\W}[1]{\\textsf{#1}}"),
	writeln("\\newcommand{\\T}[1]{\\mathit{#1}}"),
	writeln("\\begin{document}"),
	tree_to_tex(WordList),
	writeln("\\end{document}"),
	told.


tree_to_tex(WordList) :-
	parse(WordList,Term),
	tree(Term,Codes,[]),
	string_to_list(LaTeX,Codes),
	atomic_list_concat(['%'|WordList],' ',String),
	writeln(String),
	writeln("\\begin{tikzpicture}"),
	write('\\Tree '),
	writeln(LaTeX),
	writeln("\\end{tikzpicture}"),
	nl,
	fail. % backtrack for alternative derivations

tree_to_tex(_).

sample_tex(K) :- 
	tell('sample.tex'),
	writeln("\\documentclass{article}"),
	writeln("\\usepackage[a4paper,landscape]{geometry}"),
	writeln("\\usepackage{amssymb}"),
	writeln("\\usepackage{amsmath}"),
	writeln("\\setlength{\\parindent}{0pt}"),
	writeln("\\begin{document}"),
	generate_sample_tex(K),
	writeln("\\end{document}"),
	told.
	
generate_sample_tex(K) :-
	between(1,K,N),
	length(L,N),
	s(Term,L,[]),
	sort(L,L0),length(L0,N), % filter out duplicates
	generate(L,Words),
	atomics_to_string(Words,' ',String),
	theta1(Term,Sem),
	numbervars(Sem,0,_),
	reduce_sem(Sem,Reduced),
	write('% '),
	writeln(Term),
	write('$\\begin{array}{l}'),
	write('\\mbox{'),
	write(String),
	writeln('}\\\\'),
	write_sem(Reduced),nl,
	writeln('\\end{array}$'),nl,nl,
	writeln('\\bigskip'),
	fail.
	
generate_sample_tex(_).
