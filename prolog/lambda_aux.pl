
% ---------------------------------------------------------------------
% Lambda term normalization (stripped-down version of Grail code)
% ---------------------------------------------------------------------

reduce_sem(Term0,Term) :-
    reduce_sem1(Term0,Term1),
    !,
    reduce_sem(Term1,Term).

reduce_sem(Term,Term).

reduce_sem1(appl(lambda(X,T0),Y),T) :-
     replace_sem(T0,X,Y,T).
reduce_sem1(lambda(X,appl(F,X)),F):-
% toegevoegd uit de originele code (MM)
     \+ subterm(F,X).
reduce_sem1(fst(pair(T,_)),T).
reduce_sem1(snd(pair(_,T)),T).
reduce_sem1(pair(fst(T),snd(T)),T).
reduce_sem1(condia(D,dedia(D,T)),T).
reduce_sem1(dedia(D,condia(D,T)),T).
reduce_sem1(conbox(D,debox(D,T)),T).
reduce_sem1(debox(D,conbox(D,T)),T).

% = recursive case

reduce_sem1(T,U) :-
     T =.. [F|Ts],
     reduce_list(Ts,Us),
     U =.. [F|Us].

% zie boven (MM)
subterm(X,X) :- !.
subterm(X,Y) :-
     functor(X,_,N),
     subterm(N,X,Y).

subterm(N0,X,Y) :-
     N0>0,
     arg(N0,X,A),
     subterm(A,Y).

subterm(N0,X,Y) :-
     N0>0,
     N is N0-1,
     subterm(N,X,Y).

% = DRS merge; simply appends contexts and conditions

reduce_list([T|Ts],[U|Ts]) :-
     reduce_sem1(T,U).
reduce_list([T|Ts],[T|Us]) :-
     reduce_list(Ts,Us).

replace_sem(X,X,Y,Y) :- !.
replace_sem(U,X,Y,V) :-
     functor(U,F,N),
     functor(V,F,N),
     replace_sem(N,U,X,Y,V).

replace_sem(0,_,_,_,_) :- !.
replace_sem(N0,U,X,Y,V) :-
     N0>0,
     N is N0-1,
     arg(N0,U,A),
     replace_sem(A,X,Y,B),
     arg(N0,V,B),
     replace_sem(N,U,X,Y,V).

substitute_sem(L,T0,T) :-
     substitute_sem(L,T0,T,0,_).

substitute_sem(_, _, _, 0).

substitute_sem(L, T0, T, NVAR) :-
	substitute_sem(L, T0, T, NVAR, _).

substitute_sem([],T,T,N,N).
substitute_sem([X-U|Rest],T0,T,N0,N) :-
     numbervars(U,N0,N1),
     replace_sem(T0,'$VAR'(X),U,T1),
     substitute_sem(Rest,T1,T,N1,N).

% ===========================================================
% Typeset lambda terms
% ===========================================================

write_sem(T) :-
%		write('\\textcolor{red}{'),
        write_sem(T,1).
%        write('}').  

write_sem(lambda(X,V),N) :-
        !,
        write_bo(N),
        write('\\lambda '),
        write_sem(X,1),
        write('.'),
        binding(V,lambda(X,V),NV),
        write_sem(V,NV),
        write_bc(N).

%write_sem(appl(F,X),_) :-
%        output_expl_brackets(no) ->
%        write_fun_args(F,[X]),
%        !.

write_sem(appl(X,Y),_) :-
        !,
        write('('),
        write_sem(X,1),
        write(' \\  '),
        write_sem(Y,1),
        write(')').

write_sem(conbox(D,X),_) :-
        !,
        write(' \\blacktriangle_{\\mathit{'),
        write(D),
        write('}}'),
        binding(X,conbox(X),NX),
        write_sem(X,NX).

write_sem(debox(D,X),_) :-
        !,
        write(' \\blacktriangledown_{\\mathit{'),
        write(D),
        write('}}'),
        binding(X,debox(X),NX),
        write_sem(X,NX).

write_sem(condia(D,X),_) :-
        !,
        write(' \\vartriangle_{\\mathit{'),
        write(D),
        write('}}'),
        binding(X,condia(X),NX),
        write_sem(X,NX).

write_sem(dedia(D,X),_) :-
        !,
        write(' \\triangledown_{\\mathit{'),
        write(D),
        write('}}'),
        binding(X,dedia(X),NX),
        write_sem(X,NX).

write_sem('$VAR'(N),_) :-
        !,
        V is N mod 3,
        I is N // 3,
        sem_var_name(V,Name),
        format('~w_{~w}',[Name,I]).

write_sem(Const,_) :-
        write('\\textsc{'),
       (atom(Const) ->
        write_atom(Const)
       ;
        print(Const)),
        write('}').

sem_var_name(0,x).
sem_var_name(1,y).
sem_var_name(2,z).

write_bo(0) :- write('(').
write_bo(1).

write_bc(0) :- write(')').
write_bc(1).

binding(T0,T,N) :-
%   (output_expl_brackets(yes) ->
%    N=0
%   ;
    binds(T0,_,_,Num0),
    binds(T,Ass,Eq,Num),
    bind1(Eq,T0,Ass,Num0,Num,N),
    !.

binding(_,_,0).

bind1( _,T,T,M ,M,1) :- !.
bind1(=<,_,_,M0,M,N) :- (M >  M0 -> N = 0 ; N = 1).
bind1( <,_,_,M0,M,N) :- (M >= M0 -> N = 0 ; N = 1).

binds(dedia(_,_),n,=<,20).
binds(condia(_,_),n,=<,20).
binds(debox(_,_),n,=<,20).
binds(conbox(_,_),n,=<,20).
binds(lambda(_,_),n,=<,12).

write_fun_args(Fun,As) :- 
        atomic_sem(Fun),
        !,
        write_sem(Fun,1),
        write('('),
        reverse(As,[],[B|Bs]),
        write_args(Bs,B),
        write(')').

write_fun_args(appl(X,Y),As) :-
        write_fun_args(X,[Y|As]).

write_args([],A) :-
        write_sem(A,1).
write_args([A|As],A0) :-
        write_sem(A0,1),
        write(' , '),
        write_args(As,A).

atomic_sem(At) :- atom(At),!.
atomic_sem('$VAR'(_)).

% =

write_atom(At) :-
    write_atom(At,'\\_{}').

write_atom(At,Pr) :-
      atom_chars(At,L),
      write_atom_list(L,Pr).

write_atom_list(L,Pr) :-
      split(L,'_',L0,[],L1),
      !,
      atom_chars(At,L0),
      format('~w~w',[At,Pr]),
      write_atom_list(L1,Pr).
write_atom_list(L,_) :-
      atom_chars(At,L),
      write(At).

split([X|Xs],X,Ys,Ys,Xs) :- !.
split([X|Xs],S,[X|Ys0],Ys,Zs) :-
      split(Xs,S,Ys0,Ys,Zs).

