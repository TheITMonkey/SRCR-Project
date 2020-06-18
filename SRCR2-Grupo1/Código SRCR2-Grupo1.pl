%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

% utente(IdUt, Nome, Idade, Morada).
% prestador(IdPrest, Nome, Especialidade, Instituicao).
% cuidado(Data, IdUt, IdPrest, Descricao, Custo).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais
:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes

% Invariante que nao permite a insercao de um utente repetido
+utente(IdUt,Nome,Idade,Morada) :: (findall((IdUt, Nome, Idade, Morada), utente(IdUt, Nome, Idade, Morada),S),
             					   comprimento(S,N), 
						     	   N == 1).

% Invariante que nao permite a insercao de um utente com ID repetido
+utente(IdUt, Nome, Idade, Morada) :: (findall((IdUt), utente(IdUt, N, I, M),S),
             					   	  comprimento(S,N), 
						     	   	  N == 1).

% Invariante que nao permite remover um cliente que ja foi sujeito a um cuidado de saude
-utente(IdUt, Nome, Idade, Morada) :: ( findall((IdUt), cuidado(Data, IdUt, IdPrest, Descricao, Custo), S),
									  comprimento(S, N),
									  N == 0).

% Nao permite conhecimento positivo contraditorio para o predicado utente
+(-utente(IdUt, Nome, Idade, Morada)) :: (findall((IdUt), utente(IdUt, Nome, Idade, Morada), S),
										 comprimento(S,N),
										 N == 0).

% Invariante que permite a insercao de um utente se nao se souber ao certo a sua Idade
+utente(IdUt, Nome, Idade, Morada) :: (findall( (excecao(utente(IdUt, Nome, I, Morada))),
									  excecao(utente(IdUt, Nome, I, Morada)), S), 
									  comprimento(S,N),
									  N == 0).

% Invariante que permite a insercao de um utente se nao se souber ao certo a sua Morada
+utente(IdUt, Nome, Idade, Morada) :: (findall( (excecao(utente(IdUt, Nome, Idade, M))),
									  excecao(utente(IdUt, Nome, Idade, M)), S), 
									  comprimento(S, N),
									  N == 0).

% Invariante que impede a insercao de um utente com conhecimento interdito relativamente ao seu ID
+utente(IdUt, Nome, Idade, Morada) :: (findall((IdUt, Nome, Idade, Morada), (utente(idNull, Nome, Idade, Morada), null(idNull)), S),
									  comprimento(S, N),
									  N == 0).

+(-utente(IdUt, Nome, Idade, Morada)) :: (findall((IdUt, Nome, Idade, Morada), (utente(idNull, Nome, Idade, Morada), null(idNull)), S),
									  	 comprimento(S, N),
									  	 N == 0).

% Conhecimento negativo para o predicado utente
-utente(IdUt, Nome, Idade, Morada) :- nao(utente(IdUt, Nome, Idade, Morada)),
									  nao(excecao(utente(IdUt, Nome, Idade, Morada))).

% Não permite a insercao de exceções repetidas
+(excecao(utente(IdUt, Nome, Idade, Morada))) :: (findall(excecao(utente(IdUt, Nome, Idade, Morada)), excecao(utente(IdUt, Nome, Idade, Morada)), S),
												 comprimento(S, N),
												 N == 1).

% Invariante que nao permite a insercao de um prestador repetido
+prestador(IdPrest,Nome,Especialidade,Instituicao) :: (findall((IdPrest, Nome, Especialidade, Instituicao),(prestador(IdPrest, Nome, Especialidade, Instituicao)),S),
             					   				      comprimento(S,N), 
						     	   					  N == 1).

% Invariante que nao permite a insercao de um prestador com ID repetido
+prestador(IdPrest, Nome, Especialidade, Instituicao) :: (findall( (IdPrest),prestador(IdPrest, N, E, I),S),
             					   	 					 comprimento(S,N), 
						     	   						 N == 1).

% Invariante que permite a insercao de um prestador se nao se souber ao certo a sua Especialidade
+prestador(IdPrest, Nome, Especialidade, Instituicao) :: (findall( (excecao(prestador(IdPrest, Nome, E, Instituicao))),
									 					 excecao(prestador(IdPrest, Nome, E, Instituicao)), S), 
									  					 comprimento(S, N),
									 					 N == 0).

% Invariante que impede a insercao de um prestador com conhecimento interdito relativamente ao seu ID
+prestador(IdPrest, Nome, Especialidade, Instituicao) :: (findall((IdPrest, Nome, Especialidade, Instituicao), (prestador(idNull, Nome, Especialidade, Instituicao), null(idNull)), S),
									  					 comprimento(S, N),
									  					 N == 0).

+(-prestador(IdPrest, Nome, Especialidade, Instituicao)) :: (findall((IdPrest, Nome, Especialidade, Instituicao), (prestador(idNull, Nome, Especialidade, Instituicao), null(idNull)), S),
									  	 					comprimento(S, N),
									  	 					N == 0).

% Invariante que nao permite remover um prestador que ja foi prestou um cuidado de saude
-prestador(IdPrest, Nome, Especialidade, Instituicao) :: ( findall((IdPrest), cuidado(Data, IdUt, IdPrest, Descricao, Custo), S),
									  					 comprimento(S, N),
									  					 N == 0).

% Nao permite conhecimento positivo contraditorio para o predicado prestador
+(-prestador(IdPrest, Nome, Especialidade, Instituicao)) :: (findall((IdPrest), prestador(IdPrest, Nome, Especialidade, Instituicao), S),
										 					comprimento(S,N),
										 					N == 0).

% Conhecimento negativo para o predicado Prestador
-prestador(IdPrest, Nome, Especialidade, Instituicao) :- nao(prestador(IdPrest, Nome, Especialidade, Instituicao)),
														 nao(excecao(prestador(IdPrest, Nome, Especialidade, Instituicao))).

% Não permite a insercao de exceções repetidas
+(excecao(prestador(IdPrest, Nome, Especialidade, Instituicao))) :: (findall(excecao(prestador(IdPrest, Nome, Especialidade, Instituicao)), excecao(prestador(IdPrest, Nome, Especialidade, Instituicao)), S),
												 					comprimento(S, N),
												 					N == 0).

% Só é possível inserir um cuidado, se IdUt e IdPrest existirem
+cuidado(Data, IdUt, IdPrest, Descricao, Custo) :: (findall(IdUt, utente(IdUt, Nome, Idade, Morada), SU),
												   comprimento(SU, NU),
												   NU == 1,
												   (findall(IdPrest), prestador(IdPrest, Nome, Especialidade, Instituicao), SP),
												   comprimento(SP, NP),
												   NP == 1).

% Invariante que permite a insercao de um Cuidado mesmo se nao houver conhecimento relativamente á Descricao
+cuidado(Data, IdUt, IdPrest, Descricao, Custo) :: ( findall(( excecao(cuidado(Data, IdUt, IdPrest, D, Custo))),
												   excecao(cuidado(Data, IdUt, IdPrest, D, Custo)), S),
												   comprimento(S, N),
												   N == 0).

% Conhecimento negativo para o predicado Cuidado
-cuidado(Data, IdUt, IdPrest, Descricao, Custo) :- nao(cuidado(Data, IdUt, IdPrest, Descricao, Custo)),
												   nao(excecao(cuidado(Data, IdUt, IdPrest, Descricao, Custo))).

% Não permite a insercao de exceções repetidas
+(excecao(cuidado(Data, IdUt, IdPrest, Descricao, Custo))) :: (findall(excecao(cuidado(Data, IdUt, IdPrest, Descricao, Custo)), excecao(cuidado(Data, IdUt, IdPrest, Descricao, Custo)), S),
												 			  comprimento(S, N),
												 			  N == 0).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar utentes, prestadores e cuidados de saude

evolucao( Termo ) :- findall(I, +Termo::I, Li), 
					 insercao(Termo), 
					 teste(Li).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo), !, fail.

teste([]).
teste([H|S]) :- H, teste(S).

evolucaoLista([]).
evolucaoLista([H|T]) :- evolucao(H),
						evolucaoLista(T).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Remover utentes, prestadores e cuidados de saude

involucao( Termo ) :- Termo,
					  findall(I, -Termo::I, Li), 
					  remocao(Termo), 
					  teste(Li).

remocao(Termo) :- retract(Termo).
remocao(Termo) :- assert(Termo), !, fail.

involucaoLista([]).
involucaoLista([H|T]) :- involucao(H),
						 involucaoLista(T).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado "comprimento" que calcula o numero de elementos existentes numa lista

% comprimento: Lista, C -> {V, F}.
comprimento([], 0).
comprimento([X|L], C) :- comprimento(L, N), C is 1+N.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F,D}

demo( Questao,verdadeiro ) :- Questao.

demo( Questao,falso ) :- -Questao.

demo( Questao,desconhecido ) :- nao( Questao ),
								nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demoLista: Questao,Resposta -> {V,F,D}

conjuncoes(verdadeiro, verdadeiro, verdadeiro).
conjuncoes(verdadeiro, falso, falso).
conjuncoes(falso, verdadeiro, falso).
conjuncoes(falso, falso, falso).
conjuncoes(verdadeiro, desconhecido, desconhecido).
conjuncoes(desconhecido, verdadeiro, desconhecido).
conjuncoes(falso, desconhecido, falso).
conjuncoes(desconhecido, falso, falso).
conjuncoes(desconhecido, desconhecido, desconhecido).

demoListaAux( Q, [], Q).
demoListaAux( verdadeiro, [H|T], R ) :- demoListaAux(H, T, R).
demoListaAux( falso, [H|T], falso).
demoListaAux( desconhecido, [H|T], R) :- demoListaAux(H, T, R1), conjuncoes(desconhecido,R1,R).

demoLista([], verdadeiro).
demoLista([H], R) :- demo(H, R).
demoLista( [H|T], R) :- demo(H, R1), demoLista(T, R2), conjuncoes(R1, R2, R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :- Questao, !, fail.

nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Teste

% Conhecimento Positivo
utente(1, ricardo, 20, porto).
utente(2, andre, 20, porto).
utente(3, gil, 20, braga).
utente(4, joao, 14, braga).
utente(5, maria, 34, porto).
prestador(1, carlos, medicinaGeral, porto).
prestador(2, manuel, fisioterapia, braga).
prestador(3, david, medicinaGeral, porto).
prestador(4, paula, fisioterapia, porto).
prestador(5, luis, medicinaGeral, braga).
cuidado(16032018, 1, 1, consulta, 20).
cuidado(17032018, 1, 2, fisioterapia, 20).
cuidado(16032018, 5, 4, fisioterapia, 19).
cuidado(16032018, 2, 3, consulta, 15).
cuidado(18032018, 3, 4, fisioterapia, 50).
cuidado(20032018, 1, 5, consulta, 23).

% Conhecimento Negativo
-utente(6, hugo, 67, braga).
-utente(7, carla, 29, braga).
-prestador(6, bernardo, porto).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Conhecimento Imperfeito Incerto

% Nao se sabe a Idade do utente Xavier
utente(8, xavier, idadeInc, braga).
excecao( utente(IdUt, Nome, Idade, Morada)) :- utente(IdUt, Nome, idadeInc, Morada).

% Nao se sabe a Morada da utente Margarida
utente(9, margarida, 43, moradaInc).
excecao( utente(IdUt, Nome, Idade, Morada)) :- utente(IdUt, Nome, Idade, moradaInc).

% Nao se conhece a Especialidade do prestador antonio
prestador(7, antonio, especialidadeInc, braga).
excecao( prestador(IdPrest, Nome, Especialidade, Instituicao)) :- prestador(IdPrest, Nome, especialidadeInc, Instituicao).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Conhecimento Imperfeito Impreciso

% O utente Tiago tem 4 ou 5 anos
excecao(utente( 10, tiago, 4, porto)).
excecao(utente( 10, tiago, 5, porto)).

% A utente cristina mora no Porto ou em Braga
excecao(utente( 11, cristina, 37, porto)).
excecao(utente( 11, cristina, 37, braga)).

% Nao se sabe se o cuidado prestado foi consulta ou fisioterapia
excecao(cuidado( 21032018, 2, 3, consulta, 22)).
excecao(cuidado( 21032018, 2, 3, fisioterapia, 22)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Conhecimento Imperfeito Interdito

% Nunca se sabera a idade do utente Renato
utente( 12, renato, idadeInt, porto).
excecao( utente( IdUt, Nome, Idade, Morada)) :- utente(IdUt, Nome, idadeInt, Morada).
null(idadeInt).

% Nunca se sabera a especialidade do prestador Mario
prestador( 8, mario, especialidadeInt, braga).
excecao(prestador(IdPrest, Nome, Especialidade, Instituicao)) :- prestador(IdPrest, Nome, especialidadeInt, Instituicao).
null(especialidadeInt).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -