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

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+utente(IdUt,Nome,Idade,Morada) :: findall((IdUt, Nome, Idade, Morada),(utente(IdUt, Nome, Idade, Morada)),S),
             					   comprimento(S,N), 
						     	   N == 1.

+prestador(IdPrest,Nome,Especialidade,Instituicao) :: findall((IdPrest,Nome,Especialidade,Instituicao),(prestador(IdPrest,Nome,Especialidade,Instituicao)),S),
             					   comprimento(S,N), 
						     	   N == 1.

+cuidado(Data,IdUt,IdPrest,Descricao,Custo) :: findall((Data,IdUt,IdPrest,Descricao,Custo),(cuidado(Data,IdUt,IdPrest,Descricao,Custo)),S),
             					   comprimento(S,N), 
						     	   N == 1.						     	   


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Registar utentes, prestadores e cuidados de saude

evolucao( Termo ) :- findall(I, +Termo::I, Li), 
					 insercao(Termo), 
					 teste(Li).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo), !, fail.

teste([]).
teste([H|S]) :- H, teste(S).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Remover utentes, prestadores e cuidados de saude

involucao( Termo ) :- Termo,
					  findall(I, -Termo::I, Li), 
					  remocao(Termo), 
					  teste(Li).

remocao(Termo) :- retract(Termo).
remocao(Termo) :- assert(Termo), !, fail.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado "comprimento" que calcula o numero de elementos existentes numa lista

% comprimento: Lista, C -> {V, F}.
comprimento([], 0).
comprimento([X|L], C) :- comprimento(L, N), C is 1+N.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar utentes por criterios de selecao

idGetUtentes(Id, Li) :- findall((Id, Nome), utente(Id, Nome, Idade, Morada), Li).

nomeGetUtentes(Nome, Li) :- findall((Id, Nome), utente(Id, Nome, Idade, Morada), Li).

idadeGetUtentes(Idade, Li) :- findall((Id, Nome), utente(Id, Nome, Idade, Morada), Li).

moradaGetUtentes(Morada, Li) :- findall((Id, Nome), utente(Id, Nome, Idade, Morada), Li).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar as instituicoes prestadores de cuidados de saude

getInstituicoes(Li2) :- findall(Instituicao, prestador(IdPrest, Nome, Especialidade, Instituicao), Li), 
						eliminaRepetidos(Li, Li2).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar cuidados de saude prestados por instituicao

instituicaoGetCuidados(Inst, R) :- findall(IdPrest, prestador(IdPrest, Nome, Especialidade, Inst), Li),
								   instituicaoGetCuidadosAux(Li, R).

instituicaoGetCuidadosAux([], []).
instituicaoGetCuidadosAux([H|T], [HS|TS]) :- findall((Data, IdUt, H, Descricao, Custo), cuidado(Data, IdUt, H, Descricao, Custo), HS),
									         instituicaoGetCuidadosAux(T, TS).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar cuidados de saude prestados por cidade

cidadeGetCuidados(Cidade, R) :- findall(IdUt, utente(IdUt, Nome, Idade, Cidade), Li),
  								cidadeGetCuidadosAux(Li, R).

cidadeGetCuidadosAux([], []).
cidadeGetCuidadosAux([H|T], [HS|TS]) :- findall((Data, H, IdPrest, Descricao, Custo), cuidado(Data, H, IdPrest, Descricao, Custo), HS),
									    cidadeGetCuidadosAux(T, TS).								


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar cuidados de saude prestados por datas

datasGetCuidados([], []).
datasGetCuidados([D|DS], [H|T]) :- findall((D, IdUt, IdPrest, Descricao, Custo), cuidado(D, IdUt, IdPrest, Descricao, Custo), H),
								   datasGetCuidados(DS, T).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado "eliminaRepetidos" que elimina entradas duplicadas numa lista

eliminaRepetidos([], []).

eliminaRepetidos([H|T], R) :- repetido(H,T),
							  eliminaRepetidos(T,R).

eliminaRepetidos([H|T], [H|TS]) :-
							  naoRepetido(H,T),
							  eliminaRepetidos(T, TS).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado "repetido" que elimina uma entrada duplicada numa lista

repetido(X, [X|T]).
repetido(X, [H|T]) :- repetido(X,T).

naoRepetido(X, []).
naoRepetido(X, [H|T]) :- X \= H,
						 naoRepetido(X,T).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar os utentes de um prestador

prestadorGetUtentes(IdPrest, R) :- findall(IdUt, cuidado(Data, IdUt, IdPrest, Especialidade, Custo), Li),
								   utentesGetUtentes(Li, R).

utentesGetUtentes([], []).
utentesGetUtentes([H|T], [HS|TS]) :- findall((H, Nome, Idade, Morada), utente(H, Nome, Idade, Morada), HS),
									 utentesGetUtentes(T, TS).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar os utentes de uma especialidade

especialidadeGetUtentes(Especialidade, R) :- findall(IdPrest, prestador(IdPrest, Nome, Especialidade, Instituicao), Li),
											 especialidadeGetUtentesAux(Li, R).

especialidadeGetUtentesAux([], []).
especialidadeGetUtentesAux([H|T], [HS|TS]) :- findall(IdUt, cuidado(Data, IdUt, H, Descricao, Custo), HS),
											  especialidadeGetUtentesAux(T, TS).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar os utentes de uma instituicao

instituicaoGetUtentes(Instituicao, Ri) :- findall(IdPrest, prestador(IdPrest, Nome, Especialidade, Instituicao), Li),
										  prestadoresGetUtentes(Li, Ri).

prestadoresGetUtentes([], []).
prestadoresGetUtentes([H|T], [HS|TS]) :- findall(IdUt, cuidado(Data, IdUt, H, Especialidade, Custo), HS),
										 prestadoresGetUtentes(T, TS).										 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar cuidados de saude realizados por utente

utenteGetCuidados(IdUt, Li) :- findall((Data, IdUt, IdPrest, Especialidade, Custo), cuidado(Data, IdUt, IdPrest, Especialidade, Custo), Li).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar cuidados de saude realizados por instituicao

% JA FOI IMPLEMENTADA (instituicaoGetCuidados)


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Identificar cuidados de saude realizados por prestador

prestadorGetCuidados(IdPrest, Li) :- findall((Data, IdUt, IdPrest, Especialidade, Custo), cuidado(Data, IdUt, IdPrest, Especialidade, Custo), Li).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Determinar todas as instituicoes a que um utente ja recorreu

utenteGetInstituicoes(IdUt, R) :- findall(IdPrest, cuidado(Data, IdUt, IdPrest, Descricao, Custo), Li),
								  utenteGetInstituicoesAux(Li, R).

utenteGetInstituicoesAux([], []).
utenteGetInstituicoesAux([H|T], [HS|TS]) :- findall(Instituicao, prestador(H, Nome, Especialidade, Instituicao), HS),
											utenteGetInstituicoesAux(T, TS).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Determinar todas os prestadores a que um utente ja recorreu

utenteGetPrestadores(IdUt, R) :- findall(IdPrest, cuidado(Data, IdUt, IdPrest, Especialidade, Custo), Li),
							     eliminaRepetidos(Li, LiD),
								 prestadorGetPrestador(LiD, R).

prestadorGetPrestador([], []).
prestadorGetPrestador([H|T], [HS|TS]) :- findall((H, Nome, Especialidade, Instituicao), prestador(H, Nome, Especialidade, Instituicao), HS),
									 	 prestadorGetPrestador(T, TS).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcular o custo total dos cuidados de saude por utente

utenteGetCustoTotal(IdUt, C) :- findall(Custo, cuidado(Data, IdUt, IdPrest, Descricao, Custo), Li),
								calculaCusto(Li, C).

calculaCusto([], 0).
calculaCusto([H|T], C) :- calculaCusto(T,N), C is H+N.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcular o custo total dos cuidados de saude por especialidade

especialidadeGetCustoTotal(Especialidade, R) :- findall(IdPrest, prestador(IdPrest, Nome, Especialidade, Instituicao), Li),
							     				eliminaRepetidos(Li, LiD),
								 				especialidadeGetCustoTotalAux(LiD, R).

especialidadeGetCustoTotalAux([], _).
especialidadeGetCustoTotalAux([H|T], R) :- especialidadeGetCustoTotalAux(T, N), X is prestadorGetCustoTotal(H, 0), R is X+N.					 	 		 



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcular o custo total dos cuidados de saude por prestador

prestadorGetCustoTotal(IdPrest, C) :- findall(Custo, cuidado(Data, IdUt, IdPrest, Descricao, Custo), Li),
								   	  calculaCusto(Li, C).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcular o custo total dos cuidados de saude por data

dataGetCustoTotal(Data, C) :- findall(Custo, cuidado(Data, IdUt, IdPrest, Descricao, Custo), Li),
							  calculaCusto(Li, C).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% (EXTRA) Determinar os utentes cuja idade é maior que o número introduzido

idadeMinGetUtentes(IdadeMin, Li) :- findall((IdUt, Nome, Idade, Morada), (utente(IdUt, Nome, Idade, Morada), Idade > IdadeMin), Li).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% (EXTRA) Determinar os utentes cuja idade é maior que o número introduzido

idadeMaxGetUtentes(IdadeMax, Li) :- findall((IdUt, Nome, Idade, Morada), (utente(IdUt, Nome, Idade, Morada), Idade < IdadeMax), Li).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% (EXTRA) Apresentar todos os IDs e nomes dos utentes

getIdNomeUtentes(Li) :- findall((IdUt, Nome), utente(IdUt, Nome, Idade, Morada), Li).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% (EXTRA) Apresentar todos os IDs e nomes dos prestadores

getIdNomePrestadores(Li) :- findall((IdPrest, Nome), prestador(IdPrest, Nome, Especialidade, Instituicao), Li).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% (EXTRA) Determinar o ID e nome dos utentes a partir da data do cuidado

dataGetUtentes(Data, R) :- findall(IdUt, cuidado(Data, IdUt, IdPrest, Descricao, Custo), Li),
						   dataGetUtentesAux(Li, R).

dataGetUtentesAux([], []).
dataGetUtentesAux([H|T], [HS|TS]) :- findall((H, Nome), utente(H, Nome, Idade, Morada), HS),
									 dataGetUtentesAux(T, TS).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Teste

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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -