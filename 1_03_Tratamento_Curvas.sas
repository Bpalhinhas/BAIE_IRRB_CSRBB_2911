/*Este SCRIPT tem como objetivo criar uma tabela única com as curvas de todos os indexantes,
que irá servir de input à projeção do juro para os produtos de taxa variável*/

%MACRO CURVAS_INDEXANTES(TABELA_CURVAS_INDEXANTE);

/* Passo 1: Obter lista de tabelas que começam com "curva_indexante" */
proc sql noprint;
    select catx('.', 'IN', memname)
    into :lista_tabelas separated by ' '
    from dictionary.tables
    where libname = 'IN' /* Biblioteca onde as tabelas estão */
      and memname like 'CURVA_INDEXANTE_%'; /* Filtra tabelas que começam com "curva_indexante" */
quit;

/* Verifique se encontrou tabelas */
%put &lista_tabelas;

/* Passo 2: Concatenar as tabelas listadas */
data &TABELA_CURVAS_INDEXANTE.(DROP=INDEXANTE f1);
    set &lista_tabelas;
	ID_INDEXANTE=COMPRESS(INDEXANTE);
	FORMAT DATAS DATE9.;
run;

/* Exibe o dataset final */
proc print data=&TABELA_CURVAS_INDEXANTE.(obs=10);
run;

%MEND;

%MACRO CURVAS_CSRBB(TABELA_CURVAS_CSRBB);

/* Passo 1: Obter lista de tabelas que começam com "curva_indexante" */
proc sql noprint;
    select catx('.', 'IN', memname)
    into :lista_tabelas separated by ' '
    from dictionary.tables
    where libname = 'IN' /* Biblioteca onde as tabelas estão */
      and memname like 'CURVAS_%'; /* Filtra tabelas que começam com "curva_indexante" */
quit;

/* Verifique se encontrou tabelas */
%put &lista_tabelas;

/* Passo 2: Concatenar as tabelas listadas */
data &TABELA_CURVAS_CSRBB.(DROP=INDEXANTE f1);
    set &lista_tabelas;
run;

/* Exibe o dataset final */
proc print data=&TABELA_CURVAS_CSRBB.(obs=10);
run;

%MEND;