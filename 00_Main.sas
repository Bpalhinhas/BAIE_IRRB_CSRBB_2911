/************************************************************************

Objetivo do Script:

Correr o projeto de validaÃ§Ã£o das tabelas de input chamando as macros 
necessÃ¡rias por ordem.

************************************************************************/
/*Imports dos inputs necessários ao processo*/

%ImportCurvas;


/* Formatação das tabelas de input ao processo */
%TRATAMENTO_PLANO_FINANCEIRO(IN.PLANO_FINANCEIRO, IN.OPERACOES_PF,AUX.T_PLANO_FINANCEIRO);

%Construcao_TIPO_AMORT(IN.PLANO_FINANCEIRO, AUX.TIPO_AMORT_CRED);
%TrataCredito(IN.CREDITO, IN.SMES, AUX.TIPO_AMORT_CRED, AUX.T_CREDITO);
%TrataObrigacoes(IN.OBRIGACOES, IN.NIVEL_FAIRVALUE, IN.DT_CALL_OPTION, AUX.T_OBRIGACOES);
%TrataDP_DO(IN.DP_DO, IN.SMES, IN.PENALIZAÇÕES_DPs,AUX.T_DP_DO, AUX.T_CC_CR);
%TrataCartasCred(IN.CARTAS_CRED, IN.SMES, IN.CCF_CARTAS_CRED, AUX.T_CARTAS_CRED);
%Trata_OperMercado(IN.CEDIDO, IN.TOMADAS, AUX.T_CEDENCIAS, AUX.T_TOMADAS);
%Tratamento_Buckets(in.BUCKETS);
%CURVAS_INDEXANTES(AUX.TABELA_CURVAS_INDEXANTE);
%TrataBalancete(IN.BALANCETE_EUR, IN.BALANCETE_USD, IN.CONTAS_BAL, AUX.BALANCETE);

/*Construção da Tabela Master*/

%TabelasMaster(AUX.T_CARTAS_CRED, AUX.T_CC_CR, AUX.T_CREDITO, AUX.T_DP_DO, AUX.T_OBRIGACOES, AUX.T_CEDENCIAS, AUX.T_TOMADAS,
						AUX.BALANCETE, AUX.T_LIVE, IN.SEGMENT_NEG, AUX.TABELA_MASTER, TESTES.T_LIVE_MASTER);

%CORRECOES_MANUAIS_MASTER(AUX.TABELA_MASTER, IN.TABELA_ERROS, OUT.TABELA_MASTER_FINAL);


/* Data Quality sobre as tabelas importadas, sem qualquer tratamento de dados (DQ1). */

%input_structure_overview(IN, CREDITO, CREDITO, DQ);
%input_structure_overview(IN, CARTAS_CRED, CARTAS_CRED, DQ);
%input_structure_overview(IN, DP_DO, DEPOSITOS, DQ);
%input_structure_overview(IN, OBRIGACOES, OBRIGACOES, DQ);
%input_structure_overview(IN, TOMADAS, TOMADO, DQ);
%input_structure_overview(IN, CEDIDO, CEDIDO, DQ);

%input_structure_overview(OUT, TABELA_MASTER_FINAL, TABELA_MASTER, DQ);

/* Concatena todas as tabelas DQ1 em uma tabela final - ( */
%concat_tables(DQ,DQ,DQ1_Resultado_%sysfunc(today(), yymmddn8.),
 DQ1_CREDITO_%sysfunc(today(), yymmddn8.) DQ1_CARTAS_CRED_%sysfunc(today(), yymmddn8.) DQ1_DP_DO_%sysfunc(today(), yymmddn8.)
 DQ1_OBRIGACOES_%sysfunc(today(), yymmddn8.) DQ1_TOMADAS_%sysfunc(today(), yymmddn8.) DQ1_CEDIDO_%sysfunc(today(), yymmddn8.)
 DQ1_TABELA_MASTER_FINAL_%sysfunc(today(), yymmddn8.)
);


/* Chamar a macro com a tabela tratada e a data de referência 
data_controls(source_lib, ds, date, outlib);*/
%data_controls(AUX, T_OBRIGACOES, DQ);
%data_controls(AUX, T_CREDITO, DQ);
%data_controls(AUX, T_DP_DO, DQ);
%data_controls(AUX, T_CC_CR, DQ);
%data_controls(AUX, T_CARTAS_CRED, DQ);
%data_controls(AUX, T_TOMADAS, DQ);
%data_controls(AUX, T_CEDENCIAS, DQ);


%data_controls(OUT, TABELA_MASTER_FINAL, DQ);

/* Concatena todas as tabelas DQ2 em uma tabela final - ( */
%concat_tables(DQ,DQ,Q2O_Resultado_%sysfunc(today(), yymmddn8.),
 Q2C_CREDITO_%sysfunc(today(), yymmddn8.) Q2C_CARTAS_CRED_%sysfunc(today(), yymmddn8.) Q2C_DP_DO_%sysfunc(today(), yymmddn8.)
 Q2C_OBRIGACOES_%sysfunc(today(), yymmddn8.) Q2C_TOMADAS_%sysfunc(today(), yymmddn8.) Q2C_CEDIDO_%sysfunc(today(), yymmddn8.)
Q2C_CC_CR_%sysfunc(today(), yymmddn8.) Q2C_TABELA_MASTER_FINAL_%sysfunc(today(), yymmddn8.));

/* Chamar macro de reconciliaÃ§Ã£o entre Tabela Master e Balancete
reconciliacao(outlib, table_1, table_2, ref_date)*/
%reconciliacao(DQ, out.TABELA_MASTER_FINAL, AUX.BALANCETE_AGREG, '30SEP2024'd);

/*%Totais_Carteiras(work,  AUX.BALANCETE_AGREG, MONTANTE_CAPITAL, B_RecDO_EUR, MOEDA, 'EUR', ID, in ('400010','400020','40010'));




/************************************************************************************************/
/*****************************************Calculo IRRBB*****************************************/

/*Construção das projeções de cashflows*/


%ProjecaoNMDs(OUT.TABELA_MASTER_FINAL, IN.BUCKETS_NMDS, AUX.PROJECAO_DO, OUT.PROJECAO_DO_OUT);

%TRATAMENTO_PLANO_FINANCEIRO(IN.PLANO_FINANCEIRO, IN.OPERACOES_PF,AUX.T_PLANO_FINANCEIRO);

%ProjecaoDP(OUT.TABELA_MASTER_FINAL, AUX.T_BUCKETS, AUX.TABELA_CURVAS_INDEXANTE, OUT.PROJECAO_DP);

%AlocacaoOvernight(OUT.TABELA_MASTER_FINAL, OUT.ALOC_OVERNIGHT)

%Tabela_Final_Proj(AUX.PROJ_OVERNIGHT, AUX.PROJ_DO, AUX.PROJ_DP, work.T_PROJECOES, AUX.TABELA_CURVAS_INDEXANTE)

/*Calculo NII*/

%NOVA_PRODUCAO(OUT.TABELA_MASTER_FINAL,  AUX.TABELA_CURVAS_INDEXANTE, OUT.NOVA_PRODUCAO);

%PROJ_NP_DESCONTO(OUT.PROJECAO_NII_1, AUX.AUX_PROJECAO_NP_0, AUX.TABELA_CURVAS_INDEXANTE, OUT.CASHFLOWS_NII_NP, OUT.CASHFLOWS_NII)

