/*Este script tem como objetivo gerar opera��es de new busines no ambito da abordagem do balan�o constante*/
%MACRO CAL_FREQ(TABELA, COLUNA, OUT);
PROC SQL;
	CREATE TABLE &OUT. AS
		SELECT
			TIPOLOGIA_OPERACAO
			,MOEDA
			,TIPO_CLIENTE
			,TIPO_CARTEIRA
			,DSC_PRODUTO
			,TIPO_TAXA
			,ID_INDEXANTE
			,RATING
			,TIPO_AMORTIZACAO
			,FREQUENCIA_PAGAMENTO
			,FREQUENCIA_PAGAMENTO_JURO
			,PERIOD_REVISAO_TAXA
			,CCF
			,RUBRICA
			,NIVEL_FAIR_VALUE
			,PAIS
			,CLASSE_ENTIDADE
			,MATURIDADE_B
			,sum(&COLUNA.) AS SUM_&COLUNA.
	FROM &TABELA.
	GROUP BY TIPOLOGIA_OPERACAO, MOEDA, TIPO_CLIENTE, TIPO_CARTEIRA,
        		DSC_PRODUTO, TIPO_TAXA, ID_INDEXANTE, RATING, TIPO_AMORTIZACAO,
        		FREQUENCIA_PAGAMENTO, FREQUENCIA_PAGAMENTO_JURO, PERIOD_REVISAO_TAXA,
        		CCF, RUBRICA, NIVEL_FAIR_VALUE, PAIS, CLASSE_ENTIDADE, MATURIDADE_B;
	QUIT;
%MEND;

%MACRO JOIN_FREQ(TABELA_FREQ, COLUNA, TABELA_JOIN, OUT);

PROC SQL;
	CREATE TABLE &OUT. AS
	SELECT J.*
	,F.SUM_&COLUNA.
	FROM &TABELA_JOIN J
	LEFT JOIN &TABELA_FREQ. F
	ON J.TIPOLOGIA_OPERACAO=F.TIPOLOGIA_OPERACAO AND J.MOEDA=F.MOEDA AND
		J.TIPO_CLIENTE=F.TIPO_CLIENTE AND J.TIPO_CARTEIRA=F.TIPO_CARTEIRA AND
		J.DSC_PRODUTO=F.DSC_PRODUTO AND J.TIPO_TAXA=F.TIPO_TAXA AND
		J.ID_INDEXANTE=F.ID_INDEXANTE AND J.RATING=F.RATING AND 
		J.TIPO_AMORTIZACAO=F.TIPO_AMORTIZACAO AND J.FREQUENCIA_PAGAMENTO=F.FREQUENCIA_PAGAMENTO AND
		J.FREQUENCIA_PAGAMENTO_JURO=F.FREQUENCIA_PAGAMENTO_JURO AND 
		J.PERIOD_REVISAO_TAXA=F.PERIOD_REVISAO_TAXA AND J.CCF=F.CCF AND 
		J.RUBRICA=F.RUBRICA AND J.NIVEL_FAIR_VALUE=F.NIVEL_FAIR_VALUE AND 
		J.PAIS=F.PAIS AND J.CLASSE_ENTIDADE=F.CLASSE_ENTIDADE AND 
		J.MATURIDADE_B=F.MATURIDADE_B;
	QUIT;

%MEND;

%MACRO MapeamentPeriodMeses(TABELA,PERIOD,MESES, OUT);

PROC SQL;
	CREATE TABLE &OUT. AS
		SELECT *
		,CASE WHEN INDEX(&PERIOD.,'S')=1 THEN 6
				WHEN INDEX(&PERIOD.,'Semestral')=1 THEN 6
				WHEN INDEX(&PERIOD.,'Semestra')=1 THEN 6 
				WHEN INDEX(&PERIOD.,'Sem')=1 THEN 6 
				WHEN INDEX(&PERIOD.,'Y')=1 THEN 12
				WHEN INDEX(&PERIOD.,'Anual')=1 THEN 12
				WHEN INDEX(&PERIOD.,'M')=1 THEN 1
				WHEN INDEX(&PERIOD.,'Mensal')=1 THEN 1
				WHEN INDEX(&PERIOD.,'Men')=1 THEN 1
				WHEN INDEX(&PERIOD.,'da Conta')=1 THEN INTCK('month', DATA_INICIO, DATA_FIM)
				WHEN INDEX(&PERIOD.,'Da Conta')=1 THEN INTCK('month', DATA_INICIO, DATA_FIM)
				WHEN INDEX(&PERIOD.,'da')=1 THEN INTCK('month', DATA_INICIO, DATA_FIM)
				WHEN INDEX(&PERIOD.,'T')=1 THEN 3
				WHEN INDEX(&PERIOD.,'Trimestral')=1 THEN 3
				WHEN INDEX(&PERIOD.,'Trimestr')=1 THEN 3
				WHEN INDEX(&PERIOD.,'Tri')=1 THEN 3  
				WHEN INDEX(&PERIOD.,'')=1 THEN INTCK('month', DATA_INICIO, DATA_FIM)
				WHEN INDEX(&PERIOD.,'NULL')=1 THEN INTCK('month', DATA_INICIO, DATA_FIM)
				ELSE 0
				END AS &MESES.

			FROM &TABELA.;
			QUIT;


%MEND;

%MACRO CalculoProjNP(i,TABELA_MASTER, CURVAS_INDEXANTES, TABELA_OUT);

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

PROC SQL;
	CREATE TABLE AUX.AUX_INPUT_PROJ_0_&i. AS 
	SELECT *
		,CASE WHEN DATA_FIM<=DATA_REF THEN INTNX('day', DATA_REF, 1)
			ELSE DATA_FIM
			END AS DATA_FIM_A
		,CASE WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'S')=1 THEN 6
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'Semestral')=1 THEN 6
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'Semestra')=1 THEN 6 
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'Sem')=1 THEN 6 
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'Y')=1 THEN 12
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'Anual')=1 THEN 12
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'M')=1 THEN 1
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'Mensal')=1 THEN 1
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'Men')=1 THEN 1
				WHEN INDEX(COMPRESS(FREQUENCIA_PAGAMENTO_JURO),'daConta')=1 THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				WHEN INDEX(COMPRESS(FREQUENCIA_PAGAMENTO_JURO),'DaConta')=1 THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'da')=1 THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'T')=1 THEN 3
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'Trimestral')=1 THEN 3
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'Trimestr')=1 THEN 3
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'Tri')=1 THEN 3  
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'')=1 THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				WHEN FREQUENCIA_PAGAMENTO_JURO IS MISSING THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				WHEN INDEX(FREQUENCIA_PAGAMENTO_JURO,'NULL')=1 THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				END AS PERIOD_PAG_M_JURO		
		,CASE WHEN INDEX(FREQUENCIA_PAGAMENTO,'S')=1 THEN 6
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'Semestral')=1 THEN 6
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'Semestra')=1 THEN 6 
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'Sem')=1 THEN 6 
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'Y')=1 THEN 12
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'Anual')=1 THEN 12
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'M')=1 THEN 1
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'Mensal')=1 THEN 1
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'Men')=1 THEN 1
				WHEN INDEX(COMPRESS(FREQUENCIA_PAGAMENTO),'daConta')=1 THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				WHEN INDEX(COMPRESS(FREQUENCIA_PAGAMENTO),'DaConta')=1 THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'da')=1 THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'T')=1 THEN 3
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'Trimestral')=1 THEN 3
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'Trimestr')=1 THEN 3
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'Tri')=1 THEN 3  
				WHEN INDEX(FREQUENCIA_PAGAMENTO,'')=1 THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				WHEN FREQUENCIA_PAGAMENTO IS MISSING THEN INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A)
				END AS PERIOD_PAG_M_CAP

		,INTCK('month', DATA_REF, CALCULATED DATA_FIM_A) AS MATURIDADE_RESIDUAL
		,INTCK('month', DATA_INICIO, CALCULATED DATA_FIM_A) AS PRAZO
		,CASE WHEN FREQUENCIA_PAGAMENTO = '' OR FREQUENCIA_PAGAMENTO='NULL' THEN "BULLET" 
			ELSE FREQUENCIA_PAGAMENTO
			END AS TIPO_PAGAMENTO
		,CASE WHEN TIPOLOGIA_OPERACAO="DP" OR TIPOLOGIA_OPERACAO="OBRIGACOES" OR TIPOLOGIA_OPERACAO="CARTAS_CRED"
					OR TIPOLOGIA_OPERACAO="TOMADO" OR TIPOLOGIA_OPERACAO="CEDIDO"
						THEN "BULLET"
			WHEN TIPOLOGIA_OPERACAO="CREDITO" THEN TIPO_AMORTIZACAO
		END AS TIPO_AMORTIZACAO_F
		,CASE WHEN TIPO_TAXA NOT ='F' THEN SPREAD*(CALCULATED PERIOD_PAG_M_JURO/12)
			WHEN TIPO_TAXA ='F' THEN TX_JURO*(CALCULATED PERIOD_PAG_M_JURO/12)
			END AS TX_J_FIXA_A
		,CASE WHEN TIPO_TAXA NOT ='F' THEN SPREAD*(1/12)
			WHEN TIPO_TAXA ='F' AND CENARIO="BASELINE" OR (INDEX(ID_OPERACAO, 'SINTETICA')=0 AND TIPO_TAXA ='F')  THEN TX_JURO*(1/12)
			WHEN TIPO_TAXA ='F' AND CENARIO="PARALLEL_UP" AND INDEX(ID_OPERACAO, 'SINTETICA')=1 THEN (TX_JURO+0.02)*(1/12) 
			WHEN TIPO_TAXA ='F' AND CENARIO="PARALLEL_DOWN" AND INDEX(ID_OPERACAO, 'SINTETICA')=1 THEN (TX_JURO-0.02)*(1/12)
			END AS JURO_NII
		,"Mensal" AS FREQ_JURO_NII
		,1 AS PERIOD_PAG_M_NII
		
	FROM &TABELA_MASTER. WHERE (TIPOLOGIA_OPERACAO="DP" OR TIPOLOGIA_OPERACAO="OBRIGACOES" OR 
							TIPOLOGIA_OPERACAO="CREDITO" OR (TIPOLOGIA_OPERACAO="CARTAS_CRED" AND ID_OPERACAO NOT LIKE "%NULL")
							OR TIPOLOGIA_OPERACAO="TOMADO" OR TIPOLOGIA_OPERACAO="CEDIDO") AND
								(CENARIO="BASELINE" OR CENARIO="PARALLEL_UP" OR CENARIO="PARALLEL_DOWN");
	QUIT;


data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

%MapeamentPeriodMeses(AUX.AUX_INPUT_PROJ_0_&i.,PERIOD_REVISAO_TAXA,PERIOD_REV_TX_M, AUX.NII_CALC_PERIOD_REV_&i.);

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;


DATA AUX.NII_INPUT_PROJ_1_&i.(DROP=TIPO_AMORTIZACAO DATA_FIM_L);
	SET AUX.NII_CALC_PERIOD_REV_&i.(RENAME=(DATA_FIM=DATA_FIM_L DATA_FIM_A=DATA_FIM));
	IF TIPOLOGIA_OPERACAO IN ("CEDIDO", "TOMADO") THEN
		ID_OPERACAO=CATT(ID_OPERACAO,TIPOLOGIA_OPERACAO);
	IF DATA_PROXIMA_REPRICING=. THEN DATA_PROXIMA_REPRICING=DATA_PROX_PAG;
	IF DATA_PROXIMA_REPRICING<=DATA_REF THEN 
		DATA_PROXIMA_REPRICING=INTNX('MONTH', DATA_PROXIMA_REPRICING, PERIOD_REV_TX_M, 'S');
	DO T_MES=0 TO MATURIDADE_RESIDUAL;
	OUTPUT;
	END;
	FORMAT DATA_FIM DATE9.;
	RUN;

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

PROC SORT DATA=AUX.NII_INPUT_PROJ_1_&i. OUT=AUX.NII_CALC_PERIOD_REV_1_&i.;
    BY ID_OPERACAO T_MES;
RUN;

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;
DATA AUX.NII_INPUT_PROJ_2_&i.;
	SET AUX.NII_CALC_PERIOD_REV_1_&i.;
	BY ID_OPERACAO;
	IF T_MES=0 THEN DATA_PROJ=DATA_REF;
		ELSE DATA_PROJ=intnx('MONTH', DATA_REF, T_MES, 'E');	
	FORMAT DATA_PROJ date9.;
	IF DATA_PROJ<DATA_PROX_PAG OR DATA_PROJ<DATA_PROXIMA_REPRICING
		THEN DATA_CURVA=DATA_REF;
		ELSE
			IF TIPOLOGIA_OPERACAO="DP" THEN DATA_CURVA=DATA_PROXIMA_REPRICING;
				ELSE DATA_CURVA=DATA_PROX_PAG;
	IF TIPO_TAXA="V" THEN DO;
		RETAIN DATA_PROXIMA_REPRICING_NII DATA_CURVA_NII;
    	IF FIRST.ID_OPERACAO THEN DO;
        /* Inicializa as vari�veis com as primeiras datas */
        	DATA_CURVA_NII = DATA_CURVA;
        	DATA_PROXIMA_REPRICING_NII = DATA_PROXIMA_REPRICING;
    	END;
    	ELSE IF DATA_PROJ<DATA_PROXIMA_REPRICING_NII THEN DO;
			DATA_PROXIMA_REPRICING_NII=DATA_PROXIMA_REPRICING_NII;
			DATA_CURVA_NII=DATA_CURVA_NII;
			END;
		ELSE IF DATA_PROJ>=DATA_PROXIMA_REPRICING_NII AND 
				DATA_PROJ<INTNX('MONTH', DATA_PROXIMA_REPRICING_NII, PERIOD_REV_TX_M, 'E')
				THEN DO;
				DATA_CURVA_NII=DATA_PROXIMA_REPRICING_NII;
				DATA_PROXIMA_REPRICING_NII=INTNX('MONTH', DATA_PROXIMA_REPRICING_NII, PERIOD_REV_TX_M, 'E');
				END;
			
		ELSE DO;
            	/* Mant�m os valores de DATA_CURVA_NII e DATA_PROXIMA_REPRICING_NII */
            DATA_CURVA_NII = DATA_CURVA_NII;
            DATA_PROXIMA_REPRICING_NII = DATA_PROXIMA_REPRICING_NII;
    	END;
		FORMAT DATA_PROXIMA_REPRICING_NII DATA_CURVA DATA_CURVA_NII date9.;
	END;
	RUN;


data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run; 


PROC SQL;
	CREATE TABLE AUX.PROJ_JUNCAO_CURVAS_NII&i. AS
		SELECT 
		PRJ.*
		,CURV.CURVA AS BASELINE_NII
        ,CURV.PARALLEL_UP AS PARALLEL_UP_NII 
        ,CURV.PARALLEL_DOWN AS PARALLEL_DOWN_NII          
		FROM AUX.NII_INPUT_PROJ_2_&i.  PRJ
		LEFT JOIN  AUX.TABELA_CURVAS_INDEXANTE CURV
		ON PRJ.DATA_CURVA_NII=CURV.DATAS AND PRJ.ID_INDEXANTE=CURV.ID_INDEXANTE;
		QUIT;

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

PROC SQL;
    CREATE TABLE AUX.JUNCAO_CURVAS_I_NII&i. AS
    SELECT 
        PRJ.*,
        CURV.CURVA AS BASELINE_I,
        CURV.PARALLEL_UP AS PARALLEL_UP_I,
        CURV.PARALLEL_DOWN AS PARALLEL_DOWN_I          
    FROM AUX.PROJ_JUNCAO_CURVAS_NII&i. PRJ
    LEFT JOIN AUX.TABELA_CURVAS_INDEXANTE CURV
    ON (CASE 
            WHEN PRJ.DATA_INICIO >= PRJ.DATA_REF THEN PRJ.DATA_INICIO
            ELSE PRJ.DATA_REF 
        END) = CURV.DATAS 
        AND PRJ.ID_INDEXANTE = CURV.ID_INDEXANTE;
QUIT;


data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

PROC SQL;
	CREATE TABLE AUX.PROJ_JUNCAO_CURVAS_N_&i. AS
		SELECT 
		PRJ.*
		,CURV.CURVA AS BASELINE
        ,CURV.PARALLEL_UP AS PARALLEL_UP
        ,CURV.PARALLEL_DOWN AS PARALLEL_DOWN        
		FROM AUX.JUNCAO_CURVAS_I_NII&i.  PRJ
		LEFT JOIN  AUX.TABELA_CURVAS_INDEXANTE CURV
		ON PRJ.DATA_CURVA=CURV.DATAS AND PRJ.ID_INDEXANTE=CURV.ID_INDEXANTE;
		QUIT;

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

PROC SQL;
	CREATE TABLE AUX.PROJ_JUNCAO_CURVAS_1_&i. AS
		SELECT
		*
		,CASE WHEN TIPO_TAXA ='V' AND 
					(DATA_PROJ<DATA_PROX_PAG OR DATA_PROJ<DATA_PROXIMA_REPRICING)
							THEN (TX_JURO-SPREAD)*(PERIOD_PAG_M_CAP/12)
		WHEN TIPO_TAXA ='V' AND CENARIO="BASELINE" AND
					(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING)
						THEN BASELINE_I*(PERIOD_PAG_M_CAP/12)
		WHEN TIPO_TAXA ='V' AND CENARIO="PARALLEL_UP" AND
					(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING)
						THEN PARALLEL_UP_I*(PERIOD_PAG_M_CAP/12)
		WHEN TIPO_TAXA ='V' AND CENARIO="PARALLEL_DOWN" AND
					(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING)
						THEN PARALLEL_DOWN_I*(PERIOD_PAG_M_CAP/12)
		ELSE 0
		 END AS TX_VARIAVEL_A_NII
		,CASE WHEN TIPO_TAXA ='V' AND 
			(DATA_PROJ<DATA_PROX_PAG OR DATA_PROJ<DATA_PROXIMA_REPRICING)
				THEN (TX_JURO-SPREAD)*(PERIOD_PAG_M_CAP/12)
		WHEN TIPO_TAXA ='V' AND CENARIO="BASELINE" AND
					(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING)
						THEN BASELINE_I*(PERIOD_PAG_M_CAP/12)
		WHEN TIPO_TAXA ='V' AND CENARIO="PARALLEL_UP" AND
					(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING)
						THEN PARALLEL_UP_I*(PERIOD_PAG_M_CAP/12)
		WHEN TIPO_TAXA ='V' AND CENARIO="PARALLEL_DOWN" AND
					(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING)
						THEN PARALLEL_DOWN_I*(PERIOD_PAG_M_CAP/12)
		ELSE 0 
		 END AS TX_VARIAVEL_A
		,(JURO_NII*PERIOD_PAG_M_CAP)+ (CALCULATED TX_VARIAVEL_A_NII) AS TX_MOMENTO_INICIAL_NII
		,(TX_J_FIXA_A*PERIOD_PAG_M_CAP)+(CALCULATED TX_VARIAVEL_A) AS TX_MOMENTO_INICIAL
		,CATX('-', CENARIO, ID_OPERACAO) AS ID_OPERACAO_AUX
		FROM AUX.PROJ_JUNCAO_CURVAS_N_&i. 
		ORDER BY CENARIO, ID_OPERACAO, T_MES;
QUIT;

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

DATA &TABELA_OUT.;
    SET AUX.PROJ_JUNCAO_CURVAS_1_&i.(RENAME=(TIPO_AMORTIZACAO_F=TIPO_AMORTIZACAO));
    BY ID_OPERACAO_AUX;
    
    RETAIN N_PAG 0 CAPITAL_REM 0 CAPITAL_REM_NII 0;

    IF FIRST.ID_OPERACAO_AUX THEN DO;
        N_PAG = 0;

        IF (TIPOLOGIA_OPERACAO = "BALANCETE" OR TIPOLOGIA_OPERACAO = "CREDITO") THEN DO;
            CAPITAL_REM = CAPITAL_VINCENDO;
            CAPITAL_REM_NII = CAPITAL_VINCENDO;
        END;
        ELSE DO;
            CAPITAL_REM = MONTANTE_CAPITAL;
            CAPITAL_REM_NII = MONTANTE_CAPITAL;
        END;

        CF_CAPITAL = 0;
        CF_JURO_SPREAD = 0;
        CF_JURO_VAR = 0;
        CF_CAPITAL_NII = 0;
        CF_JURO_NII_VAR = 0;
        CF_JURO_NII = 0;
    END;
    
    ELSE DO;
        IF MOD(MATURIDADE_RESIDUAL - T_MES, PERIOD_PAG_M_CAP) = 0 THEN DO;
            IF TIPO_AMORTIZACAO = "Amortiza��o Constante" THEN DO;
                CF_CAPITAL_NII = CAPITAL_REM_NII / (CEIL(MATURIDADE_RESIDUAL / PERIOD_PAG_M_CAP) - N_PAG);
                CF_CAPITAL = CAPITAL_REM / (CEIL(MATURIDADE_RESIDUAL / PERIOD_PAG_M_CAP) - N_PAG);
            END;
            ELSE IF TIPO_AMORTIZACAO = "Pagamento Constante" THEN DO;
                CF_CAPITAL_NII = PPMT(TX_MOMENTO_INICIAL, 1, CEIL(MATURIDADE_RESIDUAL / PERIOD_PAG_M_CAP) - N_PAG, CAPITAL_REM_NII);                
                CF_CAPITAL = PPMT(TX_MOMENTO_INICIAL, 1, CEIL(MATURIDADE_RESIDUAL / PERIOD_PAG_M_CAP) - N_PAG, CAPITAL_REM);
            END;
            ELSE IF (INDEX(TIPO_AMORTIZACAO, "BULLET") > 0) AND (T_MES = MATURIDADE_RESIDUAL) THEN DO;
                CF_CAPITAL = CAPITAL_REM;
                CF_JURO_VAR = 0;
                CF_CAPITAL_NII = CAPITAL_REM_NII;
                CF_JURO_NII_VAR = 0;
                CF_JURO_NII = 0;
            END;
            ELSE DO;
                CF_CAPITAL = 0;
                CF_JURO_VAR = 0;
                CF_CAPITAL_NII = 0;
                CF_JURO_NII_VAR = 0;
                CF_JURO_NII = 0;
            END;
            
            N_PAG = N_PAG + 1;
        END;
        ELSE DO;
            CF_CAPITAL = 0;
            CF_CAPITAL_NII = 0;
            CF_JURO_VAR = 0;
            CF_JURO_NII_VAR = 0;
        END;

        IF MOD(MATURIDADE_RESIDUAL - T_MES, PERIOD_PAG_M_JURO) = 0 THEN DO;
            IF CENARIO = "BASELINE" OR INDEX(ID_OPERACAO, 'SINTETICA') = 0 THEN 
                CF_JURO_SPREAD = TX_J_FIXA_A * CAPITAL_REM;
            ELSE IF CENARIO = "PARALLEL_UP" AND INDEX(ID_OPERACAO, 'SINTETICA') > 0 THEN 
                CF_JURO_SPREAD = (TX_J_FIXA_A + (0.02*PERIOD_PAG_M_JURO/12)) * CAPITAL_REM;
            ELSE IF CENARIO = "PARALLEL_DOWN" AND INDEX(ID_OPERACAO, 'SINTETICA') > 0  THEN 
                CF_JURO_SPREAD = (TX_J_FIXA_A - (0.02*PERIOD_PAG_M_JURO/12)) * CAPITAL_REM;
        END;
        
        IF TIPO_TAXA = 'F' THEN 
            CF_JURO_VAR = 0;
        ELSE DO;
            IF (DATA_PROJ<DATA_PROX_PAG OR DATA_PROJ<DATA_PROXIMA_REPRICING) THEN 
					CF_JURO_VAR = CAPITAL_REM_NII *(TX_JURO-SPREAD)*(PERIOD_PAG_M_NII / 12);

			ELSE IF CENARIO = "BASELINE" AND 
				(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING) THEN 
                CF_JURO_VAR = CAPITAL_REM_NII * BASELINE * (PERIOD_PAG_M_NII / 12);
            ELSE IF CENARIO = "PARALLEL_UP" AND 
				(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING) THEN 
                CF_JURO_VAR = CAPITAL_REM_NII * PARALLEL_UP * (PERIOD_PAG_M_NII / 12);
            ELSE IF CENARIO = "PARALLEL_DOWN" AND 
				(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING) THEN 
                CF_JURO_VAR = CAPITAL_REM_NII * PARALLEL_DOWN * (PERIOD_PAG_M_NII / 12);
        END;

        IF MOD(MATURIDADE_RESIDUAL - T_MES, PERIOD_PAG_M_NII) = 0 THEN DO;
			CF_JURO_NII = MAX(JURO_NII,0) * CAPITAL_REM_NII;

            IF TIPO_TAXA = 'F' THEN 
                CF_JURO_NII_VAR = 0;
            ELSE DO;
                IF (DATA_PROJ<DATA_PROX_PAG OR DATA_PROJ<DATA_PROXIMA_REPRICING) THEN 
					CF_JURO_NII_VAR = CAPITAL_REM_NII *(TX_JURO-SPREAD)*(PERIOD_PAG_M_NII / 12);
				ELSE IF CENARIO = "BASELINE" AND 
				(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING) THEN 
                    CF_JURO_NII_VAR = CAPITAL_REM_NII * BASELINE_NII * (PERIOD_PAG_M_NII / 12);
                ELSE IF CENARIO = "PARALLEL_UP" AND 
				(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING) THEN 
                    CF_JURO_NII_VAR = CAPITAL_REM_NII * PARALLEL_UP_NII * (PERIOD_PAG_M_NII / 12);
                ELSE IF CENARIO = "PARALLEL_DOWN" AND 
				(DATA_PROJ>=DATA_PROX_PAG AND DATA_PROJ>=DATA_PROXIMA_REPRICING) THEN 
                    CF_JURO_NII_VAR = CAPITAL_REM_NII * PARALLEL_DOWN_NII * (PERIOD_PAG_M_NII / 12);
            END;
        END;
        ELSE DO;
            CF_JURO_NII_VAR = 0;
            CF_JURO_NII = 0;
            CF_JURO_SPREAD = 0;
            CF_JURO_VAR = 0;
        END;

        CAPITAL_REM = CAPITAL_REM - CF_CAPITAL;
        CAPITAL_REM_NII = CAPITAL_REM_NII - CF_CAPITAL_NII;
    END;
    
RUN;



data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

%MEND;


%MACRO NOVA_PRODUCAO(TABELA_MASTER, CURVAS_INDEXANTES, NOVA_PRODUCAO);


PROC SQL NOPRINT;
    SELECT max(DATA_REF) 
    INTO :DATA_REF
    FROM &TABELA_MASTER. 
    
QUIT;


%let DATA_REF = %sysfunc(putn(&DATA_REF, date9.));

%PUT"1";
DATA AUX.AUX_NOVA_PRODUCAO_0;
	SET &TABELA_MASTER.;
	WHERE (ELEG_NII='S' AND INDEX(ID_OPERACAO, 'OFF')=0);
	IF TIPOLOGIA_OPERACAO IN ("CEDIDO", "TOMADO") THEN
	ID_OPERACAO=CATT(ID_OPERACAO,TIPOLOGIA_OPERACAO);
	IF DATA_PROJ=. THEN DATA_PROJ=DATA_PROJ_V;
		ELSE DATA_PROJ=DATA_PROJ;
	IF DATA_FIM<DATA_REF THEN DATA_FIM=DATA_REF;
	MATURIDADE_B=intck('MONTH', DATA_INICIO, DATA_FIM);
	MES_VENCIMENTO=INTNX('MONTH', DATA_FIM,0,'E');
	IF TX_JURO=. THEN TX_JURO=0;
	IF SPREAD=. THEN SPREAD=0;
	IF (TIPOLOGIA_OPERACAO = "BALANCETE" OR TIPOLOGIA_OPERACAO = "CREDITO") THEN
            EXPOSICAO = CAPITAL_VINCENDO;
	ELSE EXPOSICAO=MONTANTE_CAPITAL;
	RUN;

%PUT"3";

DATA AUX.AUX_NOVA_PRODUCAO_01;
	SET AUX.AUX_NOVA_PRODUCAO_0;
	RUN;
%PUT"2";
%CAL_FREQ(AUX.AUX_NOVA_PRODUCAO_01, EXPOSICAO, AUX_SOMA_EXP);

%JOIN_FREQ(AUX_SOMA_EXP, EXPOSICAO, AUX.AUX_NOVA_PRODUCAO_01, AUX.AUX_NOVA_PRODUCAO_02);



PROC SQL;
	CREATE TABLE AUX.CALCULO_TX_JURO_SPRED_MATURIDADE AS
		SELECT
		TIPOLOGIA_OPERACAO
		,NIVEL_FAIR_VALUE
		,PAIS
		,CLASSE_ENTIDADE
		,MOEDA
		,TIPO_CLIENTE
		,TIPO_CARTEIRA
		,DSC_PRODUTO
		,TIPO_TAXA
		,ID_INDEXANTE
		,RATING

		,CASE WHEN TIPOLOGIA_OPERACAO="DP" OR TIPOLOGIA_OPERACAO="OBRIGACOES" OR TIPOLOGIA_OPERACAO="CARTAS_CRED"
					OR TIPOLOGIA_OPERACAO="TOMADO" OR TIPOLOGIA_OPERACAO="CEDIDO"
						THEN "BULLET"
			WHEN TIPOLOGIA_OPERACAO="CREDITO" THEN TIPO_AMORTIZACAO
		END AS TIPO_AMORTIZACAO
		,FREQUENCIA_PAGAMENTO
		,FREQUENCIA_PAGAMENTO_JURO
		,PERIOD_REVISAO_TAXA
        ,SUM(TX_JURO * (( EXPOSICAO/SUM_EXPOSICAO))) / SUM(( EXPOSICAO/SUM_EXPOSICAO)) AS TX_JURO	
		,SUM(SPREAD * (( EXPOSICAO/SUM_EXPOSICAO)))/SUM(( EXPOSICAO/SUM_EXPOSICAO)) AS SPREAD	
		,MATURIDADE_B AS MATURIDADE
		,CCF
		,RUBRICA

	FROM AUX.AUX_NOVA_PRODUCAO_02
		GROUP BY TIPOLOGIA_OPERACAO, MOEDA, TIPO_CLIENTE, TIPO_CARTEIRA,
        		DSC_PRODUTO, TIPO_TAXA, ID_INDEXANTE, RATING, TIPO_AMORTIZACAO,
        		FREQUENCIA_PAGAMENTO, FREQUENCIA_PAGAMENTO_JURO, PERIOD_REVISAO_TAXA,
        		CCF, RUBRICA, NIVEL_FAIR_VALUE, PAIS, CLASSE_ENTIDADE, MATURIDADE;
	QUIT;


/*Proje��o NII da carteira atual*/

DATA AUX.PROJECOES_PARALLEL_UP_NII;
	SET &TABELA_MASTER.(DROP=CENARIO);
		CENARIO="PARALLEL_UP";
		WHERE ELEG_NII='S' AND INDEX(ID_OPERACAO, 'OFF')=0;
		RUN;

DATA AUX.PROJECOES_PARALLEL_DOWN_NII;
	SET &TABELA_MASTER.(DROP=CENARIO);
		CENARIO="PARALLEL_DOWN";
		WHERE ELEG_NII='S' AND INDEX(ID_OPERACAO, 'OFF')=0;
		RUN;

DATA AUX.PROJECOES_BASELINE_NII;
	SET &TABELA_MASTER.(DROP=CENARIO);
		WHERE ELEG_NII='S' AND INDEX(ID_OPERACAO, 'OFF')=0;
		CENARIO="BASELINE";
		RUN;

DATA AUX.CONCAT_TODOS_CENARIOS_NII;
	SET AUX.PROJECOES_PARALLEL_DOWN_NII AUX.PROJECOES_PARALLEL_UP_NII AUX.PROJECOES_BASELINE_NII;
		RUN;


%CalculoProjNP(0, AUX.CONCAT_TODOS_CENARIOS_NII, &CURVAS_INDEXANTES., AUX.PROJECAO_NII_CARTEIRA_A)

/*Junta_proje��es originais*/


	
	DATA AUX.AUX_PROJECAO_NP_0(KEEP=TIPOLOGIA_OPERACAO MOEDA TIPO_CLIENTE DATA_REF TIPO_CARTEIRA
							CENARIO DSC_PRODUTO TIPO_TAXA ID_INDEXANTE RATING TIPO_AMORTIZACAO PAIS
							FREQUENCIA_PAGAMENTO FREQUENCIA_PAGAMENTO_JURO CAPITAL_REM_NII NIVEL_FAIR_VALUE
							PERIOD_REVISAO_TAXA CCF RUBRICA CF_CAPITAL_NII MES_VENCIMENTO DATA_PROJ 
							DATA_INICIO DATA_FIM ELEG_NII CF_JURO_NII_VAR ID_OPERACAO CF_JURO_NII
							CLASSE_ENTIDADE TX_JURO JURO_NII);
		SET AUX.PROJECAO_NII_CARTEIRA_A;
		MES_VENCIMENTO=INTNX('MONTH', DATA_FIM,0,'E');
		IF DATA_PROJ=. THEN DATA_PROJ=DATA_PROJ_V;
		ELSE DATA_PROJ=DATA_PROJ;
		WHERE ELEG_NII='S' AND INDEX(ID_OPERACAO, 'OFF')=0;
		FORMAT MES_VENCIMENTO DATA_PROJ DATE9.;
		IF DATA_PROJ=. THEN DATA_PROJ=DATA_PROJ_V;
		ELSE DATA_PROJ=DATA_PROJ;
		RUN;


%PUT"ANTES LOOP";
/*Aqui come�a o loop da cria��o da nova produ��o*/
%do i = 1 %to 12;
	%put itera��o=&i.;
	 %let i_value = &i; 
	 %LET PROJ=%EVAL(&i_value-1);
	 %PUT &PROJ;


	DATA AUX.PROJECOES_SEM_NP_1_&i_value.(KEEP=TIPOLOGIA_OPERACAO MOEDA TIPO_CLIENTE DATA_REF TIPO_CARTEIRA
							CENARIO DSC_PRODUTO TIPO_TAXA ID_INDEXANTE RATING TIPO_AMORTIZACAO NIVEL_FAIR_VALUE
							FREQUENCIA_PAGAMENTO FREQUENCIA_PAGAMENTO_JURO CAPITAL_REM_NII PAIS
							PERIOD_REVISAO_TAXA CCF RUBRICA CF_CAPITAL_NII MES_VENCIMENTO DATA_PROJ 
							DATA_INICIO DATA_FIM ELEG_NII CLASSE_ENTIDADE MATURIDADE);
		SET AUX.AUX_PROJECAO_NP_&PROJ.;
		MES_VENCIMENTO=INTNX('MONTH', DATA_FIM,0,'E');
		WHERE ELEG_NII='S' AND (DATA_PROJ <= INTNX('MONTH', %sysfunc(inputn(&DATA_REF, date9.)),13,'E'))
			AND (CENARIO="BASELINE" OR CENARIO="PARALLEL_UP" OR CENARIO="PARALLEL_DOWN") AND INDEX(ID_OPERACAO, 'OFF')=0;
		FORMAT MES_VENCIMENTO DATA_PROJ DATE9.;
		
		MATURIDADE=intck('MONTH', DATA_INICIO, DATA_FIM);
		RUN;

	PROC SQL;
		CREATE TABLE AUX.PROJ_SEM_NP_UNICAS_&i_value. AS
			SELECT
			TIPOLOGIA_OPERACAO
			,MOEDA
			,TIPO_CLIENTE
			,TIPO_CARTEIRA
			,NIVEL_FAIR_VALUE
			,PAIS
			,CLASSE_ENTIDADE
			,CENARIO
			,DSC_PRODUTO
			,TIPO_TAXA
			,ID_INDEXANTE
			,RATING
			,DATA_PROJ
			,TIPO_AMORTIZACAO
			,FREQUENCIA_PAGAMENTO
			,FREQUENCIA_PAGAMENTO_JURO 
			,PERIOD_REVISAO_TAXA
			,CCF
			,ELEG_NII
			,RUBRICA
			,MES_VENCIMENTO
			,MATURIDADE
			,DATA_REF FORMAT=DATE9.
			,SUM(CF_CAPITAL_NII) AS CF_CAPITAL

			FROM (SELECT * FROM AUX.PROJECOES_SEM_NP_1_&i_value. WHERE (DATA_PROJ<=INTNX('MONTH',DATA_REF,12,'E')))
			GROUP BY TIPOLOGIA_OPERACAO, MOEDA, TIPO_CLIENTE, TIPO_CARTEIRA,DATA_REF,
						CENARIO, DSC_PRODUTO, TIPO_TAXA, ID_INDEXANTE, RATING, TIPO_AMORTIZACAO, PAIS,
						FREQUENCIA_PAGAMENTO, FREQUENCIA_PAGAMENTO_JURO, ELEG_NII , NIVEL_FAIR_VALUE,
						PERIOD_REVISAO_TAXA, CCF, RUBRICA, MES_VENCIMENTO, DATA_PROJ, CLASSE_ENTIDADE, MATURIDADE
			ORDER BY TIPOLOGIA_OPERACAO, MOEDA, TIPO_CLIENTE, TIPO_CARTEIRA,DATA_REF,
						CENARIO, DSC_PRODUTO, TIPO_TAXA, ID_INDEXANTE, RATING, TIPO_AMORTIZACAO,
						FREQUENCIA_PAGAMENTO, FREQUENCIA_PAGAMENTO_JURO,DATA_PROJ, ELEG_NII, MATURIDADE,
						PERIOD_REVISAO_TAXA, MES_VENCIMENTO, CCF, RUBRICA, NIVEL_FAIR_VALUE, PAIS, CLASSE_ENTIDADE desc;
			QUIT;	


	PROC SQL;
	    CREATE TABLE AUX.AUX_NOVA_PRODUCAO_2_&i_value. AS
	    SELECT
	        PROJ.*
			,TX.TX_JURO
			,TX.SPREAD
	    FROM AUX.PROJ_SEM_NP_UNICAS_&i_value. PROJ
		LEFT JOIN AUX.CALCULO_TX_JURO_SPRED_MATURIDADE TX
			ON PROJ.TIPOLOGIA_OPERACAO=TX.TIPOLOGIA_OPERACAO AND PROJ.MOEDA=TX.MOEDA AND PROJ.TIPO_CLIENTE=TX.TIPO_CLIENTE AND 
			    PROJ.TIPO_TAXA=TX.TIPO_TAXA AND PROJ.ID_INDEXANTE=TX.ID_INDEXANTE AND
				PROJ.RATING=TX.RATING AND PROJ.TIPO_AMORTIZACAO=TX.TIPO_AMORTIZACAO AND PROJ.FREQUENCIA_PAGAMENTO=TX.FREQUENCIA_PAGAMENTO AND
				PROJ.FREQUENCIA_PAGAMENTO_JURO=TX.FREQUENCIA_PAGAMENTO_JURO AND PROJ.PERIOD_REVISAO_TAXA=TX.PERIOD_REVISAO_TAXA 
				AND PROJ.CCF=TX.CCF AND PROJ.DSC_PRODUTO=TX.DSC_PRODUTO AND PROJ.TIPO_CARTEIRA=TX.TIPO_CARTEIRA
				AND PROJ.RUBRICA=TX.RUBRICA AND PROJ.NIVEL_FAIR_VALUE=TX.NIVEL_FAIR_VALUE AND PROJ.PAIS=TX.PAIS
				AND PROJ.CLASSE_ENTIDADE=TX.CLASSE_ENTIDADE AND PROJ.MATURIDADE=TX.MATURIDADE;
		QUIT;
%put"tx_juro";
		/*Criar uma tabela com a exposicao a 30Jun2024*/




	DATA  AUX.EXPOSICAO_TOTAL_&i_value.;
	SET  AUX.AUX_NOVA_PRODUCAO_2_&i_value.;

	CHAVE=CATX(_,TIPOLOGIA_OPERACAO, MOEDA, TIPO_CLIENTE, CENARIO, TIPO_TAXA, ID_INDEXANTE, RATING, 
       TIPO_AMORTIZACAO, FREQUENCIA_PAGAMENTO, FREQUENCIA_PAGAMENTO_JURO, PERIOD_REVISAO_TAXA, 
       CCF, DSC_PRODUTO, TX_JURO, RUBRICA, SPREAD, MATURIDADE, TIPO_CARTEIRA, DATA_PROJ,
       MES_VENCIMENTO, ELEG_NII, DATA_PROJ, DATA_REF, NIVEL_FAIR_VALUE,
		PAIS, CLASSE_ENTIDADE);
	RUN;

/* Certifique-se de que os dados estejam ordenados */
PROC SORT DATA=AUX.EXPOSICAO_TOTAL_&i_value. out=AUX.EXPOSICAO_TOTAL_S_&i_value.;
    BY CHAVE;
RUN;

DATA AUX.AUX_NOVA_PRODUCAO_3_&i_value.;
    SET AUX.EXPOSICAO_TOTAL_S_&i_value.;

    IF TIPOLOGIA_OPERACAO = 'CREDITO' THEN DO;
        CAPITAL_VINCENDO = CF_CAPITAL;
        MONTANTE_CAPITAL = 0;
    END;
    ELSE DO;
        CAPITAL_VINCENDO = 0;
        MONTANTE_CAPITAL = CF_CAPITAL;
    END;

    EXPOSICAO = CAPITAL_VINCENDO + MONTANTE_CAPITAL;
    
RUN;

		


	PROC SQL;
	CREATE TABLE AUX.AUX_NOVA_PRODUCAO_4_&i_value. AS
		SELECT
			*
			,MONOTONIC() AS _N_
			,COMPRESS(PUT(CALCULATED _N_, 8.)) AS N_LINHA
	        ,catx('_', 'SINTETICA', &i_value., (calculated N_LINHA)) AS ID_OPERACAO
		FROM AUX.AUX_NOVA_PRODUCAO_3_&i_value.;
		QUIT;

		data _null_;
    	call sleep(3, 1); /* Aguarda 1 segundo */
		RUN;

		PROC SQL;
			CREATE TABLE AUX.AUX_NOVA_PRODUCAO_5_&i_value. AS
			SELECT
			*
			,INTNX('MONTH', DATA_PROJ, 0,'B') AS DATA_INICIO FORMAT=DATE9.
			,INTNX('MONTH', CALCULATED DATA_INICIO, MATURIDADE,'E') AS DATA_FIM FORMAT=DATE9.
			FROM AUX.AUX_NOVA_PRODUCAO_4_&i_value.;
			QUIT;




		data _null_;
    	call sleep(3, 1); /* Aguarda 1 segundo */
RUN;

		%MapeamentPeriodMeses(AUX.AUX_NOVA_PRODUCAO_5_&i_value.,PERIOD_REVISAO_TAXA,MESES_REVISAO,AUX.AUX_NOVA_PRODUCAO_5_A_&i_value.);

	data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
	RUN;

	DATA AUX.AUX_NOVA_PRODUCAO_6_&i_value.;
		SET AUX.AUX_NOVA_PRODUCAO_5_A_&i_value.;
		WHERE CAPITAL_VINCENDO NE 0 OR MONTANTE_CAPITAL NE 0;
		IF TIPO_TAXA="V" THEN DO;
			DATA_ULT_REPRICING=DATA_INICIO;
			DATA_ULT_PAG=DATA_INICIO;
			DATA_PROXIMA_REPRICING=INTNX('MONTH', DATA_INICIO,MESES_REVISAO,'B');
			DATA_PROX_PAG=INTNX('MONTH', DATA_INICIO,MESES_REVISAO,'B');
			FORMAT DATA_ULT_REPRICING DATA_ULT_PAG DATA_PROXIMA_REPRICING DATA_PROX_PAG DATA_REF DATE9.;
			END;
		RUN;

						 	
	DATA AUX.NOVA_PRODUCAO_&i_value. (KEEP=FREQUENCIA_PAGAMENTO_JURO MOEDA FREQUENCIA_PAGAMENTO DATA_FIM DATA_REF
								DATA_INICIO TIPOLOGIA_OPERACAO TIPO_TAXA SPREAD TX_JURO ID_OPERACAO NIVEL_FAIR_VALUE
								DATA_PROX_PAG DATA_PROXIMA_REPRICING ID_INDEXANTE TIPO_AMORTIZACAO PAIS
								CAPITAL_VINCENDO MONTANTE_CAPITAL ELEG_NII TIPO_CLIENTE CCF
								TIPO_CARTEIRA CENARIO DSC_PRODUTO RATING PERIOD_REVISAO_TAXA RUBRICA
								EXPOSICAO CLASSE_ENTIDADE);

		SET AUX.AUX_NOVA_PRODUCAO_6_&i_value.(rename=(DATA_REF=DATA_REF_A));
		WHERE DATA_INICIO<=INTNX('MONTH', %sysfunc(inputn(&DATA_REF, date9.)),12,'E');
		DATA_REF=INTNX("MONTH",DATA_INICIO,0,'E');
		FORMAT DATA_REF DATE9.;
		RUN;


	%PUT"CALCULO PROJ 1";
	%CalculoProjNP(&i_value.,AUX.NOVA_PRODUCAO_&i_value., &CURVAS_INDEXANTES.,AUX.AUX_PROJECAO_NP_&i_value.);
	
	%END;


%MEND;



%MACRO PROJ_NP_DESCONTO(input, input_proj_nii, CURVAS, CASHFLOWS_DESCONTADOS, CASHFLOWS_DESCONTADOS_NII);
%LET lista_tabelas= AUX.AUX_PROJECAO_NP_1 AUX.AUX_PROJECAO_NP_2
					AUX.AUX_PROJECAO_NP_3 AUX.AUX_PROJECAO_NP_4
					AUX.AUX_PROJECAO_NP_5 AUX.AUX_PROJECAO_NP_6
					AUX.AUX_PROJECAO_NP_7 AUX.AUX_PROJECAO_NP_8
					AUX.AUX_PROJECAO_NP_9 AUX.AUX_PROJECAO_NP_10
					AUX.AUX_PROJECAO_NP_11 AUX.AUX_PROJECAO_NP_12;




/* Passo 2: Concatenar as tabelas listadas */
data OUT.PROJECAO_NII_1(DROP=INDEXANTE f1);
    set &lista_tabelas;
	IF CF_JURO_NII=. THEN CF_JURO_NII=0;
		ELSE CF_JURO_NII=CF_JURO_NII;
	IF CF_JURO_NII_VAR=. THEN CF_JURO_NII_VAR=0;
		ELSE CF_JURO_NII_VAR=CF_JURO_NII_VAR;
	IF CF_CAPITAL_NII=. THEN CF_CAPITAL_NII=0;
		ELSE CF_CAPITAL_NII=CF_CAPITAL_NII;
	IF CAPITAL_REM_NII=. THEN CAPITAL_REM_NII=0;
		ELSE CAPITAL_REM_NII=CAPITAL_REM_NII;

run;

PROC SQL;
	CREATE TABLE OUT.NP_ID_PROJ_ AS
	SELECT
	TIPOLOGIA_OPERACAO
	,TIPO_AMORTIZACAO
	,RUBRICA
	,MOEDA	
	,ID_OPERACAO
	,TIPO_CLIENTE
	,CLASSE_ENTIDADE
	,TIPO_CARTEIRA	
	,TIPO_TAXA	
	,DATA_REF	
	,DATA_PROJ
	,CENARIO
	,TX_JURO
	,JURO_NII
	,DATA_INICIO
	,DATA_FIM
	,BASELINE_NII
	,PARALLEL_UP_NII
	,PARALLEL_DOWN_NII
	,SUM(CF_JURO_NII) AS CF_JURO_NII
	,SUM(CF_JURO_NII_VAR) AS CF_JURO_NII_VAR
	,SUM(CF_CAPITAL_NII) AS CF_CAPITAL_NII
	,SUM(CAPITAL_REM_NII) AS CAPITAL_REM_NII	
	,ID_INDEXANTE

	FROM &input.  
		GROUP BY TIPOLOGIA_OPERACAO, ID_OPERACAO, RUBRICA, MOEDA,TIPO_CLIENTE,TIPO_CARTEIRA,TIPO_TAXA,DATA_REF,DATA_PROJ
				,CENARIO, CLASSE_ENTIDADE,DATA_INICIO, TX_JURO, JURO_NII, DATA_FIM, ID_INDEXANTE,TIPO_AMORTIZACAO, BASELINE_NII, PARALLEL_UP_NII, PARALLEL_DOWN_NII;
	RUN;

PROC SQL;
	CREATE TABLE &CASHFLOWS_DESCONTADOS. AS
	SELECT
	TIPOLOGIA_OPERACAO
	,TIPO_AMORTIZACAO
	,RUBRICA
	,MOEDA	
	,TIPO_CLIENTE
	,CLASSE_ENTIDADE
	,TIPO_CARTEIRA	
	,TIPO_TAXA	
	,JURO_NII
	,TX_JURO
	,DATA_REF	
	,DATA_PROJ
	,CENARIO
	,DATA_INICIO
	,DATA_FIM
	,BASELINE_NII
	,PARALLEL_UP_NII
	,PARALLEL_DOWN_NII
	,SUM(CF_JURO_NII) AS CF_JURO_NII
	,SUM(CF_JURO_NII_VAR) AS CF_JURO_NII_VAR
	,SUM(CF_CAPITAL_NII) AS CF_CAPITAL_NII
	,SUM(CAPITAL_REM_NII) AS CAPITAL_REM_NII	
	,ID_INDEXANTE

	FROM &input.  
		GROUP BY TIPOLOGIA_OPERACAO, RUBRICA, MOEDA,TIPO_CLIENTE,TIPO_CARTEIRA,TIPO_TAXA,DATA_REF,DATA_PROJ
				,CENARIO,CLASSE_ENTIDADE,DATA_INICIO,JURO_NII, TX_JURO, DATA_FIM, ID_INDEXANTE,TIPO_AMORTIZACAO, BASELINE_NII, PARALLEL_UP_NII, PARALLEL_DOWN_NII;
	RUN;

PROC SQL;
	CREATE TABLE OUT.NII_ID_PROJ_ AS
	SELECT
	TIPOLOGIA_OPERACAO
	,TIPO_AMORTIZACAO
	,RUBRICA
	,MOEDA	
	,ID_OPERACAO
	,TIPO_CLIENTE
	,CLASSE_ENTIDADE
	,TIPO_CARTEIRA	
	,TIPO_TAXA
	,TX_JURO	
	,DATA_REF	
	,DATA_PROJ
	,CENARIO
	,DATA_INICIO
	,DATA_FIM
	,JURO_NII
	,SUM(CF_JURO_NII) AS CF_JURO_NII
	,SUM(CF_JURO_NII_VAR) AS CF_JURO_NII_VAR
	,SUM(CF_CAPITAL_NII) AS CF_CAPITAL_NII
	,SUM(CAPITAL_REM_NII) AS CAPITAL_REM_NII	
	,ID_INDEXANTE

	FROM &input_proj_nii.  
		GROUP BY TIPOLOGIA_OPERACAO,CLASSE_ENTIDADE, ID_OPERACAO, RUBRICA, MOEDA,TIPO_CLIENTE,TIPO_CARTEIRA,TIPO_TAXA,DATA_REF,DATA_PROJ
				,CENARIO,DATA_INICIO, DATA_FIM, TX_JURO, JURO_NII, ID_INDEXANTE,TIPO_AMORTIZACAO;
	RUN;

PROC SQL;
	CREATE TABLE &CASHFLOWS_DESCONTADOS_NII. AS
	SELECT
	TIPOLOGIA_OPERACAO
	,TIPO_AMORTIZACAO
	,RUBRICA
	,MOEDA	
	,TIPO_CLIENTE
	,CLASSE_ENTIDADE
	,TIPO_CARTEIRA	
	,TIPO_TAXA	
	,DATA_REF	
	,DATA_PROJ
	,TX_JURO
	,JURO_NII
	,CENARIO
	,DATA_INICIO
	,DATA_FIM
	,SUM(CF_JURO_NII) AS CF_JURO_NII
	,SUM(CF_JURO_NII_VAR) AS CF_JURO_NII_VAR
	,SUM(CF_CAPITAL_NII) AS CF_CAPITAL_NII
	,SUM(CAPITAL_REM_NII) AS CAPITAL_REM_NII	
	,ID_INDEXANTE

	FROM &input_proj_nii.  
		GROUP BY TIPOLOGIA_OPERACAO, CLASSE_ENTIDADE, RUBRICA, MOEDA,TIPO_CLIENTE,TIPO_CARTEIRA,TIPO_TAXA,DATA_REF,DATA_PROJ
				,CENARIO,DATA_INICIO, JURO_NII, DATA_FIM, TX_JURO, ID_INDEXANTE,TIPO_AMORTIZACAO;
	RUN;
%MEND;