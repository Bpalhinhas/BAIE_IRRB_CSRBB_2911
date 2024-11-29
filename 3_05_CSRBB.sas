%MACRO UniversoCSRBB_EVE(PROJ_ORIGINAIS_EVE, CHOQUES, INPUT_CSRBB_EVE);

DATA AUX.INPUT_CSRBB_EVE_0(KEEP=TIPO_CARTEIRA TIPO_TAXA TIPO_CLIENTE TIPOLOGIA_OPERACAO
								CENARIO MOEDA BUCKET CF_CAPITAL CF_JURO_SPREAD CF_JURO_VAR
								CF_TOTAL ID_OPERACAO DATA_PROJ DATA_REF ID_INDEXANTE
								NIVEL_FAIR_VALUE RATING PAIS SPREAD CURVA PRAZO);
	SET &PROJ_ORIGINAIS_EVE.(RENAME=(BUCKET_CAPITAL=BUCKET));
	WHERE TIPOLOGIA_OPERACAO="OBRIGACOES" AND (NIVEL_FAIR_VALUE="1" OR NIVEL_FAIR_VALUE="2") AND CENARIO="BASELINE";
	IF CLASSE_ENTIDADE="General governments" THEN CURVA="Sovereign";
		ELSE CURVA="Corporate";
	RUN;

	
	PROC SQL;
		CREATE TABLE &INPUT_CSRBB_EVE. AS 
			SELECT
				CSRBB.*
				,RP.RATING AS RATING_PAIS
			FROM AUX.INPUT_CSRBB_EVE_0 CSRBB
				LEFT JOIN IN.RATING_PAIS RP
					ON CSRBB.PAIS=RP.PAIS;
		QUIT;

DATA &INPUT_CSRBB_EVE.;
	SET &INPUT_CSRBB_EVE.;
		POS = INDEXC(RATING, '+-');
		POS_1=INDEXC(RATING_PAIS, '+-');
		IF ((RATING='') OR RATING='NULL') THEN RATING=SUBSTR(RATING_PAIS, 1, POS_1 - 1);
			ELSE RATING=SUBSTR(RATING, 1, POS-1);
	RUN;

/*Join das curvas choque*/

PROC SQL;
	CREATE TABLE AUX.CHOQUES_EVE_S_CSRBB AS
	SELECT T.*
		,C.CHOQUE_UP AS CHOQUE_UP_S
		,C.CHOQUE_DOWN AS CHOQUE_DOWN_S
	FROM (SELECT * FROM &INPUT_CSRBB_EVE. WHERE CURVA="Sovereign") T
	LEFT JOIN &CHOQUES. C
		ON C.PAIS=T.PAIS AND C.CURVA=T.CURVA AND (T.PRAZO BETWEEN C.TENOR_MIN AND C.TENOR_MAX);
	QUIT;


PROC SQL;
	CREATE TABLE AUX.CHOQUES_EVE_C_CSRBB AS
	SELECT T.*
		,C.CHOQUE_UP AS CHOQUE_UP_C
		,C.CHOQUE_DOWN AS CHOQUE_DOWN_C
	FROM (SELECT * FROM &INPUT_CSRBB_EVE. WHERE CURVA="Corporate") T
	LEFT JOIN &CHOQUES. C
		ON C.RATING=T.RATING AND C.CURVA=T.CURVA AND (T.PRAZO BETWEEN C.TENOR_MIN AND C.TENOR_MAX);
	QUIT;

DATA &INPUT_CSRBB_EVE.;
	SET AUX.CHOQUES_EVE_C_CSRBB AUX.CHOQUES_EVE_S_CSRBB;
	RUN;

DATA &INPUT_CSRBB_EVE.(DROP=CHOQUE_UP_C CHOQUE_DOWN_C CHOQUE_UP_S CHOQUE_DOWN_S
						POS POS_1 RATING_PAIS);
	SET &INPUT_CSRBB_EVE.;
	IF CURVA="Corporate" THEN DO;
		CHOQUE_UP=CHOQUE_UP_C;
		CHOQUE_DOWN=CHOQUE_DOWN_C;
		END;
	ELSE DO;
		CHOQUE_UP=CHOQUE_UP_S;
		CHOQUE_DOWN=CHOQUE_DOWN_S;
		END;
		
	RUN;

%MEND;

%MACRO UniversoCSRBB_NII(NOVA_PROD, CHOQUES, INPUT_CSRBB_NII);

%LET lista_tabelas= AUX.NOVA_PRODUCAO_1 AUX.NOVA_PRODUCAO_2
					AUX.NOVA_PRODUCAO_3 AUX.NOVA_PRODUCAO_4
					AUX.NOVA_PRODUCAO_5 AUX.NOVA_PRODUCAO_6
					AUX.NOVA_PRODUCAO_7 AUX.NOVA_PRODUCAO_8
					AUX.NOVA_PRODUCAO_9 AUX.NOVA_PRODUCAO_10
					AUX.NOVA_PRODUCAO_11 AUX.NOVA_PRODUCAO_12;


/* Passo 2: Concatenar as tabelas listadas */
data &nova_prod.(DROP=INDEXANTE f1);
    set &lista_tabelas;
run;

DATA AUX.INPUT_CSRBB_NII_0;
	SET &NOVA_PROD.;
	WHERE TIPOLOGIA_OPERACAO="OBRIGACOES" AND NIVEL_FAIR_VALUE IN ('1','2')
		AND (CENARIO="BASELINE" OR CENARIO="PARALLEL_UP" OR CENARIO="PARALLEL_DOWN");
	IF CENARIO="PARALLEL_UP" THEN CENARIO="CHOQUE_UP";
	IF CENARIO="PARALLEL_DOWN" THEN CENARIO="CHOQUE_DOWN";
	IF CLASSE_ENTIDADE="General governments" THEN CURVA="Sovereign";
		ELSE CURVA="Corporate";
	PRAZO=INTCK('month', DATA_INICIO, DATA_FIM);
	RUN;

PROC SQL;
	CREATE TABLE AUX.INPUT_CSRBB_NII_1 AS 
		SELECT
			CSRBB.*
			,RP.RATING AS RATING_PAIS
			FROM AUX.INPUT_CSRBB_NII_0 CSRBB
				LEFT JOIN IN.RATING_PAIS RP
					ON CSRBB.PAIS=RP.PAIS;
			QUIT;

DATA AUX.INPUT_CSRBB_NII_1;
	SET AUX.INPUT_CSRBB_NII_1;
	POS = INDEXC(RATING, '+-');
	POS_1=INDEXC(RATING_PAIS, '+-');
	IF ((RATING='') OR RATING='NULL') THEN RATING=SUBSTR(RATING_PAIS, 1, POS_1 - 1);
		ELSE RATING=SUBSTR(RATING, 1, POS-1);
		RUN;


PROC SQL;
	CREATE TABLE AUX.CHOQUES_NII_S_CSRBB AS
	SELECT T.*
		,C.CHOQUE_UP AS CHOQUE_UP_S
		,C.CHOQUE_DOWN AS CHOQUE_DOWN_S
	FROM (SELECT * FROM AUX.INPUT_CSRBB_NII_1 WHERE CURVA="Sovereign") T
	LEFT JOIN &CHOQUES. C
		ON C.PAIS=T.PAIS AND C.CURVA=T.CURVA AND (T.PRAZO BETWEEN C.TENOR_MIN AND C.TENOR_MAX);
	QUIT;


PROC SQL;
	CREATE TABLE AUX.CHOQUES_NII_C_CSRBB AS
	SELECT T.*
		,C.CHOQUE_UP AS CHOQUE_UP_C
		,C.CHOQUE_DOWN AS CHOQUE_DOWN_C
	FROM (SELECT * FROM AUX.INPUT_CSRBB_NII_1 WHERE CURVA="Corporate") T
	LEFT JOIN &CHOQUES. C
		ON C.RATING=T.RATING AND C.CURVA=T.CURVA AND (T.PRAZO BETWEEN C.TENOR_MIN AND C.TENOR_MAX);
	QUIT;

DATA &INPUT_CSRBB_NII.;
	SET AUX.CHOQUES_NII_C_CSRBB AUX.CHOQUES_NII_S_CSRBB;
	RUN;

DATA &INPUT_CSRBB_NII.(DROP=CHOQUE_UP_C CHOQUE_DOWN_C CHOQUE_UP_S CHOQUE_DOWN_S
						POS POS_1 RATING_PAIS);
	SET &INPUT_CSRBB_NII.;
	IF CURVA="Corporate" THEN DO;
		CHOQUE_UP=CHOQUE_UP_C;
		CHOQUE_DOWN=CHOQUE_DOWN_C;
		END;
	ELSE DO;
		CHOQUE_UP=CHOQUE_UP_S;
		CHOQUE_DOWN=CHOQUE_DOWN_S;
		END;
		
	RUN;

%MEND;


%MACRO CalculoEVE_CSRBB(INPUT_CSRBB_EVE, CURVAS, EVE_CSRBB);

/*Juncao da curva Risk Free*/

	
PROC SQL;
	CREATE TABLE WORK.CURVAS_CENARIOS_EUR AS
		SELECT 
		PRJ.*
		,CURV.CURVA AS BASELINE
         
		FROM (SELECT * FROM &INPUT_CSRBB_EVE. WHERE MOEDA="EUR") PRJ
		LEFT JOIN (SELECT * FROM &CURVAS. WHERE ID_INDEXANTE="OIS_EUR") CURV
		ON PUT(PRJ.DATA_PROJ, DATE9.) = PUT(CURV.DATAS, DATE9.);
		QUIT;

PROC SQL;
	CREATE TABLE WORK.CURVAS_CENARIOS_USD AS
		SELECT 
		PRJ.*
		,CURV.CURVA AS BASELINE
        
		FROM (SELECT * FROM &INPUT_CSRBB_EVE. WHERE MOEDA="USD") PRJ
		LEFT JOIN (SELECT * FROM &CURVAS. WHERE ID_INDEXANTE="OIS_USD") CURV
		ON PUT(PRJ.DATA_PROJ, DATE9.) = PUT(CURV.DATAS, DATE9.);
		QUIT;

DATA OUT.CSRBB_EVE_CURVAS_RISK;
	SET WORK.CURVAS_CENARIOS_USD WORK.CURVAS_CENARIOS_EUR;
	RUN;

/*Aplica��o do choque ao EVE*/

PROC SQL;
	CREATE TABLE AUX.CASHFLOWS_DESCONTADOS_ID AS
		SELECT
		*
		,CASE WHEN CENARIO="BASELINE" THEN
			CF_CAPITAL/((1+BASELINE+SPREAD)**((DATA_PROJ-DATA_REF)/365)) 

			WHEN CENARIO="CHOQUE_UP" THEN
			CF_CAPITAL/((1+BASELINE+SPREAD+CHOQUE_UP)**((DATA_PROJ-DATA_REF)/365)) 

			WHEN CENARIO="CHOQUE_DOWN" THEN
			CF_CAPITAL/((1+BASELINE+SPREAD+CHOQUE_DOWN)**((DATA_PROJ-DATA_REF)/365)) 

			END AS CF_CAPITAL_DESCT
		,CASE WHEN CENARIO="BASELINE" THEN
			CF_JURO_SPREAD/((1+BASELINE+SPREAD)**((DATA_PROJ-DATA_REF)/365)) 

			WHEN CENARIO="CHOQUE_UP" THEN
			CF_JURO_SPREAD/((1+BASELINE+SPREAD+CHOQUE_UP)**((DATA_PROJ-DATA_REF)/365)) 

			WHEN CENARIO="CHOQUE_DOWN" THEN
			CF_JURO_SPREAD/((1+BASELINE+SPREAD+CHOQUE_DOWN)**((DATA_PROJ-DATA_REF)/365)) 

		END AS CF_JURO_SPREAD_DESCT

		,CASE WHEN CENARIO="BASELINE" THEN
			CF_JURO_VAR/((1+BASELINE+SPREAD)**((DATA_PROJ-DATA_REF)/365)) 

			WHEN CENARIO="CHOQUE_UP" THEN
			CF_JURO_VAR/((1+BASELINE+SPREAD+CHOQUE_UP)**((DATA_PROJ-DATA_REF)/365)) 

			WHEN CENARIO="CHOQUE_DOWN" THEN
			CF_JURO_VAR/((1+BASELINE+SPREAD+CHOQUE_DOWN)**((DATA_PROJ-DATA_REF)/365)) 

			END AS CF_JURO_VAR_DESCT

			,CASE WHEN CENARIO="BASELINE" THEN
			CF_TOTAL/((1+BASELINE+SPREAD)**((DATA_PROJ-DATA_REF)/365)) 

			WHEN CENARIO="CHOQUE_UP" THEN
			CF_TOTAL/((1+BASELINE+SPREAD+CHOQUE_UP)**((DATA_PROJ-DATA_REF)/365)) 

			WHEN CENARIO="CHOQUE_DOWN" THEN
			CF_TOTAL/((1+BASELINE+SPREAD+CHOQUE_DOWN)**((DATA_PROJ-DATA_REF)/365)) 

			END AS CF_TOTAL_DESCT

			FROM OUT.CSRBB_EVE_CURVAS_RISK;

			QUIT;


PROC SQL;
	CREATE TABLE OUT.CASHFLOWS_DESCONTADOS AS
	SELECT
	TIPOLOGIA_OPERACAO
	,MOEDA	
	,TIPO_CLIENTE
	,TIPO_CARTEIRA	
	,TIPO_TAXA	
	,DATA_REF	
	,DATA_PROJ
	,CENARIO
	,BUCKET
	,BASELINE	
	,CHOQUE_UP
	,CHOQUE_DOWN
	,SUM(CF_CAPITAL) AS CF_CAPITAL
	,SUM(CF_JURO_VAR) AS CF_JURO_VAR
	,SUM(CF_JURO_SPREAD) AS CF_JURO_SPREAD
	,SUM(CF_TOTAL) AS CF_TOTAL	
	,SUM(CF_CAPITAL_DESCT) AS CF_CAPITAL_DESCT
	,SUM(CF_JURO_SPREAD_DESCT) AS CF_JURO_SPREAD_DESCT
	,SUM(CF_JURO_VAR_DESCT) AS CF_JURO_VAR_DESCT
	,SUM(CF_TOTAL_DESCT) AS CF_TOTAL_DESCT
	,ID_INDEXANTE

	FROM (SELECT * FROM AUX.CASHFLOWS_DESCONTADOS_ID WHERE DATA_PROJ>DATA_REF)
		GROUP BY TIPOLOGIA_OPERACAO, MOEDA,TIPO_CLIENTE,TIPO_CARTEIRA,TIPO_TAXA,DATA_REF,DATA_PROJ
				,CENARIO,BUCKET,BASELINE,CHOQUE_UP,CHOQUE_DOWN, ID_INDEXANTE;
	RUN;


%MEND;

%MACRO CalculoProjecaoNII_CSRBB(TABELA_MASTER, CURVAS_INDEXANTES, TABELA_OUT);

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

PROC SQL;
	CREATE TABLE AUX.CSRBB_INPUT_PROJ_0 AS 
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
			WHEN TIPO_TAXA ='F' THEN TX_JURO*(1/12)
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

%MapeamentPeriodMeses(AUX.CSRBB_INPUT_PROJ_0,PERIOD_REVISAO_TAXA,PERIOD_REV_TX_M, AUX.CSRBB_NII_PERIOD_REV);

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;


DATA AUX.CSRBB_NII_PROJ_1(DROP=TIPO_AMORTIZACAO DATA_FIM_L);
	SET AUX.CSRBB_NII_PERIOD_REV(RENAME=(DATA_FIM=DATA_FIM_L DATA_FIM_A=DATA_FIM));
	IF TIPOLOGIA_OPERACAO IN ("CEDIDO", "TOMADO") THEN
		ID_OPERACAO=CATT(ID_OPERACAO,TIPOLOGIA_OPERACAO);
	IF DATA_PROXIMA_REPRICING=. THEN DATA_PROXIMA_REPRICING=DATA_PROX_PAG;
	IF DATA_PROXIMA_REPRICING<=DATA_REF THEN 
		DATA_PROXIMA_REPRICING=INTNX('MONTH', DATA_PROXIMA_REPRICING, PERIOD_REV_TX_M, 'S');
	DO T_MES=0 TO MIN(MATURIDADE_RESIDUAL,13);
	OUTPUT;
	END;
	FORMAT DATA_FIM DATE9.;
	RUN;

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

PROC SORT DATA=AUX.CSRBB_NII_PROJ_1 OUT=AUX.CSRBB_NII_PERIOD_REV_1;
    BY ID_OPERACAO T_MES;
RUN;

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

DATA AUX.CSRBB_NII_PROJ_2;
	SET AUX.CSRBB_NII_PERIOD_REV_1;
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
	CREATE TABLE AUX.CSRBB_JUNCAO_CURVAS_IND AS
		SELECT 
		PRJ.*
		,CURV.CURVA AS BASELINE_NII          
		FROM AUX.CSRBB_NII_PROJ_2  PRJ
		LEFT JOIN  &CURVAS_INDEXANTES. CURV
		ON PRJ.DATA_CURVA_NII=CURV.DATAS AND PRJ.ID_INDEXANTE=CURV.ID_INDEXANTE;
		QUIT; 
		

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;


PROC SQL;
	CREATE TABLE AUX.CSRBB_JUNCAO_CURVAS_1 AS
		SELECT
		*
		,CASE WHEN TIPO_TAXA ='V' AND CENARIO="BASELINE" THEN BASELINE_NII*(PERIOD_PAG_M_CAP/12)
		ELSE 0
		 END AS TX_VARIAVEL_A_NII
		,CASE WHEN TIPO_TAXA ='V' AND CENARIO="BASELINE" THEN JURO_NII
			WHEN TIPO_TAXA ='V' AND CENARIO="CHOQUE_UP" THEN JURO_NII+(CHOQUE_UP/12)
			WHEN TIPO_TAXA ='V' AND CENARIO="CHOQUE_DOWN" THEN JURO_NII+(CHOQUE_DOWN/12)
			WHEN TIPO_TAXA ='F' THEN JURO_NII
		ELSE 0 
		 END AS TX_SPREAD_A
		,(CALCULATED TX_SPREAD_A*PERIOD_PAG_M_CAP) + (CALCULATED TX_VARIAVEL_A_NII) AS TX_MOMENTO_INICIAL_NII
		,CATX('-', CENARIO, ID_OPERACAO) AS ID_OPERACAO_AUX
		FROM AUX.CSRBB_JUNCAO_CURVAS_IND 
		ORDER BY CENARIO, ID_OPERACAO, T_MES;
QUIT;

data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

DATA &TABELA_OUT.;
    SET AUX.CSRBB_JUNCAO_CURVAS_1(RENAME=(TIPO_AMORTIZACAO_F=TIPO_AMORTIZACAO));
    BY ID_OPERACAO_AUX;
    
    RETAIN N_PAG 0 CAPITAL_REM_NII 0;

    IF FIRST.ID_OPERACAO_AUX THEN DO;
        N_PAG = 0;

        IF (TIPOLOGIA_OPERACAO = "BALANCETE" OR TIPOLOGIA_OPERACAO = "CREDITO") THEN DO;
            CAPITAL_REM_NII = CAPITAL_VINCENDO;
        END;
        ELSE DO;
            CAPITAL_REM_NII = EXPOSICAO_ON_BALANCE;
        END;

        CF_CAPITAL_NII = 0;
        CF_JURO_NII_VAR = 0;
        CF_JURO_NII = 0;
    END;
    
    ELSE DO;
        IF MOD(MATURIDADE_RESIDUAL - T_MES, PERIOD_PAG_M_CAP) = 0 THEN DO;
            IF TIPO_AMORTIZACAO = "Amortiza��o Constante" THEN DO;
                CF_CAPITAL_NII = CAPITAL_REM_NII / (CEIL(MATURIDADE_RESIDUAL / PERIOD_PAG_M_CAP) - N_PAG);
            END;
            ELSE IF TIPO_AMORTIZACAO = "Pagamento Constante" THEN DO;
                CF_CAPITAL_NII = PPMT(TX_MOMENTO_INICIAL_NII, 1, CEIL(MATURIDADE_RESIDUAL / PERIOD_PAG_M_CAP) - N_PAG, CAPITAL_REM_NII);                
            END;
            ELSE IF (INDEX(TIPO_AMORTIZACAO, "BULLET") > 0) AND (T_MES = MATURIDADE_RESIDUAL) THEN DO;
                CF_CAPITAL_NII = CAPITAL_REM_NII;
                CF_JURO_NII_VAR = 0;
                CF_JURO_NII = 0;
            END;
            ELSE DO;
                CF_CAPITAL_NII = 0;
                CF_JURO_NII_VAR = 0;
                CF_JURO_NII = 0;
            END;
            
            N_PAG = N_PAG + 1;
        END;
        ELSE DO;
            CF_CAPITAL_NII = 0;;
            CF_JURO_NII_VAR = 0;
        END;
       

        IF MOD(MATURIDADE_RESIDUAL - T_MES, PERIOD_PAG_M_NII) = 0 THEN DO;
            IF CENARIO = "BASELINE" THEN 
                CF_JURO_NII = JURO_NII * CAPITAL_REM_NII;
            ELSE IF CENARIO = "CHOQUE_UP" THEN 
                CF_JURO_NII = (JURO_NII + (CHOQUE_UP/12)) * CAPITAL_REM_NII;
            ELSE IF CENARIO = "CHOQUE_DOWN" THEN 
                CF_JURO_NII = (JURO_NII - CHOQUE_DOWN/12) * CAPITAL_REM_NII;

            IF TIPO_TAXA = 'F' THEN 
                CF_JURO_NII_VAR = 0;
            ELSE DO;
                    CF_JURO_NII_VAR = CAPITAL_REM_NII * BASELINE_NII * (PERIOD_PAG_M_NII / 12);
                END;
        END;
        ELSE DO;
            CF_JURO_NII_VAR = 0;
            CF_JURO_NII = 0;
        END;

        CAPITAL_REM_NII = CAPITAL_REM_NII - CF_CAPITAL_NII;
    END;
    
RUN;



data _null_;
    call sleep(3, 1); /* Aguarda 1 segundo */
run;

%MEND;


%MACRO CalculoNII_CSRBB(INPUT_CSRBB_NII, CURVAS, NII_CSRBB);

%CalculoProjecaoNII_CSRBB(&INPUT_CSRBB_NII., &CURVAS., AUX.PROJ_CSRBB_NII);

PROC SQL;
	CREATE TABLE OUT.NP_ID_PROJ_CSRBB AS
	SELECT
	TIPOLOGIA_OPERACAO
	,TIPO_AMORTIZACAO
	,RUBRICA
	,MOEDA	
	,ID_OPERACAO
	,TIPO_CLIENTE
	,TIPO_CARTEIRA	
	,TIPO_TAXA	
	,DATA_REF	
	,DATA_PROJ
	,CENARIO
	,DATA_INICIO
	,DATA_FIM
	,SUM(CF_JURO_NII) AS CF_JURO_NII
	,SUM(CF_JURO_NII_VAR) AS CF_JURO_NII_VAR
	,SUM(CF_CAPITAL_NII) AS CF_CAPITAL_NII
	,SUM(CAPITAL_REM_NII) AS CAPITAL_REM_NII	
	,ID_INDEXANTE

	FROM AUX.PROJ_CSRBB_NII  
		GROUP BY TIPOLOGIA_OPERACAO, ID_OPERACAO, RUBRICA, MOEDA,TIPO_CLIENTE,TIPO_CARTEIRA,TIPO_TAXA,DATA_REF,DATA_PROJ
				,CENARIO,DATA_INICIO, DATA_FIM, ID_INDEXANTE,TIPO_AMORTIZACAO;
	RUN;

PROC SQL;
	CREATE TABLE &NII_CSRBB. AS
	SELECT
	TIPOLOGIA_OPERACAO
	,TIPO_AMORTIZACAO
	,RUBRICA
	,MOEDA	
	,TIPO_CLIENTE
	,TIPO_CARTEIRA	
	,TIPO_TAXA	
	,DATA_REF	
	,DATA_PROJ
	,CENARIO
	,DATA_INICIO
	,DATA_FIM
	,SUM(CF_JURO_NII) AS CF_JURO_NII
	,SUM(CF_JURO_NII_VAR) AS CF_JURO_NII_VAR
	,SUM(CF_CAPITAL_NII) AS CF_CAPITAL_NII
	,SUM(CAPITAL_REM_NII) AS CAPITAL_REM_NII	
	,ID_INDEXANTE

	FROM AUX.PROJ_CSRBB_NII  
		GROUP BY TIPOLOGIA_OPERACAO, RUBRICA, MOEDA,TIPO_CLIENTE,TIPO_CARTEIRA,TIPO_TAXA,DATA_REF,DATA_PROJ
				,CENARIO,DATA_INICIO, DATA_FIM, ID_INDEXANTE,TIPO_AMORTIZACAO;
	RUN;



%MEND;