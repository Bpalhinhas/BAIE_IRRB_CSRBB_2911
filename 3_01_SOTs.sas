/*Contru��o das curvas de shock para o calclo do EVE*/

%MACRO AuxCalculoCurvaShock(SHOCK_EUR, SHOCK_USD, AUX_SHOCKS);

DATA AUX.DIAS;
	DO DIAS=0 TO 10958
;
	OUTPUT;
	END;
RUN;


PROC SQL;
	CREATE TABLE AUX.PROJECAO_SHOCKS AS
		SELECT D.*
		,S.DATAS
		,S.BASELINE AS BASELINE_EUR
		,S.PARALLEL_UP AS PARALLEL_UP_EUR
		,S.PARALLEL_DOWN AS PARALLEL_DOWN_EUR
		,S.RATE_UP AS RATE_UP_EUR
		,S.RATE_DOWN AS RATE_DOWN_EUR
		,S.STEEPENER AS STEEPENER_EUR
		,S.FLATTENER AS FLATTENER_EUR

	FROM AUX.DIAS D
	LEFT JOIN &SHOCK_EUR. S
	ON D.DIAS=S.DIAS;
	QUIT;

PROC SQL;
	CREATE TABLE AUX.PROJECAO_SHOCKS AS
		SELECT D.*
		,S.BASELINE AS BASELINE_USD
		,S.PARALLEL_UP AS PARALLEL_UP_USD
		,S.PARALLEL_DOWN AS PARALLEL_DOWN_USD
		,S.RATE_UP AS RATE_UP_USD
		,S.RATE_DOWN AS RATE_DOWN_USD
		,S.STEEPENER AS STEEPENER_USD
		,S.FLATTENER AS FLATTENER_USD

	FROM AUX.PROJECAO_SHOCKS D
	LEFT JOIN &SHOCK_USD. S
	ON D.DIAS=S.DIAS;
	QUIT;

%MEND;

