%MACRO BALANCO_ESTATICO(MASTER, NOVA_PROD, BALANCO);

DATA AUX.MASTER_NII;
	SET &MASTER.(KEEP=TIPOLOGIA_OPERACAO MOEDA TIPO_CLIENTE	TIPO_CARTEIRA CENARIO DSC_PRODUTO TIPO_TAXA	
						ID_INDEXANTE RATING TIPO_AMORTIZACAO FREQUENCIA_PAGAMENTO FREQUENCIA_PAGAMENTO_JURO
						PERIOD_REVISAO_TAXA CCF ELEG_NII RUBRICA TX_JURO SPREAD CAPITAL_VINCENDO 
						EXPOSICAO_ON_BALANCE ID_OPERACAO DATA_INICIO DATA_FIM DATA_PROXIMA_REPRICING 
						DATA_PROX_PAG DATA_REF);
	WHERE ELEG_NII="S" AND (EXPOSICAO_ON_BALANCE NE 0 OR t1.CAPITAL_VINCENDO NE 0);
	RUN;


DATA &BALANCO.(DROP=EXPOSICAO);
	SET AUX.MASTER_NII &NOVA_PROD.;
	RUN;
%MEND;