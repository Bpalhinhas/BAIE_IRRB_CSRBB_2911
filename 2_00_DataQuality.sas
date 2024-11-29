%macro input_structure_overview(source_lib, ds, table_type, outlib);

    /* Definição das colunas esperadas para cada tipo de tabela */
    %let expected_credito = DT_REFERENCIA ID_OPERACAO ID_CONTRAPARTE NOME_CONTRAPARTE CLASSE_ENTIDADE TAXA_JURO_EFECTIVA DATA_INICIO DATA_FIM MATURIDADE_RESIDUAL
							TIPO_TAXA_JURO SPREAD INDEXANTE PAIS DIVISA DATA_ULTIMO_PAGAMENTO_CUPAO DATA_PROXIMA_PAGAMENTO_CUPAO FREQUENCIA_PAGAMENTO
							PERIODICIDADE_REVIS_O_TAXA EXPOSICAO_ON_BALANCE EXPOSICAO_OFF_BALANCE SLD_JURO_VINCENDO SLD_JURO_VENCIDO SLD_CAP_VENCIDO SALDO
							TAXA_JURO_CONTRATUAL NTOTAL_OPR_DEBITO TOTAL_OPR_CREDITO FLG_CONTA_VENCIDA FLG_WRITTE_OFF MONTANTE_WRITTE_OFF Stage CCF
							FREQUENCIA_PAGAMENTO_CAP NUMERO_AMORTIZACOES NUMERO_UTILIZACOES TIPO_AMORTIZACAO;
    %let expected_cartas_cred = DT_REFERENCIA CARTA_CREDITO DESCONTO DT_INI_CARTA_CREDITO DT_VENC_CARTA_CREDITO DT_INI_DESCONTO DT_VENC_DESCONTO MOEDA
								MONTANTE_CARTA_CREDITO MONTANTE_DESCONTO TIPO_TAXA TX_JURO JURO_TOTAL CLASSE_ENTIDADE CCF STAGE INDEXANTE FREQUENCIA_PAGAMENTO
								FREQUENCIA_PAGAMENTO_JURO FREQUENCIA_PAGAMENTO_CAP TIPO_AMORTIZACAO DATA_ULT_PAGAMENTO_CUPAO DATA_PROXIMA_REPRICING PER_PAGAMENTO;
	%let expected_obrigacoes = DT_REFERENCIA ID_OPERACAO ID_CONTRAPARTE NOME_CONTRAPARTE REF_DOSSIER DSC_DOSSIER TIPOLOGIA TAXA_JURO_CONTRATUAL TAXA_JURO_EFECTIVA
								INDEXANTE PERIODO_INDEXANTE SPREAD VALOR_SPREAD DURATION_ESTIMADA DATA_INICIO DATA_FIM MATURIDADE_RESIDUAL TIPO_TAXA_JURO 
								INDEXANTE_0001 PAIS DATA_REPRICING DATA_ULT_PAGAMENTO_CUPAO DATA_PROX_PAGAMENTO_CUPAO DATA_CALL_OPTION FREQUENCIA_PAGAMENTO_JURO
								PERIODICIDADE_REVISAO_TAXA MOEDA QTD_COMPRA QTD_VENDA COTACAO VLR_NOMINAL JUR_CORR JUR_TOTAL Stage Fixing FAIR_VALUE 
								RATING AGENCIA_RATING TIPO_AMORTIZACAO FREQUENCIA_PAGAMENTO_CAP NIVEL_FAIRVALUE CENTRAL_CUSTODIA CLASSE_ENTIDADE;
    %let expected_depositos = DT_REFERENCIA ID_OPERACAO ID_CONTRAPARTE ID_ENTIDADE NOME_CONTRAPARTE CLASSE_ENTIDADE TIPOLOGIA_OPERACAO DSC_PRODUTO
								FLG_RAISIN TAXA_JURO_CONTRATUAL TAXA_JURO_EFECTIVA DATA_INICIO DATA_FIM MATURIDADE_RESIDUAL TIPO_TAXA SPREAD INDEXANTE
								PAIS FLG_DP_RENOVAVEL FLG_DP_MOBILIZAVEL DATA_ULTIMO_REPRICING DATA_PROXIMA_REPRICING FREQUENCIA_PAGAMENTO PERIODICIDADE_REVISAO_TAXA
								MOEDA SLD GROSS_CARRYING_AMOUNT VLR_CRD SLD_JURO_VINCENDO FREQUENCIA_PAGAMENTO_JURO Stage CCF MONTANTE_DP_COLATERAL TOTAL_OPR_CREDITO
								TOTAL_OPR_DEBITO FLG_PENALIZACAO FLG_SME FLG_WRITE_OFF;
    %let expected_tomado = DT_REFERENCIA ID_OPERACAO REF_TIPO_OPERACAO ID_CONTRAPARTE DSC_CONTRAPARTE TIPOLOGIA_OPERACAO CLASSE_ENTIDADE TAXA_JURO_CONTRATUAL
							DATA_INICIO DATA_VENCIMENTO MATURIDADE_RESIDUAL TIPO_TX_JURO PAIS DIVISA_COMPRA CUR_VAL_COMP CUR_VAL_VEND CUR_VAL_JURO EXPOSICAO
							DATA_ULTIMO_PAGAMENTO_CUPAO DATA_PROX_PAGAMENTO_CUPAO NUMERO_DIAS_OPERACAO Stage TIPO_AMORTIZACAO FREQUENCIA_PAGAMENTO_CAP;
    %let expected_cedido = DT_REFERENCIA ID_OPERACAO REF_TIPO_OPERACAO ID_CONTRAPARTE DSC_CONTRAPARTE TIPOLOGIA_OPERACAO CLASSE_ENTIDADE TAXA_JURO_CONTRATUAL
							DATA_INICIO DATA_VENCIMENTO MATURIDADE_RESIDUAL TIPO_TX_JURO PAIS DIVISA_VENDA CUR_VAL_COMP CUR_VAL_VEND CUR_VAL_JURO EXPOSICAO
							DATA_ULTIMO_PAGAMENTO_CUPAO DATA_PROX_PAGAMENTO_CUPAO NUMERO_DIAS_OPERACAO Stage;
    %let expected_MASTER = ID_OPERACAO DESCONTO NOME_CONTRAPARTE DATA_REF DATA_INICIO DATA_FIM MOEDA TIPOLOGIA_OPERACAO 
							DSC_PRODUTO CLASSE_ENTIDADE TIPO_CARTEIRA RUBRICA EXPOSICAO_ON_BALANCE EXPOSICAO_OFF_BALANCE
							CAPITAL_VINCENDO CAPITAL_VENCIDO JURO_VINCENDO JURO_VENCIDO MONTANTE_CAPITAL MONTANTE_JURO 
							TX_JURO TAXA_JURO_EFECTIVA TIPO_TAXA SPREAD ID_INDEXANTE FLG_RAISIN FLG_DP_RENOVAVEL FLG_DP_MOBILIZAVEL 
							FLG_TRANSACIONAL DATA_ULTIMO_REPRICING DATA_PROXIMA_REPRICING DATA_ULT_PAG DATA_PROX_PAG DATA_CALL_OPTION FREQUENCIA_PAGAMENTO
							FREQUENCIA_PAGAMENTO_JURO PERIOD_REVISAO_TAXA TIPO_AMORTIZACAO STAGE FIXING DURATION RATING
							CENARIO TIPO_NEGOCIO FLG_SME FLAG_FINANCIAL FLG_PENALIZACAO TIPO_CLIENTE NIVEL_FAIR_VALUE 
							CCF ELEG_NII NOVA_PROD M_DEB M_CRED MONTANTE_NOMINAL;
    /* Escolher o conjunto de colunas esperado com base no tipo de tabela */
    %let expected_columns =;
    %if %upcase(&table_type) = CREDITO %then %let expected_columns = &expected_credito;
    %else %if %upcase(&table_type) = CARTAS_CRED %then %let expected_columns = &expected_cartas_cred;
    %else %if %upcase(&table_type) = DEPOSITOS %then %let expected_columns = &expected_depositos;
    %else %if %upcase(&table_type) = OBRIGACOES %then %let expected_columns = &expected_obrigacoes;
    %else %if %upcase(&table_type) = TOMADO %then %let expected_columns = &expected_tomado;
    %else %if %upcase(&table_type) = CEDIDO %then %let expected_columns = &expected_cedido;
    %else %if %upcase(&table_type) = TABELA_MASTER %then %let expected_columns = &expected_MASTER;
    %else %do;
        %put ERROR: Invalid table type. Please specify one of the following: CREDITO, CARTAS_CRED, DEPOSITOS, OBRIGACOES, TOMADO, CEDIDO.;
        %return;
    %end;

		%let original_ds = &ds;


    /* Verificar se o dataset existe */
    %if %sysfunc(exist(&source_lib..&ds)) %then %do;
        %put INFO: Dataset &source_lib..&ds exists.;

        /* Contar o número de linhas */
        proc sql noprint;
            select count(*) into :num_operacoes
            from &source_lib..&ds;
        quit;

        /* Contar o número de colunas */
        proc sql noprint;
            select count(*) into :total_columns
            from dictionary.columns 
            where libname=upcase("&source_lib") and memname=upcase("%scan(&ds, 1, .)");
        quit;
        %put INFO: Total number of columns: &total_columns;

        /* Definir o nome da tabela final na work library */
        %let temp_table_name = DQ1_&ds._%sysfunc(today(), yymmddn8.);

        /* Adicionar número de colunas na tabela final temporária */
        data work.&temp_table_name;
            length ambito_teste $50 carteira $50 campo $100 descricao $200 resultado $100 total vazio zero preenchido 8;
            ambito_teste = "Estrutura de Carteira - Geral";
            carteira = "&ds";
            campo = "Número de Colunas";
            descricao = "N/A";
            resultado = "&total_columns";
            total = .;
            vazio = .;
            zero = .;
            preenchido = .;
            output;
        run;

        /* Verificação de linhas duplicadas */
        proc sort data=&source_lib..&ds nodupkey out=_temp_duplicates_check;
            by _all_;
        run;

        data _null_;
            set _temp_duplicates_check nobs=unique_rows;
            call symputx('unique_rows', unique_rows);
        run;

        %let duplicate_rows = %eval(&num_operacoes - &unique_rows);

        data _temp;
            length ambito_teste $50 carteira $50 campo $100 descricao $200 resultado $100 total vazio zero preenchido 8;
            ambito_teste = "Estrutura de Carteira - Geral";
            carteira = "&ds";
            campo = "Número de Operações Duplicadas";
            descricao = "N/A";
            resultado = "&duplicate_rows";
            total = .;
            vazio = .;
            zero = .;
            preenchido = .;
            output;
        run;
        proc append base=work.&temp_table_name data=_temp force;

        /* Remover a tabela temporária de verificação de duplicados */
        proc datasets lib=work nolist;
            delete _temp_duplicates_check;
        quit;

        /* Obter lista de variáveis do dataset */
        %let dsid = %sysfunc(open(&source_lib..&ds));
        %let vars = ;
        %do i = 1 %to &total_columns;
            %let varname = %sysfunc(varname(&dsid, &i));
            %let vars = &vars &varname;
        %end;
        %let rc = %sysfunc(close(&dsid));

        /* Inicializar variáveis para armazenar resultados */
        %let unexpected_vars = &vars;
		%PUT LISTA_2=&unexpected_vars;

        /* Verificar se as colunas esperadas existem no dataset e calcular contagens */
        %macro test_exists(var, original_ds);
            %let vazio = 0;
            %let zero = 0;
            %let preenchido = 0;
			
			
            %if %sysfunc(indexw(&vars, &var)) %then %do;
                %put &var: Present;
                %let unexpected_vars = %sysfunc(tranwrd(%str( )&unexpected_vars%str( ), %str( )&var%str( ), %str( )));
				%IF %length(&unexpected_vars) > 0 %THEN
				%let unexpected_vars = %sysfunc(compbl(&unexpected_vars));
				%PUT LISTA_1=&unexpected_vars;
			
				%LET TB=%SYSFUNC(substr(&ds, 1, 2));
                /* Calcular contagens para a coluna */
                data work.V_&TB.&var (keep=ID_TESTE ID_OPERACAO_I CAMPO DESCRICAO);
					length CAMPO $32 ID_OPERACAO_I $20 DESCRICAO $200;
                    set &source_lib..&ds end=last;
					ID_TESTE="C_49_&ds";
					TIPO = vtype(ID_OPERACAO);
					CAMPO="&var";
    				DESCRICAO="Coluna a vazio ou NULL";
   					 /* Se for numérica, converte para string usando PUT */
   					if TIPO = 'N' then ID_OPERACAO_I = put(ID_OPERACAO, 20.);
    				else ID_OPERACAO_I = ID_OPERACAO;
    
    				/* Verifica se o dataset é 'CARTAS_CRED' para atribuir um valor específico */
    				if upcase("&ds.") = "CARTAS_CRED" then ID_OPERACAO_I = CARTA_CREDITO;
    
    				/* Atribui o valor da tipologia de operação */
    				TIPOLOGIA_OPERACAO = "&original_ds";
                    retain vazio 0 zero 0 preenchido 0;
                    if missing(&var) or &var = "" or &var = "." THEN DO;
						vazio + 1;
						OUTPUT;
						END;
                    if &var = 0 then DO;
						zero + 1;
						OUTPUT;
						END;
                    if not (missing(&var) or &var = "" or &var = "." or &var = 0) then preenchido + 1;
                    if last then do;
                        call symputx('vazio', vazio);
                        call symputx('zero', zero);
                        call symputx('preenchido', preenchido);
                    end;
                run;
                data _temp;
                    length ambito_teste $50 carteira $50 campo $100 descricao $200 resultado $100 total vazio zero preenchido 8;
                    ambito_teste = "Estrutura de Carteira - Campos Requeridos";
                    carteira = "&ds";
                    campo = "&var";
                    descricao = "Campo Obrigatório";
                    resultado = "Coluna presente";
                    total = &num_operacoes;
                    vazio = &vazio;
                    zero = &zero;
                    preenchido = &preenchido;
                    output;
                run;
            %end;

            %else %do;
                %put &var: Absent;
                data _temp;
                    length ambito_teste $50 carteira $50 campo $100 descricao $200 resultado $100 total vazio zero preenchido 8;
                    ambito_teste = "Estrutura de Carteira - Campos Requeridos";
                    carteira = "&ds";
                    campo = "&var";
                    descricao = "Campo Obrigatório";
                    resultado = "Coluna ausente / incorretamente nomeada";
                    total = .;
                    vazio = .;
                    zero = .;
                    preenchido = .;
                    output;
                run;
            %end;
			
            proc append base=work.&temp_table_name data=_temp force;
        %mend;

        /* Iterar sobre as colunas esperadas e chamar a macro de verificação */
        %let num_expected = %sysfunc(countw(&expected_columns));
        %do i = 1 %to &num_expected;
            %let var = %scan(&expected_columns, &i);
            %test_exists(&var, &original_ds);
        %end;

        /* Listar as colunas inesperadas e calcular contagens */
        %macro count_unexpected(var);
            %let vazio = 0;
            %let zero = 0;
            %let preenchido = 0;

            data _null_;
                set &source_lib..&ds end=last;
                retain vazio 0 zero 0 preenchido 0;
                if missing(&var) or &var = "" or &var = "." then vazio + 1;
                if &var = 0 then zero + 1;
                if not (missing(&var) or &var = "" or &var = "." or &var = 0) then preenchido + 1;
                if last then do;
                    call symputx('vazio', vazio);
                    call symputx('zero', zero);
                    call symputx('preenchido', preenchido);
                end;
            run;
			
            %put &var: Unexpected;
            data _temp;
                length ambito_teste $50 carteira $50 campo $100 descricao $200 resultado $100 total vazio zero preenchido 8;
                ambito_teste = "Estrutura de Carteira - Campos Requeridos";
                carteira = "&ds";
                campo = "&var";
                descricao = "Campo Adicional";
                resultado = "Coluna não requerida / incorretamente nomeada";
                total = .;
                vazio = &vazio;
                zero = &zero;
                preenchido = &preenchido;
                output;
            run;
            proc append base=work.&temp_table_name data=_temp force;
        %mend;
		%if %length(&unexpected_vars) > 0 %then %do;
        %let num_unexpected = %sysfunc(countw(&unexpected_vars));
		%PUT LISTA = &unexpected_vars;
        %do i = 1 %to &num_unexpected;
            %let var = %scan(&unexpected_vars, &i);
            %count_unexpected(&var);
        %end;
		%end;
        /* Copiar resultados da work para a biblioteca outlib */
        proc datasets lib=work nolist;
            copy out=&outlib;
            select &temp_table_name;
        quit;

        /* Mostrar resultados */
        proc print data=&outlib..&temp_table_name;
            title "Overview da estrutura da tabela de &table_type";
        run;

        /* Remover tabelas temporárias */
        proc datasets lib=work nolist;
            delete _temp &temp_table_name;
        quit;

    %end;
    %else %put ERROR: Dataset &source_lib..&ds does not exist;

proc sql noprint;
    select catx('.', 'WORK', memname)
    into :lista_tabelas separated by ' '
    from dictionary.tables
    where libname = 'WORK' /* Biblioteca onde as tabelas estão */
      and memname like 'V_%' /* Filtra tabelas que começam com "V_" */
      and nobs > 0; /* Filtra apenas tabelas com observações (não vazias) */
quit;

/*Concatenar as tabelas listadas */
data DQ.OPERACOES_VAZIO;
    set &lista_tabelas;
run;
%mend;

%macro data_controls(source_lib, ds, outlib);

%PUT &ds.;
    /* Armazenar o nome original do dataset */
	%IF "&ds" NE "T_CEDENCIAS" AND "&ds" NE "TABELA_MASTER_FINAL" 
	%THEN %DO;
		%let original_ds = %sysfunc(substr(&ds, 3));
	%END;
	%IF "&ds" = "T_CEDENCIAS" %THEN %DO;
		%let original_ds = CEDIDO;
		%END;
	%IF "&ds" = "TABELA_MASTER_FINAL" %THEN %DO;
		%let original_ds = TABELA_MASTER_FINAL;
		%END;
	%PUT &original_ds;

    /* Macro para testes adicionais */

%macro additional_test(var, condition, description, ID_TESTE);
    %if %sysfunc(indexw(&pass_vars, &var)) %then %do;
        %put INFO: Running additional test: &description;
        %let fail_count = 0;
		%LET TB=%SYSFUNC(substr(&original_ds, 1, 2));

        /* Criar uma nova tabela para armazenar as linhas que falharam */
        data work.failed_&TB._&ID_TESTE (keep=TABELA TIPOLOGIA_OPERACAO ID_TESTE ID_OPERACAO DESCRICAO);
		LENGTH TIPOLOGIA_OPERACAO $20 DESCRICAO $200;
            set &ds end=last;
			TABELA="&original_ds";
    		TIPOLOGIA_OPERACAO = put(TIPOLOGIA_OPERACAO, $20.);
			ID_TESTE="&ID_TESTE";
			DESCRICAO=&description.;
            retain fail_count 0 empty_count 0;
            /* Contar as falhas e observações faltantes */
            if not (&condition) then do;
                fail_count + 1;
                output;  /* Salvar a linha na tabela se a condição falhar */
            end;
%PUT"5";
            if missing(&var) or &var = "" or &var = "NULL" then empty_count + 1;

            /* No final, armazenar contagens em variáveis macro */
            if last then do;
                call symputx('fail_count', fail_count);
                call symputx('empty_count', empty_count);
            end;
        run;

        /* Inicializar a tabela work.test_results se ela ainda não existir */
        %if %sysfunc(exist(work.test_results)) = 0 %then %do;
            data work.test_results;
                length ambito_de_teste $32 carteira $32 var $32 description $200 result $4 registos 8 incidências 8;
                stop;
            run;
        %end;

        /* Inserir o resultado do teste na tabela test_results */
        %if &fail_count = 0 %then %do;
            %put &description: OK;
            data work.results;
                length ambito_de_teste $32 carteira $32 var $32 description $200 result $4 registos 8 incidências 8;
                ambito_de_teste = "Controlos";
                carteira = "&original_ds";
                var = "&var";
                description = &description;
                result = "OK";
                registos = &total_rows;
                incidências = &fail_count;
                output;
            run;
        %end;
        %else %do;
            %put &description: NOK;
            data work.results;
                length ambito_de_teste $32 carteira $32 var $32 description $200 result $4 registos 8 incidências 8;
                ambito_de_teste = "Controlos";
                carteira = "&original_ds";
                var = "&var";
                description = &description;
                result = "NOK";
                registos = &total_rows;
                incidências = &fail_count;
                output;
            run;
        %end;
%put"4";
        /* Adicionar os resultados à tabela de resultados final */
        proc append base=work.test_results data=work.results force;
        run;

    %end;
%mend;




    /* Verificar se a coluna FLG_WRITTE_OFF existe */
    %let dsid = %sysfunc(open(&source_lib..&ds));
    %let has_flg_written_off = %sysfunc(varnum(&dsid, FLG_WRITTE_OFF));
    %let rc = %sysfunc(close(&dsid));

    /* Filtrar registros onde FLG_WRITTE_OFF = 0 se a coluna existir */
    %if &has_flg_written_off > 0 %then %do;
        data work.ds_filtered;
            set &source_lib..&ds;
            where FLG_WRITTE_OFF = "0";
        run;
        %let ds = work.ds_filtered;
    %end;
    %else %do;
        %let ds = &source_lib..&ds;
    %end;

    /* Definir nomes das tabelas com base no nome do dataset e data atual */
    %let dataset_name = &original_ds;
    %let test_results_name = Q2C_&dataset_name._%sysfunc(today(), yymmddn8.);
    %let columns_overview_name = Q2O_&dataset_name._%sysfunc(today(), yymmddn8.);

    /* Limpar tabelas de resultados anteriores na work e outlib */
    proc datasets lib=work nolist;
        delete &test_results_name &columns_overview_name test_results total_info columns_info not_empty title_exists title_additional results duplicates_check columns_overview _temp;
    run;
    quit;

    proc datasets lib=&outlib nolist;
        delete &test_results_name &columns_overview_name;
    run;
    quit;

    /* Verificar se o dataset existe antes de continuar */
    %if %sysfunc(exist(&ds)) %then %do;

        /* Contar o total de linhas */
        proc sql noprint;
            select count(*) into :total_rows from &ds;
        quit;
        %put INFO: Total number of rows: &total_rows;

        /* Verificação de linhas duplicadas */
        proc sort data=&ds nodupkey out=work.duplicates_check;
            by _all_;
        run;

        data _null_;
            set work.duplicates_check nobs=unique_rows;
            call symput('unique_rows', unique_rows);
        run;

        %let duplicate_rows = %eval(&total_rows - &unique_rows);
 		data work.failed_&original_ds._C_45 (keep=TABELA TIPOLOGIA_OPERACAO ID_TESTE ID_OPERACAO);
		LENGTH TIPOLOGIA_OPERACAO $20 DESCRICAO $200;
            set &ds end=last;
			TABELA="&original_ds";
    		TIPOLOGIA_OPERACAO = put(TIPOLOGIA_OPERACAO, $20.);
			ID_TESTE="C_45";
            /* Contar as falhas e observações faltantes */
            if &duplicate_rows >0 then do;
                output;  /* Salvar a linha na tabela se a condição falhar */
            end;
        run;
        %if &duplicate_rows = 0 %then %do;
            %put No duplicate rows found: OK;
            data work.duplicates;
                length ambito_de_teste $32 carteira $32 var $32 description $200 result $4 registos 8 incidências 8;
                ambito_de_teste = "Controlos";
                carteira = "&original_ds";
                var = "Operações Duplicadas";
                description = "Verificar se a tabela possui operações duplicadas";
                result = "OK";
				registos = &total_rows;
                incidências = &duplicate_rows;
                output;
            run;
        %end;
        %else %do;
            %put Duplicate rows found: NOK;
            data work.duplicates;
                length ambito_de_teste $32 carteira $32 var $32 description $200 result $4 registos 8 incidências 8;
                ambito_de_teste = "Controlos";
                carteira = "&original_ds";
                var = "Operações Duplicadas";
                description = "Verificar se a tabela possui operações duplicadas";
                result = "NOK";
				registos = &total_rows;
                incidências = &duplicate_rows;
                output;
            run;
        %end;
        proc append base=work.test_results data=work.duplicates force;
        run;

        /* Verificar se o dataset existe */
        %if %sysfunc(exist(&ds)) %then %do;
            %put INFO: Dataset &ds exists.;
            %let dsid = %sysfunc(open(&ds));

            /* Inicializar variável */
            %let pass_vars = ;

            /* Obter lista de variáveis do dataset */
            %let nvars = %sysfunc(attrn(&dsid, NVARS));
            %let vars = ;
            %do i = 1 %to &nvars;
                %let varname = %sysfunc(varname(&dsid, &i));
                %let vars = &vars &varname;
            %end;

            /* Fechar o dataset */
            %let rc = %sysfunc(close(&dsid));

            /* Testes */
            %macro test_exists(var);
                %if %sysfunc(indexw(&vars, &var)) %then %do;
                    %put &var: exists;
                    %let pass_vars = &pass_vars &var;
                %end;
            %mend;

            /* Definir testes básicos */
            %test_exists(ID_CONTRAPARTE);
            %test_exists(M_CRED);
            %test_exists(M_DEB);
            %test_exists(CCF);
            %test_exists(ID_OPERACAO);
            %test_exists(TAXA_JURO_EFECTIVA);
            %test_exists(TAXA_JURO_CONTRATUAL);
            %test_exists(EXPOSICAO_OFF_BALANCE);
            %test_exists(EXPOSICAO_ON_BALANCE);
            %test_exists(SPREAD);
            %test_exists(INDEXANTE);
            %test_exists(FLG_RAISIN);
            %test_exists(FLG_DP_MOBILIZAVEL);
            %test_exists(FLG_PENALIZACAO);
            %test_exists(FLG_DP_RENOVAVEL);
            %test_exists(NIVEL_FAIR_VALUE);
            %test_exists(TIPOLOGIA_OPERACAO);
            %test_exists(DATA_INICIO);
            %test_exists(DATA_FIM);
            %test_exists(DATA_REPRICING);
            %test_exists(DATA_CALL_OPTION);
            %test_exists(DATA_ULT_PAG);
            %test_exists(DATA_ULTIMO_REPRICING);
            %test_exists(DATA_PROXIMA_REPRICING);
            %test_exists(DATA_PROX_PAG);
            %test_exists(DATA_PROX_PAG_CUPAO);
            %test_exists(MATURIDADE_RESIDUAL);
            %test_exists(SALDO);
            %test_exists(TIPO_TAXA);
            %test_exists(FREQUENCIA_PAGAMENTO);
			%test_exists(FREQUENCIA_PAGAMENTO_JURO);
            %test_exists(PERIOD_REVISAO_TAXA);
            %test_exists(PAIS);
            %test_exists(MOEDA);
            %test_exists(ID_INDEXANTE);
            /* Testes não compativeis com a Master*/
            %let fail_count = 0;
			%IF "&original_ds" NE "TABELA_MASTER_FINAL" %THEN %DO; 

				%additional_test(TIPO_TAXA, 
			    (TIPO_TAXA IN ('F', 'Fixa', 'Fixo', 'Taxa Fixa', 'Taxa Fixo',
								'V', 'Variável', 'Variavel', 'Taxa Variavel', 'Taxa Variável')), 
			    "Verificar se TIPO_TAXA é F, Fixa, Fixo, Taxa Fixa, Taxa Fixo,	V, Variável, Variavel, Taxa Variavel, Taxa Variável",
				C_1);

				%additional_test(TIPOLOGIA_OPERACAO, 
			    (TIPOLOGIA_OPERACAO IN ('OBRIGACOES','CREDITO', 
				'CARTAS_CRED','DO', 'DP', 'CC', 'CR', 'TOMADO', 'CEDIDO') ), 
			    "Verificar se TIPOLOGIA_OPERACAO é OBRIGACOES,CREDITO, CARTAS_CRED,DO, DP, CC, CR, TOMADO, CEDIDO  para a tabela",
					C_2);

				%additional_test(TAXA_JURO_CONTRATUAL,(TAXA_JURO_CONTRATUAL > 0 and TAXA_JURO_CONTRATUAL <= 100),  
			    "Verificar se TAXA_JURO_CONTRATUAL é maior que 0 e menor ou igual a 100", C_3);
%PUT"ERROR1";
				%additional_test(VLR_CRD,(VLR_CRD < 0)OR ((TIPOLOGIA_OPERACAO NE 'CC') AND (TIPOLOGIA_OPERACAO NE 'CR')), 
			    "Verificar se  montante do valor de crédito para os CC e CR são inferiores a zero", C_4);
%PUT"ERROR2";
				%additional_test(SPREAD, (TIPO_TAXA='F' OR ((SPREAD > 0) and (SPREAD <= 100))), 
			    "Verificar se SPREAD é maior que 0 e menor ou igual a 100 quando TIPO_TAXA é 'V' ou 'Variável'"
				, C_5);
				
				%additional_test(TAXA_JURO_EFECTIVA, (TAXA_JURO_EFECTIVA > 0 and TAXA_JURO_EFECTIVA <= 100) ,  
			    "Verificar se TAXA_JURO_EFECTIVA é maior que 0 e menor ou igual a 100", C_14);
%PUT"ERRO3";
            	/* Teste de concordância entre ID_CONTRAPARTE e ID_OPERACAO */

				data work.failed_&original_ds._C_46 (keep=TABELA DESCRICAO TIPOLOGIA_OPERACAO ID_TESTE ID_OPERACAO);
					length TIPOLOGIA_OPERACAO $20 DESCRICAO $200;
                	set &ds end=last;
    				TIPOLOGIA_OPERACAO = put(TIPOLOGIA_OPERACAO, $20.);
					TABELA=	"&original_ds";
					ID_TESTE="C_46";
					DESCRICAO="Verificar se o ID_Operação se inicia com a informação de Id_Contraparte";
                	if (substr(ID_OPERACAO, 1, length(strip(ID_CONTRAPARTE))) ne INPUT(strip(ID_CONTRAPARTE),$9.)
							AND TIPOLOGIA_OPERACAO in ('DO', 'DP', 'CC', 'CR', 'CREDITO')) 
							then DO;
						fail_count + 1;
						output;
						END;
                	if last then call symputx('fail_count', fail_count);
            	run;
				
            	%if &fail_count = 0 %then %do;
             	   %put ID_CONTRAPARTE concorda com ID_OPERACAO: OK;
                	data work.concordance;
                    	length ambito_de_teste $32 carteira $32 var $32 description $200 result $4 registos 8 incidências 8;
                    	ambito_de_teste = "Controlos";
                    	carteira = "&original_ds";
                    	var = "Id_Operação contém Id_Contraparte";
                    	description = "Verificar se o ID_Operação se inicia com a informação de Id_Contraparte";
                    	result = "OK";
						registos = &total_rows;
                    	incidências = &fail_count;
                    	output;
                	run;
            	%end;
            	%else %do;

                	%put ID_CONTRAPARTE não concorda com ID_OPERACAO: NOK;
                	data work.concordance;
                    	length ambito_de_teste $32 carteira $32 var $32 description $200 result $4 registos 8 incidências 8;
                    	ambito_de_teste = "Controlos";
                    	carteira = "&original_ds";
                    	var = "Id_Operação contém Id_Contraparte";
                    	description = "Verificar se o ID_Operação se inicia com a informação de Id_Contraparte";
                    	result = "NOK";
						registos = &total_rows;
                    	incidências = &fail_count;
                    	output;
                	run;
            	%end;
            	proc append base=work.test_results data=work.concordance force;
            	run;
			%END;

%PUT"ERRO4";
			/*Testes que só fazem sentido para a Master*/
			%IF "&original_ds" = "TABELA_MASTER_FINAL" %THEN %DO; 
				/*Cria uma lista com todas as curvas que se encontram na biblioteca IN*/
				PROC SQL NOPRINT;
    				SELECT CATS("'",SUBSTR(memname, 17),"'")
    					INTO :lista_tabelas SEPARATED BY ', '
    				FROM dictionary.tables
    				WHERE libname = 'IN' /* Biblioteca onde as tabelas estão */
     	 			AND memname LIKE 'CURVA_INDEXANTE_%'; /* Filtra tabelas que começam com "curva_indexante" */
				QUIT;

				/* Verifique se encontrou tabelas */
				%put &lista_tabelas;
				%additional_test(M_CRED,(M_CRED>=0) OR TIPOLOGIA_OPERACAO NE "DP", 
			    "Validar se número movimentos a crédito é positivo", C_47);

				%additional_test(M_DEB,(M_DEB>=0) OR TIPOLOGIA_OPERACAO NE "DP", 
			    "Validar se número movimentos a débito é positivo", C_48);

				%additional_test(ID_INDEXANTE,(ID_INDEXANTE IN (&lista_tabelas.)) OR TIPO_TAXA='F', 
			    "Validar se existe uma curva para cada indexante", C_6);

				%additional_test(SPREAD,(TIPO_TAXA='F' OR ((SPREAD > 0) and (SPREAD <= 1))), 
			    "Verificar se SPREAD é maior que 0 e menor ou igual a 1 para taxa Variável", C_7);

				%additional_test(TIPOLOGIA_OPERACAO,(TIPOLOGIA_OPERACAO IN ('OBRIGACOES','CREDITO', 
				'CARTAS_CRED','DO', 'DP', 'CC', 'CR', 'TOMADO', 'CEDIDO', 'CAIXA', 'DISPONIBILIDADES IC')), 
			    "Verificar se TIPOLOGIA_OPERACAO é OBRIGACOES, CREDITO, CARTAS_CRED, DO, DP, CC, CR, TOMADO ou CEDIDO"
				, C_8);

				%additional_test(TIPO_TAXA, 
			    (TIPO_TAXA in ('F', 'V')), 
			    "Verificar se TIPO_TAXA é F, V", C_9);
				
				%additional_test(DATA_PROX_PAG, 
			    ((DATA_PROX_PAG>= DATA_INICIO) OR 
					(TIPOLOGIA_OPERACAO NOT IN ('OBRIGACOES', 'CREDITO')) OR (TIPO_TAXA='F')), 
			    "Verificar se DATA_PROX_PAG é superior a DATA_INICIO para DPs com taxa variável", C_10);

				%additional_test(DATA_PROX_PAG, 
			    ((DATA_PROX_PAG>= DATA_REF) OR 
					(TIPOLOGIA_OPERACAO NOT IN ('OBRIGACOES', 'CREDITO')) OR (TIPO_TAXA='F')), 
			    "Verificar se DATA_PROX_PAG é superior a DATA_REF para OBRIGACOES e CREDITO com taxa variável", C_11);

				%additional_test(DATA_PROX_PAG, 
			    ((DATA_PROX_PAG<= DATA_FIM) OR 
					(TIPOLOGIA_OPERACAO NOT IN ('OBRIGACOES', 'CREDITO')) OR (TIPO_TAXA='F')), 
			    "Verificar se DATA_PROX_PAG é anterior DATA_FIM para OBRIGACOES e CREDITO com taxa variável", C_12);

				%additional_test(DATA_PROX_PAG, 
			    ((DATA_PROX_PAG NE .) OR 
					(TIPOLOGIA_OPERACAO NOT IN ('CREDITO', 'OBRIGACOES')) OR (TIPO_TAXA='F')), 
			    "Verificar se existe DATA_PROX_PAG quando temos um CREDITO ou uma OBRIGACAO de taxa variável", C_13);


			%END;

            /* Testes que fazem sentido em todas as tabelas*/


			%additional_test(CFF, (CFF > 0 and CFF <= 1) ,  
			    "Verificar se CFF é maior que 0 e menor ou igual a 100", C_49);

			%additional_test(EXPOSICAO_OFF_BALANCE, 
			   ((EXPOSICAO_OFF_BALANCE >= 0) OR (TIPOLOGIA_OPERACAO IN ('DO', 'DP', 'OBRIGACOES', 'CEDIDO', 'TOMADO'))),
			    "Verificar se EXPOSICAO_OFF_BALANCE é maior ou igual a zero", C_15);

			%additional_test(EXPOSICAO_ON_BALANCE, 
			    ((EXPOSICAO_ON_BALANCE >= 0)OR (TIPOLOGIA_OPERACAO = 'CC') OR (TIPOLOGIA_OPERACAO = 'CR')OR (TIPOLOGIA_OPERACAO = 'CARTAS_CRED')), 
			    "Verificar se EXPOSICAO_ON_BALANCE é maior ou igual a zero excepto para operações de crédito", C_16);


			%additional_test(INDEXANTE, 
			    ((TIPO_TAXA='F') OR (INDEXANTE ne '') OR (INDEXANTE ne 'NULL')), 
			    "Verificar se INDEXANTE não está vazio para operações com taxa variável", C_17);

			%additional_test(FLG_RAISIN, 
			    ((FLG_RAISIN in (0, 1)) OR (TIPOLOGIA_OPERACAO NE 'DP')), 
			    "Verificar se existe FLG_RAISIN para o DPs", C_18);


			/*%additional_test(FLG_DP_MOBILIZAVEL, 
			    ((TIPOLOGIA_OPERACAO NE 'DP') OR (FLG_DP_MOBILIZAVEL IN ('0', '', 'NULL')) OR (FLG_PENALIZACAO IN ('0', '1'))) , 
			    "Verificar se quando o DP é mobilizável a FLG_PENALIZACAO está preenchida");*/

			%additional_test(FLG_DP_RENOVAVEL, 
			    ((FLG_DP_RENOVAVEL IN ('0', '1')) OR (TIPOLOGIA_OPERACAO NE 'DP')), 
			    "Verificar se existe FLG_DP_RENOVAVEL para os DPs", C_20);

			%additional_test(NIVEL_FAIR_VALUE, 
			    ((NIVEL_FAIR_VALUE IN ('1', '2', '3')) OR (TIPOLOGIA_OPERACAO NE 'OBRIGACOES')), 
			    "Verificar se existe NIVEL_FAIR_VALUE e toma os seguintes valores 1, 2 ou 3 para as Obrigações", C_21);

			%additional_test(DATA_INICIO, 
			    (DATA_INICIO < DATA_FIM), 
			    "Verificar se DATA_INICIO é anterior a DATA_FIM", C_22);

			%additional_test(DATA_INICIO, 
			    (DATA_INICIO <= DATA_REF), 
			    "Verificar se DATA_INICIO é anterior ou igual a DATA_REF", C_23);
		
			%additional_test(DATA_FIM, 
			    (DATA_FIM >= DATA_REF), 
			    "Verificar se DATA_FIM é após ou igual a DATA_REF", C_24);

			%additional_test(DATA_PROXIMA_REPRICING, 
			    ((DATA_PROXIMA_REPRICING <= DATA_FIM) OR (TIPOLOGIA_OPERACAO NE 'DP') OR (TIPO_TAXA='F')), 
			    "Verificar se DATA_PROXIMA_REPRICING é anterior DATA_FIM para DPs com taxa variável", C_25);

			%additional_test(DATA_PROXIMA_REPRICING, 
			    ((DATA_PROXIMA_REPRICING NOT IN ('', 'NULL')) OR (TIPOLOGIA_OPERACAO NE 'DP') OR (TIPO_TAXA='F')), 
			    "Verificar se existe DATA_PROXIMA_REPRICING quando temos um DP de taxa variável", C_26);

			%additional_test(DATA_PROXIMA_REPRICING, 
			    ((DATA_PROXIMA_REPRICING>= DATA_INICIO) OR (TIPOLOGIA_OPERACAO NE 'DP') OR (TIPO_TAXA='F')), 
			    "Verificar se DATA_PROXIMA_REPRICING é superior a DATA_INICIO para DPs com taxa variável", C_27);

			%additional_test(DATA_PROXIMA_REPRICING, 
			    ((DATA_PROXIMA_REPRICING>= DATA_REF) OR (TIPOLOGIA_OPERACAO NE 'DP') OR (TIPO_TAXA='F')), 
			    "Verificar se DATA_PROXIMA_REPRICING é superior a DATA_REF para DPs com taxa variável", C_28);

			%additional_test(DATA_CALL_OPTION, 
			    ((DATA_CALL_OPTION <= DATA_FIM ) OR 
				(TIPO_TAXA in ('F', 'Fixa')) OR (TIPOLOGIA_OPERACAO NE 'OBRIGACOES')), 
			    "Verificar se DATA_CALL_OPTION é menor ou igual a DATA_FIM", C_29);

			%additional_test(DATA_CALL_OPTION, 
			    ((DATA_CALL_OPTION > DATA_INICIO) OR (TIPO_TAXA in ('F', 'Fixa'))OR (TIPOLOGIA_OPERACAO NE 'OBRIGACOES')), 
			    "Verificar se DATA_CALL_OPTION é maior a DATA_INICIO", C_30);

			%additional_test(DATA_CALL_OPTION, 
			    ((DATA_CALL_OPTION > DATA_REF) OR (TIPO_TAXA in ('F', 'Fixa'))
					OR (TIPOLOGIA_OPERACAO NE 'OBRIGACOES')), 
			    "Verificar se DATA_CALL_OPTION é maior a DATA_REF", C_31);

			%additional_test(DATA_ULT_PAG, 
			    ((DATA_ULT_PAG <= DATA_FIM) OR (TIPO_TAXA in ('F', 'Fixa'))), 
			    "Verificar se DATA_ULT_PAG é menor ou igual a DATA_FIM quando temos taxa variável", C_32);

			%additional_test(DATA_ULT_PAG, 
			    (DATA_ULT_PAG >= DATA_INICIO), 
			    "Verificar se DATA_ULT_PAG é maior ou igual a DATA_INICIO quando temos taxa variável", C_33);

			%additional_test(DATA_PROX_PAG, 
			    (DATA_PROX_PAG <= DATA_FIM and DATA_PROX_PAG >= DATA_ULT_PAG) OR
				(DATA_PROX_PAG = 'NULL' AND DATA_ULT_PAG_CUDATA_ULT_PAGPAO = 'NULL'), 
			    "Verificar se DATA_PROX_PAG é maior ou igual à DATA_ULT_PAG e menor ou igual à DATA_FIM", C_34);

			%additional_test(DATA_ULTIMO_REPRICING, 
			    (DATA_ULTIMO_REPRICING <= DATA_REF) OR (TIPO_TAXA in ('F', 'Fixa'))
					OR (intnx('month', DATA_INICIO, 12, 'SAME')>=DATA_PROXIMA_REPRICING)
					OR (intnx('month', DATA_INICIO, 6, 'SAME')>=DATA_PROXIMA_REPRICING)
					OR (intnx('month', DATA_INICIO, 1, 'SAME')>=DATA_PROXIMA_REPRICING)
					OR (intnx('month', DATA_INICIO, 3, 'SAME')>=DATA_PROXIMA_REPRICING)
					OR (intnx('month', DATA_INICIO, intck('day', DATA_INICIO, DATA_FIM), 'SAME')>=DATA_PROXIMA_REPRICING), 
			    "Verificar se DATA_ULTIMO_REPRICING é menor ou igual a DATA_REF", C_35);

			%additional_test(DATA_ULT_PAG
16APR2024, 
			    (DATA_ULT_PAG <= DATA_REF) OR (TIPO_TAXA in ('F', 'Fixa'))
					OR (intnx('month', DATA_INICIO, 12, 'SAME')>=DATA_PROXIMA_REPRICING)
					OR (intnx('month', DATA_INICIO, 6, 'SAME')>=DATA_PROXIMA_REPRICING)
					OR (intnx('month', DATA_INICIO, 1, 'SAME')>=DATA_PROXIMA_REPRICING)
					OR (intnx('month', DATA_INICIO, 3, 'SAME')>=DATA_PROXIMA_REPRICING)
					OR (intnx('month', DATA_INICIO, intck('day', DATA_INICIO, DATA_FIM), 'SAME')>=DATA_PROXIMA_REPRICING), 
			    "Verificar se DATA_ULT_PAG é menor ou igual a DATA_REF", C_36);


			%additional_test(MATURIDADE_RESIDUAL, 
			    (MATURIDADE_RESIDUAL >= 0), 
			    "Verificar se MATURIDADE_RESIDUAL é maior ou igual a zero", C_37);

			%additional_test(MATURIDADE_RESIDUAL, 
			    (MATURIDADE_RESIDUAL = intck('day', DATA_REF, DATA_FIM)), 
			    "Verificar se MATURIDADE_RESIDUAL é igual à diferença entre DATA_FIM e a data de referência", C_38);

			%additional_test(SALDO, 
			    (SALDO >= 0), 
			    "Verificar se SALDO é maior ou igual a zero", C_39);

			%additional_test(FREQUENCIA_PAGAMENTO, 
			    (FREQUENCIA_PAGAMENTO in ('Mensal', 'Trimestral', 'Semestral', 'Anual', 'da Conta', 'Da Conta'
											'M', 'T', 'S', 'A')), 
			    "Verificar se FREQUENCIA_PAGAMENTO é Mensal, Trimestral, Semestral, Anual ou da Conta", C_40);

			%additional_test(PERIOD_REVISAO_TAXA, 
			    (PERIOD_REVISAO_TAXA in ('da Conta', 'Mensal', 'Trimestral', 'Semestral', 'Anual',
				'M', 'T', 'S', 'A') OR TIPO_TAXA='F'), 
			    "Verificar se PERIOD_REVISAO_TAXA é Mensal, Trimestral, Semestral, Anual ou da Conta", C_41);

			%additional_test(FREQUENCIA_PAGAMENTO_JURO, 
			    (FREQUENCIA_PAGAMENTO_JURO in ('da Conta', 'Mensal', 'Trimestral', 'Semestral', 'Anual', 'Da Conta'
				'M', 'T', 'S', 'A')), 
			    "Verificar se FREQUENCIA_PAGAMENTO_JURO é Mensal, Trimestral, Semestral, Anual ou da Conta", C_42);

			%additional_test(PAIS, 
			    (PAIS in ('PT', 'PRT', 'LBN', 'EGY', 'CHE', 'IRL', 'AGO', 'TCA', 'BEL', 'USA', 'ZAF', 'CYP', 'CSK', 'DEU', 'GBR', 'MUS', 'NGA', 'NLD', 'GIB', 'ESP', 'AUT', 'CYM', 'FRA', 'CPV', 'HKG', 'BRA', 'ARE', 'MOZ', 'VNM', 'URY', 'VEN', 'COL', 'LUX', 'MAC', 'JPN', 'STP', 'MLT', 'FIN', 'SYC', 'ITA', 'MCO', 'CIV', 'AIA', 'VGB', 'GNB', 'ROU', 'HRV', 'CHL', 'KOR', 'ISL', 'IDN', 'CHN', 'AUS', 'LTU', 'MEX', 'HUN')), 
			    "Verificar se PAIS possui um dos códigos válidos", C_43);

			%additional_test(MOEDA, 
			    (MOEDA in ('EUR', 'USD')), 
			    "Verificar se MOEDA é EUR ou USD", C_44);

            /* Criar a tabela de overview das colunas */
            %macro create_columns_overview(ds);
                /* Obter lista de variáveis do dataset */
                proc sql noprint;
                    select name into :vars separated by ' ' 
                    from dictionary.columns 
                    where libname=upcase("&source_lib") and memname=upcase("%scan(&ds, 1, .)");
                quit;

                /* Inicializar tabela de overview */
                data work.columns_overview;
                    length carteira $32 campo $32 vazio zero preenchido 8;
                    stop;
                run;

                /* Macro para calcular contagens por variável */
                %macro count_values(campo);
                    %let vazio = 0;
                    %let zero = 0;
                    %let preenchido = 0;
                    data _null_;
                        set &ds end=last;
                        retain vazio 0 zero 0 preenchido 0;
                        if missing(&campo) or &campo = "" or &campo = "." or &campo = "NULL" then vazio + 1;
                        if &campo = 0 then zero + 1;
                        if not (missing(&campo) or &campo = "" or &campo = "." or &campo = "NULL" or &campo = 0) then preenchido + 1;
                        if last then do;
                            call symputx('vazio', vazio);
                            call symputx('zero', zero);
                            call symputx('preenchido', preenchido);
                        end;
                    run;

                    data work._temp;
                        length carteira $32 campo $32 vazio zero preenchido 8;
                        carteira = "&original_ds";
                        campo = "&campo";
                        vazio = &vazio;
                        zero = &zero;
                        preenchido = &preenchido;
                        output;
                    run;

                    proc append base=work.columns_overview data=work._temp force;
                    run;
                %mend;

                /* Iterar sobre as variáveis e chamar a macro de contagem */
                %let num_vars = %sysfunc(countw(&vars));
                %do i = 1 %to &num_vars;
                    %let campo = %scan(&vars, &i);
                    %count_values(&campo);
                %end;
            %mend;

            /* Chamar a macro para criar a tabela de overview */
            %create_columns_overview(&ds);

            /* Verificar se as tabelas existem antes de mudar */
            %if %sysfunc(exist(work.test_results)) %then %do;
                proc datasets lib=work nolist;
                    change test_results=&test_results_name;
                quit;
            %end;
            
            %if %sysfunc(exist(work.columns_overview)) %then %do;
                proc datasets lib=work nolist;
                    change columns_overview=&columns_overview_name;
                quit;
            %end;

            data &outlib..&test_results_name;
                set work.&test_results_name;
            run;

            data &outlib..&columns_overview_name;
                set work.&columns_overview_name;
            run;

            /* Limpar tabelas intermédias na work*/
           /* proc datasets lib=work nolist;
                delete &test_results_name &columns_overview_name ds_filtered duplicates test_results total_info columns_info not_empty title_exists title_additional results duplicates_check columns_overview _temp;
            run;
            quit*/;

            /* Remover linhas desnecessárias e renomear colunas */
            data &outlib..&test_results_name;
				length descrição $200;
                set &outlib..&test_results_name;
                if var not in ("Test Type") then do;
                    rename ambito_de_teste=ambito_de_teste carteira=carteira var=campo description=descrição result=resultado fail_count=incidências;
                    output;
                end;
            run;

            /* Mostrar resultados */
            title "Controlos para &original_ds";
            proc print data=&outlib..&test_results_name;
            run;

            /* Mostrar overview das colunas */
            title "Visão geral das colunas para &original_ds";
            proc print data=&outlib..&columns_overview_name;
            run;

        %end;
        %else %put Dataset &source_lib..&ds does not exist;

    %end;
    %else %put Dataset &ds does not exist;

/*Obter lista de tabelas que começam com "FAILED_C" */
proc sql noprint;
    select catx('.', 'WORK', memname)
    into :lista_tabelas separated by ' '
    from dictionary.tables
    where libname = 'WORK' /* Biblioteca onde as tabelas estão */
      and memname like 'FAILED%'; /* Filtra tabelas que começam com "FAILED_C" */
quit;

/*Concatenar as tabelas listadas */
data DQ.OPERACOES_FAILED(DROP=TIPOLOGIA_OPERACAO);
	length TIPOLOGIA_OPERACAO_I $20 ID_TESTE $5 TABELA $36;
    set &lista_tabelas;
	TIPOLOGIA_OPERACAO_I = put(TIPOLOGIA_OPERACAO, $20.);
run;

%mend data_controls;

%macro Totais_Carteiras(Lib, Tabela, Campo_Soma, Var_Output, campo_1, criteria_1, campo_2, criteria_2,
                        campo_3, criteria_3, campo_4, criteria_4, campo_5, criteria_5, campo_6, criteria_6, 
                        campo_7, criteria_7);

    /* Inicializa a variável de condição como uma string vazia */
    %let cond = 1=1;

    /*Adiciona condições para cada campo e critério fornecido*/
    %if %length(&campo_1) %then %let cond = &cond AND (&campo_1 = &criteria_1);
    %if %length(&campo_2) %then %let cond = &cond AND (&campo_2 = &criteria_2);
    %if %length(&campo_3) %then %let cond = &cond AND (&campo_3 = &criteria_3);
    %if %length(&campo_4) %then %let cond = &cond AND (&campo_4 = &criteria_4);
    %if %length(&campo_5) %then %let cond = &cond AND (&campo_5 = &criteria_5);
    %if %length(&campo_6) %then %let cond = &cond AND (&campo_6 = &criteria_6);
    %if %length(&campo_7) %then %let cond = &cond AND (&campo_7 = &criteria_7);

    /* Substitui '= in' por 'in' no final */
    %let cond = %sysfunc(tranwrd(&cond, = in, in));
    %put &cond;

    /* Define o nome da tabela auxiliar */
    %let Table_Name = &Var_Output._&Campo_Soma;

    /* Filtra os dados */
    proc sql;
        create table &Lib..&Table_Name as
        select *
        from &Tabela
        where &cond;
    quit;

    /* Realiza a soma e armazena na variável especificada */
    proc sql noprint;
        select put(sum(&Campo_Soma), 32.2) into :var_output_s
        from &Lib..&Table_Name;
    quit;
	
	%let var_output_s = %sysfunc(putn(&var_output_s, BEST32.));
	%PUT"EU";
	%PUT &var_output_s.;

    /* Guarda o resultado na variável de macro Var_Output */
	%global &Var_Output;
	%if %length(&var_output_s) %then %let &Var_Output = %sysfunc(sum(&var_output_s),BEST32.);
	%else %let &Var_Output = 0;

    /* Adiciona a impressão do valor da variável de macro calculada */
    %put &Var_Output=&&&Var_Output;

%mend Totais_Carteiras;

%macro reconciliacao(outlib, table_1, table_2, ref_date);

    /* Chamadas para Tabela Master */
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_CXeDBC_EUR, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'EUR', RUBRICA, 'CAIXA');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_DispIC_EUR, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'EUR', RUBRICA, 'DISPONIBILIDADES IC');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_AppOIC_EUR, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'EUR', RUBRICA, 'APLICACOES OIC');
	%Totais_Carteiras(work, &table_1, CAPITAL_VINCENDO, M_Titulo_EUR, DATA_REF, &ref_date, MOEDA, 'EUR', RUBRICA, 'TITULOS DIVIDA');
	%Totais_Carteiras(work, &table_1, CAPITAL_VINCENDO, M_FVOCI_EUR, DATA_REF, &ref_date, MOEDA, 'EUR', RUBRICA, 'TITULOS DIVIDA', DSC_PRODUTO, in ('INV FVOCI EUR', 'DISCR FVOCI EUR'));
	%Totais_Carteiras(work, &table_1, CAPITAL_VINCENDO, M_CAmrt_EUR, DATA_REF, &ref_date, MOEDA, 'EUR', RUBRICA, 'TITULOS DIVIDA', DSC_PRODUTO, 'INV CA EUR');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_Credit_EUR, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'EUR', RUBRICA, 'CREDITO');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_Descts_EUR, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'EUR', RUBRICA, 'DESCONTOS CD');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_RecOIC_EUR, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'EUR', RUBRICA, 'RECURSOS OIC');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_RecClt_EUR, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'EUR', RUBRICA, 'RECURSOS CLIENTES');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_RecDO_EUR, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'EUR', RUBRICA, 'RECURSOS CLIENTES', TIPOLOGIA_OPERACAO, 'DO');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_RecDP_EUR, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'EUR', RUBRICA, 'RECURSOS CLIENTES', TIPOLOGIA_OPERACAO, 'DP');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_CXeDBC_USD, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'USD', RUBRICA, 'CAIXA');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_DispIC_USD, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'USD', RUBRICA, 'DISPONIBILIDADES IC');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_AppOIC_USD, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'USD', RUBRICA, 'APLICACOES OIC');
	%Totais_Carteiras(work, &table_1, CAPITAL_VINCENDO, M_Titulo_USD, DATA_REF, &ref_date, MOEDA, 'USD', RUBRICA, 'TITULOS DIVIDA');
	%Totais_Carteiras(work, &table_1, CAPITAL_VINCENDO, M_FVOCI_USD, DATA_REF, &ref_date, MOEDA, 'USD', RUBRICA, 'TITULOS DIVIDA', DSC_PRODUTO, 'INV FVOCI USD');
	%Totais_Carteiras(work, &table_1, CAPITAL_VINCENDO, M_CAmrt_USD, DATA_REF, &ref_date, MOEDA, 'USD', RUBRICA, 'TITULOS DIVIDA', DSC_PRODUTO, 'INV CA USD');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_Credit_USD, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'USD', RUBRICA, 'CREDITO');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_Descts_USD, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'USD', RUBRICA, 'DESCONTOS CD');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_RecOIC_USD, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'USD', RUBRICA, 'RECURSOS OIC');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_RecClt_USD, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'USD', RUBRICA, 'RECURSOS CLIENTES');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_RecDO_USD, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'USD', RUBRICA, 'RECURSOS CLIENTES', TIPOLOGIA_OPERACAO, 'DO');
	%Totais_Carteiras(work, &table_1, MONTANTE_CAPITAL, M_RecDP_USD, DATA_REF, &ref_date, CENARIO, 'Baseline', MOEDA, 'USD', RUBRICA, 'RECURSOS CLIENTES', TIPOLOGIA_OPERACAO, 'DP');
	
	/* Chamadas para Balancete */
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_CXeDBC_EUR, MOEDA, 'EUR', ID, '10');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_DispIC_EUR, MOEDA, 'EUR', ID, '11');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_AppOIC_EUR, MOEDA, 'EUR', ID, '13');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_Titulo_EUR, MOEDA, 'EUR', ID, in ('18', '22'));
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_FVOCI_EUR, MOEDA, 'EUR', ID, '18');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_CAmrt_EUR, MOEDA, 'EUR', ID, '22');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_Credit_EUR, MOEDA, 'EUR', ID, in ('14', '15'));
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_Descts_EUR, MOEDA, 'EUR', ID, '544');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_RecOIC_EUR, MOEDA, 'EUR', ID, '39');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_RecClt_EUR, MOEDA, 'EUR', ID, '40');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_RecDO_EUR, MOEDA, 'EUR', ID, in ('400010','400020','40010'));
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_RecDP_EUR, MOEDA, 'EUR', ID, in ('400012', '400022', '40012'));
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_CXeDBC_USD, MOEDA, 'USD', ID, '10');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_DispIC_USD, MOEDA, 'USD', ID, '11');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_AppOIC_USD, MOEDA, 'USD', ID, '13');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_Titulo_USD, MOEDA, 'USD', ID, in ('18', '22'));
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_FVOCI_USD, MOEDA, 'USD', ID, '18');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_CAmrt_USD, MOEDA, 'USD', ID, '22');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_Credit_USD, MOEDA, 'USD', ID, in ('14', '15'));
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_Descts_USD, MOEDA, 'USD', ID, '544');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_RecOIC_USD, MOEDA, 'USD', ID, '39');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_RecClt_USD, MOEDA, 'USD', ID, '40');
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_RecDO_USD, MOEDA, 'USD', ID, in ('400010','400020','40010'));
	%Totais_Carteiras(work, &table_2, MONTANTE_CAPITAL, B_RecDP_USD, MOEDA, 'USD', ID, in ('400012', '400022', '40012'));

    /* Criar tabela de resultados */
	data &outlib..DQ3_R_Reconciliacao;
	    length Carteira $50 Tabela_Master_Euro 8 Tabela_Master_Dolar 8 Balancete_Euro 8 Balancete_Dolar 8;
	    array vars[48] $32 _temporary_ 
	        ("M_CXeDBC_EUR", "M_DispIC_EUR", "M_AppOIC_EUR", "M_Titulo_EUR", "M_FVOCI_EUR", "M_CAmrt_EUR", "M_Credit_EUR", "M_Descts_EUR", "M_RecOIC_EUR", "M_RecClt_EUR", "M_RecDO_EUR", "M_RecDP_EUR", 
	         "M_CXeDBC_USD", "M_DispIC_USD", "M_AppOIC_USD", "M_Titulo_USD", "M_FVOCI_USD", "M_CAmrt_USD", "M_Credit_USD", "M_Descts_USD", "M_RecOIC_USD", "M_RecClt_USD", "M_RecDO_USD", "M_RecDP_USD", 
	         "B_CXeDBC_EUR", "B_DispIC_EUR", "B_AppOIC_EUR", "B_Titulo_EUR", "B_FVOCI_EUR", "B_CAmrt_EUR", "B_Credit_EUR", "B_Descts_EUR", "B_RecOIC_EUR", "B_RecClt_EUR", "B_RecDO_EUR", "B_RecDP_EUR", 
	         "B_CXeDBC_USD", "B_DispIC_USD", "B_AppOIC_USD", "B_Titulo_USD", "B_FVOCI_USD", "B_CAmrt_USD", "B_Credit_USD", "B_Descts_USD", "B_RecOIC_USD", "B_RecClt_USD", "B_RecDO_USD", "B_RecDP_USD");

	    do i = 1 to 12;
	        Carteira = scan("Caixa,Disponibilidades IC,Aplicações OIC,Títulos Dívida,Títulos Dívida FVOCI,Títulos Dívida CA,Crédito,Descontos,Recursos OIC,Recursos Clientes,Recursos Clientes DO,Recursos Clientes DP", i, ",");
	        Tabela_Master_Euro = input(symget(vars[i]), best.);
	        if Tabela_Master_Euro = . then Tabela_Master_Euro = 0;
	        Tabela_Master_Dolar = input(symget(vars[i+12]), best.);
	        if Tabela_Master_Dolar = . then Tabela_Master_Dolar = 0;
	        Balancete_Euro = input(symget(vars[i+24]), best.);
	        if Balancete_Euro = . then Balancete_Euro = 0;
	        Balancete_Dolar = input(symget(vars[i+36]), best.);
	        if Balancete_Dolar = . then Balancete_Dolar = 0;
	        output;
	    end;
	    drop i;
	run;

    /* Exibir a tabela de resultados */
    proc print data=&outlib..DQ3_R_Reconciliacao;
    run;

%mend reconciliacao;

%macro concat_tables(source_lib, outlib, out_table, tables);
    /* Concatena as tabelas especificadas em uma única tabela */

    /* Concatena as tabelas especificadas */
    data &outlib..&out_table;
        set 
        %do i = 1 %to %sysfunc(countw(&tables, %str( )));
            %let table = %scan(&tables, &i, %str( ));
            &source_lib..&table
        %end;
        ;
    run;

    /* Exibe a tabela concatenada */
    proc print data=&outlib..&out_table;
        title "Concatenated Table: &out_table";
    run;
%mend;