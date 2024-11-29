%macro export_to_excel(lib, filename);

    %let sheet_names =  DQ_O_DS_FILTERED_20240717 DQ_TESTS_DS_FILTERED_20240717
					    DQ_O_T_CARTAS_CRED_20240717 DQ_TESTS_T_CARTAS_CRED_20240717
						DQ_O_T_CEDIDO_20240717 DQ_TESTS_T_CEDIDO_20240717
						DQ_O_T_DP_DO_20240717 DQ_TESTS_T_DP_DO_20240717
						DQ_O_T_OBRIGACOES_20240717 DQ_TESTS_T_OBRIGACOES_20240717
						DQ_O_T_TOMADO_20240717 DQ_TESTS_T_TOMADO_20240717  
	;

    %let num_tables = 12;

    %do i = 1 %to &num_tables;
        %let table_name = %scan(&sheet_names, &i);

        proc export data=&lib..&&table_name
            outfile="&filename"
            dbms=xlsx
            replace;
            sheet="&&table_name";
        run;
    %end;

%mend export_to_excel;
