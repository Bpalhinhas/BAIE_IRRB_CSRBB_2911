/*Cria pastas para onde o programa vai correr cada tipo de tabela, tabela input, tabelas auxiliares ao calculo e tabelas finais de output.*/
/*
%let path = Z:\BANCO;
%let nome_da_pasta = 092024;            /* Defina o nome da nova pasta principal */

/* Cria a pasta principal */
/*X mkdir "&path.\&nome_da_pasta";

/* Cria subpastas adicionais dentro da pasta principal */
/*X mkdir "&path.\&nome_da_pasta\inputs";
X mkdir "&path.\&nome_da_pasta\tabelas_aux";
X mkdir "&path.\&nome_da_pasta\outputs";
X mkdir "&path.\&nome_da_pasta\valid";
X mkdir "&path.\&nome_da_pasta\tabelas";

LIBNAME  IN "&path.\&nome_da_pasta\inputs";

LIBNAME AUX "&path.\&nome_da_pasta\tabelas_aux";

LIBNAME OUT "&path.\&nome_da_pasta\outputs";

LIBNAME DQ "&path.\&nome_da_pasta\valid";

*/
LIBNAME  IN "Z:\inputs";

LIBNAME AUX "Z:\bia";

LIBNAME OUT "Z:\outputs";

LIBNAME DQ "Z:\valid";

LIBNAME TESTES "Z:\tabelas_novas";
