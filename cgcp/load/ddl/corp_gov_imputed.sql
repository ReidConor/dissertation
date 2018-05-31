-- this db holds the corp_gov tables, but imputed versions
create database corp_gov_imputed;

create table corp_gov_imputed.spx like corp_gov.spx;
create table corp_gov_imputed.sxxp like corp_gov.sxxp;
create table corp_gov_imputed.eebp like corp_gov.eebp;
