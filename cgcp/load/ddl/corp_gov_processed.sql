-- this db holds a copy of corp_gov, but with various manipualtions carried on on each table
create database corp_gov_processed;

create table corp_gov_processed.spx like corp_gov.spx;
create table corp_gov_processed.sxxp like corp_gov.sxxp;
create table corp_gov_processed.eebp like corp_gov.eebp;
