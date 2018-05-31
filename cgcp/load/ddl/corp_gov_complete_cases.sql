-- this db holds a copy of corp_gov, but only the complete cases from each table
-- any rows with any missing values are not included then
create database corp_gov_complete_cases;

create table corp_gov_complete_cases.spx like corp_gov.spx;
create table corp_gov_complete_cases.sxxp like corp_gov.sxxp;
create table corp_gov_complete_cases.eebp like corp_gov.eebp;
