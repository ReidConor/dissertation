create database if not exists corp_gov_results;

use corp_gov_results;
create table if not exists corp_gov_results.adaboost(
	datestamp date,
    	dataset varchar(50)
);

create database if not exists corp_gov_processed;
use corp_gov_processed;
