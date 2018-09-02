use mm_results;
drop table if exists tobin_q_results_max;
create table tobin_q_results_max as
select
	a.Algorithm,
	a.DataSet,
	a.Target,
	a.`Correctly Classified Instances`,
	a.`Precision Class 0`,
	a.`Precision Class 1`,
	a.`ROC Area`
from tobin_q_results a
inner join (
	select c.Algorithm, c.DataSet, c.Target, max(c.`ROC Area`) as `ROC Area`
	from tobin_q_results c
	group by c.Algorithm,  c.DataSet, c.Target
)b on
	a.Algorithm = b.Algorithm and
	a.DataSet = b.DataSet and
	a.Target = b.Target and
	a.`ROC Area` = b.`ROC Area`;


drop table if exists altman_z_results_max;
create table altman_z_results_max as
select
	a.Algorithm,
  a.DataSet,
	a.Target,
	a.`Correctly Classified Instances`,
 	a.`Precision Class 0`,
	a.`Precision Class 1`,
	a.`Precision Class 2`,
 	a.`ROC Area`
from altman_z_results a
inner join (
	select c.Algorithm, c.DataSet, c.Target, max(c.`ROC Area`) as `ROC Area`
	from altman_z_results c
	group by c.Algorithm,c.DataSet, c.Target
)b on
	a.Algorithm = b.Algorithm and
	a.DataSet = b.DataSet and
	a.Target = b.Target and
	a.`ROC Area` = b.`ROC Area`;
