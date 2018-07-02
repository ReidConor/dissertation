drop table if exists mm_results.results_avg;
create table mm_results.results_avg as
select
  a.Algorithm,
  a.DataSet,
  a.Target,
  avg(a.`Correctly Classified Instances`),
  avg(a.`Coverage Of cases`),
  avg(a.`Precision Class 0`),
  avg(a.`Precision Class 1`),
  avg(a.`ROC area`)
from mm_results.results a
group by
  a.Algorithm,
  a.DataSet,
  a.Target;
