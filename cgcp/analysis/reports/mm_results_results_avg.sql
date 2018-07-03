drop table if exists mm_results.results_avg;
create table mm_results.results_avg as
select
  a.Algorithm,
  a.DataSet,
  a.Target,
  avg(a.`Correctly Classified Instances`) avg_accuracy,
  avg(a.`Coverage Of cases`) as avg_coverage,
  avg(a.`Precision Class 0`) as avg_precision_0,
  avg(a.`Precision Class 1`) as avg_precision_1,
  avg(a.`ROC area`) as avg_roc_area
from mm_results.results a
group by
  a.Algorithm,
  a.DataSet,
  a.Target;
