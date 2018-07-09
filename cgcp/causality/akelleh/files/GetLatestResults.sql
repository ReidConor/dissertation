drop table akelleh_results_latest;
create table akelleh_results_latest as
select
  a.dataset,
  a.treatment,
  a.target,
  a.results,
  a.mm
from akelleh_results a
inner join (
  select
    dataset,
    treatment,
    target,
    max(datestamp) as max_datestamp
  from akelleh_results
  group by
    dataset,
    treatment,
    target
) b
  on a.dataset = b.dataset
  and a.treatment = b.treatment
  and a.target = b.target
  and a.datestamp = b.max_datestamp
