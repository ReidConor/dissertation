select
  count(Ticker)
from spx
where
  REPLACE(`Ticker`, ' ', '') in (
    select
      REPLACE(`Ticker`, ' ', '')
    from spx_ceo_comp
  );
