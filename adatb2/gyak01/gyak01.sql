SELECT table_name, tablespace_name FROM user_tables;

-- futásidõ: 35 sec 
SELECT sum(darab) FROM nikovits.hivas, nikovits.kozpont, nikovits.primer
WHERE hivas.kozp_azon_hivo=kozpont.kozp_azon AND kozpont.primer=primer.korzet
AND primer.varos = 'Szentendre' 
AND datum + 1 = next_day(to_date('2012.01.31', 'yyyy.mm.dd'),'hétfõ');


-- futásidõ: 1 sec
SELECT sum(darab) FROM nikovits.hivas, nikovits.kozpont, nikovits.primer
WHERE hivas.kozp_azon_hivo=kozpont.kozp_azon AND kozpont.primer=primer.korzet
AND primer.varos = 'Szentendre' 
AND datum = next_day(to_date('2012.01.31', 'yyyy.mm.dd'),'hétfõ') - 1;

SELECT *
FROM all_tables
WHERE owner = 'NIKOVITS'
  AND table_name LIKE 'C%';
  
SELECT * FROM dba_objects;

SELECT * FROM dba_tab_columns where table_name = 'DBA_TABLES' ;

SELECT *
from dba_objects
where object_name='DBA_TABLES'
  or object_type='SYNONYM';
  
select distinct object_type from dba_objects where owner='ORAUSER';

SELECT count(distinct object_type) from dba_objects;

-- oszlopok

select COUNT(*)
  from dba_tab_columns
  where table_name = 'EMP'
    and owner='NIKOVITS';

select data_type
  from dba_tab_columns
  where table_name = 'EMP'
    and owner='NIKOVITS'
    and column_id = 6;
    
    

