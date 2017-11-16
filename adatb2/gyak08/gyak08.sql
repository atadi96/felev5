--http://vargadaniel.web.elte.hu/ab2/expl.txt
--http://vargadaniel.web.elte.hu/ab2/hintek.txt
create table PLAN_TABLE (
        statement_id       varchar2(30),
        plan_id            number,
        timestamp          date,
        remarks            varchar2(4000),
        operation          varchar2(30),
        options            varchar2(255),
        object_node        varchar2(128),
        object_owner       varchar2(30),
        object_name        varchar2(30),
        object_alias       varchar2(65),
        object_instance    numeric,
        object_type        varchar2(30),
        optimizer          varchar2(255),
        search_columns     number,
        id                 numeric,
        parent_id          numeric,
        depth              numeric,
        position           numeric,
        cost               numeric,
        cardinality        numeric,
        bytes              numeric,
        other_tag          varchar2(255),
        partition_start    varchar2(255),
        partition_stop     varchar2(255),
        partition_id       numeric,
        other              long,
        distribution       varchar2(30),
        cpu_cost           numeric,
        io_cost            numeric,
        temp_space         numeric,
        access_predicates  varchar2(4000),
        filter_predicates  varchar2(4000),
        projection         varchar2(4000),
        time               numeric,
        qblock_name        varchar2(30),
        other_xml          clob
);

EXPLAIN PLAN SET statement_id='ut1'  -- ut1 -> az utasításnak egyedi nevet adunk
  FOR 
  SELECT avg(fizetes) FROM nikovits.dolgozo;

select * from PLAN_TABLE;

SELECT LPAD(' ', 2*(level-1))||operation||' + '||options||' + '||object_name terv
FROM plan_table
START WITH id = 0 AND statement_id = 'ut1'                 -- az utasítás neve szerepel itt
CONNECT BY PRIOR id = parent_id AND statement_id = 'ut1'   -- meg itt
ORDER SIBLINGS BY position;

select plan_table_output from table(dbms_xplan.display('plan_table','fizkat_not_shitty'/*,'all'*/));

explain plan set statement_id='teszt1'
    for
    select /*+ use_nl(o, d)*/ * from nikovits.osztaly o, nikovits.dolgozo d
    where o.oazon = d.oazon;
    
delete from PLAN_TABLE where STATEMENT_ID='teszt3';
--feladat7.txt
create table dolgozo as select * from nikovits.dolgozo;
create table osztaly as select * from nikovits.osztaly;
create table fiz_kategoria as select * from nikovits.fiz_kategoria;

explain plan set statement_id='fizkat_muchindex'
 for
    select distinct o.onev
        from osztaly o, dolgozo d, fiz_kategoria f
        where o.oazon = d.oazon
          and f.kategoria = 1
          and d.fizetes between f.also and f.felso;

explain plan set statement_id='fizkat_not_shitty'
 for
    select distinct o.onev
        from fiz_kategoria f
        join dolgozo d on d.fizetes between f.also and f.felso and f.kategoria = 1
        join osztaly o on o.oazon = d.oazon;
          
create index fkat_index on fiz_kategoria (kategoria);
create index dolg_oazon on dolgozo (oazon);
create index osztaly_oazon on osztaly (oazon);