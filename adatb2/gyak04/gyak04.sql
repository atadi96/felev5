--http://vargadaniel.web.elte.hu/ab2/adatszotar_nezetek.txt
--http://vargadaniel.web.elte.hu/ab2/gyak4.txt
--http://vargadaniel.web.elte.hu/ab2/zh_segitseg.txt

select rownum, rowid, szeret.*
from nikovits.szeret;

SELECT dnev, dbms_rowid.rowid_object(ROWID) adatobj, 
        dbms_rowid.rowid_relative_fno(ROWID) fajl,
        dbms_rowid.rowid_block_number(ROWID) blokk,
        dbms_rowid.rowid_row_number(ROWID) sor
 FROM nikovits.dolgozo WHERE dnev = 'KING';
 
--1. feladat 
SELECT count(dbms_rowid.rowid_block_number(rowid))
  from nikovits.cikk; --nem j� lek�rdez�s
  
select *
  from dba_tables  
  where owner = 'NIKOVITS'
    and table_name = 'CIKK'; --nem j� lek�rdez�s
    
select owner, segment_name, blocks
  from dba_extents
  where owner = 'NIKOVITS'
    and segment_name = 'CIKK'; --j� lek�rdez�s
    
select owner, segment_name, blocks
  from dba_segments
  where owner = 'NIKOVITS'
    and segment_name = 'CIKK'; --j� lek�rdez�s

--2. feladat
SELECT count(distinct dbms_rowid.rowid_block_number(rowid))
  from nikovits.cikk; --az eredm�ny j�, k�rd�s hogy mindig?

