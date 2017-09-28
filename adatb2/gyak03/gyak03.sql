--http://vargadaniel.web.elte.hu/ab2/adatszotar_nezetek.txt
--http://vargadaniel.web.elte.hu/ab2/gyak3.txt

select file_name, bytes
from dba_data_files
union
select file_name, bytes
from dba_temp_files
order by bytes desc;

select * from dba_tablespaces;

select tablespace_name, sum(bytes), count(file_name) from
(select tablespace_name, bytes, file_name
  from dba_data_files
union
select tablespace_name, bytes, file_name
  from dba_temp_files)
group by tablespace_name;

select tablespace_name from dba_tablespaces
 minus
 select tablespace_name from dba_data_files;
 
 select * from dba_segments;

select * from dba_extents;

select * from
(select segment_name, owner, sum(bytes)
  from dba_extents
  where segment_type= 'TABLE'
  group by segment_name, owner
  order by sum(bytes) desc)
  where rownum = 1;
  
select * from  
(select owner
  from dba_extents
  group by owner
  order by sum(bytes) desc)
  where rownum <= 2;
  
select * from dba_data_files;
select count(*), sum(bytes)
  from dba_extents
  where file_id =
    (select file_id from dba_data_files where file_name LIKE '%users01.dbf');
    
--H�ny �sszef�gg� szabad ter�let van a 'users01.dbf' adatf�jlban?
--Mekkora ezek �sszm�rete?

 select dba_data_files.file_id, sum(dba_data_files.bytes)
 from dba_data_files join dba_free_space on dba_free_space.FILE_ID = dba_data_files.FILE_ID
 where file_name like '%users01.dbf'
 group by dba_data_files.file_id;
 
--�rjunk meg egy PLSQL proced�r�t, amelyik a param�ter�l kapott felhaszn�l�n�vre ki�rja 
--a felhaszn�l� legr�gebben l�trehozott t�bl�j�t, annak m�ret�t byte-okban, valamint a l�trehoz�s
--d�tumat.
CREATE OR REPLACE PROCEDURE regi_tabla(p_user VARCHAR2) IS 
  cursor cs is
    select table_name
      from dba_tables
      where owner=p_user;
BEGIN
  null;
END;
SET SERVEROUTPUT ON;
CALL regi_tabla('SH');
