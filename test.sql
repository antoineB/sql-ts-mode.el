with toto as (select *
                from tutu.toto AS to,
                     titi AS ti,
                     tutu as ooo
                     join toto.tutu as tu ON to.id = tu.id
                     join abc as uu on uu.id = ooo.id
                     natural join edf as oo on oo.id = 12,
                     tptp as tp),
  tata as (select * from maison)
select * from toto;

-- toto
with zzz AS (select ('a'::text | 'b'))
select toto.tutu, (select 12)
  from tutu
       join toto AS t on t.id = tutu.id
       join tutu AS tt on tt.id = t.id
                    
 where tata = 12
       ;

insert into toto(aa) values (1);

create table toto(id serial, toto varchar default 'toto');

update toto
   set warch = 12,
       othera = 13
 where 12;

create or replace function toto ()
  language plsql
