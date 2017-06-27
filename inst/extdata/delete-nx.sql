delete from digital_tb
where id = (select id from digitalnet_id
            where name = 'nx');
