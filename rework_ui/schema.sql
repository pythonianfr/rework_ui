create table {ns}.events (
  id serial primary key,
  tstamp timestamp with time zone not null default current_timestamp,
  action text not null check (action IN ('I','D','U')),
  taskid integer not null
);


create or replace function trace_events() returns trigger as $body$
declare
  taskid integer;
begin
 if (tg_op = 'UPDATE') then
   select into taskid old.id;
   insert into {ns}.events (action, taskid) values (substring(tg_op,1,1), taskid);
 elsif (tg_op = 'DELETE') then
   select into taskid old.id;
   insert into {ns}.events (action, taskid) values(substring(tg_op,1,1), taskid);
 elsif (tg_op = 'INSERT') then
   select into taskid new.id;
   insert into {ns}.events (action, taskid) values(substring(tg_op,1,1), taskid);
 else
   raise warning 'we missed something';
 end if;
 delete from {ns}.events where tstamp < (current_timestamp - interval '1 minute');
 return null;
end;
$body$
language plpgsql;


create trigger trace_events
after insert or update or delete on {ns}.task
for each row execute procedure trace_events();
