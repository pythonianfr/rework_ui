create table {ns}.taskstable (
  id serial primary key,
  domain text default 'default',
  hash text not null,
  content text not null
);

create index ix_{ns}_taskstable_domain on {ns}.taskstable (domain);
create index ix_{ns}_taskstable_hash on {ns}.taskstable (hash);
