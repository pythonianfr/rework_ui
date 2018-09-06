from sqlalchemy import Table, Column, Integer, String

from rework.schema import meta


taskstable = Table(
    'taskstable', meta,
    Column('id', Integer, primary_key=True),
    Column('domain', String, default='default', index=True),
    Column('hash', String, nullable=False, index=True),
    Column('content', String, nullable=False),
    schema='rework'
)


def init(engine):
    with engine.begin() as cn:
        taskstable.create(cn)


def reset(engine):
    with engine.begin() as cn:
        taskstable.drop(cn, checkfirst=True)
