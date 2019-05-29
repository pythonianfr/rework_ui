from pathlib import Path

from sqlhelp import sqlfile


SCHEMAFILE = Path(__file__).parent / 'schema.sql'


def init(engine):
    sql = sqlfile(SCHEMAFILE, ns='rework')
    with engine.begin() as cn:
        cn.execute(sql)
