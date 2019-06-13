from threading import Thread
import socket
import webbrowser

import click
from sqlalchemy import create_engine

from rework import schema as baseschema, helper
from rework.helper import find_dburi
from rework_ui.app import startapp
from rework_ui import schema, taskstable


@click.command()
@click.argument('db-uri')
def view(db_uri):
    """monitor and control workers and tasks"""
    ipaddr = helper.host()
    port = 5679
    server = Thread(name='reworkui.webapp', target=startapp,
                    kwargs={'host': ipaddr, 'port': port, 'dburi': db_uri})
    server.daemon = True
    server.start()

    browser = webbrowser.open(
        'http://{ipaddr}:{port}'.format(ipaddr=ipaddr, port=port)
    )
    if not browser:  # no DISPLAY
        print('You can point your browser to http://{ipaddr}:{port}'.format(
            ipaddr=ipaddr, port=port)
        )
    input()


@click.command(name='init-db')
@click.argument('dburi')
def init_db(dburi):
    "initialize the database schema for rework in its own namespace"
    engine = create_engine(find_dburi(dburi))
    baseschema.init(engine, drop=True)
    schema.init(engine)


@click.command(name='generate-tasks-table')
@click.argument('dburi')
@click.option('--loop', is_flag=True, default=False)
@click.option('--period', type=int, default=2)
def generate_tasks_table(dburi, loop=False, period=2):
    """fill (periodically if needed) the tasks table used by the tasks view"""
    engine = create_engine(find_dburi(dburi))
    taskstable.refresh_tasks_file(engine, loop=loop, sleeptime=period)
