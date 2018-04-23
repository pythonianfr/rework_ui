from threading import Thread
import socket
import webbrowser

import click
from sqlalchemy import create_engine

from rework_ui.app import startapp
from rework_ui import schema, taskstable


@click.command()
@click.argument('db-uri')
def view(db_uri):
    """monitor and control workers and tasks"""
    ipaddr = socket.gethostbyname(socket.gethostname())
    port = 5679
    server = Thread(name='reworkui.webapp', target=startapp,
                    kwargs={'host': ipaddr, 'port': port, 'dburi': db_uri})
    server.daemon = True
    server.start()

    webbrowser.open('http://{ipaddr}:{port}'.format(ipaddr=ipaddr, port=port))
    input()


@click.command(name='complete-db')
@click.argument('dburi')
def complete_db(dburi):
    """create the db table necessary for handling big tasks table views in the client"""
    engine = create_engine(dburi)
    schema.reset(engine)
    schema.init(engine)


@click.command(name='generate-tasks-table')
@click.argument('dburi')
@click.option('--loop', is_flag=True, default=False)
@click.option('--period', type=int, default=2)
def generate_tasks_table(dburi, loop=False, period=2):
    """fill (periodically if needed) the tasks table used by the tasks view"""

    engine = create_engine(dburi)
    taskstable.refresh_tasks_file(engine, loop=loop, sleeptime=period)
