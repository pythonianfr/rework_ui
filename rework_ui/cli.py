from threading import Thread
import socket
import webbrowser

import click

from rework_ui.app import startapp


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

    webbrowser.open('http://{ipaddr}:{port}/rework'.format(ipaddr=ipaddr, port=port))
    input()
