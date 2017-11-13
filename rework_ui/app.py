from sqlalchemy import create_engine
from flask import Flask

from rework_ui.blueprint import reworkui


def startapp(host, port, dburi):
    engine = create_engine(dburi)
    app = Flask('rework')
    app.register_blueprint(reworkui(engine))
    app.run(host=host, port=port, threaded=True)
