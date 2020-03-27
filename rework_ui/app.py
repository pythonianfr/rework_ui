from sqlalchemy import create_engine
from flask import Flask

from rework_ui.blueprint import reworkui


def make_app(engine, prefix=None):
    app = Flask('rework')
    app.register_blueprint(
        reworkui(engine),
        url_prefix=prefix
    )
    return app


def startapp(host, port, dburi):
    engine = create_engine(dburi)
    app = make_app(engine)
    app.run(host=host, port=port, threaded=True)
