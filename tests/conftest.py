from pathlib import Path

import pytest
from sqlalchemy import create_engine
from pytest_sa_pg import db
import webtest

from rework import schema as reworkschema
from rework import api
from rework_ui import schema as ruischema, app


DATADIR = Path(__file__).parent / 'data'
PORT = 2346


@pytest.fixture(scope='session')
def engine(request):
    db.setup_local_pg_cluster(request, DATADIR, PORT)
    uri = 'postgresql://localhost:{}/postgres'.format(PORT)
    e = create_engine(uri)
    reworkschema.reset(e)
    reworkschema.init(e)
    ruischema.init(e)
    api.freeze_operations(e)
    return e


# Error-displaying web tester

class WebTester(webtest.TestApp):

    def _check_status(self, status, res):
        try:
            super(WebTester, self)._check_status(status, res)
        except:
            print(res.errors)
            # raise <- default behaviour on 4xx is silly


@pytest.fixture(scope='session')
def client(engine):
    yield WebTester(app.make_app(engine))


def pytest_addoption(parser):
    parser.addoption('--refresh-refs', action='store_true', default=False,
                     help='refresh reference outputs')


@pytest.fixture
def refresh(request):
    return request.config.getoption('--refresh-refs')
