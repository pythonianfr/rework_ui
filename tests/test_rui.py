import datetime
import re
from pathlib import Path
import json
import time

from lxml import etree

from rework import api, input
from rework.task import Task
from rework.testutils import workers, scrub


DATADIR = Path(__file__).parent / 'data'

# html editor

def normalize(htmlstr):
    tree = etree.fromstring(htmlstr, parser=etree.HTMLParser())
    return etree.tostring(tree.getroottree(),
                          pretty_print=True, method='html')


def edittag(tag, editor, anstr):
    if isinstance(tag, str):
        tags = [tag]
    else:
        tags = tag
    tree = etree.fromstring(anstr, parser=etree.HTMLParser())
    for tag in tags:
        for elt in tree.xpath('//%s' % tag):
            editor(elt)
    return etree.tostring(tree.getroottree(),
                          pretty_print=True, method='html')


# test tasks

@api.task
def good_job(task):
    task.save_output(b'Well done !', raw=True)


@api.task
def bad_job(task):
    raise Exception('I am a little crasher.')


@api.task
def abortme(task):
    while True:
        time.sleep(1)


@api.task(inputs=(
    input.file('babar.xlsx'),
    input.string('name', choices=('Babar', 'Celeste')),
    input.number('weight'),
    input.file('celeste.xlsx'),
    input.datetime('birthdate'))
)
def with_inputs(task):
    inputs = task.input
    task.save_output(
        f'{len(inputs["babar.xlsx"])}, {inputs["name"]}'
    )


# tests

def test_no_job(client):
    res = client.get('/job_status/babar')
    assert res.status_code == 404
    assert 'NO SUCH JOB' in res.text


def test_services_table(client):
    res = client.get('/services-table-json')

    def transform(thing):
        thing['host'] = scrub(thing['host'])
        thing['path'] = Path(thing['path']).name
        return thing

    assert [transform(x) for x in res.json] == [
        {'domain': 'default',
         'host': '<X>.<X>.<X>.<X>',
         'name': 'abortme',
         'opid': 3,
         'path': 'test_rui.py'},
        {'domain': 'default',
         'host': '<X>.<X>.<X>.<X>',
         'name': 'bad_job',
         'opid': 2,
         'path': 'test_rui.py'},
        {'domain': 'default',
         'host': '<X>.<X>.<X>.<X>',
         'name': 'good_job',
         'opid': 1,
         'path': 'test_rui.py'},
        {'domain': 'default',
         'host': '<X>.<X>.<X>.<X>',
         'name': 'with_inputs',
         'opid': 4,
         'path': 'test_rui.py'}
    ]


def test_bad_request(engine, client):
    # bad hostid
    res = client.put('/schedule-task/good_job?user={}&hostid={}'.format('Babar', 'fancyhost'),
                     upload_files=[('input_file', 'input.xml', b'the file', 'text/xml')]
    )
    assert res.status == '400 BAD REQUEST'
    assert b'No operation was found' in res.body

    # bad operation
    res = client.put('/schedule-task/fake_job?user={}'.format('Babar'),
                     upload_files=[('input_file', 'input.xml', b'the file', 'text/xml')]
    )
    assert res.status == '400 BAD REQUEST'
    assert b'No operation was found' in res.body

    # no input file
    res = client.put('/schedule-task/good_job?user={}'.format('Babar'))
    assert res.status == '200 OK'


def test_with_input(engine, client):
    res = client.get('/launchers-table-json')
    assert res.json == [
        [4,
         'with_inputs',
         'default',
         '10.211.55.3',
         [{'choices': [], 'name': 'babar.xlsx', 'required': False, 'type': 'file'},
          {'choices': ['Babar', 'Celeste'], 'name': 'name', 'required': False, 'type': 'string'},
          {'choices': [], 'name': 'weight', 'required': False, 'type': 'number'},
          {'choices': [], 'name': 'celeste.xlsx', 'required': False, 'type': 'file'},
          {'choices': [], 'name': 'birthdate', 'required': False, 'type': 'datetime'}
         ]]
    ]

    res = client.put(
        '/schedule2/with_inputs?user=Babar',
        {'name': 'Babar', 'weight': '65', 'birthdate': '2020-1-1'},
        upload_files=[
            ('babar.xlsx', 'babar.xlsx', b'babar.xslx contents',
             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'),
            ('celeste.xlsx', 'celeste.xlsx', b'celeste.xlsx contents',
             'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'),
        ]
    )

    tid = int(res.body)
    t = Task.byid(engine, tid)
    assert t.input == {
        'babar.xlsx': b'babar.xslx contents',
        'birthdate': datetime.datetime(2020, 1, 1, 0, 0),
        'celeste.xlsx': b'celeste.xlsx contents',
        'name': 'Babar',
        'weight': 65
    }

    res = client.get('/tasks-table-json')
    def filterthings(thing):
        thing.pop('queued')
        return thing

    assert [filterthings(x) for x in res.json] == [
        {'abort': False,
         'deathinfo': None,
         'domain': 'default',
         'finished': None,
         'input': '',
         'metadata': {'user': 'Babar'},
         'name': 'good_job',
         'operation': 1,
         'started': None,
         'status': 'queued',
         'tid': 1,
         'traceback': None,
         'worker': None},
        {'abort': False,
         'deathinfo': None,
         'domain': 'default',
         'finished': None,
         'input': ("{'babar.xlsx': '<0 kb file>', 'name': 'Babar', "
                   "'weight': '65', 'celeste.xlsx': '<0 kb file>', "
                   "'birthdate': '2020-01-01...'}"),
         'metadata': {'user': 'Babar'},
         'name': 'with_inputs',
         'operation': 4,
         'started': None,
         'status': 'queued',
         'tid': 2,
         'traceback': None,
         'worker': None
        }
    ]


def test_abort(engine, client):
    with workers(engine) as mon:
        res = client.put('/schedule-task/abortme?user=Babar',
                         upload_files=[('input_file', 'input.xml', b'the file', 'text/xml')])
        tid = int(res.body)
        t = Task.byid(engine, tid)
        assert not t.aborted
        res = client.get(f'/abort-task/{tid}')
        mon.preemptive_kill()
        assert t.aborted


def test_relaunch(engine, client):
    with workers(engine) as mon:
        res = client.put('/schedule-task/good_job?user=Babar',
                         upload_files=[('input_file', 'input.xml', b'the file', 'text/xml')])
        tid = int(res.body)
        t = Task.byid(engine, tid)
        t.join()
        res = client.put(f'/relaunch-task/{tid}')
        newtid = int(res.body)
        t2 = Task.byid(engine, newtid)
        t2.join()


def test_task_life_cycle(engine, client, refresh):
    with workers(engine):
        tasks = []
        for user in ('Babar', 'Babar', 'Celeste'):
            res = client.put('/schedule-task/good_job?user={}'.format(user),
                             upload_files=[('input_file', 'input.xml', b'the file', 'text/xml')]
            )
            tid = int(res.body)
            t1 = Task.byid(engine, tid)
            t1.join()

            assert t1.raw_output == b'Well done !'

            res = client.get('/job_results/{}'.format(t1.tid))
            assert res.headers['Content-Type'] == 'application/zip'
            assert res.body == b'Well done !'

            tasks.append(t1)

        res = client.put('/schedule-task/bad_job?user=Celeste',
                         upload_files=[('input_file', 'input.xml', b'the file', 'text/xml')]
        )
        tid = int(res.body)

        t2 = Task.byid(engine, tid)
        t2.join()

        tasks.append(t2)

        res = client.get('/job_results/{}'.format(t2.tid))
        assert res.headers['Content-Type'] == 'text/plain; charset=utf-8'
        assert res.body.startswith(b'Traceback')
        assert 'I am a little crasher.' in res.text

        results = []
        for t in tasks:
            results.append(client.get('/delete-task/{}'.format(t.tid)))

        assert all(res.body == b'true' for res in results)


def test_schedulers(engine, client):
    api.prepare(
        engine,
        'with_inputs'
    )

    api.prepare(
        engine,
        'with_inputs',
        rule='1 2 * * * *'
    )

    api.prepare(
        engine,
        'with_inputs',
        rule='0 9 * * * *',
        inputdata={
            'name': 'Babar',
            'babar.xlsx': b'the excel blob will eat you',
            'weight': 65,
            'birthdate': '1973-5-20 09:00:00'
        }
    )

    res = client.get('/schedulers-table-json')
    assert res.json == [
        [1, 'with_inputs', 'default', '', '* * * * * *', ''],
        [2, 'with_inputs', 'default', '', '1 2 * * * *', ''],
        [3, 'with_inputs', 'default', '', '0 9 * * * *',
         "{'babar.xlsx': '<0 kb file>', 'name': 'Babar', 'weight': '65', 'birthdate': "
         "'1973-05-20...'}"
        ]
    ]
