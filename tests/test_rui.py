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
    input.string('name', choices=('Babar', 'Celeste')))
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
         [
             {
                 'choices': None, 'name':
                 'babar.xlsx',
                 'required': False,
                 'type': 'file'
             },
             {
                 'choices': ['Babar', 'Celeste'],
                 'name': 'name',
                 'required': False,
                 'type': 'string'
             }
         ]
        ]
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

