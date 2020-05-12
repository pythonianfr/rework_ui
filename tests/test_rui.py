import re
from pathlib import Path
import time

from lxml import etree

from rework import api
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

    # bad operation
    res = client.put('/schedule-task/good_job?user={}'.format('Babar'))
    assert res.status == '400 BAD REQUEST'
    assert b'input file is mandatory' in res.body


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

        res = client.get('/services-table')

        ipaddr = re.compile('^\d{1,3}.\d{1,3}.\d{1,3}.\d{1,3}$')
        def edit(elt):
            if elt.text:
                if 'test_rui' in elt.text:
                    elt.text = Path(elt.text).name
                elif ipaddr.match(elt.text):
                    elt.text = 'HOSTNAME'
            if 'value' in elt.attrib and ipaddr.match(elt.attrib['value']):
                elt.attrib['value'] = 'HOSTNAME'
            return elt

        html = edittag(('td', 'input'), edit, res.text)
        refpath = DATADIR / 'services.html'
        if refresh:
            refpath.write_bytes(html)
        assert html == refpath.read_bytes()


def test_monitors_table(engine, client, refresh):
    with engine.begin() as cn:
        cn.execute('delete from rework.monitor')
        cn.execute('delete from rework.worker')


    with workers(engine):
        res = client.get('/workers-table')
        html = normalize(scrub(res.text))
        refpath = DATADIR / 'monitors-table.html'
        if refresh:
            refpath.write_bytes(html)
        assert html == refpath.read_bytes()

        t = api.schedule(engine, 'abortme')
        t.join('running')
        res = client.get('/workers-table')
        html = normalize(scrub(res.text))
        refpath = DATADIR / 'monitors-table-1-task.html'
        if refresh:
            refpath.write_bytes(html)
        assert html == refpath.read_bytes()
