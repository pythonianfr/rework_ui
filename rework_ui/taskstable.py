import threading
import logging
from hashlib import md5
from time import sleep

from pkg_resources import iter_entry_points
import tzlocal

from pml import HTML
from sqlhelp import select, insert
from rework.task import Task

TZ = tzlocal.get_localzone()


def latest_table_hash(engine, domain):
    q = select(
        'hash'
    ).table('rework.taskstable'
    ).order('hash'
    ).limit(1
    ).where(domain=domain)
    return q.do(engine).scalar()


def refresh_tasks(engine, inithash, domain):
    taskstates = tasks_info(engine, domain)
    thash = md5(str(taskstates).encode('ascii')).hexdigest()
    if thash != inithash:
        htmltable = generate_tasks_table(engine, taskstates)
        q = insert('rework.taskstable').values(
            hash=thash,
            domain=domain,
            content=htmltable
        )
        with engine.begin() as cn:
            q.do(cn)
        inithash = thash
        # cleanup old tables
        sql = ('delete from rework.taskstable '
               'where hash != %(hash)s '
               'and   domain = %(domain)s')
        with engine.begin() as cn:
            cn.execute(sql, hash=thash, domain=domain)
    return inithash


def refresh_tasks_file(engine, loop=False, sleeptime=2):
    domains = [dom for dom, in engine.execute(
        'select domain from rework.operation group by domain order by domain'
    ).fetchall()]
    if len(domains) > 1:
        domains.insert(0, 'all')

    inithashes = {domain: latest_table_hash(engine, domain)
                 for domain in domains}
    inithashes = {domain: refresh_tasks(engine, inithashes[domain], domain)
                  for domain in domains}

    if loop:
        print('Looping. Type Ctrl-C to stop.')
    while loop:
        newhashes = {domain: refresh_tasks(engine, inithashes[domain], domain)
                     for domain in domains}
        if newhashes != inithashes:
            print('tasks set changed')
            inithashes = newhashes
        else:
            print('nothing changed')
        sleep(sleeptime)


def tasks_info(engine, domain):
    with engine.begin() as cn:
        q = select(
            't.id', 't.status', 'op.domain'
        ).table('rework.task as t'
        ).join('rework.operation as op on (op.id = t.operation)'
        ).order('t.id', 'desc')
        if domain != 'all':
            q.where(domain=domain)
        return q.do(cn).fetchall()


MORE_TASKS_ACTIONS = set()
def add_plugin_actions():
    for ep in iter_entry_points('tasks_actions'):
        MORE_TASKS_ACTIONS.add(ep.load())

add_plugin_actions()


def threadpool(maxthreads):
    L = logging.getLogger('parallel')

    def run(func, argslist):
        count = 0
        threads = []
        L.debug('// run %s %s', func.__name__, len(argslist))

        # initial threads
        for count, args in enumerate(argslist, start=1):
            th = threading.Thread(target=func, args=args)
            threads.append(th)
            L.debug('// start thread %s', th.name)
            th.daemon = True
            th.start()
            if count == maxthreads:
                break

        while threads:
            for th in threads[:]:
                th.join(1. / maxthreads)
                if not th.is_alive():
                    threads.remove(th)
                    L.debug('// thread %s exited, %s remaining', th.name, len(threads))
                    if count < len(argslist):
                        newth = threading.Thread(target=func, args=argslist[count])
                        threads.append(newth)
                        L.debug('// thread %s started', newth.name)
                        newth.daemon = True
                        newth.start()
                        count += 1

    return run


def generate_tasks_table(engine, taskstates):
    opsql = 'select id, name from rework.operation'
    ops = dict(engine.execute(opsql).fetchall())

    # build all data from the db first
    rows = []
    def buildrow(id, domain, status):
        job = Task.byid(engine, id)
        if job is None:  # deleted
            return
        try:
            tid = job.tid
            operation = job.operation
            traceback = job.traceback
            queued = job._propvalue('queued')
            started = job._propvalue('started')
            finished = job._propvalue('finished')
            meta = job.metadata
            worker = job._propvalue('worker')
            state = job.state
            deathinfo = job.deathinfo
        except Exception as e:
            print(e)
            return
        rows.append(
            (tid, domain, status, operation, traceback,
             queued, started, finished,
             meta, worker, state, deathinfo)
        )

    poolrun = threadpool(24)
    poolrun(
        buildrow,
        [
            (row.id, row.domain, row.status)
            for row in taskstates
        ]
    )
    rows.sort(reverse=True)

    h = HTML()
    h.br()
    with h.table(klass='table table-sm table-bordered table-striped table-hover') as t:
        with t.thead(klass='thead-inverse') as th:
            with th.tr() as r:
                r.th('#')
                r.th('service')
                r.th('domain')
                r.th('queued')
                r.th('started')
                r.th('finished')
                r.th('user')
                r.th('worker')
                r.th('status')
                r.th('action')
        for (tid, domain, status, operation, traceback,
             queued, started, finished, meta, worker,
             state, deathinfo) in rows:

            with t.tr() as r:
                r.th(str(tid), scope='row')

                with r.td() as col:
                    with col.span() as sp:
                        sp.a(ops[operation],
                             title='show the tasks log (if any)',
                             target='_blank',
                             href='tasklogs/{}'.format(tid))
                    if traceback:
                        with col.span() as sp:
                            sp(' ')
                            sp.a('[traceback]',
                                 title='show the error',
                                 target='_blank',
                                 href='taskerror/{}'.format(tid))

                r.td(domain)
                r.td(queued.astimezone(TZ).strftime('%Y-%m-%d %H:%M:%S%z'))
                if started is None:
                    r.td('')
                else:
                    r.td(started.astimezone(TZ).strftime('%Y-%m-%d %H:%M:%S%z'))
                if finished is None:
                    r.td('')
                else:
                    r.td(finished.astimezone(TZ).strftime('%Y-%m-%d %H:%M:%S%z'))

                # user plus maybe run name
                user = meta.get('user', '<unknown>')
                run_name = meta.get('options', {}).get('run_name', None)
                if run_name:
                    user = '{} [{}]'.format(user, run_name)
                r.td(user)

                r.td('#{}'.format(worker or ''))

                stateattrs = {'klass': state}
                if state == 'failed':
                    stateattrs['title'] = traceback
                else:
                    stateattrs['title'] = deathinfo or ''
                r.td(state, **stateattrs)

                with r.td() as col:
                    with col.button() as b:
                        if state == 'running':
                            b('abort', type='button', klass='btn btn-danger btn-sm',
                              onclick='abort_task({})'.format(tid))
                        elif state == 'aborting':
                            b('wait', klass='btn glyphicon glyphicon-ban-circle')
                        else:
                            b('delete', type='button', klass='btn btn-warning btn-sm',
                              onclick='delete_task({})'.format(tid))
                    if status == 'done':
                        col.span(' ')
                        with col.button() as b:
                            b('relaunch', type='button', klass='btn btn-primary btn-sm',
                              onclick='relaunch_task({})'.format(tid))
                    for action in MORE_TASKS_ACTIONS:
                        action(col, tid, operation, state, ops)

    return str(h)
