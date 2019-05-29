from hashlib import md5
from time import sleep

from pkg_resources import iter_entry_points

from pml import HTML
from sqlhelp import select, insert
from rework.task import Task


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


def generate_tasks_table(engine, taskstates):
    opsql = 'select id, name from rework.operation'
    ops = dict(engine.execute(opsql).fetchall())

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
        for row in taskstates:
            job = Task.byid(engine, row.id)
            with t.tr() as r:
                r.th(str(job.tid), scope='row')

                with r.td() as col:
                    with col.span() as sp:
                        sp.a(ops[job.operation],
                             title='show the tasks log (if any)',
                             target='_blank',
                             href='tasklogs/{}'.format(row.id))
                    if job.traceback:
                        with col.span() as sp:
                            sp(' ')
                            sp.a('[traceback]',
                                 title='show the error',
                                 target='_blank',
                                 href='taskerror/{}'.format(row.id))

                r.td(row.domain)
                r.td(job._propvalue('queued').strftime('%Y-%m-%d %H:%M:%S'))
                started = job._propvalue('started')
                if started is None:
                    r.td('')
                else:
                    r.td(started.strftime('%Y-%m-%d %H:%M:%S'))
                finished = job._propvalue('finished')
                if finished is None:
                    r.td('')
                else:
                    r.td(finished.strftime('%Y-%m-%d %H:%M:%S'))

                # user plus maybe run name
                meta = job.metadata
                user = meta.get('user', '<unknown>')
                run_name = meta.get('options', {}).get('run_name', None)
                if run_name:
                    user = '{} [{}]'.format(user, run_name)
                r.td(user)

                worker = job._propvalue('worker')
                r.td('#{}'.format(worker or ''))

                state = job.state
                stateattrs = {'klass': state}
                if state == 'failed':
                    stateattrs['title'] = job.traceback
                r.td(state, **stateattrs)

                with r.td() as col:
                    with col.button() as b:
                        if state == 'running':
                            b('abort', type='button', klass='btn btn-danger btn-sm',
                              onclick='abort_task({})'.format(job.tid))
                        elif state == 'aborting':
                            b('wait', klass='btn glyphicon glyphicon-ban-circle')
                        else:
                            b('delete', type='button', klass='btn btn-warning btn-sm',
                              onclick='delete_task({})'.format(job.tid))
                    if row.status == 'done':
                        col.span(' ')
                        with col.button() as b:
                            b('relaunch', type='button', klass='btn btn-primary btn-sm',
                              onclick='relaunch_task({})'.format(job.tid))
                    for action in MORE_TASKS_ACTIONS:
                        action(col, job, state, ops)

    return str(h)
