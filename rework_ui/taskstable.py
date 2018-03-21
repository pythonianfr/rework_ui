from hashlib import md5
from time import sleep

from pkg_resources import iter_entry_points

from pml import HTML
from sqlalchemy import select, desc

from rework.task import Task
from rework.schema import task, operation

from rework_ui.schema import taskstable


def latest_table_hash(engine, domain):
    sql = select(
        [taskstable.c.hash]
    ).order_by(
        desc(taskstable.c.id)
    ).limit(
        1
    ).where(
        taskstable.c.domain == domain
    )
    return engine.execute(sql).scalar()


def refresh_tasks(engine, inithash, domain):
    taskstates = tasks_info(engine, domain)
    thash = md5(str(taskstates).encode('ascii')).hexdigest()
    if thash != inithash:
        htmltable = generate_tasks_table(engine, taskstates)
        sql = taskstable.insert().values(
            hash=thash,
            domain=domain,
            content=htmltable
        )
        with engine.connect() as cn:
            cn.execute(sql)
        inithash = thash
        # cleanup old tables
        sql = taskstable.delete().where(
            taskstable.c.hash != thash
        ).where(
            taskstable.c.domain == domain
        )
        with engine.connect() as cn:
            cn.execute(sql)
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
    with engine.connect() as cn:
        sql = select(
            [task.c.id, task.c.status, operation.c.domain]
        ).order_by(desc(task.c.id)
        ).where(task.c.operation == operation.c.id)
        if domain != 'all':
            sql = sql.where(
                operation.c.domain == domain
            )
        return cn.execute(sql).fetchall()


MORE_TASKS_ACTIONS = set()
def add_plugin_actions():
    for ep in iter_entry_points('tasks_actions'):
        MORE_TASKS_ACTIONS.add(ep.load())

add_plugin_actions()


def generate_tasks_table(engine, taskstates):
    opsql = 'select id, name from rework.operation'
    ops = dict(engine.execute(opsql).fetchall())

    h = HTML()
    with h.table(klass='table table-sm table-bordered table-striped table-hover') as t:
        with t.thead(klass='thead-inverse') as th:
            with th.tr() as r:
                r.th('#')
                r.th('service')
                r.th('domain')
                r.th('created')
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
                r.td(job._propvalue('created').strftime('%Y-%m-%d %H:%M:%S'))

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
