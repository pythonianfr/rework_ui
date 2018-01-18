import json

from flask import Blueprint, request, render_template, url_for
from pml import HTML

from rework.schema import task, worker
from rework.task import Task

from rework_ui.helper import argsdict


bp = Blueprint('reworkui', __name__,
               template_folder='rui_templates',
               static_folder='rui_static',
)


def getjob(engine, jobid):
    try:
        return Task.byid(engine, int(jobid))
    except:
        return None


class newjobargs(argsdict):
    defaults = {
        'user': '<unknown>'
    }


class sliceargs(argsdict):
    types = {
        'from_log_id': int
    }


def reworkui(engine):

    @bp.route('/job_status/<jobid>')
    def job_status(jobid):
        job = getjob(engine, jobid)

        if job is None:
            return 'NO SUCH JOB'

        return job.state

    @bp.route('/job_logslice/<jobid>')
    def job_logslice(jobid):
        job = getjob(engine, jobid)

        if job is None:
            return

        args = sliceargs(request.args)
        logs = job.logs(fromid=args.from_log_id)
        return json.dumps([[lid, line] for lid, line in logs])

    @bp.route('/kill_job/<jobid>')
    def kill_job(jobid):
        job = getjob(engine, jobid)
        if job is None:
            return 'no such job'

        if job.aborted:
            return 'was already aborted'

        job.abort()

        return 'job terminated'

    @bp.route('/list_jobs')
    def list_jobs():
        with engine.connect() as cn:
            tsql = 'select id from rework.task order by id'
            jobids = cn.execute(tsql).fetchall()
            opsql = 'select id, name from rework.operation'
            ops = dict(cn.execute(opsql).fetchall())

        output = []
        for jid, in jobids:
            job = getjob(jid)
            stat = job.status
            if stat == 'done':
                if job.traceback:
                    stat = 'failed'
                elif job.aborted:
                    stat = 'aborted'
            output.append((jid, ops[job.operation], stat))

        return json.dumps(output)

    @bp.route('/shutdown-worker/<wid>')
    def shutdown_worker(wid):
        with engine.connect() as cn:
            cn.execute(worker.update().where(worker.c.id == wid
            ).values(shutdown=True))
        return json.dumps(True)

    @bp.route('/kill-worker/<wid>')
    def kill_worker(wid):
        with engine.connect() as cn:
            cn.execute(worker.update().where(worker.c.id == wid
            ).values(kill=True))
        return json.dumps(True)

    @bp.route('/workers-table')
    def list_workers():
        workers = engine.execute('select id, host, pid, mem, shutdown, kill, domain from rework.worker '
                                 'where running = true '
                                 'order by id'
        ).fetchall()

        h = HTML()
        with h.table(klass='table table-sm table-bordered table-striped table-hover') as t:
            with t.thead(klass='thead-inverse') as th:
                with th.tr() as r:
                    r.th('#')
                    r.th('pid@host')
                    r.th('domain')
                    r.th('memory (Mb)')
                    r.th('action')
            for wid, host, pid, mem, shutdown, kill, domain in workers:
                with r.tr() as r:
                    r.th(str(wid), scope='row')
                    r.td('{}@{}'.format(pid, host))
                    r.td(domain)
                    r.td(str(mem))
                    with r.td() as col:
                        with col.button() as b:
                            if shutdown:
                                b('shutdown asked', klass='btn glyphicon glyphicon-ban-circle')
                            else:
                                b('shutdown', type='button', klass='btn btn-warning btn-sm',
                                  onclick='shutdown_worker({})'.format(wid))
                        col.span(' ')
                        with col.button() as b:
                            if kill:
                                b('kill asked', klass='btn glyphicon glyphicon-ban-circle')
                            else:
                                b('kill', type='button', klass='btn btn-danger btn-sm',
                                  onclick='kill_worker({})'.format(wid))

        return str(h)

    @bp.route('/delete-task/<tid>')
    def delete_task(tid):
        with engine.connect() as cn:
            cn.execute("delete from rework.task where id = %(tid)s and status != 'running'",
                       tid=tid)
        return json.dumps(True)

    @bp.route('/abort-task/<tid>')
    def abort_task(tid):
        with engine.connect() as cn:
            sql = task.update().where(task.c.id == tid
            ).values(abort=True)
            cn.execute(sql)
        return json.dumps(True)

    @bp.route('/tasks-table')
    def list_tasks():
        tids = engine.execute('select id from rework.task order by id desc').fetchall()
        opsql = 'select id, name from rework.operation'
        ops = dict(engine.execute(opsql).fetchall())

        h = HTML()
        with h.table(klass='table table-sm table-bordered table-striped table-hover') as t:
            with t.thead(klass='thead-inverse') as th:
                with th.tr() as r:
                    r.th('#')
                    r.th('service')
                    r.th('created')
                    r.th('user')
                    r.th('worker')
                    r.th('status')
                    r.th('action')
            for tid, in tids:
                task = Task.byid(engine, tid)
                if task is None:
                    continue  # avoid a `delete` + refresh tasks race condition
                with t.tr() as r:
                    r.th(str(task.tid), scope='row')

                    with r.td() as col:
                        col.a(ops[task.operation],
                              title='show the tasks log (if any)',
                              target='_blank',
                              href='tasklogs/{}'.format(tid))

                    r.td(task._propvalue('created').strftime('%Y-%m-%d %H:%M:%S'))
                    r.td(task.metadata.get('user', '<unknown>'))

                    worker = task._propvalue('worker')
                    r.td('#{}'.format(worker or ''))

                    state = task.state
                    stateattrs = {'klass': state}
                    if state == 'failed':
                        stateattrs['title'] = task.traceback
                    r.td(state, **stateattrs)

                    with r.td() as col:
                        state = task.state
                        with col.button() as b:
                            if state == 'running':
                                b('abort', type='button', klass='btn btn-danger btn-sm',
                                  onclick='abort_task({})'.format(task.tid))
                            elif state == 'aborting':
                                b('wait', klass='btn glyphicon glyphicon-ban-circle')
                            else:
                                b('delete', type='button', klass='btn btn-warning btn-sm',
                                  onclick='delete_task({})'.format(task.tid))
                        col.span(' ')

        return str(h)

    @bp.route('/tasklogs/<int:taskid>')
    def tasklogs(taskid):
        return render_template(
            'tasklogs.html',
            tid=taskid,
            logsliceuri=url_for('reworkui.job_logslice', jobid=taskid)
        )

    @bp.route('/services-table')
    def list_services():
        sql = 'select id, host, name, path, domain from rework.operation order by id'
        ops = engine.execute(sql)

        h = HTML()
        with h.table(klass='table table-sm table-bordered table-striped table-hover') as t:
            with t.thead(klass='thead-inverse') as th:
                with th.tr() as r:
                    r.th('#')
                    r.th('host')
                    r.th('name')
                    r.th('path')
                    r.th('domain')
            for opid, host, name, path, domain in ops.fetchall():
                with t.tr() as r:
                    r.td(str(opid), scope='row')
                    r.td(host)
                    r.td(name)
                    r.td(path)
                    r.td(domain)

        return str(h)

    @bp.route('/rework')
    def home():
        return render_template('home.html')

    return bp
