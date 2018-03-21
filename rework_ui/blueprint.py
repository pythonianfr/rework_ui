import io
import json

from flask import (
    abort,
    Blueprint,
    make_response,
    request,
    render_template,
    send_file,
    url_for
)

from sqlalchemy import select

from pml import HTML

from rework import api
from rework.schema import task, worker, operation
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


class sliceargs(argsdict):
    types = {
        'from_log_id': int
    }


def reworkui(engine, serviceactions=None):

    @bp.route('/relaunch-task/<int:tid>', methods=['PUT'])
    def relaunch_task(tid):
        t = Task.byid(engine, tid)
        if t is None:
            return json.dumps(0)

        servicename, hostid, domain = engine.execute(
            select([operation.c.name, operation.c.host, operation.c.domain]
            ).where(operation.c.id == task.c.operation
            ).where(task.c.id == t.tid)
        ).fetchone()

        newtask = api.schedule(engine,
                               servicename,
                               rawinputdata=t.raw_input,
                               domain=domain,
                               hostid=hostid,
                               metadata=t.metadata)
        return json.dumps(newtask.tid)

    @bp.route('/job_input/<jobid>')
    def job_input(jobid):
        job = getjob(engine, jobid)
        if job is None:
            abort(404, 'no such job')

        archive = job.raw_input
        return send_file(io.BytesIO(archive),
                         mimetype='application/octet-stream')


    @bp.route('/job_results/<jobid>')
    def job_results(jobid):
        job = getjob(engine, jobid)
        if job is None:
            abort(404, 'NO SUCH JOB')

        if job.status != 'done':
            return make_response('job still in state: {}'.format(job.status), 204)

        if job.traceback:
            return send_file(io.BytesIO(job.traceback.encode('utf-8')),
                             mimetype='text/plain')

        archive = job.raw_output
        return send_file(io.BytesIO(archive),
                         mimetype='application/zip')

    @bp.route('/job_status/<jobid>')
    def job_status(jobid):
        job = getjob(engine, jobid)

        if job is None:
            abort(404, 'NO SUCH JOB')

        return job.state

    @bp.route('/job_logslice/<jobid>')
    def job_logslice(jobid):
        job = getjob(engine, jobid)

        if job is None:
            abort(404, 'job does not exists')

        args = sliceargs(request.args)
        logs = job.logs(fromid=args.from_log_id)
        return json.dumps([[lid, line] for lid, line in logs])

    @bp.route('/kill_job/<jobid>')
    def kill_job(jobid):
        job = getjob(engine, jobid)
        if job is None:
            abort(404, 'NO SUCH JOB')

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
            job = getjob(engine, jid)
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

    class uiargsdict(argsdict):
        defaults = {
            'domain': 'all'
        }

    @bp.route('/workers-table')
    def list_workers():
        sql = select([worker.c.id, worker.c.host, worker.c.domain, worker.c.pid,
                      worker.c.mem, worker.c.shutdown, worker.c.kill, worker.c.debugport]
        ).order_by(worker.c.id
        ).where(worker.c.running == True)
        domain = uiargsdict(request.args).domain
        if domain != 'all':
            sql = sql.where(worker.c.domain == domain)

        workers = engine.execute(sql).fetchall()

        h = HTML()
        with h.table(klass='table table-sm table-bordered table-striped table-hover') as t:
            with t.thead(klass='thead-inverse') as th:
                with th.tr() as r:
                    r.th('#')
                    r.th('pid@host')
                    r.th('domain')
                    r.th('memory (Mb)')
                    r.th('debug port')
                    r.th('action')
            for wid, host, domain, pid, mem, shutdown, kill, debugport in workers:
                with r.tr() as r:
                    r.th(str(wid), scope='row')
                    r.td('{}@{}'.format(pid, host))
                    r.td(domain)
                    r.td(str(mem))
                    r.td(debugport and str(debugport) or '')
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
        args = uiargsdict(request.args)
        sql = select([operation.c.id, operation.c.host, operation.c.name,
                      operation.c.path, operation.c.domain]
        ).order_by(operation.c.domain, operation.c.name)
        if args.domain != 'all':
            sql = sql.where(operation.c.domain == args.domain)

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
                    if serviceactions:
                        r.th('action')
            for opid, host, name, path, domain in ops.fetchall():
                with t.tr() as r:
                    r.td(str(opid), scope='row')
                    r.td(host)
                    r.td(name)
                    r.td(path)
                    r.td(domain)
                    if not serviceactions:
                        continue
                    with r.td() as t:
                        action = serviceactions.get(name, serviceactions.get('default'))
                        action(t, host, name, domain)

        return str(h)

    @bp.route('/rework')
    def home():
        return render_template('home.html')

    return bp
