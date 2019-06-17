import io
import base64
import json

import tzlocal
from flask import (
    abort,
    Blueprint,
    make_response,
    request,
    render_template,
    render_template_string,
    send_file,
    url_for
)

from pml import HTML
from pygments import highlight
from pygments.lexers import PythonTracebackLexer
from pygments.formatters import HtmlFormatter

from sqlhelp import select, update
from rework import api
from rework.helper import utcnow
from rework.task import Task

from rework_ui.helper import argsdict
from rework_ui import taskstable


bp = Blueprint('reworkui', __name__,
               template_folder='rui_templates',
               static_folder='rui_static',
)
TZ = tzlocal.get_localzone()


def getjob(engine, jobid):
    try:
        return Task.byid(engine, int(jobid))
    except:
        return None


class sliceargs(argsdict):
    types = {
        'from_log_id': int
    }


def _schedule_job(engine,
                  service,
                  args,
                  inputfile):
    user = args.user
    if user is None:
        abort(400, 'user parameter is mandatory')

    hostid = args.hostid or api.host()
    domain = args.domain
    metadata = {'user': user}

    if args.options:
        metadata['options'] = args.options

    try:
        task = api.schedule(engine, service,
                            rawinputdata=inputfile,
                            hostid=hostid,
                            domain=domain,
                            metadata=metadata)
    except Exception as err:
        abort(400, str(err))
    return json.dumps(task.tid)


def alldomains(engine):
    return [
        dom for dom, in engine.execute(
            'select domain from rework.operation group by domain order by domain'
        ).fetchall()
    ]


def initialdomain(domains):
    return 'all' if len(domains) > 1 else domains and domains[0] or 'default'


def reworkui(engine,
             serviceactions=None,
             alttemplate=None):

    @bp.route('/schedule-task/<service>', methods=['PUT'])
    @bp.route('/new_job/<service>', methods=['PUT'])  # bw compat
    def schedule_task(service):
        args = argsdict()
        args.update(argsdict(request.form))
        args.update(argsdict(request.args))
        fileargs = argsdict(request.files)

        if 'input_file' not in fileargs:
            abort(400, 'input file is mandatory')

        return _schedule_job(engine,
                             service,
                             args,
                             fileargs.input_file.read())

    @bp.route('/relaunch-task/<int:tid>', methods=['PUT'])
    def relaunch_task(tid):
        t = Task.byid(engine, tid)
        if t is None:
            return json.dumps(0)

        op = select(
            'name', 'host', 'domain'
        ).table(
            'rework.operation'
        ).join(
            'rework.task as task on (task.operation = operation.id)'
        ).where(
            'task.id = %(tid)s', tid=t.tid
        ).do(engine).fetchone()

        newtask = api.schedule(engine,
                               op.name,
                               rawinputdata=t.raw_input,
                               domain=op.domain,
                               hostid=op.host,
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

    @bp.route('/list_jobs')
    def list_jobs():
        with engine.begin() as cn:
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
        with engine.begin() as cn:
            update('rework.worker').where(id=wid).values(
                shutdown=True
            ).do(cn)
        return json.dumps(True)

    @bp.route('/kill-worker/<wid>')
    def kill_worker(wid):
        with engine.begin() as cn:
            update('rework.worker').where(id=wid).values(
                kill=True
            ).do(cn)
        return json.dumps(True)

    class uiargsdict(argsdict):
        defaults = {
            'domain': initialdomain(alldomains(engine))
        }

    @bp.route('/workers-table')
    def list_workers():
        # workers
        q = select(
            'id', 'host', 'domain', 'pid', 'mem', 'cpu',
            'shutdown', 'kill', 'debugport', 'started'
        ).table('rework.worker'
        ).where('running = true'
        ).order('id')

        domain = uiargsdict(request.args).domain
        if domain != 'all':
            q.where(domain=domain)

        workers = q.do(engine).fetchall()

        # monitors
        q = select(
            'id', 'domain', 'lastseen', 'options'
        ).table('rework.monitor')
        if domain != 'all':
            q.where(domain=domain)

        monitors = {
            row.domain: row
            for row in q.do(engine).fetchall()
        }
        now = utcnow().astimezone(TZ)

        h = HTML()
        h.br()
        with h.table(klass='table table-sm table-bordered table-striped table-hover') as t:
            with t.thead(klass='thead-inverse') as th:
                with th.tr() as r:
                    r.th('#')
                    r.th('domain')
                    r.th('seen last')
                    r.th('options')
            for domain, row in sorted(monitors.items()):
                with t.tr() as r:
                    r.td(str(row.id))
                    r.td(row.domain)

                    delta = (now - row.lastseen).total_seconds()
                    color = 'DarkGreen'
                    if delta > 60:
                        color = 'DarkRed'
                    elif delta > 10:
                        color = 'DarkMagenta'

                    r.td(row.lastseen.astimezone(TZ).strftime('%Y-%m-%d %H:%M:%S%z'),
                         style='color: {}'.format(color))
                    r.td(', '.join('{}={}'.format(k, v) for k, v in sorted(row.options.items())))

        with h.table(klass='table table-sm table-bordered table-striped table-hover') as t:
            with t.thead(klass='thead-inverse') as th:
                with th.tr() as r:
                    r.th('#')
                    r.th('pid@host')
                    r.th('domain')
                    r.th('memory (Mb)')
                    r.th('cpu')
                    r.th('debug port')
                    r.th('started')
                    r.th('action')
            for wid, host, domain, pid, mem, cpu, shutdown, kill, debugport, started in workers:
                with t.tr() as r:
                    r.th(str(wid), scope='row')
                    r.td('{}@{}'.format(pid, host))
                    r.td(domain)
                    r.td(str(mem))
                    r.td(str(cpu / 100.))
                    r.td(debugport and str(debugport) or '')
                    if started:
                        started = started.astimezone(TZ).strftime('%Y-%m-%d %H:%M:%S%z')
                    r.td(started or '')
                    with r.td() as col:
                        with col.button() as b:
                            if shutdown:
                                b('shutdown asked', klass='btn gltyphicon glyphicon-ban-circle')
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
        with engine.begin() as cn:
            cn.execute("delete from rework.task where id = %(tid)s and status != 'running'",
                       tid=tid)
        return json.dumps(True)

    @bp.route('/abort-task/<tid>')
    def abort_task(tid):
        t = Task.byid(engine, tid)
        if t is None:
            abort(404, 'NO SUCH JOB')

        if t.aborted:
            return json.dumps(False)

        t.abort()
        return json.dumps(True)

    @bp.route('/taskerror/<int:taskid>')
    def taskerror(taskid):
        job = getjob(engine, taskid)
        if job is None:
            abort(404, 'job does not exists')

        formatter = HtmlFormatter()
        traceback = highlight(job.traceback,
                              PythonTracebackLexer(),
                              formatter)
        return render_template(
            'taskerror.html',
            tid=taskid,
            css=formatter.get_style_defs(),
            traceback=traceback
        )

    @bp.route('/tasks-table-hash')
    def tasks_table_hash():
        args = uiargsdict(request.args)
        thash = taskstable.latest_table_hash(engine, args.domain)
        return thash or 'no-hash-yet'

    @bp.route('/tasks-table')
    def list_tasks():
        args = uiargsdict(request.args)
        content = engine.execute('select content from rework.taskstable '
                                 'where domain = %(domain)s '
                                 'order by id desc limit 1',
                                 domain=args.domain).scalar()
        if content is None:
            return '<p>Table under construction ...</p>'

        return content

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
        q = select(
            'id', 'host', 'name', 'path', 'domain'
        ).table('rework.operation'
        ).order('domain, name')
        if args.domain != 'all':
            q.where(domain=args.domain)

        ops = q.do(engine)
        h = HTML()
        h.br()
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

    @bp.route('/')
    def home():
        domains = alldomains(engine)
        if not len(domains):
            return 'No operation registered: nothing to see here'

        h = HTML()
        firstdomain = initialdomain(domains)
        with h.select(id='domain-filter', name='domain-filter',
                      title='domain',
                      onchange='setdomain(this)')as s:
            if len(domains) > 1:
                s.option('all', value='all', selected='selected')
                for domain in domains:
                    s.option(domain, value=domain)
            else:
                s.option(domains[0], value=domains[0], selected='selected')

        if alttemplate:
            return render_template_string(alttemplate,
                                          domain_filter=str(h),
                                          initialdomain=firstdomain)

        return render_template('rui_home.html',
                               domain_filter=str(h),
                               initialdomain=firstdomain)

    return bp
