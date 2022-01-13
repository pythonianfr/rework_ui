import io
import base64
import json
import pickle
import mimetypes

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

from pygments import highlight
from pygments.lexers import PythonTracebackLexer
from pygments.formatters import HtmlFormatter

from sqlhelp import select, update
from rework import api

from rework.helper import (
    BetterCronTrigger,
    convert_io,
    filterio,
    iospec,
    unpack_io,
    unpack_iofiles_length,
    unpack_iofile,
    utcnow
)
from rework.task import (
    _task_state,
    Task
)

from rework_ui.helper import argsdict


TZ = tzlocal.get_localzone()


def homeurl():
    homeurl = url_for('reworkui.home')
    baseurl = homeurl[:homeurl.rindex('/')]
    if len(baseurl):
        return baseurl
    return baseurl


def getjob(engine, jobid):
    try:
        return Task.byid(engine, int(jobid))
    except:
        return None


def task_formatinput(spec, input):
    if input is None:
        return ''
    if spec is None:
        inp = pickle.loads(input)
    else:
        inp = unpack_io(spec, input)

    return format_input(inp)


def cut(thing, limit):
    if len(thing) > limit:
        return thing[:limit] + '…'
    return thing


def format_input(inp):
    if isinstance(inp, dict):
        newinp = {}
        for key, val in inp.items():
            if isinstance(val, bytes):
                val = f'<{round(len(val)/1024,2)} kb file>'
            else:
                try:
                    val = cut(val, 25)
                except:
                    val = cut(str(val), 25)
            newinp[key] = val
        inp = newinp

    return str(inp)


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


def maybetz(dt):
    if dt is None:
        return None
    return dt.astimezone(TZ).strftime('%Y-%m-%d %H:%M:%S%z')


def none_as_empty_str(alist):
    return [
        elt if elt is not None else ""
        for elt in alist
    ]


def reworkui(engine,
             serviceactions=None,
             alttemplate=None,
             has_permission=lambda perm: True):

    bp = Blueprint(
        'reworkui',
        __name__,
        template_folder='rui_templates',
        static_folder='rui_static',
    )

    @bp.route('/schedule-task/<service>', methods=['PUT'])
    def schedule_task(service):
        if not has_permission('schedule'):
            return json.dumps(-1)

        args = argsdict()
        args.update(argsdict(request.form))
        args.update(argsdict(request.args))
        fileargs = argsdict(request.files)

        if fileargs.input_file is None:
            inputfile = None
        else:
            inputfile = fileargs.input_file.read()

        return _schedule_job(engine,
                             service,
                             args,
                             inputfile)

    @bp.route('/schedule2/<service>', methods=['PUT'])
    def schedule2(service):
        args = {
            k: v.read()
            for k, v in argsdict(request.files).items()
            if v
        }
        args.update(
            argsdict(request.form)
        )

        hostid = args.pop('host', None)
        domain = args.pop('domain', None)
        meta = argsdict(request.args)

        specs = iospec(engine)
        spec = filterio(specs, service, domain, hostid)
        typed_args = convert_io(spec, args)

        try:
            task = api.schedule(
                engine,
                service,
                typed_args,
                hostid=hostid,
                domain=domain,
                metadata=meta
            )
        except Exception as err:
            abort(400, str(err))
        return json.dumps(task.tid)

    @bp.route('/relaunch-task/<int:tid>', methods=['PUT'])
    def relaunch_task(tid):
        if not has_permission('relaunch'):
            return json.dumps(0)

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

        newtask = api.schedule(
            engine,
            op.name,
            rawinputdata=t.raw_input,
            domain=op.domain,
            hostid=op.host,
            metadata=t.metadata
        )
        return json.dumps(newtask.tid)

    @bp.route('/job_input/<jobid>')
    def job_input(jobid):
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        job = getjob(engine, jobid)
        if job is None:
            abort(404, 'no such job')

        archive = job.raw_input
        return send_file(
            io.BytesIO(archive),
            mimetype='application/octet-stream'
        )

    @bp.route('/job_results/<jobid>')
    def job_results(jobid):
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        job = getjob(engine, jobid)
        if job is None:
            abort(404, 'NO SUCH JOB')

        if job.status != 'done':
            return make_response(
                'job still in state: {}'.format(job.status), 204
            )

        if job.traceback:
            return send_file(
                io.BytesIO(job.traceback.encode('utf-8')),
                mimetype='text/plain'
            )

        archive = job.raw_output
        return send_file(
            io.BytesIO(archive),
            mimetype='application/octet-stream'
        )

    @bp.route('/job_status/<jobid>')
    def job_status(jobid):
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        job = getjob(engine, jobid)

        if job is None:
            abort(404, 'NO SUCH JOB')

        return job.state

    @bp.route('/job_logslice/<jobid>')
    def job_logslice(jobid):
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        job = getjob(engine, jobid)

        if job is None:
            abort(404, 'job does not exists')

        args = sliceargs(request.args)
        logs = job.logs(fromid=args.from_log_id)
        return json.dumps([
            [lid, line] for lid, line in logs
        ])

    @bp.route('/list_jobs')
    def list_jobs():
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

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
            output.append(
                (jid, ops[job.operation], stat)
            )

        return json.dumps(output)

    @bp.route('/shutdown-worker/<wid>')
    def shutdown_worker(wid):
        if not has_permission('shutdown'):
            abort(403, 'You cannoy do that.')

        with engine.begin() as cn:
            update('rework.worker').where(id=wid).values(
                shutdown=True
            ).do(cn)
        return json.dumps(True)

    @bp.route('/kill-worker/<wid>')
    def kill_worker(wid):
        if not has_permission('kill'):
            abort(403, 'You cannoy do that.')

        with engine.begin() as cn:
            update('rework.worker').where(id=wid).values(
                kill=True
            ).do(cn)
        return json.dumps(True)

    class uiargsdict(argsdict):
        defaults = {
            'domain': initialdomain(alldomains(engine))
        }

    @bp.route('/workers-table-json')
    def list_workers_json():
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

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
        domains_list = []
        for domain, row in sorted(monitors.items()):
            domains_list.append({
                'id': row.id,
                'domain': row.domain,
                'delta': (now - row.lastseen).total_seconds(),
                'lastseen': row.lastseen.astimezone(TZ).strftime('%Y-%m-%d %H:%M:%S%z'),
                'options': sorted(row.options.items())
            })

        workers_list = []
        for wid, host, domain, pid, mem, cpu, shutdown, kill, debugport, started in workers:
            if started:
                started = started.astimezone(TZ).strftime(
                    '%Y-%m-%d %H:%M:%S%z'
                )
            workers_list.append({
                'wid': wid,
                'host':host,
                'pid': pid,
                'domain': domain,
                'mem': mem,
                'cpu': cpu,
                'debugport': debugport,
                'started': started,
                'button': {
                    'kill': kill,
                    'shutdown': shutdown
                }
            })
        return json.dumps(
            {
                'domains': domains_list,
                'workers': workers_list
            }
        )

    @bp.route('/delete-task/<tid>')
    def delete_task(tid):
        if not has_permission('delete'):
            abort(403, 'You cannot do that.')

        with engine.begin() as cn:
            cn.execute(
                "delete from rework.task "
                "where id = %(tid)s and status != 'running'",
                tid=tid
            )
        return json.dumps(True)

    @bp.route('/abort-task/<tid>')
    def abort_task(tid):
        if not has_permission('abort'):
            abort(403, 'You cannoy do that.')

        t = Task.byid(engine, tid)
        if t is None:
            abort(404, 'NO SUCH JOB')

        if t.aborted:
            return json.dumps(False)

        t.abort()
        return json.dumps(True)

    @bp.route('/taskerror/<int:taskid>')
    def taskerror(taskid):
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

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

    class tasksargs(uiargsdict):
        types = {
            'min': int,
            'max': int
        }

    @bp.route('/tasks-table-json')
    def tasks_table():
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        args = tasksargs(request.args)
        with engine.begin() as cn:
            q = select(
                't.id', 'op.name', 't.status', 'op.domain',
                't.operation', 't.traceback', 't.abort',
                't.queued', 't.started', 't.finished',
                't.metadata', 't.worker', 'w.deathinfo',
                'op.inputs', 't.input'
            ).table('rework.task as t'
            ).join('rework.operation as op on (op.id = t.operation)'
            ).join('rework.worker as w on (w.id = t.worker)', jtype='left outer'
            ).order('t.id')
            if args.domain != 'all':
                q.where('op.domain = %(domain)s', domain=args.domain)
            if args.min:
                q.where('t.id >= %(minid)s', minid=args.min)
            if args.max:
                q.where('t.id <= %(maxid)s', maxid=args.max)

            out = [
                {'tid': row.id,
                 'name' : row.name,
                 'status': row.status,
                 'abort': row.abort,
                 'domain': row.domain,
                 'operation': row.operation,
                 'queued': maybetz(row.queued),
                 'started': maybetz(row.started),
                 'finished': maybetz(row.finished),
                 'metadata': row.metadata,
                 'worker': row.worker,
                 'deathinfo': row.deathinfo,
                 'traceback': row.traceback,
                 'input': task_formatinput(row.inputs, row.input)
                }
                for row in q.do(cn).fetchall()
            ]

        return make_response(
            json.dumps(out),
            200,
            {'content-type': 'application/json'}
        )

    @bp.route('/tasklogs/<int:taskid>')
    def tasklogs(taskid):
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        return render_template(
            'tasklogs.html',
            taskid=taskid,
            homeurl=homeurl()
        )

    # info

    @bp.route('/taskinfo/<int:taskid>')
    def taskinfo(taskid):
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        return render_template(
            'taskinfo.html',
            taskid=taskid,
            homeurl=homeurl()
        )

    @bp.route('/info-for/<int:taskid>')
    def info_for(taskid):
        res = select(
            'status', 'abort', 'traceback',
            'queued', 'started', 'finished',
            'inputs', 'outputs'
        ).table('rework.task as t', 'rework.operation as o'
        ).where('t.id = %(taskid)s', taskid=taskid
        ).where('o.id = t.operation'
        ).do(engine).fetchone()

        info = {
            'state': _task_state(
                res.status,
                res.abort,
                res.traceback
            ),
            'queued': str(res.queued or ''),
            'started': str(res.started or ''),
            'finished': str(res.finished or ''),
            'inputspec': res.inputs or [],
            'outputspec': res.outputs or []
        }

        return make_response(
            json.dumps(info),
            200,
            {'content-type': 'application/json'}
        )

    def _io_payload(taskid, direction):
        q = select(
            direction
        ).table(
            'rework.task'
        ).where(id=taskid)

        return q.do(engine).scalar()

    def _io_spec(taskid, direction):
        return select(
            f'{direction}s'
        ).table(
            'rework.task as t', 'rework.operation as o'
        ).where(
            't.id = %(taskid)s', taskid=taskid
        ).where(
            'o.id = t.operation'
        ).do(engine).scalar()


    @bp.route('/read_io/<int:taskid>')
    def read_io(taskid):
        args = argsdict(request.args)
        assert args.direction in ('input', 'output')

        payload = _io_payload(taskid, args.direction)
        if payload is None:
            return make_response(
                json.dumps(None),
                200,
                {'content-type': 'application/json'}
            )

        spec = _io_spec(taskid, args.direction)

        fname = args['getfile']
        out = unpack_io(
            spec,
            payload,
            nofiles=True
        )
        return make_response(
            json.dumps(
                out
            ),
            200,
            {'content-type': 'application/json'}
        )

    @bp.route('/getiofile_lengths/<int:taskid>')
    def getiofile_lengths(taskid):
        args = argsdict(request.args)
        assert args.direction in ('input', 'output')
        fname = args['getfile']
        payload = _io_payload(taskid, args.direction)
        if payload is None:
            return make_response(
                json.dumps(None),
                200,
                {'content-type': 'application/json'}
            )

        spec = _io_spec(taskid, args.direction)
        out = unpack_iofiles_length(spec, payload)
        return make_response(
            out,
            200,
            {'content-type': 'application/json'}
        )


    @bp.route('/getiofile/<int:taskid>')
    def getiofile(taskid):
        args = argsdict(request.args)
        assert args.direction in ('input', 'output')
        fname = args['getfile']
        payload = _io_payload(taskid, args.direction)
        if payload is None:
            return make_response(
                json.dumps(None),
                200,
                {'content-type': 'application/json'}
            )

        spec = _io_spec(taskid, args.direction)
        contents = unpack_iofile(spec, payload, fname)
        mimetype = mimetypes.guess_type(fname)[0]
        return make_response(
            contents,
            200,
            {'content-type': mimetype}
        )

    class hintargs(argsdict):
        types = {
            'taskid': list,
            'direction': str
        }

    @bp.route('/getiofilehint')
    def getiofilehint():
        args = hintargs(request.args)
        assert args.direction in ('input', 'output'), args

        out = {}
        for tid in args.taskid:
            payload = _io_payload(tid, args.direction)
            if payload is None:
                continue
            spec = _io_spec(tid, args.direction)
            flenths = unpack_iofiles_length(spec, payload)
            if len(flenths) == 1:
                # more than one: we won't provide the button
                # instead, all files can be found in the Info page
                out[tid] = list(flenths.keys())[0]

        return make_response(
            json.dumps(out),
            200,
            {'content-type': 'application/json'}
        )

    # services

    @bp.route('/services-table-json')
    def list_services_json():
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        args = uiargsdict(request.args)
        q = select(
            'id', 'host', 'name', 'path', 'domain'
        ).table('rework.operation'
        ).order('domain, name', 'asc')
        if args.domain != 'all':
            q.where(domain=args.domain)

        out = []
        for opid, host, name, path, domain in q.do(engine).fetchall():
            out.append({
                'opid': opid,
                'host': host,
                'name': name,
                'path': path,
                'domain': domain
            })
        return make_response(
            json.dumps(out),
            200,
            {'content-type': 'application/json'}
        )

    @bp.route('/launchers-table-json')
    def launchers_table_json():
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        spec = iospec(engine)
        return make_response(
            json.dumps(spec),
            200,
            {'content-type': 'application/json'}
        )

    @bp.route('/schedulers-table-json')
    def schedulers_table_json():
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        with engine.begin() as cn:
            sql = (
                'select sched.id, op.name, sched.domain, sched.host, rule, '
                '       op.inputs, inputdata '
                'from rework.sched as sched, rework.operation as op '
                'where sched.operation = op.id '
                'order by sched.domain, op.name asc'
            )
            res = cn.execute(sql).fetchall()

        return make_response(
            json.dumps(
                [
                    [row.id,
                     row.name,
                     row.domain,
                     row.host or "",
                     row.rule,
                     task_formatinput(row.inputs, row.inputdata)
                    ]
                    for row in res
                ]
            ),
            200,
            {'content-type': 'application/json'}
        )

    @bp.route('/prepare-schedule', methods=['PUT'])
    def prepare_schedule():
        args = argsdict({
            k: v.read()
            for k, v in argsdict(request.files).items()
            if v
        })
        args.update(
            argsdict(request.form)
        )

        meta = argsdict(request.args)

        # separate regular _prepare_ args from task payload
        host = args.pop('host', None)
        operation, domain = args.pop('service').split(':')
        rule = args.pop('rule', None)

        try:
            api.prepare(
                engine,
                opname=operation,
                domain=domain,
                host=host,
                rule=rule,
                inputdata=args,
                metadata=meta or None
            )
        except Exception as err:
            return make_response(
                f'{err}',
                400,
                {'content-type': 'application/json'}
            )

        return make_response(
            'done',
            200,
            {'content-type': 'application/json'}
        )

    @bp.route('/delete-schedule', methods=['DELETE'])
    def delete_schedule():
        sid = json.loads(request.data)
        with engine.begin() as cn:
            cn.execute(
                'delete from rework.sched '
                'where id = %(id)s',
                id=sid
            )
        return make_response('', 200)

    @bp.route('/lasteventid')
    def lasteventid():
        eid = select('max(id)').table(
            'rework.events'
        ).do(engine).scalar() or 0
        return json.dumps(eid)

    @bp.route('/events/<int:fromid>')
    def events(fromid):
        knownid = select('id').table(
            'rework.events'
        ).where(
            id=fromid
        ).do(engine).scalar()
        if not knownid:
            # this signals to the client
            # he is needs a full refresh
            return 'null'

        q = select(
            'id', 'action', 'taskid'
        ).table('rework.events'
        ).where('id > %(eid)s', eid=fromid
        ).order('id')

        events = [
            dict(item)
            for item in q.do(engine).fetchall()
        ]
        return json.dumps(
            events
        )

    @bp.route('/test-cron-rule')
    def test_cron_rule():
        args = argsdict(request.args)
        try:
            BetterCronTrigger.from_extended_crontab(args.rule)
            return make_response('', 200)
        except Exception as err:
            return make_response(str(err), 200)

    @bp.route('/')
    def home():
        if not has_permission('read'):
            abort(403, 'Nothing to see there.')

        domains = alldomains(engine)
        if not len(domains):
            return 'No operation registered: nothing to see here'

        return render_template(
            'rui_home.html',
            homeurl=homeurl(),
            domains=json.dumps(domains)
        )

    return bp
