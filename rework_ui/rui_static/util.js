"use strict"

function append(domid, html) {
     const div = document.getElementById(domid)
     const span = document.createElement('span')
     span.innerHTML = html + '<br/>'
     div.append(span)
 }


function update(domid, html) {
    const elt = document.getElementById(domid)
    elt.innerHTML = html
}

function refresh_tasks() {
    fetch(`tasks-table-hash?domain=${domain}`).then(
        resp => resp.text()
    ).then(
        newhash => {
            if (newhash != hash) {
                hash = newhash
                refresh_section('tasks')
            }
        }
    )
}


function setdomain(form) {
    domain = form.selectedOptions[0].value
    refresh_section('tasks')
    refresh_section('services')
    refresh_section('workers')
}


function refresh_section(section) {
    fetch(`${section}-table?domain=${domain}`).then(
        resp => resp.text()
    ).then(
        resp => update(section, resp)
    )
}


function start_job(operation, form) {
    fetch(`new_job/${operation}?user=WEBUI`,
          {method: 'PUT', body: new FormData(form)})
    refresh_section('services')
}


function shutdown_worker(wid) {
    fetch(`shutdown-worker/${wid}`).then(
        () => refresh_section('workers')
    )
}


function kill_worker(wid) {
    fetch(`kill-worker/${wid}`).then(
        () => refresh_section('workers')
    )
}


function abort_task(tid) {
    fetch(`abort-task/${tid}`).then(
        () => refresh_section('tasks')
    )
}


function relaunch_task(tid) {
    fetch(`relaunch-task/${tid}`, {method: 'PUT'}).then(
        () => refresh_section('tasks')
    )
}


function delete_task(tid) {
    fetch(`delete-task/${tid}`).then(
        () => refresh_section('tasks')
    )
}


function show_logs(logsliceuri) {
    let lastid = 0
    console.log('logslice uri', logsliceuri)
    function _getmore() {
        fetch(`${logsliceuri}?from_log_id=${lastid}`).then(
            resp => {
                if (resp.status != 200) {
                    clearInterval(ival)
                    throw `task at ${logsliceuri} is gone`
                }
                return resp.json()
            }
        ).then(logs => {
            logs.forEach(id_line => {
                // let's be ruthlessly inefficient :)
                const [id, line] = id_line
                lastid = id
                append('logs', line)
            })
        }).catch(err => console.log(err))
    }

    _getmore()
    const ival = setInterval(_getmore, 3000)
}
