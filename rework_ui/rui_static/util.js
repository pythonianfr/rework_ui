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


function refresh_section(section) {
    fetch(`${section}-table`).then(
        resp => resp.text()
    ).then(
        resp => update(section, resp)
    )
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
            resp => resp.json()
        ).then(logs => {
            logs.forEach(id_line => {
                // let's be ruthlessly inefficient :)
                const [id, line] = id_line
                lastid = id
                append('logs', line)
            })
        })
    }

    _getmore()
    setInterval(_getmore, 3000)
}
