REWORK-UI
============

Rework-UI is a [rework][rework] plugin which provides a `view`
subcommand to the `rework` command.

The command launches a simple Web front-end to monitor and control
workers, tasks and operations of `rework`.

[rework]: https://bitbucket.org/pythonian/rework


It works like this:

```shell
 $ rework view postgres://babar:celeste@jobstore:5432/
  * Running on http://192.168.56.1:5679/ (Press CTRL+C to quit)
 192.168.56.1 - - [10/Nov/2017 14:32:04] "GET /rework HTTP/1.1" 200 -
 192.168.56.1 - - [10/Nov/2017 14:32:04] "GET /services-table HTTP/1.1" 200 -
 192.168.56.1 - - [10/Nov/2017 14:32:04] "GET /workers-table HTTP/1.1" 200 -
 192.168.56.1 - - [10/Nov/2017 14:32:07] "GET /tasks-table HTTP/1.1"
 200 -
```

At the same time, a browser tab opens, and we can see the big picture.

![rework view](https://bitbucket.org/pythonian/rework_ui/downloads/reworkui.png)

