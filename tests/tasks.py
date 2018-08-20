from rework.api import task


@task(domain='uranus')
def justdoit(task):
    task.save_output(True)

