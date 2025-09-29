import subprocess
from pathlib import Path
from setuptools import setup
from setuptools.command.build_ext import build_ext


WORKING_DIR = Path(__file__).resolve().parent
STATIC_DIR = WORKING_DIR / 'rework_ui' / 'rui_static'


def compile_elm(edit_kind, src):
    src = WORKING_DIR / 'elm/src' / src
    out = STATIC_DIR / f'{edit_kind}.js'
    cmd = f'cd elm && elm make --optimize --output {out} {src}'
    print(cmd, subprocess.call(cmd, shell=True))


class ElmBuild(build_ext):

    def run(self):
        for edit_kind, src in [
            ('rework_ui_elm', 'Main.elm'),
            ('logview', 'Logview.elm'),
            ('info', 'Info.elm'),
        ]:
            compile_elm(edit_kind, src)
        super().run()


# minimal setup.py for backward compatibility and custom build commands
# all metadata is now in pyproject.toml
setup(cmdclass={'build_ext': ElmBuild})