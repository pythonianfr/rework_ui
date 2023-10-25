import subprocess
from pathlib import Path
from setuptools import setup
from setuptools.command.build_ext import build_ext

from rework_ui import __version__


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


doc = Path(__file__).parent / 'README.md'


setup(name='rework_ui',
      version=__version__,
      author='Aurelien Campeas',
      author_email='aurelien.campeas@pythonian.fr',
      description='A web ui for the rework distributed task dispatcher',
      long_description=doc.read_text(),
      long_description_content_type='text/markdown',
      url='https://hg.sr.ht/~pythonian/rework_ui',

      packages=['rework_ui'],
      zip_safe=False,
      install_requires=[
          'rework',
          'flask',
          'croniter',
          'pygments',
          'werkzeug >= 1.0.0',
          'lxml',
          'sqlalchemy',
      ],
      package_data={'rework_ui': [
          'rui_static/*',
          'rui_templates/*',
          'schema.sql'
      ]},
      entry_points={'rework.subcommands': [
          'view=rework_ui.cli:view',
          'init-db=rework_ui.cli:init_db'
      ]},
      classifiers=[
          'Development Status :: 4 - Beta',
          'Intended Audience :: Developers',
          'License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)',
          'Operating System :: OS Independent',
          'Programming Language :: Python :: 3',
          'Programming Language :: ML',
          'Environment :: Web Environment',
          'Topic :: System :: Distributed Computing',
          'Topic :: Software Development :: User Interfaces'
      ],
      cmdclass={'build_ext': ElmBuild}
)
