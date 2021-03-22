from pathlib import Path
from setuptools import setup


doc = Path(__file__).parent / 'README.md'


setup(name='rework_ui',
      version='0.10.0',
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
          'pygments',
          'werkzeug >= 1.0.0',
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
      ]
)
