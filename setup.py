from pathlib import Path
from setuptools import setup


doc = Path(__file__).parent / 'README.md'


setup(name='rework_ui',
      version='0.8.0',
      author='Aurelien Campeas',
      author_email='aurelien.campeas@pythonian.fr',
      description='A web ui for the rework distributed task dispatcher',
      long_description=doc.read_text(),
      long_description_content_type='text/markdown',
      url='https://bitbucket.org/pythonian/rework_ui',

      packages=['rework_ui'],
      zip_safe=False,
      install_requires=[
          'rework',
          'flask',
          'pml',
          'sqlalchemy',
      ],
      package_data={'rework_ui': [
          'rui_static/*',
          'rui_templates/*',
          'schema.sql'
      ]},
      entry_points={'rework.subcommands': [
          'view=rework_ui.cli:view',
          'init-db=rework_ui.cli:init_db',
          'generate-tasks-table=rework_ui.cli:generate_tasks_table'
      ]},
      classifiers=[
          'Development Status :: 4 - Beta',
          'Intended Audience :: Developers',
          'License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)',
          'Operating System :: OS Independent',
          'Programming Language :: Python :: 3',
          'Environment :: Web Environment',
          'Topic :: System :: Distributed Computing',
          'Topic :: Software Development :: User Interfaces'
      ]
)
