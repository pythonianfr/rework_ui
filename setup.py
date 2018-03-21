from setuptools import setup


setup(name='rework_ui',
      version='0.1.0',
      author='Aurelien Campeas',
      author_email='aurelien.campeas@pythonian.fr',
      description='A web ui for the rework distributed task dispatcher',
      url='https://bitbucket.org/pythonian/rework_ui',
      packages=['rework_ui'],
      zip_safe=False,
      install_requires=[
          'rework',
          'flask',
          'pml',
          'sqlalchemy',
          'webtest'
      ],
      package_data={'rework_ui': [
          'rui_static/*',
          'rui_templates/*'
      ]},
      entry_points={'rework.subcommands': [
          'view=rework_ui.cli:view',
          'complete-db=rework_ui.cli:complete_db',
          'generate-tasks-table=rework_ui.cli:generate_tasks_table'
      ]},
      classifiers=[
          'Development Status :: 4 - Beta',
          'Intended Audience :: Developers',
          'License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)',
          'Operating System :: OS Independent',
          'Programming Language :: Python :: 2',
          'Programming Language :: Python :: 3',
          'Environment :: Web Environment',
          'Topic :: System :: Distributed Computing',
          'Topic :: Software Development :: User Interfaces'
      ]
)
