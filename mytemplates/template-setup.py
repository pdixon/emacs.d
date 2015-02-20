from setuptools import setup

setup(
    name='$1',
    version='0.1',
    py_modules=['$1'],
    install_requires=[
        '$0',
    ],
    entry_points='''
        [console_scripts]
        $1=$1:cli
    ''',
)
