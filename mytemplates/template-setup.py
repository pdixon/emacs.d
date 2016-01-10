from setuptools import setup

setup(
    name='${1:`(car (last (split-string (file-name-directory (buffer-file-name)) "/" t)))`}',
    version='0.1',
    py_modules=['${1:$(subst-char-in-string ?- ?_ yas-text)}'],
    install_requires=[
        '$0',
    ],
    entry_points='''
        [console_scripts]
        $1=${1:$(subst-char-in-string ?- ?_ yas-text)}:cli
    ''',
)
