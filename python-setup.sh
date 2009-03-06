#! /bin/bash

cd $(dirname "$0")

virtualenv ./usr
source ./usr/bin/activate

DIRS=(Pymacs-0.24-beta1 rope-0.9.2 ropemacs pyflakes-0.3.0)
FILES=(Pymacs-0.24-beta1 rope-0.9.2 ropemacs-tip ropemode-tip pyflakes-0.3.0)
rm -rf .usr/ ${DIRS[@]}

for D in ${FILES[@]}
do tar xfz $D.tar.gz; done

for D in ${DIRS[@]}
do cd $D; python setup.py install; cd ..; done

mv ropemode/ropemode usr/lib/python2.5/site-packages/
mv ropemacs/ropemacs usr/lib/python2.5/site-packages/

rm -f ./pymacs.el
ln -s Pymacs-0.24-beta1/pymacs.el ./pymacs.el