#! /bin/bash

cd $(dirname "$0")

virtualenv ./usr
source ./usr/bin/activate

## PYMACS INSTALLATION
cd vendor
# download pymacs 0.23 tarball into vendor directory
curl -O http://pymacs.progiciels-bpi.ca/archives/Pymacs.tar.gz
# expand pymacs
tar xzvf Pymacs.tar.gz
# step into the pymacs directory
cd Pymacs-0.23
# install the pymacs proper
python setup.py install
# copy the pymacs lisp file into the vendor directory
cp pymacs.el ../pymacs.el
cd ..
# cleanup. we don't need these anymore
rm Pymacs.tar.gz
rm -rf Pymacs-0.23/
 
 
## ROPE, ROPEMACS INSTALLATION
# we assume you already have mercurial installed
# clone the necessary files (rope, ropemacs and ropemode)
hg clone http://bitbucket.org/agr/rope
hg clone http://bitbucket.org/agr/ropemacs
hg clone http://bitbucket.org/agr/ropemode
cd rope
# install Rope
python setup.py install
cd ..
# symlink ropemode which is needed for the ropemacs install
ln -s ../ropemode/ropemode ropemacs/
cd ropemacs
# install ropemacs
python setup.py install
cd ..
rm -rf rope
rm -rf ropemacs
rm -rf ropemode

## PYFLAKES & FLYMAKE INSTALL
cd ..
mkdir tmp
cd tmp
svn co http://divmod.org/svn/Divmod/trunk/Pyflakes PyFlakes
cd PyFlakes
python setup.py install
cd ..
rm -rf PyFlakes