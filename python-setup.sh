#! /bin/bash

## PYFLAKES INSTALLATION
mkdir tmp
cd tmp
curl -O http://pypi.python.org/packages/source/p/pyflakes/pyflakes-0.3.0.tar.gz
tar xzvf pyflakes-0.3.0.tar.gz
cd pyflakes-0.3.0
sudo python setup.py install
cd ..

## PYMACS INSTALLATION
curl -O http://pymacs.progiciels-bpi.ca/archives/Pymacs.tar.gz
tar xzvf Pymacs.tar.gz
cd Pymacs-0.23
sudo python setup.py install
cp pymacs.el ../../vendor/
cd ..

## PYSMELL INSTALLATION
curl -O http://pypi.python.org/packages/source/p/pysmell/pysmell-0.7.3.zip
unzip pysmell-0.7.3.zip
cd pysmell-0.7.3
sudo python setup.py install
cp pysmell.el ../../vendor/


