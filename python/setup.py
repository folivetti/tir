#!/usr/bin/env python

from setuptools import setup

setup(
	name='pyTIR',
	version='1.0.0',
	description='Python TIR wrapper',
	author='Fabricio Olivetti de Franca',
	author_email='folivetti@ufabc.edu.br',
	url='https://github.com/folivetti/tir',
	packages=['pyTIR'],
	data_files=[('bin', ['tir'])],
    install_requires=['scikit-learn', 'numpy', 'pandas'],
	)
