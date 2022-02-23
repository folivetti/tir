"""Example Scikit-learn compatible Python wrapper for a Symbolic Regression CLI.

This module demonstrates how to implement a python wrapper compatible with
scikit-learn for any Symbolic Regression CLI that returns a python compatible expression
in plain text.

This example assumes that your CLI allows to pass the parameters via command line 
and that it returns a readable and python-compatible expression:

./bin/regressor 500 1000 0.3 0.7 42

np.sin(x[:,0]*np.exp(-x[:,1]/2));8;0.4572

In this example, the regressor returns a ; separated string where
the first field is the python-compatible expression, the second field
is the length of the expression and the third field the evaluated
error of the training data.

@Author: Fabricio Olivetti de França
@Date: 2020-01-05
"""

from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.utils.validation import check_X_y, check_array, check_is_fitted

import os
from tempfile import TemporaryDirectory
import subprocess
import numpy as np

class YourRegressor(BaseEstimator, RegressorMixin):

    def __init__(self, npop, ngens, pc, pm, random_state=-1):
        """ Builds a Symbolic Regression using the cli interface of your algorithm.

        Examples
        --------
        >>> from python_wrapper_template import YourRegressor
        >>> import numpy as np
        >>> X = np.arange(100).reshape(100, 1)
        >>> y = x**2
        >>> reg = YourRegressor(100, 100, 0.3, 0.7)
        >>> reg.fit(X, y)
        """

        self.npop = npop
        self.ngens = ngens
        self.pc = pc
        self.pm = pm
        self.random_state = random_state
    
    def fit(self, X_train, y_train):
        """A reference implementation of a fitting function.
        Parameters
        ----------
        X : {array-like, sparse matrix}, shape (n_samples, n_features)
            The training input samples.
        y : array-like, shape (n_samples,) or (n_samples, n_outputs)
            The target values (class labels in classification, real numbers in
            regression).
        Returns
        -------
        self : object
            Returns self.
        """

        # 1. create a temporary directory to store the training data set
        with TemporaryDirectory() as temp_dir:
            # 2. validate the consistency of the data matrices and create a single 2D array with X and y
            X_train, y_train = check_X_y(X_train, y_train, accept_sparse=False)
            if len(y_train.shape) == 1:
                Z_train = np.hstack((X_train, y_train[:,None]))
            else:
                Z_train = np.hstack((X_train, y_train))
            
            # 3. create a temp file and store the data
            fname   = temp_dir + "/tmpdata.csv"            
            np.savetxt(f"{fname}", Z_train, delimiter=",")               
            
            # 4. call your cli binary with the parameters
            cwd = os.path.dirname(os.path.realpath(__file__))
            ans = subprocess.check_output(["bin/regressor", f"{self.ngens}", f"{self.npop}", f"{self.pc}", f"{self.pm}", f"{self.random_state}", f"{fname}"], cwd=cwd)
            # in this example, we assume the cli returns 
            # a string with the expression, size of the expression 
            # and training error separated by ;
            # the eval command changes the bytestring to a string.
            self.expr, n, e = eval(ans).split(";")
            self.len = int(n)
            self.is_fitted_ = True

        return self

    def eval_expr(self, x):
        """ Evaluates the expression with data point x. We assume that the expression is compatible with numpy """
        Z = eval(self.expr)
        
        # we can change any NaN or Inf to 0 to avoid
        # evaluation error
        inds = np.where(np.isnan(Z))[0]
        inds2 = np.where(np.isinf(Z))[0]
        Z[inds] = 0
        Z[inds2] = 0

        return Z

    def predict(self, X_test, ic=None):
        """ A reference implementation of a predicting function.
        Parameters
        ----------
        X : {array-like, sparse matrix}, shape (n_samples, n_features)
            The training input samples.
        Returns
        -------
        y : ndarray, shape (n_samples,)
            Returns an array of ones.
        """        
        check_is_fitted(self)
        X_test = check_array(X_test, accept_sparse=False)
        X_test = X_test[:,self.cols]
        return self.eval_expr(X_test)
