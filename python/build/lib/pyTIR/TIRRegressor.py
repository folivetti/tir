import os
import sys

sys.path.insert(0, os.path.join(
    os.path.dirname(os.path.dirname(os.path.realpath(__file__))), 'pyTIR'))
    
from tempfile import TemporaryDirectory
import subprocess
import pandas as pd
import numpy as np
import re

from sklearn.base import BaseEstimator, RegressorMixin
from sklearn.utils.validation import check_X_y, check_array, check_is_fitted

    
def sqrtAbs(a):
    """ Auxiliary function to calculate sqrt.abs """
    return np.sqrt(np.abs(a))


class TIRRegressor(BaseEstimator, RegressorMixin):

    def __init__(self, npop, ngens, pc, pm, exponents, transfunctions='Id,Sin,Tanh,Sqrt,Log,Exp', ytransfunctions='Id', error="RMSE", penalty=0.00, random_state=-1):
        """ Builds a Symbolic Regression model using ITEA.

        Parameters
        ----------
        npop : population size
        ngens : number of generations
        exponents : a tuple of int representing the minimum and maximum exponents of the interactions
        termlimit : a tuple of int representing the minimum and maximum number of terms in an expression
        nonzeroexps : number of non-zero exponents in each term of the initial population
        transfunctions : a string with the list of transformation functions. Currently supported: Id, Sin, Cos, Tanh, SqrtAbs, Log, Exp


        Examples
        --------
        >>> from itea import ITEARegressor
        >>> import numpy as np
        >>> X = np.arange(100).reshape(100, 1)
        >>> y = x**2
        >>> reg = SymbolicRegressor(100, 100, (1,2), (1,3))
        >>> reg.fit(X, y)
        """

        self.exponents = exponents
        self.transfunctions = transfunctions
        self.ytransfunctions = ytransfunctions
        self.npop = npop
        self.ngens = ngens
        self.pc = pc
        self.pm = pm
        self.random_state = random_state
        self.error = error
        self.penalty = penalty
        
        print(self.penalty)
    
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

        with TemporaryDirectory() as temp_dir:
            X_train, y_train = check_X_y(X_train, y_train, accept_sparse=False)
            self.cols = np.nonzero(X_train.var(axis=0))[0]
            X_train = X_train[:, self.cols]
            if len(y_train.shape) == 1:
                Z_train = np.hstack((X_train, y_train[:,None]))
            else:
                Z_train = np.hstack((X_train, y_train))
            
            fname   = temp_dir + "/tmpdata.csv"
            
            ixs = np.arange(0, Z_train.shape[0])
            np.random.shuffle(ixs)
            np.savetxt(f"{fname}", Z_train[ixs,:], delimiter=",")    
            
            minK, maxK = self.exponents
            
            cwd = os.path.dirname(os.path.realpath(__file__))
            ans = subprocess.check_output(["tir", "cli", f"{minK}", f"{maxK}", f"{self.transfunctions}", f"{self.ytransfunctions}", f"{self.error}", f"{self.ngens}", f"{self.npop}", f"{self.pc}", f"{self.pm}", f"{self.random_state}", f"{self.penalty}", f"{fname}"], cwd=cwd)
            self.expr, n, e = eval(ans).split(";")
            print(e)

            self.expr = self.expr.replace("/ ((1.0) + ())", "").replace("atan","arctan")

            self.len = int(n)
            self.is_fitted_ = True

        return self

    def eval_expr(self, x):
        """ Evaluates the expression with data point x. """
        Z = eval(self.expr)
        inds = np.where(np.isnan(Z))[0]
        inds2 = np.where(np.isinf(Z))[0]
        Z[inds] = 0
        Z[inds2] = 0
        #print(Z)

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
