import numpy as np
import optuna
import sklearn.model_selection as sk 
import pyTIR as tir

def objective(trial):
    Z = np.loadtxt("datasets/airfoil/airfoil-train-0.dat", delimiter=",")
    rng_min = trial.suggest_int('rng_min', -3, 1)
    rng_max = trial.suggest_int('rng_max', 1, 3)
    pc = trial.suggest_float('pc', 0.1, 1.0, step=0.1)
    pm = trial.suggest_float('pm', 0.1, 1.0, step=0.1)
    clr = tir.TIRRegressor(200, 200, pc, pm, (rng_min, rng_max))
    return sk.cross_val_score(clr, Z[:,:-1], Z[:,-1], cv=2).mean()

study = optuna.create_study(direction='maximize')
study.optimize(objective, n_trials=30)
trial = study.best_trial
print('Accuracy: {}'.format(trial.value))
print("Best hyperparameters: {}".format(trial.params))
optuna.visualization.plot_optimization_history(study)
optuna.visualization.plot_slice(study)

