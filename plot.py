%matplotlib inline
import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')
import numpy as np

x = [1000, 10000, 100000, 1000000]
ladic_compile = [0.002, 0.090, 1.412, 18.210]
ladic_solve = [0.0005, 0.030, 0.425, 4.465]

circom_compile = [0.047, 0.195, 1.816, 19.333]
circom_solve = [0.0001, 0.0001, 0.003, 0.034]

plt.xscale("log")
plt.xlabel("Multiplications")

plt.ylabel("Run Time")


plt.plot(x, ladic_compile, 'o', color='red', label='ℓ-adic');
plt.plot(x, circom_compile, 'x', color='blue', label='circom');
plt.legend(['ℓ-adic', 'circom'], fontsize='large', frameon=True)

