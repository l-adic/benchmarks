%matplotlib inline
import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')
import numpy as np

x = [1000, 10000, 100000, 1000000]
ladic_compile = [0.002, 0.090, 1.412, 18.210]
ladic_compile_memory = [16.998, 65.712, 593.732, 6118.116]
ladic_solve = [0.0005, 0.030, 0.425, 4.465]
ladic_solve_memory = [15.574, 33.862, 251.981, 2880.300]

circom_compile = [0.047, 0.195, 1.816, 19.333]
circom_compile_memory = [10.813, 33.881, 252.252, 2501.666]
circom_solve = [0.0001, 0.0001, 0.003, 0.034]
circom_solve_memory = [4.222, 4.599, 8.864, 51.054]


plt.xscale("log")
plt.xlabel("Multiplications")

plt.ylabel("Max Memory (MB)")
plt.yscale("log")

plt.plot(x, ladic_compile_memory, 'o', color='red', label='ℓ-adic');
plt.plot(x, circom_compile_memory, 'x', color='blue', label='circom');
plt.legend(['ℓ-adic', 'circom'], fontsize='large', frameon=True)
