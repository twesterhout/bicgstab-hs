import numpy as np
import scipy
import scipy.io
import scipy.sparse
import scipy.sparse.linalg

np.random.seed(12345)

m1 = np.random.rand(2, 2) # scipy.io.mmread("impcol_a.mtx.gz")
b1 = np.random.rand(m1.shape[1])
(x1, info) = scipy.sparse.linalg.bicgstab(m1, b1)
assert info == 0
print((m1 @ b1).tolist())
print(x1)

scipy.io.mmwrite("m1.mtx", scipy.sparse.coo_matrix(m1))
scipy.io.mmwrite("b1.mtx", b1.reshape(-1, 1))
scipy.io.mmwrite("x1.mtx", x1.reshape(-1, 1))
