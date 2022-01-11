#include "dlpack/dlpack.h"

#if defined(__cplusplus)
extern "C" {
#endif

typedef int (*bicgstab_hs_matrix)(DLTensor const *src, DLTensor *dest);

typedef struct bicgstab_hs_parameters {
  int64_t dimension;
  bicgstab_hs_matrix matrix;
  int64_t max_iters;
  double tol;
  DLTensor const *b;
  DLTensor const *x0;
  DLTensor const *x;
} bicgstab_hs_parameters;

int bicgstab_hs_solve(bicgstab_hs_parameters const *settings);

#if defined(__cplusplus)
} // extern "C"
#endif
