#include "bicgstab.h"

#include <Eigen/Core>
#include <Eigen/Dense>
#include <Eigen/IterativeLinearSolvers>
#include <unsupported/Eigen/IterativeSolvers>

namespace {
template <typename Scalar> class CustomMatrix;
}

namespace Eigen {
namespace internal {
template <typename Scalar>
struct traits<CustomMatrix<Scalar>>
    : public Eigen::internal::traits<Eigen::SparseMatrix<Scalar>> {};
} // namespace internal
} // namespace Eigen

namespace {
template <typename T>
class CustomMatrix : public Eigen::EigenBase<CustomMatrix<T>> {
public:
  using Scalar = T;
  using RealScalar = typename Eigen::SparseMatrix<Scalar>::RealScalar;
  using Index = typename Eigen::SparseMatrix<Scalar>::Index;
  using StorageIndex = typename Eigen::SparseMatrix<Scalar>::StorageIndex;
  enum {
    ColsAtCompileTime = Eigen::Dynamic,
    MaxColsAtCompileTime = Eigen::Dynamic,
    IsRowMajor = false
  };

  auto rows() const noexcept -> Index { return _settings->dimension; }
  auto cols() const noexcept -> Index { return _settings->dimension; }

  template <typename Rhs>
  auto operator*(Eigen::MatrixBase<Rhs> const &x) const
      -> Eigen::Product<CustomMatrix<Scalar>, Rhs, Eigen::AliasFreeProduct> {
    return Eigen::Product<CustomMatrix<Scalar>, Rhs, Eigen::AliasFreeProduct>{
        *this, x.derived()};
  }

  CustomMatrix(bicgstab_hs_parameters const &settings) : _settings(&settings) {}

  bicgstab_hs_parameters const *_settings;
};

constexpr auto type_of(float const *) noexcept -> DLDataType {
  return DLDataType{kDLFloat, 32, 1};
}

constexpr auto type_of(double const *) noexcept -> DLDataType {
  return DLDataType{kDLFloat, 64, 1};
}

struct StaticTensor {
  DLTensor tensor;
  int64_t shape[1];
  int64_t strides[1];
};

template <typename Scalar, typename Index>
StaticTensor vector_as_tensor(Scalar *data, Index const size,
                              Index const stride) noexcept {
  auto t = StaticTensor{
      DLTensor{const_cast<typename std::remove_const<Scalar>::type *>(data),
               DLDevice{kDLCPU, 0}, 1, type_of(data), nullptr, nullptr, 0},
      {size},
      {stride}};
  t.tensor.shape = t.shape;
  t.tensor.strides = t.strides;
  return t;
}

template <typename Scalar>
auto tensor_as_vector(DLTensor const &t)
    -> Eigen::Map<Eigen::Matrix<Scalar, Eigen::Dynamic, 1>, Eigen::Unaligned,
                  Eigen::OuterStride<>> {
  return Eigen::Map<Eigen::Matrix<Scalar, Eigen::Dynamic, 1>, Eigen::Unaligned,
                    Eigen::OuterStride<>>{static_cast<Scalar *>(t.data),
                                          t.shape[0],
                                          Eigen::OuterStride<>{t.strides[0]}};
}
} // namespace

// Implementation of CustomMatrix * Eigen::DenseVector though a
// specialization of internal::generic_product_impl:
namespace Eigen {
namespace internal {
template <typename Scalar, typename Rhs>
struct generic_product_impl<CustomMatrix<Scalar>, Rhs, SparseShape, DenseShape,
                            GemvProduct>
    : generic_product_impl_base<
          CustomMatrix<Scalar>, Rhs,
          generic_product_impl<CustomMatrix<Scalar>, Rhs>> {
  template <typename Dest>
  static void scaleAndAddTo(Dest &dst, const CustomMatrix<Scalar> &lhs,
                            const Rhs &rhs, const Scalar &alpha) {
    // This method should implement "dst += alpha * lhs * rhs" inplace
    auto x = vector_as_tensor(rhs.data(), rhs.size(), rhs.stride());
    auto y = vector_as_tensor(dst.data(), dst.size(), dst.stride());
    auto const status = (*lhs._settings->matrix)(&x.tensor, &y.tensor);
    if (status != 0) {
      throw std::runtime_error{"shoooot!"};
    }
    if (alpha != Scalar{1}) {
      dst *= alpha;
    }
  }
};
} // namespace internal
} // namespace Eigen

namespace {
template <typename Scalar> int solve(bicgstab_hs_parameters const *settings) {
  CustomMatrix<Scalar> solver(*settings);
  Eigen::BiCGSTAB<CustomMatrix<Scalar>, Eigen::IdentityPreconditioner> bicg;
  bicg.setMaxIterations(settings->max_iters);
  bicg.setTolerance(static_cast<Scalar>(settings->tol));
  auto b = tensor_as_vector<Scalar>(*settings->b);
  auto x = tensor_as_vector<Scalar>(*settings->b);
  if (settings->x0 != nullptr) {
    auto x0 = tensor_as_vector<Scalar>(*settings->x0);
    x.noalias() = bicg.solveWithGuess(b, x0);
  } else {
    x.noalias() = bicg.solve(b);
  }
  return 0;
}

constexpr auto operator==(DLDataType a, DLDataType b) noexcept -> bool {
  return a.code == b.code && a.bits == b.bits && a.lanes == b.lanes;
}
} // namespace

extern "C" int bicgstab_hs_solve(bicgstab_hs_parameters const *settings) {
  if (settings->b->dtype == type_of(static_cast<const float *>(nullptr))) {
    return solve<float>(settings);
  }
  if (settings->b->dtype == type_of(static_cast<const double *>(nullptr))) {
    return solve<double>(settings);
  }
  return -1;
}
