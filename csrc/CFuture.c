#include <HsFFI.h>
#include <CFuture.h>

/**
 * Aborts the calculation
 * and also frees the pointers in the Future.
 */
void abortC(CFuture future) {
  hs_try_putmvar(-1, future[0]);
  hs_free_stable_ptr(future[1]);
}

/**
 * Frees the pointers in the Future.
 */
void freeC(CFuture future) {
  hs_free_stable_ptr(future[0]);
  hs_free_stable_ptr(future[1]);
}
