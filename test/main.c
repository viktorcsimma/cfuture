#include <stdio.h>

#include <HsFFI.h>
#include "OurTasks_stub.h"
#include "CFuture.h"

/**
 * See HsTestCase.h.
 */
int hsTestCase(void);

/**
 * A non-zero return value means a fail.
 */
int main(int argc, char* argv[]) {
  hs_init(&argc, &argv);

  /* Test 1: a successful run */

  CFuture future;
  cFunction(future);

  int result;
  int success = getC_Int(future, &result);
  freeC(future);

  printf(success ? "Successful background calculation\n" : "Failed background calculation\n");
  printf("Result of cFunction: %d\n", result);

  int firstTestPassed = success && 110 == result;

  /* Test 2: an aborted run */

  cFunction(future);
  /* cannot use sleep() as that also halts the Haskell thread */
  abortC(future); /* this also frees it */

  printf(success ? "Aborted successfully\n" : "Failed while aborting\n");

  int secondTestPassed = !success;
  
  /* Test 3: an aborted run in Haskell */
  int thirdTestPassed = hsTestCase();

  /* Finally: */

  hs_exit();
  return (!firstTestPassed && !secondTestPassed && thirdTestPassed); /* 0 if all tests passed */
}
