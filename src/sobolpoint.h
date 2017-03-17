#pragma once
#ifndef SOBOL_POINT_H
#define SOBOL_POINT_H

#include <iostream>
#include <inttypes.h>
#include <Rcpp.h>

namespace MCQMCIntegration {
    bool get_sobol_base(Rcpp::NumericMatrix sobolMatrix,
                        uint32_t s, uint32_t m,  uint64_t base[]);
}
#endif //SOBOL_POINT_H
