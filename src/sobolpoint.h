#pragma once
#ifndef SOBOL_POINT_H
#define SOBOL_POINT_H

#include <iostream>
#include <inttypes.h>
#if defined(IN_RCPP)
#include <Rcpp.h>
#endif

namespace DigitalNetNS {
    bool get_sobol_base(std::istream& is,
                        uint32_t s, uint32_t m,  uint64_t base[]);
#if defined(IN_RCPP)
    bool read_sobol_base(Rcpp::DataFrame df,
                         uint32_t s, uint32_t m,  uint64_t base[]);
#else
    bool select_sobol_base(const std::string& path,
                           uint32_t s, uint32_t m,  uint64_t base[]);
#endif
    int get_sobol_s_max(const std::string& path);
    int get_sobol_s_min(const std::string& path);
    int get_sobol_m_max(const std::string& path, int s);
    int get_sobol_m_min(const std::string& path, int s);
}
#endif //SOBOL_POINT_H
