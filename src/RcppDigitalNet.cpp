#include <Rcpp.h>
#include "DigitalNet.h"
#include "sobolpoint.h"

// [[Rcpp::plugins(cpp11)]]

using namespace std;
using namespace Rcpp;
using namespace DigitalNetNS;

// [[Rcpp::export(rng = false)]]
NumericMatrix rcppDigitalNetPoints(DataFrame df,
                                   int id,
                                   int dimR,
                                   int dimF2,
                                   uint64_t count,
                                   bool digitalShift)
{
    digital_net_id digitalNetId;
    if (id == 1) {
        digitalNetId = NXLW;
    } else if (id == 2) {
        digitalNetId = SOLW;
    } else { // id == 3
        digitalNetId = SOBOL;
    }
    DigitalNet<uint64_t> digitalNet(df, digitalNetId, dimR, dimF2);
    digitalNet.setDigitalShift(digitalShift);
    digitalNet.pointInitialize();
    //uint32_t cnt = 0;
    NumericMatrix mx(count, dimR);
    // assume that count <= 2^dimF2
    for (size_t i = 0; i < count; i++) {
        checkUserInterrupt();
        for (size_t j = 0; j < dimR; j++) {
            mx(i, j) = digitalNet.getPoint(j);
        }
        digitalNet.nextPoint();
    }
    return mx;
}
