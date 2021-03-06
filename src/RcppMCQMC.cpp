#include <Rcpp.h>
#include <random>
#include <time.h>
#include "DigitalNet.h"

// [[Rcpp::plugins(cpp11)]]

using namespace std;
using namespace Rcpp;
using namespace DigitalNetNS;

//#define DEBUG 1

namespace {
    /*
     * calculate variance
     *
     * See "Online algorithm" at
     * https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm
     */
    class OnlineVariance {
    public:
        OnlineVariance();
        void addData(const double x) {
            n++;
            df = n - 1;
            double delta = x - mean;
            mean += delta / static_cast<double>(n);
            M2 += delta * (x - mean);
        }
        double getMean() const;
        double unbiasedVar() const;
        double var() const;
        double absErr(const int prob) const;
        double relErr(const int prob) const;
    private:
        int n;
        int df;
        double mean;
        double M2;
    };

    int probToInt(double probability);
    double tvalue(const int prob, const int df);

}

// [[Rcpp::export(rng = false)]]
List rcppQMCIntegration(Function integrand,
                        uint32_t N,
                        DataFrame df,
                        int id,
                        int s,
                        int m,
                        double probability)
{
#if defined(DEBUG)
    cout << "N:" << dec << N << endl;
    cout << "id:" << dec << id << endl;
    cout << "s:" << dec << s << endl;
    cout << "m:" << dec << m << endl;
    cout << "probability:" << probability << endl;
#endif
    digital_net_id digitalNetId;
    if (id == 1) {
        digitalNetId = NXLW;
    } else { // id == 2
        digitalNetId = SOLW;
    }
    DigitalNet<uint64_t> digitalNet(df, digitalNetId, s, m);
    digitalNet.pointInitialize();
    OnlineVariance eachintval;
    uint32_t cnt = 0;
    int p = probToInt(probability);
    NumericVector nv(s);
    do {
        checkUserInterrupt();
        OnlineVariance intsum;
        uint64_t max = 1;
        max = max << m;
        for (uint64_t j = 0; j < max; ++j) {
            for (int k = 0; k < s; ++k) {
                nv[k] = digitalNet.getPoint(k);
            }
            double d = as<double>(integrand(nv));
#if defined(DEBUG)
            //cout << "o:" << o << endl;
            cout << "d:" << d << endl;
#endif
            intsum.addData(d);
            digitalNet.nextPoint();
        }
        eachintval.addData(intsum.getMean());
        digitalNet.setDigitalShift(true);
        digitalNet.pointInitialize();
        cnt++;
    } while ( cnt < N );
    List data = List::create(Named("mean")=eachintval.getMean(),
                             Named("absError")=eachintval.absErr(p));
    return data;
}

// [[Rcpp::export(rng = false)]]
List rcppMCIntegration(Function integrand,
                       uint32_t N,
                       int s,
                       int m,
                       double probability)
{
#if defined(DEBUG)
    cout << "N:" << dec << N << endl;
    cout << "s:" << dec << s << endl;
    cout << "m:" << dec << m << endl;
    cout << "probability:" << probability << endl;
#endif
    uint64_t seed = static_cast<uint32_t>(clock());
    mt19937_64 rand(seed);
    uniform_real_distribution<double> dist(0.0, 1.0);
    OnlineVariance eachintval;
    uint32_t cnt = 0;
    int p = probToInt(probability);
    NumericVector nv(s);
    do {
        checkUserInterrupt();
        OnlineVariance intsum;
        uint64_t max = 1;
        max = max << m;
        for (uint64_t j = 0; j < max; ++j) {
            for (int k = 0; k < s; ++k) {
                nv[k] = dist(rand);
            }
            double d = as<double>(integrand(nv));
#if defined(DEBUG)
            //cout << "o:" << o << endl;
            cout << "d:" << d << endl;
#endif
            intsum.addData(d);
        }
        eachintval.addData(intsum.getMean());
        cnt++;
    } while ( cnt < N );
    List data = List::create(Named("mean")=eachintval.getMean(),
                             Named("absError")=eachintval.absErr(p));
    return data;
}

namespace {
    int probToInt(double probability)
    {
        double x = 1.0 - probability;
        if (x < 0.00011) {
            return 9999;
        } else if (x < 0.0011) {
            return 999;
        } else if (x < 0.011) {
            return 99;
        } else {
            return 95;
        }
    }


    /*
     * calculate variance
     *
     * See "Online algorithm" at
     * https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm
     */
    OnlineVariance::OnlineVariance()
    {
        n = 0;
        df = n - 1;
        mean = 0.0;
        M2 = 0.0;
    }

    double OnlineVariance::getMean() const
    {
        return mean;
    }

    double OnlineVariance::unbiasedVar() const
    {
        return M2 / static_cast<double>(df);
    }

    double OnlineVariance::var() const
    {
        if ( n == 1 ) {
            return 0.0;
        }
        return (1.0 -  1.0 / static_cast<double>(n)) * unbiasedVar();
    }

    double OnlineVariance::absErr(const int prob) const
    {
        return tvalue(prob, df) * sqrt(unbiasedVar() / static_cast<double>(n));
    }

    double OnlineVariance::relErr(const int prob) const
    {
        return absErr(prob) / mean;
    }

    const double tval95[100] = {
        INFINITY,
        12.70620473617471,
        4.302652729749464,
        3.182446305283709,
        2.776445105197794,
        2.570581835636316,
        2.446911851144969,
        2.364624251592784,
        2.306004135204168,
        2.262157162798205,
        2.228138851986274,
        2.200985160091639,
        2.178812829667228,
        2.160368656462791,
        2.144786687917803,
        2.131449545559774,
        2.119905299221253,
        2.109815577833314,
        2.100922040241038,
        2.09302405440831,
        2.085963447265865,
        2.079613844727679,
        2.073873067904025,
        2.068657610419046,
        2.063898561628024,
        2.059538552753296,
        2.055529438642873,
        2.051830516480284,
        2.048407141795245,
        2.045229642132703,
        2.042272456301236,
        2.039513446396406,
        2.036933343460098,
        2.034515297449337,
        2.032244509317717,
        2.03010792825034,
        2.028094000980448,
        2.026192463029105,
        2.024394163911966,
        2.02269092003676,
        2.021075390306271,
        2.019540970441374,
        2.018081702818442,
        2.016692199227822,
        2.01536757444376,
        2.014103388880843,
        2.012895598919425,
        2.011740513729767,
        2.010634757624229,
        2.009575237129233,
        2.008559112100758,
        2.007583770315835,
        2.006646805061686,
        2.005745995317864,
        2.004879288188052,
        2.004044783289136,
        2.003240718847872,
        2.002465459291013,
        2.001717484145232,
        2.000995378088259,
        2.000297822014258,
        1.999623584994937,
        1.99897151703338,
        1.998340542520748,
        1.997729654317688,
        1.997137908391998,
        1.996564418952312,
        1.9960083540253,
        1.995468931429831,
        1.994945415107228,
        1.994437111771185,
        1.993943367845623,
        1.993463566661876,
        1.992997125889848,
        1.99254349518093,
        1.992102154002228,
        1.991672609644675,
        1.991254395388376,
        1.990847068811688,
        1.990450210230115,
        1.990063421254447,
        1.98968632345689,
        1.989318557136571,
        1.988959780175168,
        1.98860966697571,
        1.988267907477211,
        1.987934206239023,
        1.987608281589075,
        1.987289864831162,
        1.98697869950627,
        1.986674540703772,
        1.98637715441862,
        1.986086316951114,
        1.985801814345816,
        1.985523441866611,
        1.985251003505502,
        1.984984311522451,
        1.984723186013967,
        1.984467454508484,
        1.984216951586394
    };
    const double tval99[100] = {
        INFINITY,
        63.65674116287158,
        9.92484320091829,
        5.840909309733357,
        4.604094871349994,
        4.032142983555229,
        3.707428021324779,
        3.499483297350493,
        3.355387331333395,
        3.249835541592125,
        3.169272672616958,
        3.105806515539281,
        3.054539589392901,
        3.012275838716581,
        2.976842734370837,
        2.946712883475236,
        2.9207816224251,
        2.89823051967741,
        2.878440472738607,
        2.860934606464975,
        2.84533970978611,
        2.831359558023051,
        2.818756060600139,
        2.80733568377,
        2.796939504774456,
        2.787435813676966,
        2.778714533329683,
        2.770682957122215,
        2.76326245546145,
        2.756385903670604,
        2.749995653567222,
        2.744041919294267,
        2.738481482012186,
        2.733276642350837,
        2.728394367070722,
        2.723805589208087,
        2.719484630450008,
        2.715408721549987,
        2.711557601913079,
        2.707913183517662,
        2.704459267433159,
        2.701181303578525,
        2.698066186219984,
        2.695102079157675,
        2.692278265693025,
        2.689585019374642,
        2.687013492242213,
        2.684555617866529,
        2.682204026950213,
        2.679951973631546,
        2.677793270940843,
        2.675722234110649,
        2.673733630647216,
        2.671822636240999,
        2.669984795734891,
        2.668215988486186,
        2.666512397556062,
        2.664870482241977,
        2.663286953537656,
        2.661758752162956,
        2.660283028855036,
        2.658857126653925,
        2.657478564951159,
        2.656145025099861,
        2.654854337411088,
        2.653604469382921,
        2.652393515028315,
        2.651219685183664,
        2.650081298694729,
        2.648976774388622,
        2.647904623751154,
        2.646863444238384,
        2.645851913159326,
        2.644868782073378,
        2.64391287165309,
        2.642983066967388,
        2.642078313146006,
        2.641197611389271,
        2.640340015292127,
        2.639504627453213,
        2.638690596344182,
        2.63789711341577,
        2.637123410420375,
        2.636368756932125,
        2.635632458047966,
        2.634913852254293,
        2.634212309445641,
        2.633527229082502,
        2.632858038477643,
        2.632204191200006,
        2.631565165587168,
        2.63094046335777,
        2.630329608316278,
        2.629732145142834,
        2.629147638261708,
        2.628575670782746,
        2.628015843510056,
        2.627467774013243,
        2.626931095756376,
        2.626405457280807
    };
    const double tval999[100] = {
        INFINITY,
        636.6192487687195,
        31.59905457644384,
        12.92397863668748,
        8.61030158137928,
        6.868826625881111,
        5.958816178818759,
        5.407882520861724,
        5.041305433373368,
        4.780912585931139,
        4.586893858702645,
        4.436979338234478,
        4.317791283606168,
        4.220831727707082,
        4.14045411273823,
        4.072765195903752,
        4.014996327184011,
        3.965126272119054,
        3.92164582508522,
        3.883405852592038,
        3.849516274930783,
        3.819277164274519,
        3.792130671698386,
        3.767626804311771,
        3.745398619290085,
        3.725143949728602,
        3.706611743480885,
        3.68959171345926,
        3.673906400701251,
        3.659405019466324,
        3.64595863504204,
        3.633456349758311,
        3.621802259867474,
        3.610913007654441,
        3.600715797386407,
        3.59114677581078,
        3.582149701456308,
        3.573674844445201,
        3.565678071580218,
        3.558120081332715,
        3.550965760863278,
        3.54418364297154,
        3.53774544532746,
        3.531625677808036,
        3.525801306487192,
        3.520251464971102,
        3.5149572054818,
        3.509901283449472,
        3.505067970470219,
        3.50044289136738,
        3.496012881811146,
        3.491765863533892,
        3.487690734657202,
        3.483777273038441,
        3.480016050870256,
        3.476398359033575,
        3.472916139929921,
        3.469561927704801,
        3.466328794931011,
        3.46321030495197,
        3.460200469196346,
        3.457293708870371,
        3.454484820512028,
        3.451768944961039,
        3.449141539356402,
        3.446598351821966,
        3.444135398543995,
        3.441748942981201,
        3.439435476979527,
        3.437191703591056,
        3.43501452142082,
        3.432901010344113,
        3.43084841845811,
        3.428854150143901,
        3.426915755130341,
        3.425030918463961,
        3.423197451297198,
        3.42141328241927,
        3.419676450460553,
        3.417985096707856,
        3.416337458476955,
        3.414731862991554,
        3.41316672172466,
        3.41164052516158,
        3.410151837948819,
        3.408699294396417,
        3.407281594302747,
        3.405897499076685,
        3.40454582813179,
        3.403225455530765,
        3.401935306860252,
        3.400674356317186,
        3.399441623991326,
        3.39823617332754,
        3.397057108754526,
        3.395903573468006,
        3.394774747355655,
        3.393669845054,
        3.392588114128191,
        3.391528833363675
    };
    const double tval9999[100] = {
        INFINITY,
        6366.197671315937,
        99.9924998437626,
        28.00013001094897,
        15.54410058154568,
        11.17771007027861,
        9.08234632729399,
        7.884584262416603,
        7.120003882734538,
        6.593682583944785,
        6.211050891290663,
        5.921194162473098,
        5.694465793270489,
        5.51251504959594,
        5.36341304115686,
        5.239088211753351,
        5.133893517546163,
        5.043764976637879,
        4.965706285291434,
        4.897461588862503,
        4.837301152910126,
        4.78387711643666,
        4.736124060964879,
        4.693189001074219,
        4.654381146882377,
        4.619135234934404,
        4.586984347628043,
        4.557539482371515,
        4.530473997385761,
        4.505511631211056,
        4.48241717540962,
        4.460989140783963,
        4.441053938116005,
        4.422461221367341,
        4.405080131644159,
        4.388796245353427,
        4.373509077449665,
        4.359130025631424,
        4.345580667374725,
        4.332791341212695,
        4.320699958497129,
        4.309251003158447,
        4.298394685708726,
        4.288086224457699,
        4.278285232207232,
        4.268955190813545,
        4.260062999285661,
        4.251578583711409,
        4.243474559354365,
        4.235725936988641,
        4.22830986684588,
        4.221205414689356,
        4.214393365408906,
        4.207856050271693,
        4.201577194576874,
        4.195541782955869,
        4.189735939982705,
        4.184146824100817,
        4.178762533171716,
        4.173572020183271,
        4.168565017864455,
        4.163731971136108,
        4.159063976455232,
        4.154552727250363,
        4.150190464752762,
        4.145969933594872,
        4.141884341665224,
        4.137927323737989,
        4.134092908475612,
        4.130375488441361,
        4.126769792813175,
        4.123270862510752,
        4.119874027497455,
        4.116574886036276,
        4.113369285702419,
        4.110253305989199,
        4.107223242347968,
        4.104275591528019,
        4.101407038092006,
        4.098614442002582,
        4.09589482718146,
        4.093245370948945,
        4.090663394268818,
        4.088146352735075,
        4.085691828217225,
        4.083297521129293,
        4.080961243255047,
        4.078680911091212,
        4.076454539657677,
        4.074280236743662,
        4.072156197557194,
        4.070080699731753,
        4.068052098675123,
        4.06606882323561,
        4.064129371642391,
        4.062232307723325,
        4.060376257359947,
        4.058559905177522,
        4.056781991433366,
        4.055041309114493
    };

// prob = 95 or 99 or 999 or 9999
// df is degree of freedom. df = n - 1.
    double tvalue(const int prob, const int df)
    {
        if ( df < 0 ) {
            string message = "Error in tvalue()\n";
            message += "df must be >= 0, but now, df = ";
            message += df;
            //throw invalid_argument(message.c_str());
            Rcpp::stop(message);
        }
        const double * array;
        switch ( prob ) {
        case 95:
            array = tval95;
            break;
        case 99:
            array = tval99;
            break;
        case 999:
            array = tval999;
            break;
        case 9999:
            array = tval9999;
            break;
        default:
            string message = "Error in tvalue()\n";
            message += "prob = ";
            message += prob;
            message += " is not supported.";
            //throw invalid_argument(message.c_str());
            Rcpp::stop(message);
        }

        if (df <= 99) {
            return array[df];
        } else {
            return array[99];
        }
    }
}
