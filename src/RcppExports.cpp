// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/voteR.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// findICPoints
DataFrame findICPoints(CharacterVector voterID, NumericVector idealPoint, NumericVector altPointVector, double orderScalar, NumericVector salienceVector, double precision);
static SEXP _voteR_findICPoints_try(SEXP voterIDSEXP, SEXP idealPointSEXP, SEXP altPointVectorSEXP, SEXP orderScalarSEXP, SEXP salienceVectorSEXP, SEXP precisionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type voterID(voterIDSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type idealPoint(idealPointSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type altPointVector(altPointVectorSEXP);
    Rcpp::traits::input_parameter< double >::type orderScalar(orderScalarSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type salienceVector(salienceVectorSEXP);
    Rcpp::traits::input_parameter< double >::type precision(precisionSEXP);
    rcpp_result_gen = Rcpp::wrap(findICPoints(voterID, idealPoint, altPointVector, orderScalar, salienceVector, precision));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _voteR_findICPoints(SEXP voterIDSEXP, SEXP idealPointSEXP, SEXP altPointVectorSEXP, SEXP orderScalarSEXP, SEXP salienceVectorSEXP, SEXP precisionSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_voteR_findICPoints_try(voterIDSEXP, idealPointSEXP, altPointVectorSEXP, orderScalarSEXP, salienceVectorSEXP, precisionSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// findParetoSet
DataFrame findParetoSet(SEXP idealPoints);
static SEXP _voteR_findParetoSet_try(SEXP idealPointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type idealPoints(idealPointsSEXP);
    rcpp_result_gen = Rcpp::wrap(findParetoSet(idealPoints));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _voteR_findParetoSet(SEXP idealPointsSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_voteR_findParetoSet_try(idealPointsSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// findSuperElipseRadius
long double findSuperElipseRadius(NumericVector idealPoint, NumericVector altPoint, double orderScalar, NumericVector salienceVector);
static SEXP _voteR_findSuperElipseRadius_try(SEXP idealPointSEXP, SEXP altPointSEXP, SEXP orderScalarSEXP, SEXP salienceVectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericVector >::type idealPoint(idealPointSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type altPoint(altPointSEXP);
    Rcpp::traits::input_parameter< double >::type orderScalar(orderScalarSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type salienceVector(salienceVectorSEXP);
    rcpp_result_gen = Rcpp::wrap(findSuperElipseRadius(idealPoint, altPoint, orderScalar, salienceVector));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _voteR_findSuperElipseRadius(SEXP idealPointSEXP, SEXP altPointSEXP, SEXP orderScalarSEXP, SEXP salienceVectorSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_voteR_findSuperElipseRadius_try(idealPointSEXP, altPointSEXP, orderScalarSEXP, salienceVectorSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// findWinSet
List findWinSet(List indifferenceCurves);
static SEXP _voteR_findWinSet_try(SEXP indifferenceCurvesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< List >::type indifferenceCurves(indifferenceCurvesSEXP);
    rcpp_result_gen = Rcpp::wrap(findWinSet(indifferenceCurves));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _voteR_findWinSet(SEXP indifferenceCurvesSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_voteR_findWinSet_try(indifferenceCurvesSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// isInICFromPointAndIC
LogicalVector isInICFromPointAndIC(SEXP aSexpPoint, SEXP aSexpMatrix);
RcppExport SEXP _voteR_isInICFromPointAndIC(SEXP aSexpPointSEXP, SEXP aSexpMatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type aSexpPoint(aSexpPointSEXP);
    Rcpp::traits::input_parameter< SEXP >::type aSexpMatrix(aSexpMatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(isInICFromPointAndIC(aSexpPoint, aSexpMatrix));
    return rcpp_result_gen;
END_RCPP
}
// isInParetoSetFromPointAndIdeals
LogicalVector isInParetoSetFromPointAndIdeals(SEXP aSexpPoint, SEXP aSexpMatrix);
RcppExport SEXP _voteR_isInParetoSetFromPointAndIdeals(SEXP aSexpPointSEXP, SEXP aSexpMatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type aSexpPoint(aSexpPointSEXP);
    Rcpp::traits::input_parameter< SEXP >::type aSexpMatrix(aSexpMatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(isInParetoSetFromPointAndIdeals(aSexpPoint, aSexpMatrix));
    return rcpp_result_gen;
END_RCPP
}
// isInParetoSetFromPointAndPS
LogicalVector isInParetoSetFromPointAndPS(SEXP aSexpPoint, SEXP aSexpMatrix);
RcppExport SEXP _voteR_isInParetoSetFromPointAndPS(SEXP aSexpPointSEXP, SEXP aSexpMatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type aSexpPoint(aSexpPointSEXP);
    Rcpp::traits::input_parameter< SEXP >::type aSexpMatrix(aSexpMatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(isInParetoSetFromPointAndPS(aSexpPoint, aSexpMatrix));
    return rcpp_result_gen;
END_RCPP
}
// minkowskiDistancePairOfPoints
long double minkowskiDistancePairOfPoints(NumericVector idealVector, NumericVector altVector, double orderScalar, NumericVector salienceVector);
static SEXP _voteR_minkowskiDistancePairOfPoints_try(SEXP idealVectorSEXP, SEXP altVectorSEXP, SEXP orderScalarSEXP, SEXP salienceVectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericVector >::type idealVector(idealVectorSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type altVector(altVectorSEXP);
    Rcpp::traits::input_parameter< double >::type orderScalar(orderScalarSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type salienceVector(salienceVectorSEXP);
    rcpp_result_gen = Rcpp::wrap(minkowskiDistancePairOfPoints(idealVector, altVector, orderScalar, salienceVector));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _voteR_minkowskiDistancePairOfPoints(SEXP idealVectorSEXP, SEXP altVectorSEXP, SEXP orderScalarSEXP, SEXP salienceVectorSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_voteR_minkowskiDistancePairOfPoints_try(idealVectorSEXP, altVectorSEXP, orderScalarSEXP, salienceVectorSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// minkowskiDistanceSets
NumericMatrix minkowskiDistanceSets(NumericMatrix idealsMatrix, NumericMatrix altsMatrix, NumericVector minkoOrderVector, NumericMatrix salienceMatrix);
static SEXP _voteR_minkowskiDistanceSets_try(SEXP idealsMatrixSEXP, SEXP altsMatrixSEXP, SEXP minkoOrderVectorSEXP, SEXP salienceMatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type idealsMatrix(idealsMatrixSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type altsMatrix(altsMatrixSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type minkoOrderVector(minkoOrderVectorSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type salienceMatrix(salienceMatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(minkowskiDistanceSets(idealsMatrix, altsMatrix, minkoOrderVector, salienceMatrix));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _voteR_minkowskiDistanceSets(SEXP idealsMatrixSEXP, SEXP altsMatrixSEXP, SEXP minkoOrderVectorSEXP, SEXP salienceMatrixSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_voteR_minkowskiDistanceSets_try(idealsMatrixSEXP, altsMatrixSEXP, minkoOrderVectorSEXP, salienceMatrixSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// minkowskiUtilitySets
NumericMatrix minkowskiUtilitySets(NumericMatrix idealsMatrix, NumericMatrix altsMatrix, NumericVector minkoOrderVector, NumericVector lossOrderVector, NumericMatrix salienceMatrix);
static SEXP _voteR_minkowskiUtilitySets_try(SEXP idealsMatrixSEXP, SEXP altsMatrixSEXP, SEXP minkoOrderVectorSEXP, SEXP lossOrderVectorSEXP, SEXP salienceMatrixSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type idealsMatrix(idealsMatrixSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type altsMatrix(altsMatrixSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type minkoOrderVector(minkoOrderVectorSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lossOrderVector(lossOrderVectorSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type salienceMatrix(salienceMatrixSEXP);
    rcpp_result_gen = Rcpp::wrap(minkowskiUtilitySets(idealsMatrix, altsMatrix, minkoOrderVector, lossOrderVector, salienceMatrix));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _voteR_minkowskiUtilitySets(SEXP idealsMatrixSEXP, SEXP altsMatrixSEXP, SEXP minkoOrderVectorSEXP, SEXP lossOrderVectorSEXP, SEXP salienceMatrixSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_voteR_minkowskiUtilitySets_try(idealsMatrixSEXP, altsMatrixSEXP, minkoOrderVectorSEXP, lossOrderVectorSEXP, salienceMatrixSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// prefOrderMinko
NumericMatrix prefOrderMinko(NumericMatrix idealsMatrix, NumericMatrix altsMatrix, NumericVector minkoOrderVector, NumericMatrix salienceMatrix, double lossOrderVector);
RcppExport SEXP _voteR_prefOrderMinko(SEXP idealsMatrixSEXP, SEXP altsMatrixSEXP, SEXP minkoOrderVectorSEXP, SEXP salienceMatrixSEXP, SEXP lossOrderVectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type idealsMatrix(idealsMatrixSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type altsMatrix(altsMatrixSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type minkoOrderVector(minkoOrderVectorSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type salienceMatrix(salienceMatrixSEXP);
    Rcpp::traits::input_parameter< double >::type lossOrderVector(lossOrderVectorSEXP);
    rcpp_result_gen = Rcpp::wrap(prefOrderMinko(idealsMatrix, altsMatrix, minkoOrderVector, salienceMatrix, lossOrderVector));
    return rcpp_result_gen;
END_RCPP
}
// sgn
int sgn(float aScalar);
static SEXP _voteR_sgn_try(SEXP aScalarSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< float >::type aScalar(aScalarSEXP);
    rcpp_result_gen = Rcpp::wrap(sgn(aScalar));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _voteR_sgn(SEXP aScalarSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_voteR_sgn_try(aScalarSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// superElipseRadius
long double superElipseRadius(NumericVector idealPoint, NumericVector altPoint, double orderScalar, NumericVector salienceVector);
static SEXP _voteR_superElipseRadius_try(SEXP idealPointSEXP, SEXP altPointSEXP, SEXP orderScalarSEXP, SEXP salienceVectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericVector >::type idealPoint(idealPointSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type altPoint(altPointSEXP);
    Rcpp::traits::input_parameter< double >::type orderScalar(orderScalarSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type salienceVector(salienceVectorSEXP);
    rcpp_result_gen = Rcpp::wrap(superElipseRadius(idealPoint, altPoint, orderScalar, salienceVector));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _voteR_superElipseRadius(SEXP idealPointSEXP, SEXP altPointSEXP, SEXP orderScalarSEXP, SEXP salienceVectorSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_voteR_superElipseRadius_try(idealPointSEXP, altPointSEXP, orderScalarSEXP, salienceVectorSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _voteR_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("DataFrame(*findICPoints)(CharacterVector,NumericVector,NumericVector,double,NumericVector,double)");
        signatures.insert("DataFrame(*findParetoSet)(SEXP)");
        signatures.insert("long double(*findSuperElipseRadius)(NumericVector,NumericVector,double,NumericVector)");
        signatures.insert("List(*findWinSet)(List)");
        signatures.insert("long double(*minkowskiDistancePairOfPoints)(NumericVector,NumericVector,double,NumericVector)");
        signatures.insert("NumericMatrix(*minkowskiDistanceSets)(NumericMatrix,NumericMatrix,NumericVector,NumericMatrix)");
        signatures.insert("NumericMatrix(*minkowskiUtilitySets)(NumericMatrix,NumericMatrix,NumericVector,NumericVector,NumericMatrix)");
        signatures.insert("int(*sgn)(float)");
        signatures.insert("long double(*superElipseRadius)(NumericVector,NumericVector,double,NumericVector)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _voteR_RcppExport_registerCCallable() { 
    R_RegisterCCallable("voteR", "_voteR_findICPoints", (DL_FUNC)_voteR_findICPoints_try);
    R_RegisterCCallable("voteR", "_voteR_findParetoSet", (DL_FUNC)_voteR_findParetoSet_try);
    R_RegisterCCallable("voteR", "_voteR_findSuperElipseRadius", (DL_FUNC)_voteR_findSuperElipseRadius_try);
    R_RegisterCCallable("voteR", "_voteR_findWinSet", (DL_FUNC)_voteR_findWinSet_try);
    R_RegisterCCallable("voteR", "_voteR_minkowskiDistancePairOfPoints", (DL_FUNC)_voteR_minkowskiDistancePairOfPoints_try);
    R_RegisterCCallable("voteR", "_voteR_minkowskiDistanceSets", (DL_FUNC)_voteR_minkowskiDistanceSets_try);
    R_RegisterCCallable("voteR", "_voteR_minkowskiUtilitySets", (DL_FUNC)_voteR_minkowskiUtilitySets_try);
    R_RegisterCCallable("voteR", "_voteR_sgn", (DL_FUNC)_voteR_sgn_try);
    R_RegisterCCallable("voteR", "_voteR_superElipseRadius", (DL_FUNC)_voteR_superElipseRadius_try);
    R_RegisterCCallable("voteR", "_voteR_RcppExport_validate", (DL_FUNC)_voteR_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_voteR_findICPoints", (DL_FUNC) &_voteR_findICPoints, 6},
    {"_voteR_findParetoSet", (DL_FUNC) &_voteR_findParetoSet, 1},
    {"_voteR_findSuperElipseRadius", (DL_FUNC) &_voteR_findSuperElipseRadius, 4},
    {"_voteR_findWinSet", (DL_FUNC) &_voteR_findWinSet, 1},
    {"_voteR_isInICFromPointAndIC", (DL_FUNC) &_voteR_isInICFromPointAndIC, 2},
    {"_voteR_isInParetoSetFromPointAndIdeals", (DL_FUNC) &_voteR_isInParetoSetFromPointAndIdeals, 2},
    {"_voteR_isInParetoSetFromPointAndPS", (DL_FUNC) &_voteR_isInParetoSetFromPointAndPS, 2},
    {"_voteR_minkowskiDistancePairOfPoints", (DL_FUNC) &_voteR_minkowskiDistancePairOfPoints, 4},
    {"_voteR_minkowskiDistanceSets", (DL_FUNC) &_voteR_minkowskiDistanceSets, 4},
    {"_voteR_minkowskiUtilitySets", (DL_FUNC) &_voteR_minkowskiUtilitySets, 5},
    {"_voteR_prefOrderMinko", (DL_FUNC) &_voteR_prefOrderMinko, 5},
    {"_voteR_sgn", (DL_FUNC) &_voteR_sgn, 1},
    {"_voteR_superElipseRadius", (DL_FUNC) &_voteR_superElipseRadius, 4},
    {"_voteR_RcppExport_registerCCallable", (DL_FUNC) &_voteR_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_voteR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
