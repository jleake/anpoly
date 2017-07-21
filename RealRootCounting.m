(* ::Package:: *)

(* ::Subsubtitle:: *)
(*A module to count real roots using sturm sequences. Can yield either total number of real roots or number of real roots with given multiplicities.*)
(**)
(*Also includes functions to compute the squarefree decomposition of a polynomial in any subring of C[[x]]. *)
(**)
(*Possible Improvements:*)
(*-Allow searching in intervals instead of all of real line*)
(*-Implement Yunn's algorithm instead of Musser's algorithm for slightly faster squarefree decomposition (same asymptotics).*)
(*-Implement Subresultants instead of Sturm sequences for better bit complexity.*)
(**)
(*Possible Issues:*)
(*-Could assume nonzero polynomials*)
(*-Could blow up bitcomplexity in integer computations.*)


(*)Takes in a polynomial and returns the sturm sequence (with no zero polynomials)


SturmSequence[p_,var_:x]:=Module[{n=Exponent[p,var],sturm = {p,D[p,x]},rem},If[Exponent[p,var]>0,While[rem=-PolynomialRemainder[sturm[[-2]],sturm[[-1]],var];Exponent[rem,var]>=0,sturm = Append[sturm, rem]];sturm,{p}]]


(*)Takes in a real coeff polynomial and returns the number of distinct real roots


DistinctRealRootCount[p_,var_:x]:=Module[{sturm = SturmSequence[p,var],sturmsigns},sturmsigns = Map[InfinityValues,sturm];InfinitySignChanges@sturmsigns]


(*)Returns a list where the ith entry is the number of real roots with multiplicity i


RealRootMultiplicitiesCount[p_,var_:x]:=Module[{},Map[DistinctRealRootCount[#,var]&,MusserSquareFreeDecomposition[p,var]]]


(*)Returns the number of real roots (counted with multiplicity)


RealRootCount[p_,var_:x]:=Module[{rrmc=RealRootMultiplicitiesCount[p,var]},Total[(Range@Length@rrmc)*rrmc]]


InfinityValues[p_,var_:x]:=Module[{n=Exponent[p,x],leadingsign },If[n==-Infinity, n = 0];leadingsign=Sign[D[p,{x,n}]];leadingsign{(-1)^n,1}]


InfinitySignChanges[signlist_]:=Module[{positivevalues = Map[Last,signlist],negativevalues=Map[First,signlist]},SignChanges@negativevalues-SignChanges@positivevalues]


SignChanges[list_]:=Module[{nozerolist=Select[list,#!=0&]},Total@Table[If[nozerolist[[i]]!=nozerolist[[i-1]],1,0],{i,2,Length@nozerolist}]]
