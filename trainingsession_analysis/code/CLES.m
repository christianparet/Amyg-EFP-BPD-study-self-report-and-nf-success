function f = CLES(BL, NF)
% this function calculates the Common Langauge Effect Size as implemented
% here: https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test#Effect_sizes
% input: 
% BL - baseline samples; NF - regulate samples
% output:
% f - CLES score between 0-1 with the directionality of NF<BL - i.e a score
% above 0.5 means a larger proportion of samples of the NF array are lower than BL.
merge = [BL NF];
n1 = length(BL);
n2 = length(NF);
R = tiedrank(merge);
R1 = nansum(R(1:n1));
U1 = R1 - (n1*(n1+1)/2);
f = U1/(n1*n2);
end