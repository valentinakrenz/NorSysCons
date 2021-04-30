%trial-unique
%%EOS
try
    delete(workers);
catch
end
workers=parpool(3);%change to number of available kernels
radius=3;
parfor sj=1:104
    compTime= Compute_trialUnique_EOS_Searchlight_fishertransformedr(sj,radius) 
end
delete(workers);
%%ENS
try
    delete(workers);
catch
end
workers=parpool(3);%change to number of available kernels
radius=3;
parfor sj=1:104
    compTime= Compute_trialUnique_ENS_Searchlight_fishertransformedr(sj,radius) 
end
delete(workers);

%cross-trial
%%EOS
try
    delete(workers);
catch
end
workers=parpool(3);%change to number of available kernels
radius=3;
parfor sj=1:104
    compTime= Compute_crossTrial_EOS_Searchlight_fishertransformedr(sj,radius) 
end
delete(workers);
%%ENS
try
    delete(workers);
catch
end
workers=parpool(3);%change to number of available kernels
radius=3;
parfor sj=1:104
    compTime= Compute_crossTrial_ENS_Searchlight_fishertransformedr(sj,radius) 
end
delete(workers);