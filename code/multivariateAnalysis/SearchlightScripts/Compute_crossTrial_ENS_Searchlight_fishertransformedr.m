function compTime=Compute_crossTrial_ENS_Searchlight_fishertransformedr(sj,radius)
%Indices per condition
% Item1to60_Encoding1_Inds=1:60;
% Item1to60_Encoding2_Inds=61:120;
% Item1to60_Encoding3_Inds=121:180;
% Item1to60_Old=181:240
% Item1to60_New=361:420;
% Session constants=421:426;

addpath NiftiTools\
dataFolder='.\fmriData\';%change accordingly
SubFolder='\trialwiseGLM\';

SJfolders=dir([dataFolder 'NOR*']);
%for sj=1:1%numel(SJfolders)
sj
tic
betaPath=[dataFolder,SJfolders(sj).name,SubFolder];
betaFiles=dir([betaPath, '*.nii']);
for b=1:numel(betaFiles)
    betaDat=load_nii([betaPath,betaFiles(b).name]);
    if b==1
        BetaMaps=zeros([numel(betaFiles),size(betaDat.img)],'single');
    end
    BetaMaps(b,:,:,:)= betaDat.img;
end
% we are only considering Enc3 and new trials for ENS
SelDat=BetaMaps([121:180 361:420],:,:,:);

% only place SLs within the brain
SLmask=squeeze(std(SelDat))~=0&~squeeze(isnan(std(SelDat)));
[SLindsLinear, LinearInds, NrOfValidVoxelsPerSL]=GenerateAll_SL_Inds(radius,SLmask);

toc
ENSPerSL=NaN(size(SLmask),'single');
HypMat=eye(60);
for sl=1:numel(LinearInds)
    %tic
    slInds=SLindsLinear(:,sl);
    slInds=slInds(~isnan(slInds));
    SL_Patts=SelDat(:,slInds);
     EncNewRDM=corr(SL_Patts(1:60,:)', SL_Patts(61:120,:)');
     ENS=mean(EncNewRDM(HypMat==0));%cross-trial (off-diagonal)
     ENSPerSL(LinearInds(sl))=ENS;
     ENSPerSL_fishertransformed=atanh(ENSPerSL);
end

%%original
save(['SL_outputMaps_crossTrial\ENSPerSL_fishertransformed_Rad' num2str(radius) '_SJ_timages' num2str(sj) '.mat'],'ENSPerSL_fishertransformed','-v7.3');
nii_template=load_nii('beta_0001.nii');
nii_template.img=ENSPerSL_fishertransformed;
save_nii(nii_template,['SL_outputMaps_crossTrial\ENSPerSL_fishertransformed_Rad' num2str(radius) '_SJ_timages' num2str(sj) '.nii']);
compTime=toc