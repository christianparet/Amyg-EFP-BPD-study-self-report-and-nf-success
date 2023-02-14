%% Aggregate EFP training timecourse data
% Christian Paret, ZI Mannheim, 2022

% The script reads the logfiles that were written during EFP training
% sessions and calculates several success measures. An output text file is
% written with the session-wise success metrics per participant for further
% statistical analysis.

clear

%% basic settings
srcdata_path = uigetdir(pwd,'Select sourcedata directory.'); 
outpath = uigetdir(pwd,'Select output directory for aggregated subject data.');
% Use this option to run through all subjects:
subj_list = dir([srcdata_path,filesep,'EFP*']); 
% Or list subjects to be analyzed here:
% subj_list(1).name = 'EFP01';
% subj_list(2).name = 'EFP02'; % and so on...

% initiate variables
% lower case variables for sessionwise output
subjectID = [];
sessionID = [];
fixed_threshold = [];
success_rate = [];
success_index = [];
personal_effect_size = [];
common_language_effect_size = [];
mean_efp_neurofeedback = [];
mean_efp_baseline = [];
mean_vol_neurofeedback = [];

% upper case variables for blockwise output
SubjectID = [];
SessionID = [];
BlockID = [];

%%
for i = 1:length(subj_list)
    training_list = dir(fullfile(srcdata_path,subj_list(i).name,'\ses-training*')); % scan number of training-data files
    sess_counter = 0; % counts number of blocks per session
    for j = 1:length(training_list)
        training_dir = fullfile(srcdata_path,subj_list(i).name,training_list(j).name); % initiate training-data file
        logfile = dir(fullfile(training_dir,'EFP*.log')); % identify logfile
        try
            fileID = fopen(fullfile(training_dir,logfile.name)); % try to open logfile
            C = textscan(fileID,'%s %s','Delimiter','\t');
            fclose(fileID);
            
            % delete any variables that might mess with procedure below
            clear base_block mean_base_blockwise efp_feedback_block pooledsd pers_es_blockwise mean_efp_blockwise cles_blockwise si_blockwise fxth H sr pers_es si_mean mean_base mean_nf mean_base_blockwise pers_es_blockwise mean_efp_blockwise si_blockwise sr
            
            % initiate variables and count upwards
            sess_counter = sess_counter + 1;
            feedback = 0; % if feedback=0 then write values to baseline
            ba_efp = [];
            fe_efp = [];
            fe_vol = [];
            block_counter = 1;
            for k = 3:length(C{1,1}) % go throug logfile data, identify relevant events and perform actions
                if contains(C{1,1}(k),'NF') % this is a neurofeedback block
                    feedback = 1;
                    base_block(block_counter,:) = ba_efp; % write baseline data vector to baseline data matrix
                    mean_base_blockwise(block_counter) = nanmean(base_block(block_counter,:));
                    ba_efp = []; % initiate empty baseline data vector
                elseif contains(C{1,1}(k),'BASE') % this is a baseline block
                    feedback = 0;
                    efp_feedback_block(block_counter,:) = fe_efp; % write efp feedback data vector to feedback data matrix
                    vol_feedback_block(block_counter,:) = fe_vol; % write volume feedback data vector to feedback data matrix
                    fe_efp = []; % initiate empty feedback data vector
                    fe_vol = [];
                    pooledsd = (nanstd(efp_feedback_block(block_counter,:))*length(efp_feedback_block(block_counter,:))+ nanstd(base_block(block_counter,:))*length(base_block(block_counter,:)))/((length(efp_feedback_block(block_counter,:))+length(base_block(block_counter,:))-2)); % pooled std from baseline and nf
                    pers_es_blockwise(block_counter) = (nanmean(efp_feedback_block(block_counter,:))-nanmean(base_block(block_counter,:)))/pooledsd; % Personal effect size measure
                    mean_efp_blockwise(block_counter) = nanmean(efp_feedback_block(block_counter,:));
                    mean_vol_blockwise(block_counter) = nanmean(vol_feedback_block(block_counter,:));
                    cles_blockwise(block_counter) = CLES(base_block(block_counter,:),efp_feedback_block(block_counter,:)); % common language effect size
                    block_counter = block_counter+1; % next block comes up
                elseif contains(C{1,1}(k),'Feedback') % write feedback data to data vector, depending on condition (feedback block or baseline block).
                    if feedback
                        fe_vol = [fe_vol str2double(C{1,1}{k,1}(11:end))]; % feedback value is written as text to cell, e.g. "Feedback: 0.66"
                    end
                elseif contains(C{1,1}(k),'Received') % write efp data to data vector, depending on condition (feedback block or baseline block). Note that "Received" denotes the actual efp value in the logfile
                    if feedback
                        fe_efp = [fe_efp str2double(C{1,2}(k))]; 
                    elseif ~feedback
                        ba_efp = [ba_efp str2double(C{1,2}(k))];
                    end
                elseif contains(C{1,1}(k),'Success index') % write out success index determined on-the-fly during neurofeedback
                    dum = C{1,1}{k}(strfind(C{1,1}{k},'('):strfind(C{1,1}{k},')'));
                    si_blockwise(block_counter) = str2double(dum(2:end-1));
                elseif contains(C{1,1}(k),'closing') % experiment is over
                    efp_feedback_block(block_counter,:) = [fe_efp NaN]; % final data point appears to be missing, fill vector with nan
                    mean_efp_blockwise(block_counter) = nanmean(efp_feedback_block(block_counter,:));

                    vol_feedback_block(block_counter,:) = fe_vol;
                    mean_vol_blockwise(block_counter) = nanmean(vol_feedback_block(block_counter,:));

                    pooledsd = (nanstd(efp_feedback_block(block_counter,:))*length(efp_feedback_block(block_counter,:))+ nanstd(base_block(block_counter,:))*length(base_block(block_counter,:)))/((length(efp_feedback_block(block_counter,:))+length(base_block(block_counter,:))-2)); % pooled std from baseline and nf
                    pers_es_blockwise(block_counter) = (nanmean(efp_feedback_block(block_counter,:))-nanmean(base_block(block_counter,:)))/pooledsd; % Personal effect size
                    
                    cles_blockwise(block_counter) = CLES(base_block(block_counter,:),efp_feedback_block(block_counter,:)); % common language effect size
                    
                    % calculate session success metrics
                    [fxth,~] = ttest2(mean(base_block'),mean(efp_feedback_block'),'tail','right','dim',2); % fixed threshold: test significance mean base vs mean nf one-sided t-test (p<0.05)
                    [H,~] = ttest2(base_block,efp_feedback_block,'tail','right','dim',2); % test significance for each baseline-nf pair with one-sided t-test (p<0.05), for success rate
                    sr = (sum(H)/length(H))*100; % success rate defined as percent significant blocks
                    
                    % save session-wise data to arrays
                    subjectID = [subjectID; subj_list(i).name];
                    sessionID = [sessionID; sess_counter];
                    fixed_threshold = [fixed_threshold; fxth];
                    success_rate = [success_rate; sr];
                    
                    % save block-wise data to arrays
                    SubjectID = [SubjectID; repmat(subj_list(i).name,length(si_blockwise),1)];
                    SessionID = [SessionID; repmat(sess_counter,length(si_blockwise),1)];
                    BlockID = [BlockID; (1:length(si_blockwise))'];
                    success_index = [success_index; si_blockwise'];
                    personal_effect_size = [personal_effect_size; pers_es_blockwise'];
                    common_language_effect_size = [common_language_effect_size; cles_blockwise'];
                    mean_efp_neurofeedback = [mean_efp_neurofeedback; mean_efp_blockwise'];
                    mean_vol_neurofeedback = [mean_vol_neurofeedback; mean_vol_blockwise'];
                    mean_efp_baseline = [mean_efp_baseline; mean_base_blockwise'];
                end
            end
        catch
            fprintf([subj_list(i).name,' ',training_list(j).name,': logfile is missing\n'])
        end
    end
end

sessionwise_events = table(subjectID, sessionID, fixed_threshold, success_rate);
writetable(sessionwise_events,fullfile(outpath,'Pooleddata_sessionwise_task-efpnftraining.txt'),'Delimiter','tab');   

sessionwise_events = table(SubjectID, SessionID, BlockID, success_index, personal_effect_size, common_language_effect_size, mean_efp_neurofeedback, mean_efp_baseline, mean_vol_neurofeedback);
writetable(sessionwise_events,fullfile(outpath,'Pooleddata_blockwise_task-efpnftraining.txt'),'Delimiter','tab');

fprintf('Finished processing, output written to output path.\n')
    
    
            

