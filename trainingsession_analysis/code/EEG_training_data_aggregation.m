%% Aggregate EFP training timecourse data
% Christian Paret, ZI Mannheim, 2022-2023

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
pes_across_session = [];
cles_across_session = [];
mean_vol_across_session = [];
personal_effect_size = []; % =PES
common_language_effect_size = []; % =CLES
mean_efp_neurofeedback = [];
mean_efp_baseline = [];
mean_vol_neurofeedback = [];

% upper case ID variables for blockwise output
SubjectID = [];
SessionID = [];
BlockID = [];

% variables for subjectwise output
SubjectID2 = [];
pes_first_session = [];
pes_last_session = [];
pes_initial_2_sessions = [];
pes_final_2_sessions = [];
cles_first_session = [];
cles_last_session = [];
cles_initial_2_sessions = [];
cles_final_2_sessions = [];
mean_vol_first_session = [];
mean_vol_last_session = [];
mean_vol_initial_2_sessions = [];
mean_vol_final_2_sessions = [];
pes_last_vs_first = [];
cles_last_vs_first = [];
mean_vol_last_vs_first = [];
pes_final_vs_initial = [];
cles_final_vs_initial = [];
mean_vol_final_vs_initial = [];

%% Collect individual data in arrays

for i = 1:length(subj_list)

    % initiate variables to be updated with every subject
    training_list = dir(fullfile(srcdata_path,subj_list(i).name,'\ses-training*')); % scan number of training-data files
    sess_counter = 0; % counts number of blocks per session
    pes_first = 999; % initiate with missing value 999
    cles_first = 999;
    mean_vol_first = 999;
    pes_last = 999; % initiate with missing value 999
    cles_last = 999;
    mean_vol_last = 999;
    pes_initial = 999; % initiate with missing value 999
    cles_initial = 999;
    mean_vol_initial = 999;
    pes_final = 999; % initiate with missing value 999
    cles_final = 999;
    mean_vol_final = 999;

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

                    ba_efp(find(ba_efp>=10))=NaN; % exclude extreme values
                    ba_efp(find(ba_efp<=-10))=NaN;

                    base_block(block_counter,:) = ba_efp; % write baseline data vector to baseline data matrix
                    mean_base_blockwise(block_counter) = mean(base_block(block_counter,:),"omitnan");
                    ba_efp = []; % initiate empty baseline data vector
                elseif contains(C{1,1}(k),'BASE') % this is a baseline block
                    feedback = 0;                 
                    
                    fe_vol(find(fe_efp>=10))=NaN; % exclude extreme values
                    fe_vol(find(fe_efp<=-10))=NaN;
                    fe_efp(find(fe_efp>=10))=NaN;
                    fe_efp(find(fe_efp<=-10))=NaN;

                    efp_feedback_block(block_counter,:) = fe_efp; % write efp feedback data vector to feedback data matrix
                    vol_feedback_block(block_counter,:) = fe_vol; % write volume feedback data vector to feedback data matrix
                    fe_efp = []; % initiate empty feedback data vector
                    fe_vol = [];
                    
                    pooledsd = (...
                                std(efp_feedback_block(block_counter,:),"omitnan") * sum(~isnan(efp_feedback_block(block_counter,:))) + ...
                                std(base_block(block_counter,:),"omitnan")         * sum(~isnan(base_block(block_counter,:)))...
                                )/...
                                (...
                                (sum(~isnan(efp_feedback_block(block_counter,:)))  + sum(~isnan(base_block(block_counter,:))) -2 )...
                                ); % pooled std from baseline and nf

                    pers_es_blockwise(block_counter) = (mean(efp_feedback_block(block_counter,:),"omitnan")-mean(base_block(block_counter,:),"omitnan"))/pooledsd; % Personal effect size measure
                    mean_efp_blockwise(block_counter) = mean(efp_feedback_block(block_counter,:),"omitnan");
                    mean_vol_blockwise(block_counter) = mean(vol_feedback_block(block_counter,:),"omitnan");
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
                    mean_efp_blockwise(block_counter) = mean(efp_feedback_block(block_counter,:),"omitnan");

                    vol_feedback_block(block_counter,:) = fe_vol;
                    mean_vol_blockwise(block_counter) = mean(vol_feedback_block(block_counter,:),"omitnan");
                   
                    pooledsd = (...
                                std(efp_feedback_block(block_counter,:),"omitnan") * sum(~isnan(efp_feedback_block(block_counter,:))) + ...
                                std(base_block(block_counter,:),"omitnan")         * sum(~isnan(base_block(block_counter,:)))...
                                )/...
                                (...
                                (sum(~isnan(efp_feedback_block(block_counter,:)))  + sum(~isnan(base_block(block_counter,:))) -2 )...
                                ); % pooled std from baseline and nf
                    
                    pers_es_blockwise(block_counter) = (mean(efp_feedback_block(block_counter,:),"omitnan")-mean(base_block(block_counter,:),"omitnan"))/pooledsd; % Personal effect size
                    
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
                    pes_across_session = [pes_across_session; mean(pers_es_blockwise)];
                    cles_across_session = [cles_across_session; mean(cles_blockwise)];
                    mean_vol_across_session = [mean_vol_across_session; mean(mean_vol_blockwise)];
                    
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

                    % collect data from sessions at beginning and at the end of training
                    if length(training_list)>=4 % measures make sense only in case there are 4 or more sessions available
                        if j==1 % enter if this is the first session of subject
                            pes_first = mean(pers_es_blockwise);
                            cles_first = mean(cles_blockwise);
                            mean_vol_first = mean(mean_vol_blockwise);
                        elseif j==2 % average of initial two sessions as alternative variable
                            pes_initial = mean([pes_first, mean(pers_es_blockwise)]);
                            cles_initial = mean([cles_first, mean(cles_blockwise)]);
                            mean_vol_initial = mean([mean_vol_first, mean(mean_vol_blockwise)]);
                        elseif j==length(training_list)-1 % enter if this is the secondlast session of subject
                            pes_secondlast = mean(pers_es_blockwise);
                            cles_secondlast = mean(cles_blockwise);
                            mean_vol_secondlast = mean(mean_vol_blockwise);
                        elseif j==length(training_list) % enter if this is the last session of subject
                            pes_final = mean([pes_secondlast, mean(pers_es_blockwise)]);
                            cles_final = mean([cles_secondlast, mean(cles_blockwise)]);
                            mean_vol_final = mean([mean_vol_secondlast, mean(mean_vol_blockwise)]);
                            pes_last = mean(pers_es_blockwise);
                            cles_last = mean(cles_blockwise);
                            mean_vol_last = mean(mean_vol_blockwise);
                        end  
                    end

                end
            end
            fprintf([subj_list(i).name,' ',training_list(j).name,' done\n'])
        catch
            fprintf([subj_list(i).name,' ',training_list(j).name,': logfile is missing\n'])
        end
    end

    % fill subjectwise variable, calculate difference scores for correlation analysis
    SubjectID2 = [SubjectID2; subj_list(i).name];

    pes_first_session = [pes_first_session; pes_first];
    pes_last_session = [pes_last_session; pes_last];
    pes_initial_2_sessions = [pes_initial_2_sessions; pes_initial];
    pes_final_2_sessions = [pes_final_2_sessions; pes_final];

    cles_first_session = [cles_first_session; cles_first];
    cles_last_session = [cles_last_session; cles_last];
    cles_initial_2_sessions = [cles_initial_2_sessions; cles_initial];
    cles_final_2_sessions = [cles_final_2_sessions; cles_final];

    mean_vol_first_session = [mean_vol_first_session; mean_vol_first];
    mean_vol_last_session = [mean_vol_last_session; mean_vol_last];
    mean_vol_initial_2_sessions = [mean_vol_initial_2_sessions; mean_vol_initial];
    mean_vol_final_2_sessions = [mean_vol_final_2_sessions; mean_vol_final];

    if pes_first~=999 && pes_last~=999 && pes_secondlast~=999 
        pes_last_vs_first = [pes_last_vs_first; pes_last-pes_first];
        pes_final_vs_initial = [pes_final_vs_initial; pes_final-pes_initial];
        
        cles_last_vs_first = [cles_last_vs_first; cles_last-cles_first];
        cles_final_vs_initial = [cles_final_vs_initial; cles_final-cles_initial];
        
        mean_vol_last_vs_first = [mean_vol_last_vs_first; mean_vol_last-mean_vol_first];
        mean_vol_final_vs_initial = [mean_vol_final_vs_initial; mean_vol_final-mean_vol_initial];
        
    else % should only be necessary if <4 sessions exist
        pes_last_vs_first = [pes_last_vs_first; 999];
        pes_final_vs_initial = [pes_final_vs_initial; 999];

        cles_last_vs_first = [cles_last_vs_first; 999];
        cles_final_vs_initial = [cles_final_vs_initial; 999];

        mean_vol_last_vs_first = [mean_vol_last_vs_first; 999];
        mean_vol_final_vs_initial = [mean_vol_final_vs_initial; 999];
    end

end

%% Write tab-delimited text files for statistical analysis

sessionwise_events = table(subjectID, sessionID, fixed_threshold, success_rate, pes_across_session, cles_across_session, mean_vol_across_session);
writetable(sessionwise_events,fullfile(outpath,'Pooleddata_sessionwise_task-efpnftraining.txt'),'Delimiter','tab');   

blockwise_events = table(SubjectID, SessionID, BlockID, success_index, personal_effect_size, common_language_effect_size, mean_efp_neurofeedback, mean_efp_baseline, mean_vol_neurofeedback);
writetable(blockwise_events,fullfile(outpath,'Pooleddata_blockwise_task-efpnftraining.txt'),'Delimiter','tab');

subjectwise_diff = table(SubjectID2, pes_first_session, pes_last_session, pes_initial_2_sessions, pes_final_2_sessions, pes_last_vs_first, pes_final_vs_initial,...
                                     cles_first_session, cles_last_session, cles_initial_2_sessions, cles_final_2_sessions, cles_last_vs_first, cles_final_vs_initial,...
                                     mean_vol_first_session, mean_vol_last_session, mean_vol_initial_2_sessions, mean_vol_final_2_sessions, mean_vol_last_vs_first, mean_vol_final_vs_initial);
writetable(subjectwise_diff,fullfile(outpath,'Pooleddata_difference-score_task-efpnftraining.txt'),'Delimiter','tab');   

fprintf('Finished processing, output written to output path.\n')
    
    
            

