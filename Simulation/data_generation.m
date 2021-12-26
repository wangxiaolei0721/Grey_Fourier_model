%% clear data and figure
clc; 
clear; 
close all;
%% parameter setting
omega=pi/6; % angular frequency
order=2; % Fourier order
time=[1:72]'; % time
time_length=length(time);
dev=[0.1;0.2;0.3;0.4]; % error deviation
dev_length=length(dev);
rep=1000; % repetitions
par=[0.01;5;0;1;1;-1;-1]; % model parameter
%% data generation
x=datageneration( omega,order,time,par); % original time series without error
xrep=repmat(x,1,rep);
for i=1:dev_length  % error deviation
    for j=1:rep   % repetitions
        rng(j); % random seed
        noise(:,j)=dev(i)*randn(time_length,1); % random noise
    end
    x_sim{i}=xrep+noise;
end
%% save simulation data
save('simulation_case.mat','x_sim','x')