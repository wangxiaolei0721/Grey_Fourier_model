%% clear data and figure
clc;
clear;
close all;
%% add path to MATLAB
addpath('..\')
%% parameter setting
omega=pi/6; % angular frequency
FN=[4,3,6,6]; % Fourier order
%% load data
load PM25.mat;
data=[PM25.shanghai,PM25.nanjing,PM25.hangzhou,PM25.hefei];
date=PM25.datetime;
l=length(data);
test=24; % test size
train=l-test;  % training size
%% read data from csv
PM25_HW = readtable("benchmark_other_model_data\PM25_HW.csv",'VariableNamingRule','preserve'); 
PM25_HW.Var1=[];
PM25_HW = table2array(PM25_HW);
PM25_ARIMA = readtable("benchmark_other_model_data\PM25_ARIMA.csv",'VariableNamingRule','preserve'); 
PM25_ARIMA.Var1=[];
PM25_ARIMA = table2array(PM25_ARIMA);
PM25_Net = readtable("benchmark_other_model_data\PM25_Net.csv",'VariableNamingRule','preserve'); 
PM25_Net.Var1=[];
PM25_Net = table2array(PM25_Net);
PM25_LSTM = readtable("benchmark_other_model_data\PM25_LSTM.csv",'VariableNamingRule','preserve'); 
PM25_LSTM.Var1=[];
PM25_LSTM = table2array(PM25_LSTM);
%% figure setting
fig=figure('unit','centimeters','position',[10,5,30,20],'PaperPosition',[0, 0, 30,20],'PaperSize',[30,20]);
pos=[0.08,0.58,0.40,0.38; 0.57,0.58,0.40,0.38;0.08,0.08,0.40,0.38;0.57,0.08,0.40,0.38 ];
tit=["Shanghai";"Nangjing";"Hangzhou";"Hefei"];
xlim=[dateshift(date(1),'start','month',-2),dateshift(date(end),'start','month',2)];
ylim=[0,100;0,140;0,120;0,160];
len={["Actual data","HR","SARIMA","NNAR","LSTM","GFM(1,1,4)"],...
    ["Real data","HR","SARIMA","NNAR","LSTM","GFM(1,1,3)"],...
    ["Real data","HR","SARIMA","NNAR","LSTM","GFM(1,1,6)"],...
    ["Real data","HR","SARIMA","NNAR","LSTM","GFM(1,1,6)"]};
%% begin loop
for i=1:4
    x=data(:,i);
    % x_fit(:,1)= Fourierfit(x(1:train),omega,test,FN(i)); % x is all data
    x_fit(:,1)= PM25_HW(:,i);
    x_fit(:,2)=PM25_ARIMA(:,i);
    x_fit(:,3)=PM25_Net(:,i);
    x_fit(:,4)=PM25_LSTM(:,i);
    x_fit(:,5)=GFM_linear_integral(x(1:60),omega,FN(i),test);
    % mean absolute percetage error
    x_copy=repmat(x,1,5);
    ape=100*abs(x_fit-x_copy)./x_copy;
    mape_fit(i,:)=mean(ape(1:train,:),'omitnan');
    mape_test(i,:)=mean(ape(train+1:train+test,:),'omitnan');
    mape2latex(:,2*i-1)=mape_fit(i,:)';
    mape2latex(:,2*i)=mape_test(i,:)';
     % subplot i
    axes('position',pos(i,:),'Box','on');
    plot(date,x,'Color','k','LineStyle','--','Marker','x','MarkerSize',8,'LineWidth',1.5);
    hold on;
    plot(date,x_fit(:,1),'Marker','none','MarkerSize',8,'LineWidth',1.5);
    plot(date,x_fit(:,2),'LineStyle','-','LineWidth',1.5);
    plot(date,x_fit(:,3),'Marker','none','MarkerSize',4,'LineWidth',1.5);
    plot(date,x_fit(:,4),'Marker','none','MarkerSize',4,'LineWidth',1.5);
    plot(date,x_fit(:,5),'Marker','none','MarkerSize',4,'LineWidth',1.5);
    grid on
    title(tit(i,:),'FontWeight','bold','FontSize',16);
    xlabel({'Month'},'FontSize',14);
    xtickformat('yyyy-MM')
    ylabel(['PM_{2.5} concentration (¦Ìg/m^3)'],'FontSize',12)
    set(gca,'FontName','Book Antiqua','FontSize',12,'YLim',ylim(i,:),'XLim',xlim);
    legend(len{i},'location','northeast','FontSize',10,'NumColumns',2);
    xline(date(train),'--','HandleVisibility','off')
end
%%
%% save figure
savefig(fig,'figure\exam_other.fig');