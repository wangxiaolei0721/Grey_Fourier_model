%% clear data and figure
clc;
clear;
close all;
%% add path to MATLAB
addpath('..\')
%% order setting
omega=pi/6; % angular frequency
FN=[4,3,6,6]; % Fourier order
%% load data
load PM25.mat;
data=[PM25.shanghai,PM25.nanjing,PM25.hangzhou,PM25.hefei];
train=length(data);
predict=24; 
date=PM25.datetime;
tstart = date(1);
date_predict=dateshift(tstart,'start','month',0:train+predict-1)'; % train + predict date
%% figure setting
fig=figure('unit','centimeters','position',[10,5,30,20],'PaperPosition',[0, 0, 30,20],'PaperSize',[30,20]);
pos=[0.08,0.58,0.40,0.38; 0.57,0.58,0.40,0.38;0.08,0.08,0.40,0.38;0.57,0.08,0.40,0.38  ];
col = [0, 114, 189,255; 125, 46, 142, 255;119, 171, 47,255;217, 83, 24,255]/255;
tit=["Shanghai";"Nangjing";"Hangzhou";"Hefei"];
xlim=[dateshift(date_predict(1),'start','month',-2),dateshift(date_predict(end),'start','month',2)];
ylim=[0,100;0,120;0,100;0,120];
train_position=[80;100;80;100];
%% bigin loop
for i=1:4 % four cities
    x=data(:,i);
    x_fit(:,i)=GFM_linear_integral(x(1:train),omega,FN(i),predict);
    % subplot i
    axes('position',pos(i,:),'Box','on');
    plot(date,x,'Color',[0, 113, 188,200]/255,'Marker','o','MarkerSize',5,'Linestyle',"none",'LineWidth',1.5);
    hold on
    plot(date_predict,x_fit(:,i),'Color',[216, 82, 24,200]/255,'Marker','.','MarkerSize',13,'Linestyle',"-.",'LineWidth',1.5)
    title(tit(i,:),'FontWeight','bold','FontSize',14);
    xlabel(['Month'],'FontSize',14);
    xtickformat('yyyy-MM')
    ylabel(['PM_{2.5} concentration (μg/m^3)'],'FontSize',12)
    grid on
    set(gca,'FontName','Book Antiqua','FontSize',12,'YLim',ylim(i,:),'XLim',xlim);
    if i==1
        legend(["Actual data","Predicted data"],'location','north','FontSize',12,'Orientation','horizontal');
    end
    xline(date(train),'--','HandleVisibility','off')
    text(date(train-21),train_position(i),"training",'FontSize',12,'FontName','Book Antiqua')
end
%% annotation
annotation(fig,'arrow',[0.385 0.366],[0.884 0.884]);
annotation(fig,'arrow',[0.385 0.366]+0.491,[0.896 0.896]);
annotation(fig,'arrow',[0.385 0.366],[0.885 0.885]-0.5);
annotation(fig,'arrow',[0.385 0.366]+0.491,[0.896 0.896]-0.5);
%% mean absolute percetage error
mape_test=100*mean(abs(x_fit(1:train,:)-data)./data,'omitnan');
%% save figure
savefig(fig,'figure\exam2.fig');