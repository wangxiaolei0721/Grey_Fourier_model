%% clear data and figure
clc;
clear;
close all;
%% add path to MATLAB
addpath('..\','.\benchmark_grey_model')
%% order setting
omega=pi/6; % angular frequency
FN=[4,3,6,6]; % Fourier order
PN=[1,1,1,1]; % Polynomial order
%% load data
load PM25.mat;
data=[PM25.shanghai,PM25.nanjing,PM25.hangzhou,PM25.hefei];
date=PM25.datetime;
l=length(data);
test=24; % test size
train=l-test;  % training size
%% figure setting
fig=figure('unit','centimeters','position',[10,5,30,20],'PaperPosition',[0, 0, 30,20],'PaperSize',[30,20]);
pos=[0.08,0.58,0.40,0.38; 0.57,0.58,0.40,0.38;0.08,0.08,0.40,0.38;0.57,0.08,0.40,0.38 ];
tit=["Shanghai";"Nangjing";"Hangzhou";"Hefei"];
xlim=[dateshift(date(1),'start','month',-2),dateshift(date(end),'start','month',2)];
ylim=[0,100;0,140;0,120;0,160];
len={["Actual data","GPM(1,1,1)","DGGM(1,1)","GM(1,1|cos,sin)","GFM(1,1,4)"],...
    ["Real data","GPM(1,1,1)","DGGM(1,1)","GMSC(1,1)","GFM(1,1,3)"],...
    ["Real data","GPM(1,1,1)","DGGM(1,1)","GMSC(1,1)","GFM(1,1,6)"],...
    ["Real data","GPM(1,1,1)","DGGM(1,1)","GMSC(1,1)","GFM(1,1,6)"]};
%% begin loop 
for i=1:4 % four cities
    x=data(:,i);
    x_fit(:,1)=GMP11(x(1:train),test,PN(i)); % grey model with polynomial
    x_fit(:,2)=DGGM(x(1:train),omega,test);
    x_fit(:,3)=GM11_Gurcan(x(1:train),omega,test );
    [x_fit(:,4),par]=GFM_linear_integral(x(1:train),omega,FN(i),test);
    % mean absolute percetage error
    x_copy=repmat(x,1,4);
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
    grid on
    title(tit(i,:),'FontWeight','bold','FontSize',16);
    xlabel({'Month'},'FontSize',14);
    xtickformat('yyyy-MM')
    ylabel(['PM_{2.5} concentration (¦Ìg/m^3)'],'FontSize',12)
    set(gca,'FontName','Book Antiqua','FontSize',12,'YLim',ylim(i,:),'XLim',xlim);
    if i==1 % legend
        legend(len{i},'location','northeast','FontSize',10,'NumColumns',2);
    end
    xline(date(train),'--','HandleVisibility','off')
end
%% save figure
savefig(fig,'figure\exam_grey.fig');