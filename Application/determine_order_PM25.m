%% clear data and figure
clc;
clear;
close all;
%% add path to MATLAB
addpath('..\')
%% angular frequency setting
omega=pi/6;
order_max=6;
%% load data
load PM25.mat;
data=[PM25.shanghai,PM25.nanjing,PM25.hangzhou,PM25.hefei];
date=PM25.datetime;
l=length(data);
test=12; % test size
val=24; % verification size
train=l-test-val; % training size
%% figure setting
fig=figure('unit','centimeters','position',[10,5,30,10],'PaperPosition',[0, 0, 30,10],'PaperSize',[30,10]);
pos=[0.07,0.13,0.2,0.80; 0.31,0.13,0.2,0.80;0.55,0.13,0.2,0.80;0.79,0.13,0.2,0.80  ];
tit=["(a) Shanghai";"(b) Nangjing";"(c) Hangzhou";"(d) Hefei"];
%% begin
for i=1:4
    x=data(:,i);
    % from 0 to max order
    for order=0:order_max
        x_fit(:,order+1)=GFM_linear_integral(x(1:train),omega,order,val);
    end
    % mean absolute percetage error
    x_copy=repmat(x(1:train+val),1,order_max+1);
    ape=100*abs(x_fit-x_copy)./x_copy;
    mape_fit=mean(ape(1:train,:));
    mape_val=mean(ape(train+1:train+val,:));
    % subplot i
    axes('position',pos(i,:));
    order=[0:order_max];
    plot(order,mape_fit,'Marker','o','MarkerSize',7,'LineWidth',1.5);
    hold on
    plot(order,mape_val,'Marker','^','MarkerSize',7,'LineWidth',1.5);
    grid minor
    title(tit(i),'FontWeight','bold','FontSize',14);
    xlabel(['Fourier order'],'FontSize',12);
    if i==1
        ylabel(['MAPE '],'FontSize',12);
        legend(["MAPE_{F}","MAPE_{V}"],'location','northeast','FontSize',10);
    end
    set(gca,'FontName','Book Antiqua','FontSize',12,'Box','on','Xlim',[-0.5,6.5],'XTick',[0:6])
end
%% save figure
savefig(fig,'figure\exam1.fig');