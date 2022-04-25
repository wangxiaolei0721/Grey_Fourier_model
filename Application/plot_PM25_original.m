clc;
clear;
close all;
%% load data
load PM25.mat;
data=[PM25.shanghai,PM25.nanjing,PM25.hangzhou,PM25.hefei];
date=PM25.datetime;
%% figure setting
fig=figure('unit','centimeters','position',[10,5,30,20],'PaperPosition',[0, 0, 30,20],'PaperSize',[30,20]);
pos=[0.08,0.58,0.40,0.38; 0.57,0.58,0.40,0.38;0.08,0.08,0.40,0.38;0.57,0.08,0.40,0.38  ];
col = [0, 114, 189,255; 125, 46, 142, 255;119, 171, 47,255;217, 83, 24,255]/255;
tit=["Shanghai";"Nangjing";"Hangzhou";"Hefei"];
xlim=[dateshift(date(1),'start','month',-2),dateshift(date(end),'start','month',2)];
ylim=[0,100;0,120;0,100;0,120];
train_position=[80;100;80;100];
%% bigin loop
for i=1:4 % four cities
    x=data(:,i);
    x_fit(:,i)=GM11(x,0);
    % subplot i
    axes('position',pos(i,:),'Box','on');
    semilogy(date,x,'Color',[0, 113, 188,200]/255,'Marker','o','MarkerSize',5,'Linestyle',"-.",'LineWidth',1.5);
    hold on
    semilogy(date,x_fit(:,i),'Color',[216, 82, 24,200]/255,'Marker','.','MarkerSize',10,'Linestyle',"-.",'LineWidth',1.5)
    title(tit(i,:),'FontWeight','bold','FontSize',14);
    xlabel(['Month'],'FontSize',14);
    xtickformat('yyyy-MM')
    ylabel(['PM_{2.5} concentration (μg/m^3)'],'FontSize',12)
    grid on
    set(gca,'FontName','Book Antiqua','FontSize',12,'YLim',ylim(i,:),'XLim',xlim);
    if i==1
        legend(["Actual data","Fitting data by GM(1,1)"],'location','northeast','FontSize',12,'Orientation','horizontal');
    end
end
%% save figure
savefig(fig,'figure\exam0.fig');