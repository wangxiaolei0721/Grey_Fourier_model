%% clear data and figure
clc;
clear;
close all;
tic
load simulation_case1.mat;
%% model setting
omega=pi/6; % angular frequency
order=2;
train=60; % train data
test=12; % test为test步长
x_sim_train=xrep(1:train,:);
%% figure setting
fig=figure('unit','centimeters','position',[10,10,30,10],'PaperPosition',[0, 0, 30,10],'PaperSize',[30,10]);
pos=[0.07,0.13,0.2,0.80; 0.31,0.13,0.2,0.80;0.55,0.13,0.2,0.80;0.79,0.13,0.2,0.80  ];
titles=["\sigma=0.10";"\sigma=0.20";"\sigma=0.30";"\sigma=0.40"];
ylims=[2,20;2,20;2,20;2,20];
%% begin loop
for i=1:dev_length  % error deviation
    x_sim_train=x_sim{i}(1:train,:);
    for k=1:rep  % repetitions
        x_pre_test(:,k)=GFM_linear_integral(x_sim_train(:,k),omega,order,test);
    end
    x_pre_mean=mean(x_pre_test,2);
    axes('position',pos(i,:));
    plot(time,x_pre_test,'color',[180, 180, 180]/255,'LineWidth',1.5,'HandleVisibility',"off")
    hold on;
    plot(time,x_pre_mean,'color',[180, 180, 180]/255,'LineWidth',5)
    plot(time,x_pre_mean,'color',[217, 83, 25, 200]/255,'LineWidth',1.5)
    plot(time,x,'color',[0, 0, 0, 255]/255,'MarkerSize',4,'Marker','*','LineStyle','none')
    title(titles(i),'FontWeight','bold','FontSize',16);
    xlabel("time t",'FontSize',14);
    if i==1 % ylabel
        text(-17,8,['time series'],'Rotation',90,'FontName','Book Antiqua','FontSize',14)
    end
    if i==1
        legend(["interval of predicted data","average of predicted data ","actual data"],'FontSize',8,'Location','northwest')
    end
    grid on
    xline(60,'--','HandleVisibility','off')
    text(32,3.6,"training",'FontSize',12,'FontName','Book Antiqua')
    set(gca,'FontName','Book Antiqua','FontSize',12,'Box','on','XLim',[-2,75],'YLim',ylims(i,:))
end
%% annotation
annotation(fig,'arrow',[0.23 0.21],[0.20 0.20]);
annotation(fig,'arrow',[0.23 0.21]+0.24,[0.20 0.20]);
annotation(fig,'arrow',[0.23 0.21]+0.48,[0.20 0.20]);
annotation(fig,'arrow',[0.23 0.21]+0.72,[0.20 0.20]);
%%
savefig(fig,'figure\sim2.fig');
exportgraphics(gcf,'F:\博士\Grey Fourier model\Grey Fourier model\figure\sim2.pdf')
time=toc