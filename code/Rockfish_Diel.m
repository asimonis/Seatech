%Summary plots of rockfish chorusing

addpath('C:\Users\anne.simonis\Documents\Tethys working folder\Tethys3')
%initialize Tethys
q=dbInit();

%load detections
log = readmatrix('C:\Users\anne.simonis\Documents\GitHub\Seatech\data\2024_Rockfish\RC_MB05_CombinedLog.xlsx');
detections=log(:,5:6);
detections = x2mdate(sort(detections));

%Effort
formatIn =  'dd-mmm-yyyy';
effortStart = datenum(['01-Mar-2023';	 '27-Mar-2023'],formatIn);
effortEnd = datenum(['21-Mar-2023';'30-Mar-2023'],formatIn);

effort=[effortStart, effortEnd];

Time1 = '01-Mar-2023 00:00:01';
Time2 = '31-Mar-2023 23:59:00';
starttime = datenum(Time1); endtime = datenum(Time2);
%
% Set the Latitude and Longitude
% as numbers, not a string (near the Aleutian islands)
Latitude =35.883333; % + for north, - for south
Longitude = 360-121.55; % degrees east
% Determine when the sun is down between start and end times
night = dbDiel(q, Latitude, Longitude, starttime, endtime);

UTCOffset = -7; 

nightH = visPresence(night, 'Color', 'black', 'LineStyle',...
 'none', 'Transparency', .15,'Resolution_m', 1/60, ...
 'DateRange',[starttime, endtime],'UTCOffset', UTCOffset);

%Get Lunar Illumination
interval = 15;
% Set the getDaylight flag; false for no returns in daylight
getDaylight = false;
%
% Use dbGetLunarIllumination to get moon illumination
% returns serial date in column 1, and percent lunar illumination in 2
illu = dbGetLunarIllumination(q, Latitude, Longitude,...
 starttime,endtime, interval, 'getDaylight', getDaylight);

nightH = visPresence(night, 'Color', 'black', 'LineStyle',...
 'none', 'Transparency', .15,'Resolution_m', 1/60, ...
 'DateRange',[starttime, endtime],'UTCOffset', UTCOffset);

% add in the amount of lunar illumination to the plot
lunarH = visLunarIllumination(illu, 'UTCOffset', UTCOffset);

% add detections of selectes species to plot using visPresence.m
NECol = [.7 .7 .7];%'k'; %No Effort Color
speciesH =  visPresence(detections,...
    'DateTickInterval',7,'DateRange',[starttime endtime],'Effort',effort,...
    'NoEffortColor',NECol,'UTCOffset',-7,alpha=.5);
title('MB05:Rockfish Chorusing')

saveas(gcf,'C:\Users\anne.simonis\Documents\GitHub\Seatech\figures\MB05_Rockfish_LunarIllumination.png')
