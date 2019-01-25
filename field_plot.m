clearvars; clc
close all

fileID = fopen('diff_final.dat','r');
T = fscanf(fileID, '%f %f %f', [3 Inf]);
fclose(fileID);

T = T.';

gridnum = 4096;

[X,Y] = meshgrid(linspace(0,1,gridnum));
Z = reshape(T(:,3),[gridnum,gridnum]);

surf(X,Y,Z)
colorbar

