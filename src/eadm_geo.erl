%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 国际WSG84坐标系转换为国内GCJ02高德坐标系，火星坐标系
%%%
%%% @end
%%% Created : 2024-07-01 下午2:11
%%%-------------------------------------------------------------------
-module(eadm_geo).
-author("wangcw").

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([wgs84_to_gcj02/1]).

-define(PI, 3.14159265358979323846).
-define(X_PI, 3.0 * ?PI).
-define(A, 6378245.0).
-define(EE, 0.00669342162296594323).

%%====================================================================
%% API 函数
%%====================================================================
%% @doc
%% 转换函数
%% @end
wgs84_to_gcj02({Lng, Lat}) ->
    case out_of_china({Lng, Lat}) of
        true -> {Lng, Lat};
        false ->
            DLat = convertlat(Lng - 105.0, Lat - 35.0),
            DLng = convertlng(Lng - 105.0, Lat - 35.0),
            RadLat = Lat * ?PI / 180.0,
            Magic = math:sin(RadLat),
            Magic = 1 - ?EE * Magic * Magic,
            SqrtMagic = math:sqrt(Magic),
            DLat = (DLat * 180.0) / ((?A * (1 - ?EE)) / (Magic * SqrtMagic) * ?PI),
            DLng = (DLng * 180.0) / (?A / SqrtMagic * math:cos(RadLat) * ?PI),
            MgLat = Lat + DLat,
            MgLng = Lng + DLng,
            {MgLng, MgLat}
    end.

%%====================================================================
%% 内部函数
%%====================================================================
%% @doc
%% 坐标点是否在国内（外国坐标点不在火星系坐标范围内）
%% @end
out_of_china({Lng, Lat}) ->
    Lng > 73.66 andalso Lng < 135.05 andalso Lat > 3.86 andalso Lat < 53.55.

%% @doc
%% 经纬度坐标转换
%% @end
convertlat(Lng, Lat) ->
    Ret = -100.0 + 2.0 * Lng + 3.0 * Lat + 0.2 * Lat * Lat +
        0.1 * Lng * Lat + 0.2 * math:sqrt(math:abs(Lng)),
    Ret + sin_convert(Lng, 6.0, 20.0) * 2.0 / 3.0 +
        sin_convert(Lat, 1.0, 20.0) * 2.0 / 3.0 +
        sin_convert(Lat, 12.0, 160.0, 320.0) * 2.0 / 3.0.

convertlng(Lng, Lat) ->
    Ret = 300.0 + Lng + 2.0 * Lat + 0.1 * Lng * Lng +
        0.1 * Lng * Lat + 0.1 * math:sqrt(math:abs(Lng)),
    Ret + sin_convert(Lng, 6.0, 20.0) * 2.0 / 3.0 +
        sin_convert(Lng, 1.0, 20.0, 40.0) * 2.0 / 3.0 +
        sin_convert(Lng, 12.0, 150.0, 300.0) * 2.0 / 3.0.

%% @doc
%% 正弦计算函数
%% @end
sin_convert(Coord, Div, Amp1) ->
    Amp1 * math:sin(Coord * Div) + Amp1 * math:sin(Coord / Div).

sin_convert(Coord, Div1, Amp1, Amp2) ->
    Amp1 * math:sin(Coord * Div1) + Amp2 * math:sin(Coord / Div1).
