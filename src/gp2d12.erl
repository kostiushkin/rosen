
%
% gp2d12.erl
%
% ----------------------------------------------------------------------
%
%  ROSEN, a RObotic Simulation Erlang eNgine
%  Copyright (C) 2007 Corrado Santoro (csanto@diit.unict.it)
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%
% $Id: gp2d12.erl,v 1.8 2008/10/26 22:34:54 aristidena Exp $
%
-module (gp2d12).

-include ("geometry.hrl").
-include ("robot.hrl").

-behaviour (gen_activity).

-export ([init/2, step/5, model/2, get_property/3, terminate/2]).

-define (MIN_DIST, ?CM(10)).
-define (MAX_DIST, ?CM(80)).%140)).
-define (DEGREE_ERROR, 1).

%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: model/2
%%====================================================================
%%
model (Position, Color) ->
  X = Position#vector.x,
  Y = Position#vector.y,
  Z = Position#vector.z,
  #object3d { type = cube,
              size = ?CM(1),
              %% position = rosen_world:world_to_3d (?VECTOR (-Y, -X, Z)),
              position = rosen_world:world_to_3d (?VECTOR (Y, -X, Z)),
              color = Color}.

%%====================================================================
%% Func: step/5
%%====================================================================
%%
%%

step (ObjectPid, ObjectState, Time, DeltaTime, Sensor) ->
  Position = Sensor#sensor.position,
  XSensor = Position#vector.x,
  YSensor = Position#vector.y,
  ZSensor = Position#vector.z,

  ThetaSensor = Sensor#sensor.parameters,

  {XRobot, YRobot, ThetaRobot} = robot:command (Sensor#sensor.robot,
                                                {get_real_position}),
  {XS, YS} = rosen_world:robot_to_world ({XRobot, YRobot},
                                   ThetaRobot,
                                   {XSensor, YSensor}),
  ThetaS = geometry:normalize_angle (ThetaSensor + ThetaRobot),

%io:format("ThetaS Sensor Orientation ~p ThetaRobot ~p   ThetaSensor ~p~n",[ThetaS,ThetaRobot, ThetaSensor]),

  SensorLine = geometry:line_for_one_point (XS, YS, ThetaS),

  Walls = rosen_world:get_walls (Sensor#sensor.world),

  Intersections = lists:map (fun (X) ->
                             P = geometry:line_intersect (SensorLine,
                                                          X#wall.line),
                                 {P, X}
                             end, Walls),

  ValidIntersections =
    lists:filter (fun ({{parallel}, _}) -> false;
                      ({{ok, {XI, YI}}, Wall}) ->
                      if
                        ZSensor =< Wall#wall.height ->
                        %  case rosen_world:is_in_front ({XRobot, YRobot},
                         %                         ThetaRobot,
                          %                        {XI, YI}) of
			    case point_in_semiLine (Sensor,
						   {XI, YI},
                                                   {{XS,YS},
						   ThetaS})of
                            false -> false;
                            true -> geometry:point_in_segment ({XI, YI},
                                                               Wall, ?CM(1)) 

                          end;
                        true ->
                          false
                      end
                  end, Intersections),

  Distances = lists:map (fun ({{ok, {XI, YI}}, _}) ->
                             XD = (XI - XS),
                             YD = (YI - YS),
                             XD * XD + YD * YD
                         end, ValidIntersections),

  RetVal =
    if
      Distances == [] ->
        DMin = empty,
        {ok, Sensor#sensor { value = ?MAX_DIST}};
      true ->
        DMin = math:sqrt (lists:min (Distances)),
        if
          DMin < ?MIN_DIST ->
            {ok, Sensor#sensor { value = ?MIN_DIST}};
          DMin > ?MAX_DIST ->
            {ok, Sensor#sensor { value = ?MAX_DIST}};
          true ->
            {ok, Sensor#sensor { value = DMin}}
        end
    end,
  RetVal.



%
point_in_semiLine(Sensor,{Xp,Yp},{{Xs,Ys},Orientation})-> 				 
    OrientationPoint = geometry:to_degrees(math:atan2(Yp-Ys,Xp-Xs)),
    abs(OrientationPoint - geometry:normalize_angle(Orientation))< ?DEGREE_ERROR.
    

%%====================================================================
%% Func: get_property/3
%%====================================================================
%%
%% @spec get_property (Object3d, Property, State) -> term()
%%       Object3d = object_3d()
%%       Property = min_dist | max_dist | range
%%
%% @doc  Retrieve the value of gp2d12 parameters.
%%       Depending on the Property parameter passed (either <i>min_dist</i> or
%%       <i>max_dist</i> or <i>range</i>), this function returns the lowest,
%%       the highest and both distance value, respectively.
%%
%%
get_property (_,min_dist,_) ->
    ?MIN_DIST;
%%
get_property (_,max_dist,_) ->
    ?MAX_DIST;
%%
get_property (_,range,_) ->
    {?MIN_DIST,?MAX_DIST};
%%
get_property (_, _, _) ->
    undefined.

%%====================================================================
%% Func: init/2
%%====================================================================

init (_, Sensor) ->
  {ok, Sensor#sensor { value = ?MAX_DIST,
                       state = 0}}.

%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (_Reason, _State) ->
  ok.
