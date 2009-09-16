%
% rosen_world.erl
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
% $Id: world.erl,v 1.10 2008/08/06 13:33:10 slackydeb Exp $
%
-module (rosen_world).

-include ("geometry.hrl").
-include ("robot.hrl").

-behaviour (gen_server).

-export ([start_link/1,
          paint_floor/6,
          wall/8,
          get_walls/1,
          world_from_3d/1,
          world_to_3d/1,
          world_to_3d/2,
          world_to_3d/3,
          robot_to_world/3,
          world_to_robot/3,
          is_in_front/3,
          get_dimensions/1,
          stop/1,
          add_robot/2]).

-export ([init/1, handle_call/3,terminate/2,handle_cast/2]).


%%====================================================================
%% Func: start_link/1
%%====================================================================
start_link (World = #world{}) ->
  if
    World#world.name == noname ->
      {ok, WorldPid} = gen_server:start_link (?MODULE, World, []);
    true ->
      {ok, WorldPid} = gen_server:start_link ({local, World#world.name},
                                              ?MODULE, World, [])
  end,
  rosen:add_world (WorldPid),
  {ok, WorldPid}.


%%====================================================================
%% Func: paint_floor/6
%%====================================================================
paint_floor (Pid, X, Y, Width, Height, Color) ->
  gen_server:call (Pid, {paint_floor, X, Y, Width, Height, Color}).


%%====================================================================
%% Func: wall/8
%%====================================================================
wall (Pid, X, Y, Width, Height, Tickness, Orientation, Color) ->
  gen_server:call (Pid, {wall, X, Y, Width, Height, Tickness,
                         Orientation, Color}).


%%====================================================================
%% Func: get_walls/1
%%====================================================================
get_walls (Pid) ->
  gen_server:call (Pid, {get_walls}).


%%====================================================================
%% Func: world_to_3d/2
%%====================================================================
world_to_3d (X, Y) ->
  ?VECTOR (X, 0, -Y).


%%====================================================================
%% Func: world_to_3d/1
%%====================================================================
world_to_3d (#vector { x = X, y = Y, z = Z}) ->
  ?VECTOR (X, Z, -Y).


%%====================================================================
%% Func: world_to_3d/3
%%====================================================================
world_to_3d (X, Y, Z) ->
  ?VECTOR (X, Z, -Y).


%%====================================================================
%% Func: world_from_3d/1
%%====================================================================
world_from_3d (V = #vector {}) ->
  {V#vector.x, -V#vector.z}.


%%====================================================================
%% Func: robot_to_world/3
%%====================================================================
robot_to_world ({XR, YR}, Theta, {X, Y}) ->
  RTheta = geometry:to_radiants (Theta),
  COS = math:cos (RTheta),
  SIN = math:sin (RTheta),
  {XR + X * COS - Y * SIN,
   YR + X * SIN + Y * COS}.


%%====================================================================
%% Func: world_to_robot/3
%%====================================================================
world_to_robot ({XR, YR}, Theta, {X, Y}) ->
  RTheta = geometry:to_radiants (Theta),
  COS = math:cos (RTheta),
  SIN = math:sin (RTheta),
  DX = X - XR,
  DY = Y - YR,
  {DX * COS + DY * SIN,
   - DX * SIN + DY * COS}.


%%====================================================================
%% Func: is_in_front/3
%%====================================================================
is_in_front ({XR, YR}, Theta, {X, Y}) ->
  {XRelative, _} = world_to_robot ({XR, YR}, Theta, {X, Y}),
  XRelative > 0.


%%====================================================================
%% Func: get_dimensions/1
%%====================================================================
get_dimensions (Pid) ->
  gen_server:call (Pid, {get_dimensions}).


%%====================================================================
%% Func: stop/1
%%====================================================================
%% @spec stop(Pid::pid()) -> ok
%%
%% @doc Destroys the world.
%%
stop (Pid) ->
  gen_server:cast (Pid, stop).


%%====================================================================
%% Func: add_robot/2
%%====================================================================
add_robot (WorldPid, RobotPid) ->
  gen_server:call (WorldPid, {add_robot, RobotPid}).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
%%
init (World) ->
  Floor = floor (0.0, 0.0,
                 World#world.width, World#world.height,
                 World#world.color),
  {ok, WorldObjectPid} = object3d:new (#object3d { type = compound,
                                                   objects = [Floor] }),
  {ok, World#world { object_pid = WorldObjectPid }}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
%% @private
%%
terminate (_Reason, State) ->
  lists:foreach (fun (RobotPid) ->
                     robot:stop (RobotPid)
                 end,
                 State#world.robots),
  ok.


%%====================================================================
%% Func: handle_cast/2
%%====================================================================
%% @private
%%
handle_cast (stop, State) ->
  {stop, normal, State}.


%%====================================================================
%% Func: handle_call/3
%%====================================================================
%% @private
%%
handle_call ({add_robot, RobotPid}, _, World) ->
  {reply, ok, World#world { robots = [RobotPid | World#world.robots] }};
%%
handle_call ({paint_floor, X, Y, Width, Height, Color}, _, World) ->
  FloorObject = floor (X, Y, Width, Height, Color),
  object3d:add_to_compound (World#world.object_pid, FloorObject),
  {reply, ok, World};
%%
handle_call ({get_walls}, _, World) ->
  {reply, World#world.walls, World};
%%
handle_call ({wall, X, Y, Width, Height, Tickness, Orientation, Color}, _,
             World) ->
  WallObject = #object3d { type = box,
                           width = Width,
                           height = Height,
                           depth = Tickness,
                           position = ?VECTOR (X, 0, -Y),
                           axis = rotate_axis (Orientation),
                           color = Color },
  object3d:add_to_compound (World#world.object_pid, WallObject),


  { {XA, YA}, {XB, YB}, Line } =
    geometry:line_for_one_point (X, Y, Orientation, Width),

  Wall = #wall {center = {X, Y},
                length = Width,
                height = Height,
                orientation = Orientation,
                line = Line,
                point_a = {XA, YA},
                point_b = {XB, YB}},

  {reply, ok, World#world { walls = [Wall | World#world.walls] }};
%%

%%
handle_call ({get_dimensions}, _, World) ->
  {reply, {World#world.width,World#world.height}, World}.


%%
%%
rotate_axis (Angle) ->
  geometry:spheric2cartesian (#spheric { rho = 1.0,
                                         theta = 0.0,
                                         phi = Angle}).

%%
%%
%%
floor (X, Y, Width, Height, Color) ->
  #object3d { type = box,
              width = Width,
              height = 0.0,
              depth = Height,
              position = ?VECTOR (X, 0, -Y),
              color = Color,
              relative_meshes = []}.
%%
%%
%%
