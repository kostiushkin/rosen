%
% more_robots.erl
%
% ----------------------------------------------------------------------
%
%  ROSEN, a RObotic Simulation Erlang eNgine
%  Copyright (C) 2008 Corrado Santoro (csanto@diit.unict.it)
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
% $Id: more_robots.erl,v 1.3 2008/07/30 17:57:12 slackydeb Exp $
%
-module(more_robots).
-export([go/0]).

-include("geometry.hrl").
-include("robot.hrl").

-define (ROSEN_OPTIONS, [{collision, [object_not_in_a_compound_list,
                                      to_object_dict,
                                      add_mesh,
                                      add_aabb,
                                      to_object_couples,
                                      broad_phase_aabbs_intersect,
                                      narrow_phase_trimeshes_intersect,
                                                %clean_collision_dict,
                                                %set_objects_collision_points,
                                      from_collision_dict_to_object_list,
                                      response_set_previous]}]).

-define(WORLD_WIDTH, ?M(1)).
-define(WORLD_HEIGHT, ?M(2)).
-define(WORLD_COLOR, ?RGB(0.25, 1, 0.25)).

-define(WALLS_HEIGHT, ?CM(4)).
-define(WALLS_TICKNESS, ?CM(1)).
-define(WALLS_COLOR, ?RGB(1, 1, 1)).

-define(ROBOT_SIZE, ?CM(10)).
-define(WHEEL_RADIUS, ?ROBOT_SIZE / 10).
-define(ROBOT_COLOR, ?RGBA(0.1, 0.1, 1, 0.1)).

                                                %TODO: try to change
                                                %parameters in sensors
                                                %(90...)

go() ->
    %% rosen
    rosen:start_link (?ROSEN_OPTIONS),
                                                %rosen:start_link([fullscreen]),
    rosen:zoom (-?CM(300)),
    rosen:up (?M(11)),

    %% world
    %%
    %% at the moment no walls
    {ok, WorldPid} = rosen_world:start_link(#world { name = campo,
                                               width = ?WORLD_WIDTH,
                                               height = ?WORLD_HEIGHT,
                                               color = ?WORLD_COLOR}),

    %% add walls
    rosen_world:wall (
      WorldPid,
      %% X, Y, Width, Height, Tickness, Orientation
      ?M(0), ?WORLD_HEIGHT/2, ?WORLD_WIDTH, ?WALLS_HEIGHT, ?WALLS_TICKNESS, 0,
      ?WALLS_COLOR),

    rosen_world:wall (
      WorldPid,
      %% X, Y, Width, Height, Tickness, Orientation
      ?M(0), -?WORLD_HEIGHT/2, ?WORLD_WIDTH, ?WALLS_HEIGHT, ?WALLS_TICKNESS, 0,
      ?WALLS_COLOR),

    rosen_world:wall (
      WorldPid,
      %% X, Y, Width, Height, Tickness, Orientation
      ?WORLD_WIDTH/2, ?M(0), ?WORLD_HEIGHT, ?WALLS_HEIGHT, ?WALLS_TICKNESS, 90,
      ?WALLS_COLOR),

    rosen_world:wall (
      WorldPid,
      %% X, Y, Width, Height, Tickness, Orientation
      -?WORLD_WIDTH/2, ?M(0), ?WORLD_HEIGHT, ?WALLS_HEIGHT, ?WALLS_TICKNESS, 90,
      ?WALLS_COLOR),

    %% robot materazzi
    %%
    %% at the moment no sensors: could it be possible to add them
    %% here?
    {ok, MaterazziPid} =
        robot:start_link (
          #robot { type = two_wheels_driving,
                   name = materazzi,
                   structure = [#object3d { type = cube,
                                            size = ?ROBOT_SIZE,
                                            position = ?VECTOR(0,
                                                               ?ROBOT_SIZE/2,
                                                               ?WHEEL_RADIUS),
                                            color = ?ROBOT_COLOR}],
                   wheel_radius = ?WHEEL_RADIUS,
                   wheel_distance = ?ROBOT_SIZE,
                   wheel_ticks = 100,
                   world = WorldPid}),

    %% add sensors
    robot:add_sensor (
      materazzi,
      #sensor { type = contact_point,
                sensor_name = front_contact,
                position = ?VECTOR(?ROBOT_SIZE, 0, ?WHEEL_RADIUS),
                parameters = 0,
                color = ?RGB(0, 0, 0.75)}),

    robot:add_sensor (
      materazzi,
      #sensor { type = gp2d12,
                sensor_name = distance_left,
                position = ?VECTOR(?ROBOT_SIZE, -?ROBOT_SIZE/2 , ?WHEEL_RADIUS),
                parameters = 0,
                color = ?RGB(0.75, 0, 0)}),

    robot:add_sensor (
      materazzi,
      #sensor { type = gp2d12,
                sensor_name = distance_right,
                position = ?VECTOR (?ROBOT_SIZE, ?ROBOT_SIZE/2 , ?WHEEL_RADIUS),
                parameters = 0.0,
                color = ?RGB(0, 0.75, 0)}),

    %% add activity
    %%
    %% object_name in robot, if not specified, is automatically added
    %% as <name>_object
    object3d:add_activity (
      materazzi_object, obstacle_avoid, [{robot, MaterazziPid}]),

    %% robot gattuso
    %%
    %% at the moment no sensors: could it be possible to add them
    %% here?
    {ok, GattusoPid} =
        robot:start_link (
          #robot { type = two_wheels_driving,
                   name = gattuso,
                   structure = [#object3d { type = cube,
                                            size = ?ROBOT_SIZE * 2,
                                            position = ?VECTOR (0,
                                                                ?ROBOT_SIZE/2,
                                                                ?WHEEL_RADIUS),
                                            color = ?ROBOT_COLOR}],
                   wheel_radius = ?WHEEL_RADIUS,
                   wheel_distance = ?ROBOT_SIZE,
                   wheel_ticks = 100,
                   world = WorldPid}),

    %% add sensors
    robot:add_sensor (
      gattuso,
      #sensor { type = contact_point,
                sensor_name = front_contact,
                position = ?VECTOR(?ROBOT_SIZE, 0, ?WHEEL_RADIUS),
                parameters = 0,
                color = ?RGB(0, 0, 0.75)}),

    robot:add_sensor (
      gattuso,
      #sensor { type = gp2d12,
                sensor_name = distance_left,
                position = ?VECTOR(?ROBOT_SIZE, -?ROBOT_SIZE/2 , ?WHEEL_RADIUS),
                parameters = 0,
                color = ?RGB(0.75, 0, 0)}),

    robot:add_sensor (
      gattuso,
      #sensor { type = gp2d12,
                sensor_name = distance_right,
                position = ?VECTOR (?ROBOT_SIZE, ?ROBOT_SIZE/2 , ?WHEEL_RADIUS),
                parameters = 0,
                color = ?RGB(0, 0.75, 0)}),

    robot:command (gattuso, {set_position, 0.0, -?CM(45), 90.0}),
%    robot:command (gattuso, {set_position, 0.0, -?CM(105) + ?CM(6), 90.0}),

    %% add activity
    %%
    %% object_name in robot, if not specified, is automatically added
    %% as <name>_object
    object3d:add_activity (
      gattuso_object,
      obstacle_avoid, [{robot, GattusoPid}]),

    ok.
