%
% collision_test.erl
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
% $Id: collision_test.erl,v 1.7 2008/10/21 22:46:52 aristidena Exp $
%
%% @doc Module providing collision tests.
%%

-module (collision_test).
-include ("geometry.hrl").

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

-export([t1/0,
         t2/0,
         t3/0,
         t11/0,
         t12/0,
         t13/0,
         t14/0,
         t15/0,
         t21/0,
         t22/0]).


%% 2 intersecting rotating cubes
t1 () ->
    rosen:start_link(?ROSEN_OPTIONS),

    object3d:new (#object3d { type = cube,
                              name = cube1,
                              color = ?RGB(0.0, 0.5, 0.0),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, ?CM(26)) }),
    object3d:add_activity (cube1, kinematics,
                           [{v, 0}, {omega, ?DEG_S(40)}, {plane, xz}]),

    object3d:new (#object3d { type = cube,
                              name = cube2,
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, -?CM(26)) }),
    object3d:add_activity (cube2, kinematics,
                          [{v, 0}, {omega, -?DEG_S(35)}, {plane, xz}]),

    ok.


%% 2 intersecting rotating cones
t2 () ->
    rosen:start_link(?ROSEN_OPTIONS),

    object3d:new (#object3d { type = cone,
                              name = cone1,
                              color = ?RGB(0.0, 0.5, 0.0),
                              base_radius = ?CM(25),
                              top_radius = 0.0,
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, ?CM(26)) }),
    object3d:add_activity (cone1, kinematics,
                           [{v, 0}, {omega, ?DEG_S(40)}, {plane, xz}]),

    object3d:new (#object3d { type = cone,
                              name = cone2,
                              base_radius = ?CM(25),
                              top_radius = 0.0,
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, -?CM(26)) }),
    object3d:add_activity (cone2, kinematics,
                           [{v, 0}, {omega, -?DEG_S(35)}, {plane, xz}]),

    ok.


%% 2 intersecting rototraslating compounds
t3 () ->
    rosen:start_link (?ROSEN_OPTIONS),
    rosen:zoom (-?M(5)),
    rosen:up (?M(10)),

    object3d:new (#object3d { type = surface,
                              name = mybox,
                              width = ?CM(400),
                              depth = ?CM(400),
                              position = ?VECTOR (0.0, -?CM(13), 0.0),
                              color = ?RGB (0.25, 0.25, 0.25)}),

    Base = #object3d { type = cube,
                       size = ?CM(20) },
    LeftW = #object3d { type = cylinder,
                        radius = ?CM(3),
                        axis = ?VECTOR (1.0, 0.0, 0.0),
                        size = ?CM(1),
                        color = ?RGB (0, 1.0, 0),
                        position = ?VECTOR (-?CM(10.5), -?CM(10), -?CM(6)),
                        relative_meshes = [] },
    RightW =
        LeftW#object3d { position = ?VECTOR (?CM(9.5), -?CM(10), -?CM(6)) },
    Sphere = #object3d { type = sphere,
                         position = ?VECTOR (0.0, -?CM(10), ?CM(7)),
                         radius = ?CM(3)},

    object3d:new (#object3d { type = compound,
                              name = robot1,
                              position = ?VECTOR (0.0, 0.0, ?CM(21)),
                              objects = [Base, LeftW, RightW, Sphere] }),
    object3d:add_activity (robot1, kinematics, robot1_k,
                           [{v, ?CM_S(100)}, {omega, ?DEG_S(80)}, {plane, xz}]),

    object3d:new (#object3d { type = compound,
                              name = robot2,
                              objects = [Base, LeftW, RightW, Sphere] }),
    object3d:add_activity (robot2, kinematics, robot2_k,
                           [{v, ?CM_S(50)}, {omega, -?DEG_S(70)}, {plane, xz}]),

    ok.


%% 3 cubes: a big static one, a little one trying to enter and a little
%% one trying to exit
t11 () ->
    rosen:start_link (?ROSEN_OPTIONS),
    rosen:zoom (-?M(20)),
    rosen:up (?M(5)),

    object3d:new (#object3d { type = cube,
                              name = static_cube,
                              color = ?RGBA (0.0, 0.5, 0.0, 0.5),
                              size = ?CM(500) }),

    object3d:new (#object3d { type = cube,
                              name = cube1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50) }),
    object3d:add_activity (cube1, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),

    object3d:new (#object3d { type = cube,
                              name = cube2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, ?CM(500)) }),
    object3d:add_activity (cube2, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),

    ok.


%% 3 cylinders: a big static one, a little one trying to enter and a
%% little one trying to exit
t12 () ->
    rosen:start_link (?ROSEN_OPTIONS),
    rosen:zoom (-?M(20)),
    rosen:up (?M(5)),

    object3d:new (#object3d { type = cylinder,
                              name = static_cylinder,
                              color = ?RGBA (0.0, 0.5, 0.0, 0.5),
                              radius = ?CM(250),
                              size = ?CM(500) }),

    object3d:new (#object3d { type = cylinder,
                              name = cylinder1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              radius = ?CM(25),
                              size = ?CM(50) }),
    object3d:add_activity (cylinder1, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),

    object3d:new (#object3d { type = cylinder,
                              name = cylinder2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              radius = ?CM(25),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, ?CM(750)) }),
    object3d:add_activity (cylinder2, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),

    ok.


%% a big static cylinder and a lot of little cubes
t13 () ->
    rosen:start_link (?ROSEN_OPTIONS),
    rosen:zoom (-?M(20)),
    rosen:up (?M(5)),

    object3d:new (#object3d { type = cylinder,
                              name = static_cylinder,
                              color = ?RGBA (0.0, 0.5, 0.0, 0.5),
                              radius = ?CM(250),
                              size = ?CM(500) }),

    object3d:new (#object3d { type = cube,
                              name = cube_inside1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50) }),
    object3d:add_activity (cube_inside1, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_inside2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, ?CM(75), 0.0) }),
    object3d:add_activity (cube_inside2, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_inside3,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, ?CM(150), 0.0) }),
    object3d:add_activity (cube_inside3, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),

    object3d:new (#object3d { type = cube,
                              name = cube_outside1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, ?CM(750)) }),
    object3d:add_activity (cube_outside1, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(75), ?CM(75), ?CM(750)) }),
    object3d:add_activity (cube_outside2, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside3,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(150), ?CM(150), ?CM(750)) }),
    object3d:add_activity (cube_outside3, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside4,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(225), ?CM(225), ?CM(750)) }),
    object3d:add_activity (cube_outside4, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),

    ok.


%% a big static truncated cone and a lot of little cubes
t14 () ->
    rosen:start_link (?ROSEN_OPTIONS),
    rosen:zoom (-?M(20)),
    rosen:up (?M(5)),

    object3d:new (#object3d { type = cone,
                              name = static_cone,
                              color = ?RGBA (0.0, 0.5, 0.0, 0.5),
                              top_radius = ?CM(50),
                              base_radius = ?CM(250),
                              size = ?CM(500) }),
    object3d:new (#object3d { type = cube,
                              name = cube_inside1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50) }),
    object3d:add_activity (cube_inside1, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_inside2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, ?CM(75), 0.0) }),
    object3d:add_activity (cube_inside2, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_inside3,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, ?CM(150), 0.0) }),
    object3d:add_activity (cube_inside3, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),

    object3d:new (#object3d { type = cube,
                              name = cube_outside1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, ?CM(750)) }),
    object3d:add_activity (cube_outside1, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(75), ?CM(75), ?CM(750)) }),
    object3d:add_activity (cube_outside2, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside3,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(150), ?CM(150), ?CM(750)) }),
    object3d:add_activity (cube_outside3, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside4,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(225), ?CM(225), ?CM(750)) }),
    object3d:add_activity (cube_outside4, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),

    ok.


%% a big static sphere and a lot of little cubes
t15 () ->
    rosen:start_link (?ROSEN_OPTIONS),
    rosen:zoom (-?M(20)),
    rosen:up (?M(5)),

    object3d:new (#object3d { type = sphere,
                              name = static_sphere,
                              color = ?RGBA (0.0, 0.5, 0.0, 0.5),
                              radius = ?CM(250),
                              size = ?CM(500) }),
    object3d:new (#object3d { type = cube,
                              name = cube_inside1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50) }),
    object3d:add_activity (cube_inside1, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_inside2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, ?CM(75), 0.0) }),
    object3d:add_activity (cube_inside2, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_inside3,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, ?CM(150), 0.0) }),
    object3d:add_activity (cube_inside3, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),

    object3d:new (#object3d { type = cube,
                              name = cube_outside1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, ?CM(750)) }),
    object3d:add_activity (cube_outside1, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(75), ?CM(75), ?CM(750)) }),
    object3d:add_activity (cube_outside2, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside3,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(150), ?CM(150), ?CM(750)) }),
    object3d:add_activity (cube_outside3, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside4,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(225), ?CM(225), ?CM(750)) }),
    object3d:add_activity (cube_outside4, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),

    ok.


%% a big static truncated cone (with a non default axis) and a lot of
%% little cubes
t21 () ->
    rosen:start_link (?ROSEN_OPTIONS),
    rosen:zoom (-?M(20)),
    rosen:up (?M(5)),

    object3d:new (#object3d { type = cone,
                              name = static_cone,
                              axis = ?VECTOR(1.0, 0.0, -1.0),
                              color = ?RGBA (0.0, 0.5, 0.0, 0.5),
                              top_radius = ?CM(50),
                              base_radius = ?CM(250),
                              size = ?CM(500) }),
    object3d:new (#object3d { type = cube,
                              name = cube_inside1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50) }),
    object3d:add_activity (cube_inside1, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_inside2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, ?CM(75), 0.0) }),
    object3d:add_activity (cube_inside2, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_inside3,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, ?CM(150), 0.0) }),
    object3d:add_activity (cube_inside3, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),

    object3d:new (#object3d { type = cube,
                              name = cube_outside1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, ?CM(750)) }),
    object3d:add_activity (cube_outside1, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(75), ?CM(75), ?CM(750)) }),
    object3d:add_activity (cube_outside2, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside3,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(150), ?CM(150), ?CM(750)) }),
    object3d:add_activity (cube_outside3, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside4,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(225), ?CM(225), ?CM(750)) }),
    object3d:add_activity (cube_outside4, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),

    io:format ("axis ~p~n", [object3d:axis (static_cone)]),
    io:format ("up ~p~n", [object3d:up (static_cone)]),

    ok.


%% a big static box (with a non default up) and a lot of little cubes
t22 () ->
    rosen:start_link (?ROSEN_OPTIONS),
    rosen:zoom (-?M(20)),
    rosen:up (?M(5)),

    object3d:new (#object3d { type = box,
                              name = static_box,
                              up = ?VECTOR(0.25, 1.0, 0.0),
                              color = ?RGBA (0.0, 0.5, 0.0, 0.5),
                              width = ?CM(100),
                              height = ?CM(500),
                              depth = ?CM(250) }),
    object3d:new (#object3d { type = cube,
                              name = cube_inside1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50) }),
    object3d:add_activity (cube_inside1, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_inside2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, ?CM(75), 0.0) }),
    object3d:add_activity (cube_inside2, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_inside3,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, ?CM(150), 0.0) }),
    object3d:add_activity (cube_inside3, kinematics,
                           [{v, ?CM_S(75)}, {omega, ?DEG_S(15)}, {plane, xz}]),

    object3d:new (#object3d { type = cube,
                              name = cube_outside1,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (0.0, 0.0, ?CM(750)) }),
    object3d:add_activity (cube_outside1, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside2,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(75), ?CM(75), ?CM(750)) }),
    object3d:add_activity (cube_outside2, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside3,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(150), ?CM(150), ?CM(750)) }),
    object3d:add_activity (cube_outside3, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),
    object3d:new (#object3d { type = cube,
                              name = cube_outside4,
                              color = ?RGB (0.5, 0.5, 0.5),
                              size = ?CM(50),
                              position = ?VECTOR (
                                            ?CM(225), ?CM(225), ?CM(750)) }),
    object3d:add_activity (cube_outside4, kinematics,
                           [{v, -?CM_S(50)}, {omega, 0}, {plane, xz}]),

    io:format ("axis ~p~n", [object3d:axis (static_box)]),
    io:format ("up ~p~n", [object3d:up (static_box)]),

    ok.
