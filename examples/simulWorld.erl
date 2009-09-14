%
% simulWorld.erl
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
% $Id: simulWorld.erl,v 1.3 2008/10/21 21:20:37 aristidena Exp $
%%
%% @doc The module provides customizable creation of swarm robots.
%% 

-module (simulWorld).
-export ([go/0]).

-include ("geometry.hrl").
-include ("robot.hrl").

%Physical defines
-define (ROBOT_SIZE,  ?CM(10)).
-define (WHEEL_RADIUS,?CM(2)).
-define (WHEEL_TICKS,250).

%Motion defines
-define (ROTATION_TOLERANCE,3).
-define (ROTATION_SPEED, ?DEG_S(20)).
-define (MOTION_TOLERANCE, ?CM(4)).
-define (MOTION_SPEED, ?CM_S(20)).
-define (COLONY_MEMBERS,7).

%% -define (ROSEN_OPTIONS, [{collision, [object_not_in_a_compound_list,
%%                                       to_object_dict,
%%                                       add_mesh,
%%                                       add_aabb,
%%                                       to_object_couples,
%%                                       broad_phase_aabbs_intersect,
%%                                       narrow_phase_trimeshes_intersect,
%% 				      set_objects_collision_points ,
%% 				      from_collision_dict_to_object_list,
%% 				      response_set_previous
%% 				     ]}%,
%% 			% {timeType,simulated}
%% 				]).



go () ->
% rosen:start_link(?ROSEN_OPTIONS),
  rosen:start_link(),
  rosen:zoom (-?CM(950)),
  rosen:up (?CM(100)),


  %% the working area
%  {ok, WPid} = createWorld:create(),
   {ok, WPid} = simpleWorld:create(),

%% Discretization of the working area---------------------------------------------
world_discretization:start_link(WPid,?CM(20),strategy),
world_discretization:discrete(),

%% ------Robots Creation-----------------------------------------------------------

       Base =  #object3d {type = cylinder,
   		       radius = ?ROBOT_SIZE / 2.0,
   		       size = ?ROBOT_SIZE / 2.0 + ?CM(2),
   		       position = ?VECTOR (0.0, ?ROBOT_SIZE/2.0 -?CM(5), 0.0),
   		       axis = ?VECTOR (0.0, 1.0, 0.0),
   		       color = ?RGB(4.0, 1.0, 5.0)},

       Sphere = #object3d {type = sphere,
   			position = ?VECTOR (0.0, 0.0, ?ROBOT_SIZE - ?CM(7)),
   			radius = ?CM(2)},

 Structure = [Sphere,Base], 
 Position  = {-?CM(190),-?CM(320),90.0},
%Position  = {-?CM(190),-?CM(100),90.0},
 swarmRobots(Structure,Position,WPid,?COLONY_MEMBERS),

%%---------------------------------------------------------------------------------						     
ok.
 


%% Creation Functions

swarmRobots(_,_,_,0)-> ok;
swarmRobots(Structure, {X,Y,Theta} = Position, WPid, N_colony)->
    Robot_Name = list_to_atom(string:concat("robot_",integer_to_list(N_colony))),
    inizializeRobot(Structure,Robot_Name,Position,WPid),
    swarmRobots(Structure,{X + ?CM(40),Y,Theta},WPid,N_colony-1).

%
inizializeRobot(Structure,Name,{X,Y,Theta},WPid)->
    
       {ok, RobotPid_Cylinder} =
           robot:start_link (#robot { world = WPid,
                                      type = two_wheels_driving,
                                      name = Name,
                                      wheel_radius = ?WHEEL_RADIUS,
                                      wheel_distance = ?ROBOT_SIZE,
                                      wheel_ticks = ?WHEEL_TICKS,
                                      structure = Structure}),
		       
      %Sensors																
      add_sensor_inRobot(Name,gp2d12, ?RGB(0.0, 0.0, 0.0)),
      robot:command (Name, {set_position, X, Y, Theta}),
      ActivityName =  list_to_atom(string:concat(atom_to_list(Name),"_object")),

       {ok, ActivityPid} =
            object3d:add_activity (ActivityName,         
 				   robot_path,
 				   [{robot, RobotPid_Cylinder},
 				    {rotation_tolerance, ?ROTATION_TOLERANCE},
 				    {rotation_speed, ?ROTATION_SPEED},
 				    {motion_tolerance, ?MOTION_TOLERANCE},
 				    {motion_speed,?MOTION_SPEED},
 				    {path, []},
 				    {restart, false}]),

 object3d:add_activity ( ActivityName, path_handler,[{robot, RobotPid_Cylinder},
 						     {robot_path_Pid,ActivityPid},
						     {target_finder_module, target_finder}]).
 


%
add_sensor_inRobot(RobotName,Type,Color) ->  add_sensor_inRobot(RobotName,Type,Color,0,360.0,0.0).
add_sensor_inRobot(_,_,_,_,Stop_orient,Acc) when Stop_orient == Acc -> ok;
add_sensor_inRobot(RobotName,Type,?RGB(X1, X2, X3),Index,Stop_orient,Acc) ->
						Sensor_Name = list_to_atom(string:concat("distance_",integer_to_list(Index))),
						robot:add_sensor (RobotName,
								  Sensor_Name,
								  #sensor { type = Type,
								  sensor_name = Sensor_Name,						
								  parameters = Acc,	
								  position = ?VECTOR(?ROBOT_SIZE/2*math:cos(geometry:to_radiants(Acc)),
										     ?ROBOT_SIZE/2*math:sin(geometry:to_radiants(Acc)),
										     ?CM(4)), 
								  color = ?RGB(X1, X2, X3)}),
						 add_sensor_inRobot(RobotName,Type,?RGB(X1+0.2, X2, X3),Index+1,Stop_orient,Acc+45.0).
  
