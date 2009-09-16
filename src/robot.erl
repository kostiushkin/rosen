%
% robot.erl
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
% $Id: robot.erl,v 1.15 2008/10/26 22:34:54 aristidena Exp $
%

-module (robot).

-include ("geometry.hrl").

-include ("robot.hrl").

-behaviour (gen_server).

-export ([start_link/1,
          behaviour_info/1,
          angle_to_3d/1,
          get_world/1,
          command/2,
          get_motion_pid/1,
          add_sensor/2,
          add_sensor/3,
          get_sensor_value/2,
          get_sensors/1,
          get_sensors/2,
          get_sensors_range/2,
	  get_object_pid/1,
          stop/1]).

-export ([init/1, handle_call/3,terminate/2,handle_cast/2]).


%%====================================================================
%% Function: behaviour_info/1
%% @private
%%====================================================================
behaviour_info (callbacks) ->
  [ {create_driving_objects, 1},
    {create_activities, 4},
    {handle_command, 2} ];
behaviour_info (_Other) ->
  undefined.


%%====================================================================
%% Func: start_link/1
%%====================================================================
start_link (Robot = #robot{}) ->
  if
    Robot#robot.name == noname ->
      {ok, RobotPid} = gen_server:start_link (?MODULE, Robot, []);
    true ->
      ObjectName = list_to_atom (atom_to_list (Robot#robot.name) ++
                                 "_object"),
      {ok, RobotPid} = gen_server:start_link ({local, Robot#robot.name}, ?MODULE,
                             Robot#robot { object_name = ObjectName},
                             [])
  end,
  rosen_world:add_robot (Robot#robot.world, RobotPid),
  {ok, RobotPid}.


%%====================================================================
%% Func: command/2
%%====================================================================
command (Pid, Cmd) ->
  gen_server:call (Pid, {command, Cmd}).


%%====================================================================
%% Func: add_sensor/2
%%====================================================================
add_sensor (Pid, Sensor = #sensor {}) ->
  gen_server:call (Pid, {add_sensor, undefined, Sensor}).


%%====================================================================
%% Func: add_sensor/3
%%====================================================================
add_sensor (Pid, SensorName, Sensor = #sensor {}) ->
  gen_server:call (Pid, {add_sensor, SensorName, Sensor}).


%%====================================================================
%% Func: get_sensor_value/2
%%====================================================================
get_sensor_value (Pid, SensorName) ->
  gen_server:call (Pid, {get_sensor_value, SensorName}).


%%====================================================================
%% Func: get_sensors/1
%%====================================================================
get_sensors (Pid) ->
  gen_server:call (Pid, {get_sensors}).


%%====================================================================
%% Func: get_sensors/2
%%====================================================================
get_sensors (Pid,SensorType) ->
  gen_server:call (Pid, {get_sensors,SensorType}).

%%====================================================================
%% Func: get_sensors_range/2
%%====================================================================
get_sensors_range (Pid,SensorType) ->
  gen_server:call (Pid, {get_sensors_range,SensorType}).
 
%%====================================================================
%% Func: get_motion_pid/1
%%====================================================================
get_motion_pid (Pid) ->
  gen_server:call (Pid, {get_motion_pid}).


%%====================================================================
%% Func: get_world/1
%%====================================================================
get_world (Pid) ->
  gen_server:call (Pid, {get_world}).


%%====================================================================
%% Func: angle_to_3d/1
%%====================================================================
angle_to_3d (Angle) ->
  geometry:normalize_angle (Angle + 90.0).

%%====================================================================
%% Func: get_object_pid/1
%%====================================================================
get_object_pid (Pid) ->
  gen_server:call (Pid, {get_object_pid}).
  

%%====================================================================
%% Func: stop/1
%%====================================================================
%%
%% @spec stop(Pid::pid()) -> ok
%%
%% @doc Destroys the robot.
%%
stop (Pid) ->
  gen_server:cast (Pid, stop).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
%%
init (Robot) ->
  Module = Robot#robot.type,
  %% first obtain the objects for the driving model
  {DrivingObjects, DrivingInfo} = Module:create_driving_objects (Robot),
  %% now create the 3D objects for the robot
  OpenGLPos = ?VECTOR (0.0, Robot#robot.wheel_radius, 0.0),
  TheRobot = #object3d { type = compound,
                         name = Robot#robot.object_name,
                         position = OpenGLPos,
                         objects = lists:flatten ([DrivingObjects,
                                                   Robot#robot.structure]) },
  {ok, TheRobotPid} = object3d:new (TheRobot),
  MotionPid = Module:create_activities (Robot, TheRobotPid,
                                        DrivingObjects, DrivingInfo),
  {ok, Robot#robot { object_pid = TheRobotPid,
                     motion_pid = MotionPid}}.

%%====================================================================
%% Func: terminate/2
%%====================================================================
%% @private
terminate (_, _) ->
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
handle_call ({command, Cmd}, _, Robot) ->
  Module = Robot#robot.type,
  case catch (Module:handle_command (Cmd, Robot)) of
    {'EXIT', Reason} ->
      Reply = {error, Reason},
      NewRobot = Robot;
    Other ->
      {Reply, NewRobot} = Other
  end,
  {reply, Reply, NewRobot};
%%
handle_call ({add_sensor, SensorName, Sensor}, _, Robot) ->
  CompleteSensorName = make_sensor_name (SensorName, Robot),
  SensorNew = Sensor#sensor {robot = self(),
                             robot_object = Robot#robot.object_pid,
                             world = Robot#robot.world },

  Sensor3DObject = (Sensor#sensor.type):model (Sensor#sensor.position,
                                               Sensor#sensor.color),
  Sensor3DObjectNoMesh = Sensor3DObject#object3d { relative_meshes = [] },

  object3d:add_to_compound (Robot#robot.object_pid, Sensor3DObjectNoMesh),

  Sensors = Robot#robot.sensors,
  if
    CompleteSensorName == undefined ->
      {ok, Pid} = object3d:add_activity (Robot#robot.object_pid,
                                         Sensor#sensor.type,
                                         SensorNew);
    true ->
      {ok, Pid} = object3d:add_activity (Robot#robot.object_pid,
                                         Sensor#sensor.type,
                                         CompleteSensorName,
                                         SensorNew)
  end,

  {reply, ok, Robot#robot { sensors = [Pid | Sensors] }};
%%   {X,Y,Z,Theta} = SensorNew#sensor.parameters,
%%   SensorD = #object3d { type = sphere,
%%                         position = ?VECTOR(X,Y,Z),
%%                         color = SensorNew#sensor.color,
%%                         radius = ?CM(2)},


%%   {reply, ok, Robot#robot { sensors = [Pid | Sensors],
%%                             structure = lists:append([SensorD],
%%                                                      Robot#robot.structure)}};
%%
handle_call ({get_sensor_value, SensorName}, _, Robot) ->
  CompleteSensorName = make_sensor_name (SensorName, Robot),
  Reply = gen_activity:module_state (CompleteSensorName),
  {reply, Reply#sensor.value, Robot};

handle_call ({get_sensors,SensorType}, _, Robot) ->
	V = lists:map (fun (Pid) ->
                          ModuleState = gen_activity:module_state (Pid),
                          {ModuleState#sensor.type,
			    ModuleState#sensor.sensor_name,
                            ModuleState#sensor.value}
                      end,
                      Robot#robot.sensors),
      Values = [{N,Va}|| {T,N,Va} <-V, T == SensorType],
  {reply, Values, Robot};

%%
handle_call ({get_sensors}, _, Robot) ->
  Values = lists:map (fun (Pid) ->
                          ModuleState = gen_activity:module_state (Pid),
                          {ModuleState#sensor.sensor_name,
                           ModuleState#sensor.value}
                      end,
                      Robot#robot.sensors),
  {reply, Values, Robot};

%%
handle_call ({get_sensors_range,SensorType}, _, Robot) ->
 
    V = lists:map (fun (Pid) ->
			   ModuleState = gen_activity:module_state (Pid),
			   {ModuleState#sensor.type,
			    R = gen_activity:get_property(Pid,range)}
		   end,
		   Robot#robot.sensors),
    
    Range = proplists:get_value(SensorType,V),
    {reply, Range, Robot};
%%
handle_call ({get_world}, _, Robot) ->
  {reply, Robot#robot.world, Robot};
%%
handle_call ({get_motion_pid}, _, Robot) ->
  {reply, Robot#robot.motion_pid, Robot};
%%
handle_call ({get_object_pid}, _, Robot) ->
  {reply, Robot#robot.object_pid, Robot}.


make_sensor_name (undefined, _) -> undefined;
make_sensor_name (_, _Robot = #robot { name = noname } ) -> undefined;
make_sensor_name (SN, Robot) ->
  list_to_atom (lists:flatten ([atom_to_list (Robot#robot.name),
                                "_",
                                atom_to_list (SN)])).
