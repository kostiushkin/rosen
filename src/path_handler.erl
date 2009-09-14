%
% path_handler.erl
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
% $Id: path_handler.erl,v 1.3 2008/10/21 21:36:35 aristidena Exp $
%
%%
%% @doc The module implements a handler for robots' path.
%%
%% <p> This module represents the path handler. It handles all states 
%% which robot takes during his route.</p>
%% 


-module (path_handler).

-include ("geometry.hrl").
-include ("robot.hrl").

-behaviour(gen_activity).

-export ([init/2,
          step/5,
          terminate/2]).

-record (path_state, {target = false,
		      direction = to_target,
		      target_finder_module,
		      robotPath_Pid,
		      robotPid
		     }).

%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: step/5
%%====================================================================
%% @spec step(ObjectPid, ObjectState,Time,DeltaTime,PathState) -> {ok,NewPathState}
%%       ObjectPid = pid()
%%       ObjectState = object3d()
%%       Time = float()
%%       DeltaTime = float()
%%       PathState = activity_state()
%% @doc Perform a step of the path_handler activity.
%%      This callback calculates the next state of the robot according
%%      to the current state and actions performed.
%%
step (ObjectPid, ObjectState, Time,DeltaTime,
      PathState = #path_state { target = false }) ->
    
    RobotPid = PathState#path_state.robotPid,
    {X, Y, Theta} = robot:command (RobotPid,{get_position}),
    world_discretization:put_Pheromone({X,Y},
				       opposite(PathState#path_state.direction)),	
    io:format("TARGET Posizione in path_handler del put Pheromone ~p~n",[{X,Y}] ),
    NewPos = world_discretization:get_NewPosition(RobotPid,
						  {X,Y},
						  PathState#path_state.direction),
    {Angle,Distance} = compute_W_V(NewPos,{X,Y,Theta}),
    
    Activity_Pid = PathState#path_state.robotPath_Pid,
    io:format("Angle ~p Distance ~p~n ",[Angle,Distance]),
    gen_activity:set_property (Activity_Pid, path,[{rotate, Angle},
						   {forward,Distance}]), 
    NewPathState = PathState#path_state { target = to_control },	
    
    {ok, NewPathState};

%%
step (ObjectPid, ObjectState, Time,DeltaTime,
      PathState = #path_state { target = to_handle }) ->
	
      Activity_Pid = PathState#path_state.robotPath_Pid,
      collision_response_action(Activity_Pid),
      NewPathState = PathState#path_state { target = to_control_handle },
    
        {ok, NewPathState};

%%
step (ObjectPid, ObjectState, _, _,
      PathState = #path_state { target = to_control_handle }) ->
    
        RobotPid = PathState#path_state.robotPid,
        Activity_Pid = PathState#path_state.robotPath_Pid,
        A = gen_activity:get_property (Activity_Pid, path_done),

        io:format("DONE HANDLE ~p ~n",[A]),

    case A of

	true ->	
	    {X, Y, Theta} = robot:command (RobotPid,{get_position}),
	    Module = PathState#path_state.target_finder_module,
	    NewPathState = PathState#path_state { target = Module:is_target(X,Y,
									    PathState#path_state.direction) },
	   
		{ok, NewPathState};      
	_ ->
		{ok,PathState}
    end;

%%
step (ObjectPid, ObjectState, _, _,
      PathState = #path_state { target = to_control }) ->

        RobotPid = PathState#path_state.robotPid,
        Activity_Pid = PathState#path_state.robotPath_Pid,

	%Controllo Collisioni
        ObjPid = robot:get_object_pid(RobotPid),
        Collision = object3d:collision_points(ObjPid),
%	io:format("Collision ~p~n",[Collision]),

	%Controllo path
        A = gen_activity:get_property (Activity_Pid, path_done),   
%        io:format("DONE ~p ~n",[A]),

    case A of

	true ->	
	{X, Y, Th} =  robot:command (RobotPid,{get_position}),
	Module = PathState#path_state.target_finder_module,
	    io:format("X ~p  Y ~p  RES ~p~n",[X,Y, Module:is_target(X,Y,
								    PathState#path_state.direction)]),

	NewPathState = PathState#path_state { target = Module:is_target(X,Y,
									PathState#path_state.direction) },
		{ok, NewPathState};      
	_ ->
	    if Collision =/= []->
		NewState = PathState#path_state {target = to_handle},
		{ok, NewState};
	    true ->
		{ok,PathState}
	    end
	
    end;
  
%%
step (ObjectPid, ObjectState, _, _,
      PathState = #path_state { target = true}) ->  
    
      Activity_Pid = PathState#path_state.robotPath_Pid,
      Dir = PathState#path_state.direction,
      gen_activity:set_property (Activity_Pid, path,[{rotate, 180}]), 
      NewPathState = PathState#path_state { target = to_control,		
					      direction = opposite(Dir)},
      {ok, NewPathState}.  

%%====================================================================
%% Func: init/1
%% @private
%%====================================================================
init (_, Properties) ->

{ok,
   #path_state {target = false,
		target_finder_module = proplists:get_value (target_finder_module, Properties),
		direction = to_target,
		robotPath_Pid = proplists:get_value (robot_path_Pid, Properties),
		robotPid =  proplists:get_value (robot, Properties)
		}}.

%%====================================================================
%% Func: terminate/2
%% @private
%%====================================================================
terminate (Reason, State) ->
  ok.
 
  
%%====================================================================
%% Private Functions
%%====================================================================
%
compute_W_V([],_)-> {0,0};
compute_W_V({Xnew,Ynew},{Xprev,Yprev,Theta}) ->
                        M_deg = ang_coeff_DEG({Xprev,Yprev},{Xnew,Ynew}),
			DeltaTheta = M_deg - Theta ,
		        Angle = geometry:normalize_angle(DeltaTheta),
			R = geometry:distance({Xprev,Yprev},{Xnew,Ynew}),
			Distance = R,
			{Angle,Distance}.
%
opposite(to_target)->	to_nest;
opposite(to_nest)->	to_target.


ang_coeff_DEG({X,Y},{Xnew,Ynew}) ->	Num = (Ynew-Y),
					Den = (Xnew-X),
					M_rad = math:atan2(Num,Den),
					geometry:to_degrees(M_rad).
%
collision_response_action(Activity_Pid) ->
     gen_activity:set_property (Activity_Pid, path,[{rotate,180}]).%{rotate,180},{forward, -?CM(50)}]),
   
%=====================================================================
