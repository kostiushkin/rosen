%
% world_discretization.erl
%
% ----------------------------------------------------------------------
%%
%%  ROSEN, a RObotic Simulation Erlang eNgine
%%  Copyright (C) 2007 Corrado Santoro (csanto@diit.unict.it)
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%%
% $Id: world_discretization.erl,v 1.3 2008/10/21 21:36:35 aristidena Exp $
%%
%%  @doc The module handles the discretization of the environment. <br/>
%%       It is a <I>gen_server</I>, which provides functions to:
%%       <ul>
%%       <li> discrete world obtaining a matrix,</li> 
%%       <li> select new position, analyzing pheromone trail all around the 
%%       current position, </li>
%%       <li> add pheromone in occupied cell during the motion.</li>
%%       </ul>
%%       It is also a <I>behaviour</I>, because a callback module has to 
%%       develope to implements specific motion stategy.
%%



-module (world_discretization).
-behaviour(gen_server).

-include ("geometry.hrl").

-export ([init/1, handle_call/3,start_link/3,terminate/2]).

-export ([discrete/0,
 	  get_NewPosition/3,
 	  put_Pheromone/2
	 ]).


-record (discretization_state, {associated_world_Pid = nil,
				step = 0,
				dimensions = nil,
				module_strategy
			       }).

%%====================================================================
%% Function: behaviour_info/1
%%====================================================================
%% @private
behaviour_info (callbacks) ->
  [ {init, 2},
    {get_NewPosition, 3},
    {get_NewPosition, 4},
    {put_Pheromone, 1},
    {terminate, 2}];
behaviour_info (_Other) ->
  undefined.

%%====================================================================
%% Func: start_link/2
%%====================================================================
%%
%% @spec start_link(ObjectPid, Step,Callback_Module) -> Result
%%		ObjectPid = pid()
%%       	Step = term()
%%              Callback_Module = atom()
%%       	Result = {ok, Pid} | {error, Reason} | undefined
%%
%% @doc Create a  <I>gen_server</I> to discrete the working area.<br/>
%%      {ObjectPid,Step,Callback_Module} are parameters to be passed to 
%%      the <code>Module:init</code> function:<br/>
%%       <ul>
%%       <li><code>ObjectPid</code> is the pid of the working area,</li>
%%       <li><code>Step</code> is the discretization step,</li>
%%       <li><code>Callback_Module</code> is the name of the module where 
%%       callbacks are implemented.</li><br/>
%%       </ul>

start_link (ObjectPid,Step,Module) ->
  gen_server:start_link ({local,?MODULE},?MODULE, {ObjectPid,Step,Module},[]).
  
%%====================================================================
%% Func: get_NewPosition/3
%%====================================================================
%%
%% @spec get_NewPosition(RobotPid,CurrentPosition,Direction) -> Result
%%       RobotPid = pid()
%%       CurrentPosition = term()
%%       Direction = to_nest | to_target 
%%       Result = Position
%%       Position = term()
%% @doc Get next Position in the world which corresponds to center
%%      coordinates of the chosen cell. 
%%
get_NewPosition(RobotPid,CurrentPosition,Direction) ->
  gen_server:call (?MODULE, {get_NewPosition,RobotPid,CurrentPosition,Direction}).
 
%%====================================================================
%% Func: put_pheromone/2
%%====================================================================
%%
%% @spec put_Pheromone(Position,Type) -> ok
%%       Position = term()
%%       Type  = to_nest | to_target 
%% @doc Add a certain amount of pheromone in the current cell.
%%
put_Pheromone(Position,Type) ->
  gen_server:call (?MODULE, {put_Pheromone,Position,Type}).

%%====================================================================
%% Func: discrete/0
%% @doc Start the discretization process.

%%====================================================================
discrete () ->
  gen_server:call (?MODULE, discrete).
  
%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: handle_call/3
%%====================================================================
%% @private
%%

%% get_NewPosition

handle_call ({get_NewPosition,RobotPid,{X,Y},Direction}, _, State) ->
    {Cp,Rp} = pos_to_key({X,Y},State),
    io:format("RobotPid ~p Position ~p   ~p ~n",[RobotPid,Cp,Rp]),
    io:format("PositionPrima ~p   ~p ~n",[X,Y]),
%
% ________
%|4 |3 |2 |
%|__|__|__|
%|5 |->|1 |  -> Orientation = 0°
%|__|__|__|
%|6 |7 |8 |
%|__|__|__|
%  
%       [   1     ,     2     ,   3     ,    4      ,    5    ,     6     ,   7     ,     8     ];
    L = [{Cp+1,Rp},{Cp+1,Rp+1},{Cp,Rp+1},{Cp-1,Rp+1},{Cp-1,Rp},{Cp-1,Rp-1},{Cp,Rp-1},{Cp+1,Rp-1}],
    PheromoneArray = cells_Around(L,Direction,State),
  io:format("FEROMONE AROUND 1  ~p ~n",[PheromoneArray]),
    Module = State#discretization_state.module_strategy,
    Step = State#discretization_state.step,
    New_Key_Position = Module:get_NewPosition (PheromoneArray,RobotPid, Step*2),
    NewPosition = key_to_pos(New_Key_Position,State),
    io:format("Position da raggiungere ~p ~p ~n",[New_Key_Position,NewPosition]),
{reply, NewPosition, State};
    
%% discrete

handle_call (discrete, _, State) ->
    {Width,Height} = rosen_world:get_dimensions(State#discretization_state.associated_world_Pid),
    Step = State#discretization_state.step,
    {Column,Row} = {ceil(Width/Step),ceil(Height/Step)},
    NewState = State#discretization_state{dimensions = {Column,Row} },
{reply,ok, NewState};

%%  put_Pheromone

handle_call ({put_Pheromone,{X,Y},Type}, _, State) ->
    Key = pos_to_key({X,Y},State),
    Module = State#discretization_state.module_strategy,
    Control = out_of_World(Key,State#discretization_state.dimensions) ,
    Step = State#discretization_state.step,
    io:format("~nKey e Position in Put Pheromone ~p ~p ~p ~n",[Key,X,Y]),
    
    if 
	Control	->
	    {reply, ok, State};
	true->	
	    Cell_Name = createName(Key),
	    case catch (pheromoneCell:get_cellValue(Cell_Name)) of
		{'EXIT', _} ->
		    io:format("~nCellName CREATO ~p~n",[Cell_Name]),
		    {Xo,Yo} = key_to_pos(Key,State),
		    {ok,Pid} =  pheromoneCell:new(Cell_Name,
						  #object3d {  
						    type = box,
						    width = Step,
						    height = 0.0,
						    depth = Step,
						    position = ?VECTOR (Xo, 0, -Yo),
						    relative_meshes = []},
						  Type,
						  Module);
		Other ->
		    Value_f = filterVal(Other,Type),
		    NewVal = Module:put_Pheromone(Value_f),
		    Val = insertCell(Other,Type,NewVal),
		    pheromoneCell:set_cellValue(Cell_Name,Val)
	    end,
	      {reply, ok, State}
	end.

%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
init ({ObjectPid,Step,Module_strategy}) ->
    {ok, #discretization_state {associated_world_Pid = ObjectPid,
				step = Step,
				module_strategy = Module_strategy}}.   
%%====================================================================
%% Func: terminate/2
%%====================================================================
%% @private
terminate (_, _) ->
  ok.
  
%%====================================================================
%% Private Functions
%%====================================================================
%
pos_to_key({X,Y},State)-> 	Step = State#discretization_state.step,
				{W,H} = rosen_world:get_dimensions(State#discretization_state.associated_world_Pid),
	                        {trunc((W/2 + X)/Step),trunc((H/2 + Y)/Step)}.
%
key_to_pos([],_)-> [];
key_to_pos({C,R},State)-> 	Step = State#discretization_state.step,
				{W,H} = rosen_world:get_dimensions(State#discretization_state.associated_world_Pid),
				{( C * Step - W/2 + Step/2),(R  * Step - H/2 + Step/2)}.
%
%% ceil(X)when (X - trunc(X))> 0.0 -> trunc(X + 1);
%% ceil(X) -> trunc(X).
ceil(X)when (X - trunc(X))> 0.0 -> trunc(X);
ceil(X) -> trunc(X-1).

%
out_of_World({X,Y},{C,R}) when X>C; X<0; Y>R; Y<0 ->  true;
out_of_World(_,_) ->     false.

%
filterVal([{_,H}|_],to_nest)->     H;
filterVal([_|[{_,T}]],to_target)-> T.

%
cVal([{_,H},{_,T}],to_nest)->   {H,T};
cVal([{_,H},{_,T}],to_target)-> {T,H}.

%
insertCell([_|T],to_nest,V) -> [{to_nest,V}| T];
insertCell([H|_],to_target,V) ->  [H |[{to_target,V}]].

%
cells_Around(L,Dir,State) -> cells_Around(L,Dir,[],State). 
cells_Around([],_,Acc,_)-> lists:reverse(Acc);	
cells_Around([H|T],Dir,Acc,State) ->

    A = out_of_World(H,State#discretization_state.dimensions),
    if 
	A ->   			
	    Val = undefined;
        true ->
	    Cell_Name = createName(H),
	    case catch (pheromoneCell:get_cellValue(Cell_Name)) of
		{'EXIT', _} ->
		    Module = State#discretization_state.module_strategy,
		    {Min,_}= Module:get_Pheromone_Range(),
		    Val = {Min,Min};
		Other ->
		   % Val = filterVal(Other,Dir)
		   % io:format("DSA ~p OTHER ~p~n",[cVal(Other,Dir),Other]), 
		    Val = cVal(Other,Dir)
	    end
    end,
    Tr = [{H,Val} | Acc],
    cells_Around(T,Dir,Tr,State).

%
createName({K1,K2})->
    SubName1 = string:concat("cell_",integer_to_list(K1)),
    SubName2 = string:concat("_",integer_to_list(K2)),
    list_to_atom(string:concat(SubName1,SubName2)).

%==================================================================================
