% pherononeCell.erl
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
% $Id: pheromoneCell.erl,v 1.3 2008/10/21 21:36:35 aristidena Exp $
%%
%% @doc Main module for handling PheromoneCell.
%%
%% <p>This module provides functions to create a cell that rapresents
%% pheromone value and manipulating it in order to change its attributes
%% like <code>to_nest</code> or <code>to_target</code> value and <code>Color</code>
%% of the relative <code>Object3d</code>.</p>
%%

-module (pheromoneCell).
-behaviour(gen_server).

-include ("geometry.hrl").
-include ("robot.hrl").

-export ([init/1, handle_call/3,handle_info/2,start_link/4,terminate/2]).

-export ([new/4,
	  updatePheromone_handler/0,
	  get_cellValue/1,
	  set_cellValue/2
	 ]).

%% Value in the cell:
%      [{to_nest, val}, {to_target,val}]

%%%% RGB     R = amount of target pheromone
%%	     G = 0.0	
%%           B = amount of nest pheromone

-define(DEFAULT_TIMEOUT,10000).

-record (pheromone_state, {cellName = nil,
	                   object3d_pid = nil,
			   value =  [],        %[{to_nest, ?Min_Pheromone}, {to_target,?Min_Pheromone}],
			   range = nil,	       % range = {Min_Pheromone,Max_Pheromone}
			   module_strategy = nil
			  }).

%%====================================================================
%% Function: behaviour_info/1
%%====================================================================
%% @private
behaviour_info (callbacks) ->
  [ {init, 1},
    {updatePheromone, 1},
    {get_Pheromone_Range/0},
    {terminate, 2}];
behaviour_info (_Other) ->
  undefined.

%%====================================================================
%% Function: new/1
%%====================================================================
%% @spec new(ModuleName, Obj::object3d(),Type,Module_Strategy) -> {ok, Pid}
%%
%% @doc Creates a new Pheromone_Cell as specified in parameters.
%% <br/>
%%      <code>{{Object,NameModule},Type,Module}</code> is parameters to be passed to the 
%%      <code>Module:init</code> function:<br/>
%%      <ul>
%%      <li><code>Object</code>, is the object3d correspondent to cell,</li> 
%%      <li><code>NameModule</code>, is the registered name.</li><br/>
%%      <li><code>Type</code>, is the atom <I><code>to_nest</code></I> | 
%%      <I><code>to_target</code></I></li>
%%      <li><code>Module</code>, is the name of the module where callbacks are implemented.</li>
%%      </ul>
%% The Pheromone_Cell is handled has a new process linked with the 
%% calling process. It is bound to name specified, so that, in all 
%% the functions of this module, the cell can be referred with either 
%% the pid or its registered name.<br/> 
%% <code>Module_Strategy</code> is the name of the Callback Module where the specific
%% strategy are defined. The function result is thus the pid of the 
%% spawned process.
%%
new (Cell_Name,Object,Type,Module) ->
 start_link (Cell_Name,Object,Type,Module).

%%====================================================================
%% Func: start_link/4
%%====================================================================
%%
%% @spec start_link(NameModule,Object::object3d(),Type, Module) -> Result
%%       	NameModule = {local,Name} 
%%              Type = to_nest | to_target
%%       	Module = atom()
%%       	Result = {ok, Pid} | {error, Reason} | undefined
%% @doc Create a new cell process to handle pheromone trail.<br/>
%%      See <code>new/1</code> for details.

start_link (Cell_Name,Object,Type,Module) ->
  {ok, Pid_cell}= gen_server:start_link ({local,Cell_Name},?MODULE, 
			 {{Object,Cell_Name},Type,Module},[]).


%%====================================================================
%% Func: get_cellValue/1
%%====================================================================
%%
%% @spec get_cellValue(Pid) -> Result
%%       Pid = pid()
%%       Result = term()
%% @doc Get the value stored in the cell. 
%%
get_cellValue(Pid) ->
  gen_server:call (Pid, {get_cellValue}).

%%====================================================================
%% Func: set_cellValue/1
%%====================================================================
%%
%% @spec set_cellValue(Pid,Value) -> Result
%%       Pid = term()
%%       Result = ok | {error, noprop}
%% @doc Set the cell value.
%%   	If the property esists, it is set to the new value
%%      and the result is <code>ok</code>. Otherwise, the callback function 
%%      should return
%%      	{error, noprop}
%%
set_cellValue (Pid,Value) ->
  gen_server:call (Pid, {set_cellValue, {Value}}).
 
 
%%====================================================================
%% Func: updatePheromone/0
%% @doc Update pheromone Value.

%%====================================================================
updatePheromone_handler() ->
  gen_server:call (?MODULE,updatePheromone_handler).

 
%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: handle_call/3
%%====================================================================
%% @private

%% get_CellValue

handle_call ({get_cellValue}, _, State) ->
    Value = State#pheromone_state.value,
    Range = State#pheromone_state.range,
    Control = controlValue(Value,Range),
{reply, State#pheromone_state.value, State,?DEFAULT_TIMEOUT};

%% set_cellValue

handle_call ({set_cellValue,{Value}}, _, State) ->
    Object3d_pid = State#pheromone_state.object3d_pid,
    Range = State#pheromone_state.range,
    NewState = State#pheromone_state{value = Value},	
    object3d:set_Visible(Object3d_pid,true),
    NewColor = setColor(Value),
    object3d:set_Color(Object3d_pid,NewColor),
    {reply, ok, NewState,?DEFAULT_TIMEOUT}.
    
%
%%====================================================================
%% Func: handle_info/2
%%====================================================================
%% @private
%%

%% UpdatePheromone

handle_info (timeout, State) ->
    Value = State#pheromone_state.value,
    ModuleStrategy = State#pheromone_state.module_strategy,
    NewValue = ModuleStrategy:updatePheromone(Value),
    Object3d_pid = State#pheromone_state.object3d_pid,
%%     NewColor = setColor(Value),
%%     object3d:set_Color(Object3d_pid,NewColor),
    NewState = State#pheromone_state{value = NewValue},
    Range = State#pheromone_state.range,
    Control = controlValue(NewValue,Range),
    object3d:set_Visible(Object3d_pid,not Control),
%    io:format("Control ~p~n",[Control]),
%io:format("~n~n~nUpadate ~n~nVal ~p Pid ~p ~n",[NewValue,Object3d_pid]),

if(Control)->   
	io:format("DELETE DELL'OGGETTO ~p Name ~p~n",[Object3d_pid,State#pheromone_state.cellName]),
        {noreply, NewState};
  true -> 
	NewColor = setColor(Value),
	object3d:set_Color(Object3d_pid,NewColor),
	{noreply, NewState,?DEFAULT_TIMEOUT}
end.
   
    
%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
init ({{Object,Name},Type,Module_strategy}) ->
{Min,Max} = Module_strategy:get_Pheromone_Range(),
{Color,Val} = set_InitVal(Type,Min),
NewObject = Object#object3d { color = Color },
{ok,Pid_obj} = object3d:new(NewObject),
io:format("Processo ~p oggetto ~p~n",[Name, Pid_obj]),
   {ok, #pheromone_state {cellName = Name,
			  object3d_pid = Pid_obj,
			  value = Val,
			  range = {Min,Max},
			  module_strategy = Module_strategy},8000}.   

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
controlValue([{to_nest, Vn}, {to_target,Vt}],{Min,Max})when Vn =< Min ,
							    Vt =< Min -> true;
controlValue(_,_) -> false.

%
setColor([{to_nest,V1},{to_target,V2}]) ->
    Color = ?RGB(trunc(V2*10.0)/10.0,0.0,trunc(V1*10.0)/10.0).

%
set_InitVal(to_nest,Min)-> Color = ?RGB(trunc(Min*20.0)/10.0,0.0,trunc(Min*10.0)/10.0),
			   Val = [{to_nest, Min*2},{to_target,Min}],
			   {Color,Val};
set_InitVal(to_target,Min) -> Color = ?RGB(trunc(Min*10.0)/10.0,0.0,trunc(Min*20.0)/10.0),
			      Val = [{to_nest, Min},{to_target,Min*2}],
			      {Color,Val}.

















