%
% strategy.erl
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
%%
%%  @doc This module provides the simulation strategy.<br/>
%%       It is a necessary <code>callback module</code> to develope
%%       specific motion rules.
%%       <br/>
%%
% $Id: strategy.erl,v 1.5 2008/10/26 22:34:54 aristidena Exp $
%%


-module (strategy).
-behaviour(world_discretization).

-export ([init/2,
	  terminate/2]).
	  
-export ([get_NewPosition/3,
	  updatePheromone/1,
	  get_Pheromone_Range/0,
	  multiple_maxVal/1,
	  put_Pheromone/1,maxVal/3]).	

-define(ALPHA,0.1).
-define(BETA,0.6).
-define(RHO_evaporationRate,0.09).			
-define(NewPosition_probability,0.5).
-define(Min_Pheromone,0.1).				
-define(Max_Pheromone,10.0).				
%-define(THRESHOLD,4.0).

%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: get_NewPosition/3
%%====================================================================
%% @spec get_NewPosition(PheromoneArray,RobotPid,Threshold) -> Result
%%       PheromoneArray = list()
%%       RobotPid = pid()
%%       Threshold = int()
%%       Result = tuple()
%%
%% @doc Get next Position according to specific chosen rule.
%%      <ul>
%%      <li><code>PheromoneArray</code> is a list of all possible positions
%%      when elements are a couple of {Key, Pheromone_Value}</li>
%%      <li><code>Threshold</code> is the limit value to determine obstacle </li>
%%      <li><code>Result</code> is the next position <code>Key</code>, or better 
%%      a tuple with the chosen coordinates </li>
%%      </ul>
%%

get_NewPosition(PheromoneArray,RobotPid,Threshold) ->

    {_,_,ThetaRobot} = robot:command (RobotPid,
				      {get_real_position}),
    SensorName = around_sensors(ThetaRobot),
    Sensors = robot:get_sensors (RobotPid,gp2d12),  
%%    lists:map(fun(X)->
%% 		      under_threshold(proplists:get_value(X,Sensors))
%% 	      end,SensorName);
   {_,MaxDist} = robot:get_sensors_range(RobotPid,gp2d12),
    L11 = createZipList(PheromoneArray,SensorName,Sensors,{Threshold,MaxDist}),
    io:format("L11 ~p ~n",[L11]),
    L2 =  lists:filter (fun ([]) -> [];
			    ({{_,X},Dii})->  not (X == undefined)  and 
					     not (Dii == obstacle)
			end, L11),
    io:format("L2 ~p~n",[L2]),
    
    Prob = random:uniform(),
    
    if
	Prob < ?NewPosition_probability -> 
	    NewPosition = pseudo_random_proportional(L2),%;
	    io:format("New Position PSEUDO RANDOM ~p~n",[NewPosition]);
	
	true->
	    NewPosition = random_proportional(L2),
	    io:format("New Position RANDOM POS ~p~n",[NewPosition])
    
    end,
    
    io:format("NewPosition ~p~n",[NewPosition]),
NewPosition.


%%====================================================================
%% Func: updatePheromone/1
%%====================================================================
%%
%% @spec updatePheromone(Val) -> Result
%%       	Val = list()
%%       	Result = list()
%%
%% @doc Get new value based on pheromone evaporation rule. <br/>
%%      <code>Val</code> and <code>Result</code> are the list 
%%     <b><I><code>[{to_nest,Vn},{to_target,Vt}]</code></I></b> where
%%      <ul>
%%      <li>Vn = pheromone value in <I>"to_nest"</I> direction </li>
%%      <li>Vt = pheromone value in <I>"to_target"</I> direction</li>
%%      </ul>
%%
updatePheromone([{to_nest,V1},{to_target,V2}]= Val ) ->
		V1_new = min(V1 * (1- ?RHO_evaporationRate),?Min_Pheromone),
		V2_new = min(V2 * (1- ?RHO_evaporationRate),?Min_Pheromone),

PheromoneVal_i = [{to_nest,V1_new},{to_target,V2_new}].

%%====================================================================
%% Func: get_Pheromone_Range/0
%%====================================================================
%%
%% @spec get_Pheromone_Range() -> Result
%%       	Result = {Min_Pheromone,Max_Pheromone}
%% @doc Get the lowest and highest pheromone value. 
%%

get_Pheromone_Range()->
		Range = {?Min_Pheromone,?Max_Pheromone}.

%%====================================================================
%% Func: put_Pheromone/1
%%====================================================================
%%
%% @spec put_Pheromone(CurrentVal) -> Result
%%       	Result = float()
%%       	CurrentVal = float()
%% @doc Get new value based on pheromone update rule. 
%%
put_Pheromone(CurrentVal)->
%                Delta = random:uniform(),
                Delta = 0.1,
		Val = max(CurrentVal + Delta,?Max_Pheromone).

%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
init (_,Properties) ->
{A1,A2,A3} = now(),
random:seed(A1, A2, A3),
 {ok, Properties}.

%%====================================================================
%% Func: terminate/2
%%====================================================================
%% @private
terminate (_, _) ->
  ok.
  
%%====================================================================
%% Private Functions
%%====================================================================
%% @private
min(X,Lim) when X < Lim -> Lim;
min(X,_) -> X.

%% @private
%
max(X,Lim) when X > Lim -> Lim;
max(X,_) -> X.

%% @private
%
maxVal([],Max_Val,Acc)-> Acc;
maxVal([{K,V}|T],{_, Max_Val},Acc) when V == Max_Val ->  maxVal(T,{K,V},[{K,V}|Acc]);	
maxVal([{K,V}|T],{_, Max_Val},Acc) when V > Max_Val ->  maxVal(T,{K,V},[{K,V}]);	
maxVal([{_,V}|T],{K_Max, Max_Val},Acc) ->  maxVal(T,{K_Max,Max_Val},[{K_Max,Max_Val}]).	

%% @private
%
random_proportional([])->[];
random_proportional(L1)->
    Beta = ?BETA,
    L2 = [math:pow(Vd, ?ALPHA)  + math:pow(1/(D_i + 1),Beta) || {{_,{Vd,Vo}}, D_i} <- L1],
    Ar = [{K1,((math:pow(Vdi, ?ALPHA) + math:pow(1/(D_i +1),Beta))/lists:sum(L2))} || {{K1,{Vdi,Vopp}},D_i} <- L1],
%    L2 = [math:pow(Vd, ?ALPHA)  + math:pow(D_i/(D_i + 1),Beta) || {{_,{Vd,Vo}}, D_i} <- L1],
%    Ar = [{K1,((math:pow(Vdi, ?ALPHA) + math:pow(D_i/(D_i +1),Beta))/lists:sum(L2))} || {{K1,{Vdi,Vopp}},D_i} <- L1],
 
    [{_,V1}|_] = SortedList = lists:sort(fun({_,V1},{_,V2}) -> V1 < V2 end,Ar),
    Prob = random:uniform(),
    io:format("RrANDOM proporz ~p~n",[SortedList]),
    verifyProbability(Prob,SortedList,V1).

%% @private
%
pseudo_random_proportional([])->[];
pseudo_random_proportional([H|T] = L1)->
    Beta = ?BETA,
    [Hr|Tr] = Lq = [{K1,math:pow(V_dir, ?ALPHA) + math:pow(1/(D_i +1),Beta)} || {{K1,{V_dir,V_opp}}, D_i} <- L1],
%    [Hr|Tr] = Lq = [{K1,math:pow(V_dir, ?ALPHA) + math:pow(D_i/(D_i +1),Beta)} || {{K1,{V_dir,V_opp}}, D_i} <- L1],
    Val_max = maxVal(Tr,Hr,[Hr]),
    io:format("Pseudo Random Proportional ~p~n",[Lq]),
    multiple_maxVal(Val_max).

%% @private
%
multiple_maxVal([])-> [];
multiple_maxVal([{K,V}|[]])-> K;
multiple_maxVal(L)-> Res = length(L),					
		     N = random:uniform(Res),
		     {Knew,_} = lists:nth(N,L),
		     Knew.
    
%% @private
%
under_threshold (0,_)-> 0;
under_threshold (Distance,{Threshold,_}) when Distance < Threshold -> obstacle;
under_threshold (Distance,{_,MaxDist})-> MaxDist - Distance.

%% @private
%
verifyProbability(P,[{K,_}|T],Acc) when P < Acc ; T==[] -> K;										
verifyProbability(P,[{_,V}|T],Acc) when P > Acc ->         verifyProbability(P,T,Acc+V).


%
% ________
%|s3|s2|s1|
%|__|__|__|
%|s4|->|s0|  -> Orientation = 0° 
%|__|__|__|   
%|s5|s6|s7|  si = i-nth sensor (sensorName = distance_i)
%|__|__|__|
%
%% @private

around_sensors(Theta) when Theta >= 0.0    , Theta < 45.0   ->   
    [distance_0,distance_1,distance_2, nil, nil, nil, distance_6,distance_7];

around_sensors(Theta) when Theta >= 45.0   , Theta < 90.0   ->  
    [distance_7, distance_0, distance_1, distance_2, nil, nil, nil, distance_6];

around_sensors(Theta) when Theta >= 90.0   , Theta < 135.0  -> 
    [ distance_6,  distance_7,  distance_0,  distance_1,  distance_2, nil, nil, nil];
  
around_sensors(Theta) when Theta >= 135.0  , Theta =< 180.0 ->
    [ nil, distance_6,  distance_7,  distance_0,  distance_1,  distance_2, nil, nil];
  
around_sensors(Theta) when Theta >= -180.0 , Theta < -135.0 ->
    [nil, nil,  distance_6,  distance_7,  distance_0,  distance_1,  distance_2, nil];
  
around_sensors(Theta) when Theta >= -135.0 , Theta < -90.0  ->
    [nil, nil, nil, distance_6, distance_7, distance_0, distance_1, distance_2];
  
around_sensors(Theta) when Theta >= -90.0  , Theta < -45.0  ->
    [ distance_2, nil, nil, nil, distance_6, distance_7, distance_0, distance_1];

around_sensors(Theta) when Theta >= -45.0  , Theta < 0.0    ->
    [ distance_1, distance_2,nil, nil, nil, distance_6, distance_7, distance_0].

%% @private
%
createZipList(L,S,Sensors,{Threshold,MaxDist})->  createZipList(L,S,Sensors,{Threshold,MaxDist},[]).
%createZipList(L,S,_,_,Acc) when L==[] ; S==[]-> Acc;
createZipList([],_,_,_,Acc)-> Acc;
createZipList([_|PT],[nil|ST],Sensors,{Threshold,MaxDist},Acc)->  createZipList(PT,ST,Sensors,{Threshold,MaxDist},Acc);
createZipList([PH|PT],[SH|ST],Sensors,{Threshold,MaxDist},Acc)-> 

    A = [{PH,
	  under_threshold(proplists:get_value(SH,Sensors),
			  {Threshold,MaxDist})}|Acc],

    createZipList(PT,ST,Sensors,{Threshold,MaxDist},A).

%=====================================================================
