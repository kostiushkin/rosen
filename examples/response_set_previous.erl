%
% response_set_previous.erl
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
% $Id: response_set_previous.erl,v 1.2 2008/07/16 20:17:28 slackydeb Exp $
%
%% @doc A collision layer implementing a basic collision response.

-module (response_set_previous).
-behaviour (collision).

-include ("geometry.hrl").

-export([layer/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Function: layer/2
%%====================================================================
%% @spec layer(CollidingObjectPidList,
%%             CompleteObjectPidList) -> ok
%%       CollidingObjectPidList = CompleteObjectPidList = [pid()]
%%
%% @doc Acts a basic collision response, moving colliding objects to
%%      their previous position, axis and up.
%%      The second parameter is not used.
%%
layer (CollidingObjectPidList, _CompleteObjectPidList) ->
    response (CollidingObjectPidList).

%% response/1
response (CollidingObjectPidList) ->
    lists:foreach (
      fun (CollidingObjectPid) ->
              set_previous_position_axis_up (CollidingObjectPid)
      end,
      CollidingObjectPidList),
    ok.

%% set_previous_position_axis_up/1
set_previous_position_axis_up (ObjectPid) ->
    CurrentObjectRecord = object3d:obj (ObjectPid),
    object3d:position (ObjectPid, CurrentObjectRecord#object3d.prev_position),
    object3d:axis (ObjectPid, CurrentObjectRecord#object3d.prev_axis),
    object3d:up (ObjectPid, CurrentObjectRecord#object3d.prev_up),
    ok.
