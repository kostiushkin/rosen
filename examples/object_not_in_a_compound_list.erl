%
% object_not_in_a_compound_list.erl
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
% $Id: object_not_in_a_compound_list.erl,v 1.2 2008/07/16 20:17:28 slackydeb Exp $
%
%% @doc A collision layer that filters an <code>object3d</code> pid
%%      list to return the list of pids not in a compound object.
%%
%% <p>This module is very useful before other collision layers.</p>

-module (object_not_in_a_compound_list).
-behaviour (collision).

-include ("geometry.hrl").

-export([layer/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Function: layer/2
%%====================================================================
%% @spec layer(ObjectPidList,
%%             CompleteObjectPidList) -> ObjectPidNotInACompoundList
%%       ObjectPidList = ObjectPidNotInACompoundList = [pid()]
%%       CompleteObjectPidList = [pid()]
%%
%% @doc Filters an <code>object3d</code> pid list to return the list
%%      of pids not in a compound object.
%%      The second parameter is not used.
%%
layer (ObjectPidList, _CompleteObjectPidList) ->
    object_not_in_a_compound_list (ObjectPidList).

%% object_not_in_a_compound_list/1
object_not_in_a_compound_list (ObjectPidList) ->
    lists:filter (
      fun (ObjPid) ->
              ObjRecord = object3d:obj (ObjPid),
              %% objects not in a compound are those without parent
              %% object
              not (ObjRecord#object3d.parent_object =/= noname)
      end,
      ObjectPidList).
