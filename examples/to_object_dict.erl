%
% to_object_dict.erl
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
% $Id: to_object_dict.erl,v 1.2 2008/07/16 20:17:28 slackydeb Exp $
%
%% @doc A collision layer converting a list to a dictionary.
%%
%% <p>This module is useful before <code>add_mesh</code> and
%% <code>add_aabb</code> layers.</p>

-module (to_object_dict).
-behaviour (collision).

-export([layer/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Function: layer/2
%%====================================================================
%% @spec layer(ObjectPidList, CompleteObjectPidList) -> ObjectDict
%%       ObjectPidList = CompleteObjectPidList = [pid()]
%%       ObjectDict = dictionary
%%
%% @doc Converts the first list to a dictionary, with
%%      <ul>
%%      <li>key: the object pid of the first list</li>
%%      <li>value: an empty dictionary of features</li>
%%      </ul>
%%      The second parameter is not used.
%%
layer (ObjectPidList, _CompleteObjectPidList) ->
    dict:from_list (lists:map (fun (ObjectPid) ->
                                       {ObjectPid, dict:new ()}
                               end,
                               ObjectPidList)).
