%
% from_collision_dict_to_object_list.erl
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
% $Id: from_collision_dict_to_object_list.erl,v 1.3 2008/07/30 15:35:49 slackydeb Exp $
%
%% @doc A collision layer converting a dictionary to a list.
%%
%% <p>This module expects a clean collision dictionary as returned by
%% <code>narrow_phase_trimeshes_intersect</code> layer. If you are not
%% sure the collision dict is clean, use the
%% <code>clean_collision_dict</code> layer before this one.</p>

-module (from_collision_dict_to_object_list).
-behaviour (collision).

-export([layer/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Function: layer/2
%%====================================================================
%% @spec layer(CleanedCollisionDict,
%%             CompleteObjectPidList) -> CollidingObjectPidList
%%       CleanedCollisionDict = dictionary()
%%       CompleteObjectPidList = CollidingObjectPidList = [pid()]
%%
%% @doc Returns the list of colliding <code>object3d</code> in the
%%      dictionary specified.
%%      The second parameter is not used.
%%
layer (CleanedCollisionDict, _CompleteObjectPidList) ->
    dict:fetch_keys (CleanedCollisionDict).
