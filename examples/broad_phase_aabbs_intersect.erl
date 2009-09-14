%
% broad_phase_aabbs_intersect.erl
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
% $Id: broad_phase_aabbs_intersect.erl,v 1.3 2008/08/04 21:49:03 slackydeb Exp $
%
%% @doc A collision layer implementing a broad phase collision
%%      detection between AABB.
%%
%% <p>This module filters a list of couples of objects with AABB (use
%% <code>add_aabb</code> and <code>to_object_couples</code> layers)
%% returning the couples whose AABB intersect.</p>

-module (broad_phase_aabbs_intersect).
-behaviour (collision).

-export([layer/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Function: layer/2
%%====================================================================
%% @spec layer({ObjectDict, ObjectPidCoupleList},
%%             CompleteObjectPidList) -> {ObjectDict, NewObjectPidCoupleList}
%%       ObjectDict = dictionary()
%%       ObjectPidCoupleList = NewObjectPidCoupleList = {pid(), pid()}
%%       CompleteObjectPidList = [pid()]
%%
%% @doc For each object in the dictionary specified, considers the
%%      feature named <code>aabb</code> (that must be an AABB list),
%%      remove couples whose AABB don't intersect and returns the
%%      first parameter updated.
%%      The second parameter is not used.
%%
layer (Tuple = {_ObjectDict, _ObjectPidCoupleList},
       _CompleteObjectPidList) ->
    broad_phase_aabbs_intersect (Tuple).

%% broad_phase_aabbs_intersect/1
broad_phase_aabbs_intersect ({ObjectDict, ObjectPidCoupleList}) ->
    NewObjectPidCoupleList =
        broad_phase_aabbs_intersect (ObjectDict, ObjectPidCoupleList),
    {ObjectDict, NewObjectPidCoupleList}.

%% broad_phase_aabbs_intersect/2
broad_phase_aabbs_intersect (ObjectDict, ObjectPidCoupleList) ->
    broad_phase_collision_detection:pairwise_prune (
      _Pred = fun (_BV1 = AABBList1,
                   _BV2 = AABBList2) ->
                      aabb:aabbs_intersect (AABBList1, AABBList2)
              end,
      _Couples = ObjectPidCoupleList,
      _Fun = fun (_Elem = ObjectPid) ->
                     dict:fetch (aabb, dict:fetch (ObjectPid, ObjectDict))
             end).
