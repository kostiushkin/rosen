%
% set_objects_collision_points.erl
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
% $Id: set_objects_collision_points.erl,v 1.2 2008/08/16 21:06:52 slackydeb Exp $
%
%% @doc A collision layer related to
%%      <code>narrow_phase_trimeshes_intersect</code>.
%%
%% <p>This module sets the collision points of each
%% <code>object3d</code> of the list specified.</p>
%%
%% <p>This module expects a clean collision dictionary as returned by
%% <code>narrow_phase_trimeshes_intersect</code> layer. If you are not
%% sure the collision dict is clean, use the
%% <code>clean_collision_dict</code> layer before this one.</p>

-module (set_objects_collision_points).
-behaviour (collision).

-export([layer/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Function: layer/2
%%====================================================================
%% @spec layer(CollisionDict, CompleteObjectPidList) -> CollisionDict
%%       CollisionDict = dictionary()
%%       CompleteObjectPidList = [pid()]
%%
%% @doc Sets the collision points (i.e. the centroids of the colliding
%%      triangles) of each <code>object3d</code> of the list
%%      specified, and returns the first parameter. Colliding objects
%%      are obtained from the dictionary; objects in the list but not
%%      in the dictionary are considered not colliding.
%%
layer (CollisionDict, CompleteObjectPidList) ->
    ok = set_objects_collision_points (CollisionDict, CompleteObjectPidList),
    CollisionDict.

%% set_objects_collision_points/2
set_objects_collision_points (CollisionDict, CompleteObjectPidList) ->

    %% set the collision points of the colliding objects
    dict:map (
      fun (ObjectPid, ObjectDict) ->
              object3d:collision_points (ObjectPid,
                                         collision_points (ObjectDict))
      end,
      CollisionDict),

    %% set the collision points (i.e. none) of the not colliding
    %% objects
    CollidingObjectPidList = dict:fetch_keys (CollisionDict),
    lists:map (
      fun (ObjectPid) ->
              object3d:collision_points (ObjectPid, [])
      end,
      _NotCollidingObjectPidList = lists:subtract (CompleteObjectPidList,
                                                   CollidingObjectPidList)),

    ok.

%% collision_points/1
%%
%% fixed an object dictionary, compute collision points
collision_points (ObjectDict) ->
    lists:map (fun (T) -> triangle:centroid (T) end,
               colliding_triangles (ObjectDict)).

%% colliding_triangles/1
%%
%% fixed an object dictionary, fetch colliding triangles
colliding_triangles (ObjectDict) ->
    dict:fetch_keys (ObjectDict).
