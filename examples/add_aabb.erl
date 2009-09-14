%
% add_aabb.erl
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
% $Id: add_aabb.erl,v 1.2 2008/07/16 20:17:28 slackydeb Exp $
%
%% @doc A collision layer computing the AABB (axis aligned bounding
%%      box) of each object.
%%
%% <p>This module is useful before
%% <code>broad_phase_aabbs_intersect</code> layer.</p>
%%
%% <p>This module expects an object dictionary (use
%% <code>to_object_dict</code> layer).</p>

-module (add_aabb).
-behaviour (collision).

-export([layer/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Function: layer/2
%%====================================================================
%% @spec layer(ObjectDict, CompleteObjectPidList) -> NewObjectDict
%%       ObjectDict = NewObjectDict = dictionary()
%%       CompleteObjectPidList = [pid()]
%%
%% @doc Adds the AABB list to the features dictionary of each object
%%      of the dictionary specified, inserting the atom
%%      <code>aabb</code> as key and the AABB list as value.
%%      The second parameter is not used.
%%
layer (ObjectDict, _CompleteObjectPidList) ->
    add_aabb_to_object_dict (ObjectDict).

%% add_aabb_to_object_dict/1
add_aabb_to_object_dict (ObjectDict) ->
    dict:map (fun (_ObjectPid, FeatureDict) ->
                      add_aabb_to_feature_dict (FeatureDict)
              end,
              ObjectDict).

%% add_aabb_to_feature_dict/1
add_aabb_to_feature_dict (FeatureDict) ->
    dict:store (aabb,
                lists:map (fun (Mesh) ->
                                   aabb:from_mesh (Mesh)
                           end,
                           dict:fetch (mesh, FeatureDict)),
                FeatureDict).
