%
% add_mesh.erl
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
% $Id: add_mesh.erl,v 1.2 2008/07/16 20:17:28 slackydeb Exp $
%
%% @doc A collision layer computing the static mesh of each object.
%%
%% <p>This module is useful before
%% <code>narrow_phase_trimeshes_intersect</code> layer.</p>
%%
%% <p>This module expects an object dictionary (use
%% <code>to_object_dict</code> layer).</p>

-module (add_mesh).
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
%% @doc Adds the static absolute mesh list to the features dictionary
%%      of each object of the dictionary specified, inserting the atom
%%      <code>mesh</code> as key and the mesh list as value.
%%      The second parameter is not used.
%%
layer (ObjectDict, _CompleteObjectPidList) ->
    add_mesh_to_object_dict (ObjectDict).

%% add_mesh_to_object_dict/1
add_mesh_to_object_dict (ObjectDict) ->
    dict:map (fun (ObjectPid, FeatureDict) ->
                      add_mesh_to_feature_dict (ObjectPid, FeatureDict)
              end,
              ObjectDict).

%% add_mesh_to_feature_dict/1
add_mesh_to_feature_dict (ObjectPid, FeatureDict) ->
    dict:store (mesh,
                mesh:from_object (object3d:obj (ObjectPid)),
                FeatureDict).
