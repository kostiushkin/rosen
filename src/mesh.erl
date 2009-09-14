%
% mesh.erl
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
% $Id: mesh.erl,v 1.5 2008/07/25 22:06:01 slackydeb Exp $
%
%% @doc Module providing basic functions for meshes.
%%
%% @type mesh() = #mesh{}.
%% A record representing a mesh in 3D space; has the fields:
%% <ul>
%% <li><code>vertices</code>, a dictionary of points (use the
%%     <code>?VECTOR(X,Y,Z)</code> macro defined in
%%     <code>geometry.hrl</code>).</li>
%% <li><code>faces</code>, a list of tuples <code>{K0,K1,K2}</code>,
%%     where <code>K0</code>, <code>K1</code> and <code>K2</code> are
%%     keys of the dictionary <code>vertices</code>.</li>
%% </ul>

-module (mesh).
-include ("mesh.hrl").
-include ("geometry.hrl").

-export([from_object/1]).

                                                %TODO
                                                %
                                                %a function to move
                                                %and rotate a point
                                                %from relative to
                                                %absolute
                                                %(i.e. absolute_object_vector
                                                %or similar) should be
                                                %in object3d.erl or
                                                %geometry.erl, not
                                                %here

%%====================================================================
%% Func: from_object/1
%%====================================================================
%% @spec from_object(object3d()) -> [mesh()]
%%
%% @doc Returns the static absolute mesh list of the
%%      <code>object3d</code> specified, and of its children:
%%      <ul>
%%      <li>static, because considers only the current position and
%%          orientation.</li>
%%      <li>absolute, because all vertices have absolute
%%          coordinates.</li>
%%      </ul>
%%
from_object (#object3d{ pids = Pids,
                        position = Position,
                        default_axis = DefaultAxis,
                        axis = Axis,
                        default_up = DefaultUp,
                        up = Up,
                        relative_meshes = Meshes }) ->
    %% don't using #object3d.objects because it could be outdated
    %% (e.g. without mesh)
    Objects = lists:map (fun (P) -> object3d:obj (P) end, Pids),
    from_object_fields (Objects,
                        Position,
                        DefaultAxis, Axis,
                        DefaultUp, Up,
                        Meshes).

%% from_object_fields/7
%%
%% not a compound object
from_object_fields (_Objects = [],
                    Position = #vector{},
                    DefaultAxis = #vector{}, Axis,
                    DefaultUp = #vector{}, Up,
                    Meshes) ->
    from_object_fields (Position,
                        DefaultAxis, Axis,
                        DefaultUp, Up,
                        Meshes);

%% a compound object
from_object_fields (Objects,
                    Position = #vector{},
                    DefaultAxis = #vector{}, Axis,
                    DefaultUp = #vector{}, Up,
                    _Meshes) ->
    %% collecting meshes of the objects of the compound
    ObjectMeshList =
        lists:flatmap (fun (ObjectRecord) ->
                               from_object (ObjectRecord)
                       end,
                       Objects),
    from_object_fields (Position,
                        DefaultAxis, Axis,
                        DefaultUp, Up,
                        ObjectMeshList).

%% from_object_fields/6
%%
%% only a mesh to make absolute
from_object_fields (Position = #vector{},
                    DefaultAxis = #vector{}, Axis,
                    DefaultUp = #vector{}, Up,
                    Mesh = #mesh{}) ->
    %% return the same mesh, substituting relative vertices with
    %% absolute ones
    ?MESH(dict:map (fun (_Key, RelativeVector) ->
                            absolute_object_vector (RelativeVector,
                                                    Position,
                                                    DefaultAxis, Axis,
                                                    DefaultUp, Up)
                    end,
                    Mesh#mesh.vertices),
          Mesh#mesh.faces);

%% a list of meshes to make absolute
from_object_fields (Position = #vector{},
                    DefaultAxis = #vector{}, Axis,
                    DefaultUp = #vector{}, Up,
                    Meshes) ->
    lists:map (fun (M) ->
                       from_object_fields (Position,
                                           DefaultAxis, Axis,
                                           DefaultUp, Up,
                                           M)
               end,
               Meshes).

%% absolute_object_vector/6
%%
%% only a vector of the object to make absolute
absolute_object_vector (RelativeVector = #vector{},
                        Position = #vector{},
                        DefaultAxis = #vector{}, Axis,
                        DefaultUp = #vector{}, Up) ->

    %% considering up, then axis and finally position
    OrientationUp =
        if
            Up =/= undefined ->
                geometry:rotate (RelativeVector,
                                 - geometry:angle (DefaultUp, Up),
                                 geometry:cross (DefaultUp, Up));
            true -> RelativeVector
        end,

    Orientation = geometry:rotate (OrientationUp,
                                   - geometry:angle (DefaultAxis, Axis),
                                   geometry:cross (DefaultAxis, Axis)),

    geometry:add (Position, Orientation).
