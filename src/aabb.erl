%
% aabb.erl
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
% $Id: aabb.erl,v 1.4 2008/08/06 09:23:52 slackydeb Exp $
%
%% @doc Module providing basic functions for AABB.
%%
%% @type aabb() = #aabb{}.
%% A record representing an AABB (axis aligned bounding box) in 3D
%% space; has the fields:
%% <ul>
%% <li><code>min_vertex</code>, the vertex of the box with the minimum
%%     coordinates (use the <code>?VECTOR(X,Y,Z)</code> macro defined
%%     in <code>geometry.hrl</code>).</li>
%% <li><code>max_vertex</code>, the vertex of the box with the maximum
%%     coordinates (use the <code>?VECTOR(X,Y,Z)</code> macro defined
%%     in <code>geometry.hrl</code>).</li>
%% </ul>

-module (aabb).
-include ("aabb.hrl").
-include ("mesh.hrl").
-include ("geometry.hrl").

-export([aabbs_intersect/2,
         from_vertex_list/1,
         from_mesh/1]).


%%====================================================================
%% Function: aabbs_intersect/2
%%====================================================================
%% @spec aabbs_intersect(AABB1, AABB2) -> bool()
%%       AABB1 = aabb() | [aabb()]
%%       AABB2 = aabb() | [aabb()]
%%
%% @doc Determines whether the two AABBs specified intersect.
%%

%% main clause
aabbs_intersect (#aabb{ min_vertex = MinV1, max_vertex = MaxV1 },
                 #aabb{ min_vertex = MinV2, max_vertex = MaxV2 }) ->
    #vector{ x = MinX1, y = MinY1, z = MinZ1 } = MinV1,
    #vector{ x = MaxX1, y = MaxY1, z = MaxZ1 } = MaxV1,
    #vector{ x = MinX2, y = MinY2, z = MinZ2 } = MinV2,
    #vector{ x = MaxX2, y = MaxY2, z = MaxZ2 } = MaxV2,

    %% separating axis theorem
    not lists:any (fun ({A, B}) -> (A > B) end,
                   [{MinX1, MaxX2}, {MinX2, MaxX1},
                    {MinY1, MaxY2}, {MinY2, MaxY1},
                    {MinZ1, MaxZ2}, {MinZ2, MaxZ1}]);
%%
aabbs_intersect (A = #aabb{}, AABBList) ->
    aabbs_intersect ([A], AABBList);
%%
aabbs_intersect (AABBList, A = #aabb{}) ->
    aabbs_intersect (AABBList, [A]);
%%
aabbs_intersect (AABBList1, AABBList2) ->
    lists:any (fun ({A1 = #aabb{}, A2 = #aabb{}}) ->
                       aabbs_intersect (A1, A2)
               end,
               [{AABB1, AABB2} || AABB1 <- AABBList1,
                                  AABB2 <- AABBList2]).


%%====================================================================
%% Func: from_vertex_list/1
%%====================================================================
%% @spec from_vertex_list([vector()]) -> aabb()
%%
%% @doc Returns the AABB of the finite set of points specified.
%%
from_vertex_list (_VertexList = [H = #vector{} | T]) ->
    {MinVertex, MaxVertex} =
        lists:foldl (
          fun (_V = #vector{ x = X, y = Y, z = Z },
               _AccIn = {#vector{ x = XMin, y = YMin, z = ZMin },
                         #vector{ x = XMax, y = YMax, z = ZMax }}) ->
                  {?VECTOR (lists:min ([X, XMin]),
                            lists:min ([Y, YMin]),
                            lists:min ([Z, ZMin])),
                   ?VECTOR (lists:max ([X, XMax]),
                            lists:max ([Y, YMax]),
                            lists:max ([Z, ZMax]))}
          end,
          _Acc0 = {H, H},
          _List = T),
    ?AABB (MinVertex, MaxVertex).


%%====================================================================
%% Func: from_mesh/1
%%====================================================================
%% @spec from_mesh(mesh()) -> aabb()
%%
%% @doc Returns the AABB of the mesh specified. The mesh must contain
%%      at least a vertex.
%%
from_mesh (#mesh{ vertices = VertexDict }) ->
    from_vertex_list (
      lists:map (fun (K) ->
                         dict:fetch (K, VertexDict)
                 end,
                 dict:fetch_keys(VertexDict))).
