%
% trimesh.erl
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
% $Id: trimesh.erl,v 1.4 2008/08/06 09:23:52 slackydeb Exp $
%
%% @doc Module providing basic functions for trimeshes.
%%
%% <p>For functions dealing with meshes in general (i.e. with not only
%% triangles as faces) see the <code>mesh</code> module.</p>

-module (trimesh).
-include ("aabb.hrl").
-include ("geometry.hrl").
-include ("mesh.hrl").
-include ("triangle.hrl").

-export([trimeshes_intersect/2,
         from_mesh/1,
         test/0]).


%%====================================================================
%% Function: trimeshes_intersect/2
%%====================================================================
%% @spec trimeshes_intersect(Mesh1, Mesh2) -> TupleList
%%       Mesh1 = mesh() | [mesh()]
%%       Mesh2 = mesh() | [mesh()]
%%       TupleList = [{triangle(), triangle()}]
%%
%% @doc Given two absolute trimeshes, returns the couples of faces
%%      that intersect; the first face is of the first trimesh, the
%%      second face is of the second trimesh.
%%

%% main clause
trimeshes_intersect (#mesh{ vertices = VertexDict1, faces = FaceList1 },
                     #mesh{ vertices = VertexDict2, faces = FaceList2 }) ->
    AllFaceCouples =
        [{?TRIANGLE(dict:fetch(K0, VertexDict1),
                    dict:fetch(K1, VertexDict1),
                    dict:fetch(K2, VertexDict1)),
          ?TRIANGLE(dict:fetch(J0, VertexDict2),
                    dict:fetch(J1, VertexDict2),
                    dict:fetch(J2, VertexDict2))} || {K0,K1,K2} <- FaceList1,
                                                     {J0,J1,J2} <- FaceList2],
    PossiblyCollidingFaceCouples = broad_phase (AllFaceCouples),
    narrow_phase (PossiblyCollidingFaceCouples);
%%
trimeshes_intersect (M = #mesh{}, MeshList) ->
    trimeshes_intersect ([M], MeshList);
%%
trimeshes_intersect (MeshList, M = #mesh{}) ->
    trimeshes_intersect (MeshList, [M]);
%%
trimeshes_intersect (MeshList1, MeshList2) ->
    lists:flatmap (fun ({M1 = #mesh{}, M2 = #mesh{}}) ->
                           trimeshes_intersect (M1, M2)
                   end,
                   [{Mesh1, Mesh2} || Mesh1 <- MeshList1,
                                      Mesh2 <- MeshList2]).

%% broad_phase/1
broad_phase (AllFaceCouples) ->
    broad_phase_collision_detection:pairwise_prune (
      _Pred = fun (_BV1 = AABB1 = #aabb{},
                   _BV2 = AABB2 = #aabb{}) ->
                      aabb:aabbs_intersect (AABB1, AABB2)
              end,
      _Couples = AllFaceCouples,
      _Fun = fun (_Elem = Face = #triangle{}) ->
                     triangle:aabb (Face)
             end).

%% narrow_phase/1
narrow_phase (PossiblyCollidingFaceCouples) ->
    lists:filter (fun ({Face1 = #triangle{}, Face2 = #triangle{}}) ->
                          triangle:triangles_intersect (Face1, Face2)
                  end,
                  PossiblyCollidingFaceCouples).


%%====================================================================
%% Func: from_mesh/1
%%====================================================================
%% @spec from_mesh(Mesh) -> Trimesh
%%       Mesh = Trimesh = mesh() | [mesh()]
%%
%% @doc Converts the mesh specified to a trimesh (i.e. faces that are
%%      polygons with 4 or more vertices are triangulated); it is
%%      assumed that all faces are convex.
%%

%% main clause
from_mesh (M = #mesh{ faces = FaceList }) ->
    TriangulatedFaceList =
        lists:flatmap (fun (F) -> triangulate_face (F) end,
                       FaceList),
    M#mesh{ faces = TriangulatedFaceList };
%%
from_mesh (MeshList) ->
    lists:map (fun (M = #mesh{}) -> from_mesh (M) end,
               MeshList).

%% triangulate_face/1
triangulate_face (Face) ->
    [V0 | [V1 | Rest]] = tuple_to_list (Face),
    triangulate_face (V0, V1, Rest, []).

%% triangulate_face/4
%%
%% only a triangle
triangulate_face (V0, V1, _OtherVertices = [V2], Acc) ->
    [{V0, V1, V2} | Acc];
%%
triangulate_face (V0, V1, _OtherVertices = [V2 | Rest], Acc) ->
    triangulate_face (V0, V2, Rest,
                      triangulate_face (V0, V1, [V2], Acc)).


test () ->
    test_trimeshes_intersect (),

    ok.



%%====================================================================
%% Test functions
%%====================================================================

test_trimeshes_intersect () ->
    %% at least one of the two meshes is empty
    [] = trimeshes_intersect (?MESH(dict:new (), []),
                              ?MESH(dict:new (), [])),

    %% both meshes contain only one face
    %%
    %% not parallel, a triangle intersecting the other plane but not
    %% the other triangle
    VL0 = [{0, ?VECTOR(-1,-1,0.5)},
           {1, ?VECTOR(-1,-2,0.5)},
           {2, ?VECTOR(-2,-1,0.5)}],
    VL1 = [{0, ?VECTOR(0,0,0)},
           {1, ?VECTOR(0,0,1)},
           {2, ?VECTOR(0,1,0)}],
    [] = trimeshes_intersect (?MESH(dict:from_list (VL0),
                                    [{0,1,2}]),
                              ?MESH(dict:from_list (VL1),
                                    [{0,1,2}])),
    %% intersecting
    VL2 = [{0, ?VECTOR(0,0,0)},
           {1, ?VECTOR(1,0,0)},
           {2, ?VECTOR(0,1,0)}],
    VL3 = [{0, ?VECTOR(-1,-1,0)},
           {1, ?VECTOR(0.5,0.5,-1)},
           {2, ?VECTOR(0.5,0.5,1)}],
    [{?TRIANGLE(?VECTOR(0,0,0),
                ?VECTOR(1,0,0),
                ?VECTOR(0,1,0)),
      ?TRIANGLE(?VECTOR(-1,-1,0),
                ?VECTOR(0.5,0.5,-1),
                ?VECTOR(0.5,0.5,1))}] =
        trimeshes_intersect (?MESH(dict:from_list (VL2),
                                   [{0,1,2}]),
                             ?MESH(dict:from_list (VL3),
                                   [{0,1,2}])),

    %% the first mesh contains only one face and the second mesh
    %% contains more than one face
    VL11 = [{3, ?VECTOR(1,1,1)} | VL1],
    [] = trimeshes_intersect (?MESH(dict:from_list (VL0),
                                    [{0,1,2}]),
                              ?MESH(dict:from_list (VL11),
                                    [{0,1,2},
                                     {0,1,3},
                                     {0,2,3},
                                     {1,2,3}])),
    VL31 = [{3, ?VECTOR(1,1,1)} | VL3],
    [{?TRIANGLE(?VECTOR(0,0,0),
                ?VECTOR(1,0,0),
                ?VECTOR(0,1,0)),
      ?TRIANGLE(?VECTOR(-1,-1,0),
                ?VECTOR(0.5,0.5,-1),
                ?VECTOR(0.5,0.5,1))}] =
        trimeshes_intersect (?MESH(dict:from_list (VL2),
                                   [{0,1,2}]),
                             ?MESH(dict:from_list (VL31),
                                   [{0,1,2},
                                    {0,2,3}])),

    %% the first mesh contains more than one face and the second mesh
    %% is not empty
    VL4 = [{0, ?VECTOR(-10,-10,-10)},
           {1, ?VECTOR(-11,-11,-11)},
           {2, ?VECTOR(-10,-10,-11)},
           {3, ?VECTOR(-12,-10,-11)}],
    [] = trimeshes_intersect (?MESH(dict:from_list (VL11),
                                    [{0,1,2},
                                     {0,1,3},
                                     {0,2,3},
                                     {1,2,3}]),
                              ?MESH(dict:from_list (VL4),
                                    [{0,1,2},
                                     {0,1,3},
                                     {0,2,3},
                                     {1,2,3}])),

    ok.
