%
% sphere.erl
%
% ----------------------------------------------------------------------
%
%  ROSEN, a RObotic Simulation Erlang eNgine
%  Copyright (C) 2007 Corrado Santoro (csanto@diit.unict.it)
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
%
% $Id: sphere.erl,v 1.5 2008/07/15 23:18:04 slackydeb Exp $
%
%%
%% @doc The module implementing a 3D sphere.
%%
%% <p>A sphere is created using the
%% function <code>oject3d:new/1</code>, passing the proper
%% <code>#object3d{}</code>
%% record, whose fields have the following meaning:</p>
%% <ul>
%% <li><code>type</code>, must be set equal to atom <code>sphere</code>.</li>
%% <li><code>name</code>, a name assigned to the process handling
%%     this sphere (set it only if you want to register the process).</li>
%% <li><code>position</code>, a <code>#vector{}</code> representing the
%%     <i>(x,y,z)</i> position of the center of the sphere (use the
%%     <code>?VECTOR(X,Y,Z)</code> macro defined in <code>geometry.hrl</code>).
%%     </li>
%% <li><code>color</code>, the box's color, expressed in RGB using
%%     macro <code>?RGB(R,G,B)</code> defined in <code>geometry.hrl</code>.
%%     </li>
%% <li><code>radius</code>, sphere's radius.</li>
%% </ul>
%%
%% <p>Example:</p>
%% <pre>
%%  object3d:new (#object3d { type = sphere,
%%                            name = mysphere,
%%                            radius = 0.75,
%%                            position = ?VECTOR (0.0, -0.1, 0.6),
%%                            color = ?RGB(1.0, 1.0, 0.0)}).
%% </pre>
%%
%
-module (sphere).
-behaviour (object3d).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").

-include("geometry.hrl").
-include("mesh.hrl").

-export ([init/1,
          draw/1,
          terminate/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: draw/1
%%====================================================================
%% @private
draw (Obj) ->
  rosen:color (Obj#object3d.color),
  glu:sphere (Obj#object3d.quad,
              Obj#object3d.radius,
              32,
              32),
  {ok, Obj}.

%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
init (Obj) ->
  O = object3d:copy_default_axis (
        Obj#object3d { quad = glu:newQuadric (),
                       default_axis = ?VECTOR (0.0, 0.0, 1.0) }),

  %% build relative mesh
  RelativeMeshes = [ build_sphere_relative_mesh (Obj) ],
  O2 = O#object3d { relative_meshes = RelativeMeshes },

  {ok, O2}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
%% @private
terminate (_, Obj) ->
  glu:deleteQuadric (Obj#object3d.quad),
  ok.


%%====================================================================
%% Function: build_sphere_relative_mesh/1
%% @private
%%====================================================================
%% @spec build_sphere_relative_mesh(TheSphere::object3d()) -> mesh()
%%
%% @doc Builds the relative mesh of the sphere specified.
%%
build_sphere_relative_mesh (#object3d { radius = Radius }) ->
  build_sphere_relative_mesh (Radius);
%%
build_sphere_relative_mesh (Radius) ->
  NSlices = 8, %% tested with 8 and 16 slices
  NStacks = NSlices div 2,
  %% assertion; this lets an easier implementation
  0 = NStacks rem 2,

  %% number of vertices
  NMiddleVertices = (NStacks - 1) * NSlices,
  NVertices = 1 + NMiddleVertices + 1,

  %% keys
  %%
  %% key 0 and (NVertices - 1) are the low and high vertices
  LowKey = 0,
  HighKey = NVertices - 1,

  %% vertices
  %%
  %% from key 1 to NMiddleVertices are middle vertices; from key 1 to
  %% NSlices are the nearest vertices to the low vertex, and so on
  %% going up
  %%
  %% vertex N is above vertex (N + NSlices), but on the same side
  LowVertex = {LowKey, ?VECTOR (0.0, 0.0, -Radius)},
  HighVertex = {HighKey, ?VECTOR (0.0, 0.0, Radius)},

  MiddleZs = lists:map (fun (TmpZ) ->
                                Radius * TmpZ / (NStacks / 2)
                        end,
                        lists:seq ( - ((NStacks div 2) - 1),
                                    (NStacks div 2) - 1 )),
  MiddleStubs = lists:zip (lists:seq (0, NStacks - 2),
                           MiddleZs),
  MiddleVertices =
        lists:flatmap (
          fun ({PseudoKey, Z}) ->
                  lists:map (
                    fun (Slice) ->
                            Angle = (2 * math:pi ()) * Slice / NSlices,
                            StackRadius = math:sqrt (Radius * Radius - Z * Z),
                            X = StackRadius * math:cos (Angle),
                            Y = StackRadius * math:sin (Angle),
                            {(PseudoKey * NSlices) + Slice + 1,
                             ?VECTOR(X,Y,Z)}
                    end,
                    lists:seq (0, NSlices - 1))
          end,
          MiddleStubs),

  Vertices = lists:append ([LowVertex, HighVertex], MiddleVertices),

  %% faces
  FirstStackLowKeys = lists:seq (1, NSlices),

  LowTriangles = [{LowKey, Key1, Key2} || Key1 <- FirstStackLowKeys,
                                          Key2 <- FirstStackLowKeys,
                                          Key2 == (Key1 rem NSlices) + 1],

  HighTriangles = lists:map (fun ({_LowKey, Key1, Key2}) ->
                                     {HighKey,
                                      Key1 + NMiddleVertices - NSlices,
                                      Key2 + NMiddleVertices - NSlices}
                             end,
                             LowTriangles),

  FirstStackQuads = [{Key1,
                      Key2,
                      Key2 + NSlices,
                      Key1 + NSlices} || Key1 <- FirstStackLowKeys,
                                         Key2 <- FirstStackLowKeys,
                                         Key2 == (Key1 rem NSlices) + 1],

  RemainingQuads =
        lists:foldl (
          fun (StackIndex, AccIn) ->
                  Offset = StackIndex * NSlices,
                  NewStackQuads =
                      lists:map (fun ({K1,K2,K3,K4}) ->
                                         {K1 + Offset,
                                          K2 + Offset,
                                          K3 + Offset,
                                          K4 + Offset}
                                 end, FirstStackQuads),
                  lists:append (AccIn, NewStackQuads)
          end,
          _Acc0 = [],
          lists:seq (1, NStacks - 3)),

  Faces = lists:append ([LowTriangles,
                         HighTriangles,
                         FirstStackQuads,
                         RemainingQuads]),

  ?MESH (dict:from_list (Vertices), Faces).
