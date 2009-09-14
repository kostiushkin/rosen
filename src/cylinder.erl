%
% cylinder.erl
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
% $Id: cylinder.erl,v 1.6 2008/08/06 20:32:30 slackydeb Exp $
%
%%
%% @doc The module implementing a 3D cylinder.
%%
%% <p>A cylinder is created using the
%% function <code>oject3d:new/1</code>, passing the proper
%% <code>#object3d{}</code>
%% record, whose fields have the following meaning:</p>
%% <ul>
%% <li><code>type</code>, must be set equal to atom <code>cylinder</code>.</li>
%% <li><code>name</code>, a name assigned to the process handling
%%     this cylinder (set it only if you want to register the process).</li>
%% <li><code>position</code>, a <code>#vector{} </code>representing the
%%     <i>(x,y,z)</i> position of the center of the <b>base</b> of the
%%     cylinder (use the
%%     <code>?VECTOR(X,Y,Z)</code> macro defined in <code>geometry.hrl</code>).
%%     </li>
%% <li><code>axis</code>, a <code>#vector{}</code> representing the orientation
%%     of the cylinder length (height).
%%     The default is along the <i>z</i> axis.</li>
%% <li><code>up</code>, a <code>#vector{}</code> representing the orientation
%%     of the up vector. The default is along the <i>y</i> axis.</li>
%% <li><code>color</code>, the cylinder's color, expressed in RGB using
%%     macro <code>?RGB(R,G,B)</code> defined in <code>geometry.hrl</code>.
%%     </li>
%% <li><code>radius</code>, the radius of the base of the cylinder.</li>
%% <li><code>size</code>, cylinder's height.</li>
%% </ul>
%%
%% <p>Example:</p>
%% <pre>
%%  object3d:new (#object3d { type = cylinder,
%%                            name = mycylinder,
%%                            radius = 0.1,
%%                            size = 0.75,
%%                            position = ?VECTOR (0.0, -0.1, 0.6),
%%                            axis = ?VECTOR (0.0, 1.0, 1.0),
%%                            color = ?RGB(1.0, 1.0, 0.0)}).
%% </pre>
%%
%
%
-module (cylinder).
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
%% @private
%%====================================================================
draw (Obj) ->
  rosen:color (Obj#object3d.color),
  glu:cylinder (Obj#object3d.quad,
                Obj#object3d.radius,
                Obj#object3d.radius,
                Obj#object3d.size,
                32,
                32),
  glu:disk (Obj#object3d.quad,
            0,
            Obj#object3d.radius,
            32,
            32),
  gl:pushMatrix (),
  gl:translatef (0.0, 0.0, Obj#object3d.size),
  glu:disk (Obj#object3d.quad,
            0,
            Obj#object3d.radius,
            16,
            16),
  gl:popMatrix (),

  {ok, Obj}.

%%====================================================================
%% Func: init/1
%% @private
%%====================================================================
init (Obj = #object3d {}) ->
  O = object3d:copy_default_axis (
        Obj#object3d { quad = glu:newQuadric (),
                       default_axis = ?VECTOR (0.0, 0.0, 1.0) }),
  O2 = object3d:copy_default_up (
         O#object3d { default_up = ?VECTOR (0.0, 1.0, 0.0) }),

  %% build relative mesh
  RelativeMeshes = [ build_cylinder_relative_mesh (Obj) ],
  O3 = O2#object3d { relative_meshes = RelativeMeshes },

  {ok, O3}.


%%====================================================================
%% Func: terminate/2
%% @private
%%====================================================================
terminate (_, Obj) ->
  glu:deleteQuadric (Obj#object3d.quad),
  ok.


%%====================================================================
%% Function: build_cylinder_relative_mesh/1
%% @private
%%====================================================================
%% @spec build_cylinder_relative_mesh(TheCylinder::object3d()) -> mesh()
%%
%% @doc Builds the relative mesh of the cylinder specified.
%%
build_cylinder_relative_mesh (#object3d { radius = Radius, size = Size }) ->
  build_cylinder_relative_mesh (Radius, Size).

%% build_cylinder_relative_mesh/2
build_cylinder_relative_mesh (Radius, Size) ->
  NSlices = 8,

  %% keys
  %%
  %% from key 0 to (NSlices - 1) are the low vertices; from key
  %% NSlices to ((2 * NSlices) - 1) are the high vertices
  LowKeys = lists:seq (0, NSlices - 1),
  HighKeys = lists:map (fun (K) -> (K + NSlices) end, LowKeys),

  %% vertices
  %%
  %% vertices n and (n + NSlices) are almost the same, differs only on
  %% the z axis
  Vertices =
    lists:flatmap (
      fun (LowKey) ->
              Angle = (2 * math:pi ()) * LowKey / NSlices,
              X = Radius * math:cos (Angle),
              Y = Radius * math:sin (Angle),
              [{LowKey, ?VECTOR(X,Y,0)},
               {LowKey + NSlices, ?VECTOR(X,Y,Size)}]
      end,
      LowKeys),

  %% faces
  Faces =
    lists:append (
      [{LowKey1,
        LowKey2,
        LowKey2 + NSlices,
        LowKey1 + NSlices} || LowKey1 <- LowKeys,
                              LowKey2 <- LowKeys,
                              LowKey2 == (LowKey1 + 1) rem NSlices],
      [list_to_tuple (LowKeys),
       list_to_tuple (HighKeys)]),

  ?MESH (dict:from_list (Vertices), Faces).
