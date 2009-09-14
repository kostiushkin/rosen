%
% cube.erl
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
% $Id: cube.erl,v 1.8 2008/10/21 22:17:05 aristidena Exp $
%
%%
%% @doc The module implementing a 3D cube.
%%
%% <p>A cube is created using the
%% function <code>oject3d:new/1</code>, passing the proper
%% <code>#object3d{}</code>
%% record, whose fields have the following meaning:</p>
%% <ul>
%% <li><code>type</code>, must be set equal to atom <code>cube</code>.</li>
%% <li><code>name</code>, a name assigned to the process handling
%%     this cube (set it only if you want to register the process).</li>
%% <li><code>position</code>, a <code>#vector{} </code>representing the
%%     <i>(x,y,z)</i> position of the center of the cube (use the
%%     <code>?VECTOR(X,Y,Z)</code> macro defined in <code>geometry.hrl</code>).
%%     </li>
%% <li><code>axis</code>, a <code>#vector{}</code> representing the orientation
%%     of the cube axis. The default is along the <i>z</i> axis.</li>
%% <li><code>up</code>, a <code>#vector{}</code> representing the orientation
%%     of the up vector. The default is along the <i>y</i> axis.</li>
%% <li><code>color</code>, the cube's color, expressed in RGB using
%%     macro <code>?RGB(R,G,B)</code> defined in <code>geometry.hrl</code>.
%%     </li>
%% <li><code>size</code>, the size of the cube's edge.</li>
%% </ul>
%%
%% <p>Example:</p>
%% <pre>
%%  object3d:new (#object3d { type = cube,
%%                            name = mycube,
%%                            size = 2.0,
%%                            position = ?VECTOR (0.0, -0.1, 0.6),
%%                            axis = ?VECTOR (0.0, 1.0, 1.0),
%%                            color = ?RGB(1.0, 1.0, 0.0)}).
%% </pre>
%%

-module (cube).
-behaviour (object3d).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").

-include("geometry.hrl").

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
draw (TheCube) ->
  box:draw (cube2box (TheCube)),
  {ok, TheCube}.


%%====================================================================
%% Func: init/1
%% @private
%%====================================================================
init (Obj) ->
  box:init (cube2box (Obj)).


%%====================================================================
%% Func: terminate/2
%% @private
%%====================================================================
terminate (_, _) ->
  ok.


%%====================================================================
%% Function: cube2box/1
%% @private
%%====================================================================
%% @spec cube2box(TheCube::object3d()) -> TheBox::object3d()
%%
%% @doc Converts a cube to a box.
%%
cube2box (TheCube = #object3d { size = S }) ->
  _TheBox = TheCube#object3d { width = S,
                               height = S,
                               depth = S }.
