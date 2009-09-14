%
% line.erl
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
% $Id: line.erl,v 1.2 2008/03/31 19:05:54 slackydeb Exp $
%
-module (line).
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
%%====================================================================
%% @private
draw(TheLine)->
  StartPoint = TheLine#object3d.start_point,
  EndPoint = TheLine#object3d.end_point,
  gl:glBegin(?GL_LINES),
  gl:color3fv (TheLine#object3d.color),
  gl:vertex3d(StartPoint#vector.x,StartPoint#vector.y,StartPoint#vector.z),
  gl:vertex3d(EndPoint#vector.x,EndPoint#vector.y,EndPoint#vector.z),
  gl:glEnd(),
  {ok,TheLine}.

%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
init (Obj) ->
  O = object3d:copy_default_axis (
        Obj#object3d { default_axis = ?VECTOR (0.0, 0.0, 1.0) }),
  O2 = object3d:copy_default_up (
         O#object3d { default_up = ?VECTOR (0.0, 1.0, 0.0) }),
  {ok, O2}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
%% @private
terminate (_, _) ->
  ok.

