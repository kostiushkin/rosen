%
% target_finder.erl
%
% ----------------------------------------------------------------------
%%
%%  ROSEN, a RObotic Simulation Erlang eNgine
%%  Copyright (C) 2007 Corrado Santoro (csanto@diit.unict.it)
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%%
%%
%%  @doc The module provides functions to find interesting zones as nest and 
%%       target areas.
%%
% $Id: target_finder.erl,v 1.3 2008/10/21 21:36:35 aristidena Exp $
%%

-module (target_finder).

-include ("geometry.hrl").
-include ("robot.hrl").

-export ([is_target/3]).



%%====================================================================
%% Functions
%%====================================================================
%% Func: is_target/3
%%====================================================================
%% @spec is_target(X,Y,Direction) -> Result
%%       X = term() 
%%       Y = term()
%%       Direction = to_nest | to_target
%%       Result = bool()
%%
%% @doc Determines if the current position is in a target area.
%%

%CONTROLLO PER IL QUADRATO ARANCIO (TARGET)
is_target(X,Y,to_target) when X <  -?CM(50) ; X > ?CM(50) -> false;
is_target(X,Y,to_target) when Y <  -?CM(50) ; Y > ?CM(50) -> false;
%_____________________________________________________________________

%CONTROLLO PER IL QUADRATO GIALLO (NEST)
is_target(X,Y,to_nest) when X < -?CM(200)   ; X > ?CM(200)     -> false; 
is_target(X,Y,to_nest) when Y < -?CM(367.5) ; Y > -?CM(267.5)  -> false; 
is_target(_, _,Direction) -> true.
