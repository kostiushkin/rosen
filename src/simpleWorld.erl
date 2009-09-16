%
% simpleWorld.erl
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
% $Id: simpleWorld.erl,v 1.5 2008/10/21 21:36:35 aristidena Exp $
%%
%% @doc The module provides customizable creation of simulation 
%%      environments 
%% 


-module (simpleWorld).
-export ([create/0]).

-include ("geometry.hrl").
-include ("robot.hrl").

-define (ROBOT_SIZE, ?CM(10)).

create () ->

  %% the working area
  {ok, WPid} = rosen_world:start_link (#world {name= home_plant,
					 width = ?CM(780),
                                         height = ?CM(740),
                                         color = ?RGB(1.0, 1.0, 0.90)}),

Walls_List = [
	      %% Prospetto Frontale
	      {-?CM(392.5), -?CM(320),
	       ?CM(105), ?CM(30), 
	       ?CM(10), 90.0,
	       ?RGB(1.0, 0.97, 0.52)},
	      
	      {-?CM(392.5), -?CM(166),
	       ?CM(70), ?CM(30), 
	       ?CM(10),90.0,
	       ?RGB(1.0, 0.97, 0.52)},

	      {-?CM(392.5), ?CM(98),
	       ?CM(194), ?CM(30),
	       ?CM(10),90.0,
	       ?RGB(1.0, 0.97, 0.52)},
	      
	      {-?CM(392.5), ?CM(319),
	       ?CM(107), ?CM(30), 
	       ?CM(10),90.0,
	       ?RGB(1.0, 0.97, 0.52)},

	      %% Prospetti Laterali
	      {?CM(0),?CM(372.5), 
	       ?CM(782.5),?CM(30), 
	       ?CM(10),0.0,
	       ?RGB(1.0, 0.97, 0.52)},

	      {?CM(0),-?CM(372.5), 
	       ?CM(782.5), ?CM(30), 
	       ?CM(10), 0.0,
	       ?RGB(1.0, 0.97, 0.52)},

	      %% Prospetto Posteriore
	      {?CM(392.5), ?CM(311),
	       ?CM(123), ?CM(30), 
	       ?CM(10),90.0,
	       ?RGB(1.0, 0.97, 0.52)},

	      {?CM(392.5), 0,
	       ?CM(230), ?CM(30), 
	       ?CM(10),90.0,
	       ?RGB(1.0, 0.97, 0.52)},

	      {?CM(392.5), -?CM(311),
	       ?CM(123), ?CM(30), 
	       ?CM(10),90.0,
	       ?RGB(1.0, 0.97, 0.52)}],

	      %,%% Parete interna
%% %% 	    {-?CM(280), -?CM(200),
%% %% 	    ?CM(220), ?CM(30), 
%% %%       ?CM(10), 0.0,
%% %% 	    ?RGB(1.0, 0.97, 0.52)},
	    %],

createWalls(Walls_List,WPid),

 % Nest Box
 	rosen_world:paint_floor (WPid,
 			   ?CM(0),-?CM(317.5),
 			   ?CM(400),?CM(110),
 			   ?RGB(1.0, 1.0, 0.0)),		     
 %Target Box
 	rosen_world:paint_floor (WPid,
 			   ?CM(0),?CM(0),
 			   ?CM(100),?CM(100),
 			   ?RGB(1.0, 0.50, 0.0)),	
    

  {ok, WPid}.
  
  
  
%
createWalls([],_)-> ok;
createWalls([{X0, Y0,Length, Width,Tickness,Orientation,Color}|T],WPid)->    
    rosen_world:wall (WPid,
		X0, Y0,
		Length, Width, Tickness,
		Orientation,
		Color),
    createWalls(T,WPid).
