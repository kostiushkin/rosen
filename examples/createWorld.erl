%
% createWorld.erl
%
% ----------------------------------------------------------------------
%
% $Id: createWorld.erl,v 1.3 2008/10/21 21:11:30 aristidena Exp $


-module (createWorld).
-export ([create/0]).

-include ("geometry.hrl").
-include ("robot.hrl").

-define (ROBOT_SIZE, ?CM(10)).

create () ->

  %% the working area
  {ok, WPid} = rosen_world:start_link (#world {name= home_plant,
					 width = ?CM(780),
                                         height = ?CM(730),
                                         color = ?RGB(1.0, 1.0, 0.90)}),



   Walls_List = [
		%% Prospetto Frontale		
		{-?CM(392.5), -?CM(314),
		 ?CM(102), ?CM(30), 
		 ?CM(5.0), 90.0,
		 ?RGB(1.0, 0.97, 0.52)},

		{-?CM(392.5), -?CM(166),
		 ?CM(70), ?CM(30), 
		 ?CM(5.0),90.0,
		 ?RGB(1.0, 0.97, 0.52)},
		
		{-?CM(392.5), ?CM(98),
		 ?CM(194), ?CM(30), 
		 ?CM(5.0), 90.0,
		 ?RGB(1.0, 0.97, 0.52)},
		
		{-?CM(392.5), ?CM(315),
		 ?CM(100), ?CM(30), 
		 ?CM(5.0), 90.0,
		 ?RGB(1.0, 0.97, 0.52)},
		
		%% Prospetti Laterali
		{?CM(0),?CM(367.5), 
		 ?CM(780),?CM(30), 
		 ?CM(5.0),0.0,
		 ?RGB(1.0, 0.97, 0.52)},

		{?CM(0),-?CM(367.5), 
		 ?CM(780), ?CM(30), 
		 ?CM(5.0),0.0,
		 ?RGB(1.0, 0.97, 0.52)},

		%% Prospetto Posteriore
		{?CM(392.5), ?CM(306),
		 ?CM(118), ?CM(30), 
		 ?CM(5.0),90.0,
		 ?RGB(1.0, 0.97, 0.52)},
		
		{?CM(392.5), 0,
		 ?CM(230), ?CM(30), 
		 ?CM(5.0),90.0,
		 ?RGB(1.0, 0.97, 0.52)},
		
		{?CM(392.5), -?CM(306),
		 ?CM(118), ?CM(30), 
		 ?CM(5.0),90.0,
		 ?RGB(1.0, 0.97, 0.52)},
		
		%% Pareti interne
		
		{?CM(175), ?CM(0),
		 ?CM(432), ?CM(30), 
		 ?CM(10.0),0.0,
 		 ?RGB(1.0, 0.97, 0.52)},

                {-?CM(45), ?CM(240),
		 ?CM(250), ?CM(30), 
		 ?CM(10.0),90.0,
  		 ?RGB(1.0, 0.97, 0.52)},

                {-?CM(275), ?CM(180),
		 ?CM(230), ?CM(30), 
		 ?CM(10.0),0.0,
  		 ?RGB(1.0, 0.97, 0.52)},

                {-?CM(275), ?CM(40),
		 ?CM(220), ?CM(30), 
		 ?CM(10.0),0.0,
  		 ?RGB(1.0, 0.97, 0.52)},

                {-?CM(165), ?CM(70),
		 ?CM(70), ?CM(30), 
		 ?CM(10.0),90.0,
  		 ?RGB(1.0, 0.97, 0.52)},

                {-?CM(285), -?CM(180),
		 ?CM(200), ?CM(30), 
		 ?CM(10.0),0.0,
  		 ?RGB(1.0, 0.97, 0.52)},

                {-?CM(115), -?CM(270),
		 ?CM(180), ?CM(30), 
		 ?CM(10.0),90.0,
  		 ?RGB(1.0, 0.97, 0.52)},

		{-?CM(135), -?CM(180),
                 ?CM(30), ?CM(30),
		 ?CM(10.0),0.0,
   		 ?RGB(1.0, 0.97, 0.52)}],


createWalls(Walls_List,WPid), 

 {ok, WPid}.
  

  
%
createWalls([],_)-> ok;
createWalls([{X0, Y0,Length, Width,Tickness,Orientation,Color}|T],WPid)->    
    rosen_world:wall (WPid,
		      X0, Y0,
		      Length, Width, Tickness,
		      Orientation,
		      Color).
