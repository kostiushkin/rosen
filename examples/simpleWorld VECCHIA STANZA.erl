%
% simpleWorld.erl
%
% ----------------------------------------------------------------------
%

-module (simpleWorld).
-export ([create/0]).

-include ("geometry.hrl").
-include ("robot.hrl").

-define (ROBOT_SIZE, ?CM(10)).

create () ->

  %% the working area
  {ok, WPid} = world:start_link (#world {name= home_plant,
					 width = ?CM(780),
                                         height = ?CM(730),
                                         color = ?RGB(1.0, 1.0, 0.90)}),





%% Prospetto Frontale
world:wall (WPid,
	    -?CM(392.5), -?CM(314),
%              ?CM(104.5), ?CM(30), ?CM(2.5),
	    ?CM(107), ?CM(30), ?CM(2.5),
	    90.0,
	    ?RGB(1.0, 0.97, 0.52)),

world:wall (WPid,
	    -?CM(392.5), -?CM(166),
	    ?CM(70), ?CM(30), ?CM(2.5),
	    90.0,
	    ?RGB(1.0, 0.97, 0.52)),

world:wall (WPid,
	    -?CM(392.5), ?CM(98),
	    ?CM(194), ?CM(30), ?CM(2.5),
	    90.0,
	    ?RGB(1.0, 0.97, 0.52)),

world:wall (WPid,
	    -?CM(392.5), ?CM(315),
	    ?CM(105), ?CM(30), ?CM(2.5),
	    90.0,
	    ?RGB(1.0, 0.97, 0.52)),


%% Prospetti Laterali

world:wall (WPid,
	    ?CM(0),?CM(367.5), 
	    ?CM(782.5),?CM(30), ?CM(2.5),
	    0.0,
	    ?RGB(1.0, 0.97, 0.52)),

world:wall (WPid,
	    ?CM(0),-?CM(367.5), 
	    ?CM(782.5), ?CM(30), ?CM(2.5),
	    0.0,
	    ?RGB(1.0, 0.97, 0.52)),

%% Prospetto Posteriore
world:wall (WPid,
	    ?CM(392.5), ?CM(306),
	    ?CM(123), ?CM(30), ?CM(2.5),
	    90.0,
	    ?RGB(1.0, 0.97, 0.52)),

world:wall (WPid,
	    ?CM(392.5), 0,
	    ?CM(230), ?CM(30), ?CM(2.5),
	    90.0,
	    ?RGB(1.0, 0.97, 0.52)),

world:wall (WPid,
	    ?CM(392.5), -?CM(306),
	    ?CM(123), ?CM(30), ?CM(2.5),
	    90.0,
	    ?RGB(1.0, 0.97, 0.52)),



 % Nest Box
 	world:paint_floor (WPid,
 			   ?CM(0),-?CM(317.5),
 			   ?CM(400),?CM(100),
 			   ?RGB(1.0, 1.0, 0.0)),		     




 %Target Box
 	world:paint_floor (WPid,
 			   ?CM(0),?CM(0),
 			   ?CM(100),?CM(100),
 			   ?RGB(1.0, 0.50, 0.0)),		     




%% Creazione zone nest e target

%% {ok,Nest_Box} = object3d:new(#object3d {type = box,
%% 					width = ?CM(200),
%% 					height = 0.0,
%% 					depth = ?CM(96),
%% 					position = ?VECTOR (-?CM(0), 0, ?CM(317.5)),
%% 					color = ?RGB(1.0, 1.0, 0.0),
%% 					relative_meshes = []}),

%% {ok,Target_Box} = object3d:new(#object3d {type = box,
%% 					  width = ?CM(100),
%% 					  height = 0.0,
%% 					  depth = ?CM(100),
%% 					  position = ?VECTOR (?CM(0), 0, ?CM(0)),
%% 					  color = ?RGB(1.0, 0.50, 0.0),
%% 					  relative_meshes = []}),


  {ok, WPid}.
  
  
