%
% rosen.erl
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
% $Id: rosen.erl,v 1.16 2008/10/21 21:36:35 aristidena Exp $
%
%%
%% @doc The module implementing the 3D engine.
%%
%% <p>This module represents the 3D engine. It handles the collection
%% of the objects of the world, providing the functions to add new objects
%% and to change the view of the world. It also has the responsibility
%% of triggering redrawing and activity execution.</p>
%%

-module (rosen).
-behaviour(gen_fsm).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").
-include("geometry.hrl").

-define (DEFAULT_PERIOD, 10).
-define (DEFAULT_ROTATION_INCREMENT, 0.25).
-define (DEFAULT_ZOOM_INCREMENT, 0.1).
-define (SIMULATED_DELTATIME, 0.1).


-record (engine_state, {object_list = [],
                        world_list = [],
                        period = ?DEFAULT_PERIOD,
                        screen_h,
                        screen_w,
                        rotation = ?VECTOR (0.0, 0.0, 0.0),
                        rotation_increment = ?VECTOR (0.0, 0.0, 0.0),
                        zoom = 0.0,
                        zoom_increment = 0.0,
                        collision_options,
			collision_enable = false,
			time_type, 
			time,
			delta,
			last_time
		       }).


-export ([start_link/0,
          start_link/1,
          zoom/1,
          up/1,
          left/1,
          add_object/1,
          color/1,
          stop/0,
          add_world/1,
	  set_collision/1]).
-export ([init/1, idle_draw/2, terminate/3, handle_event/3]).


%%====================================================================
%% Func: start_link/0
%%====================================================================
%% @spec start_link() -> {ok, Pid}
%%
%% @doc Starts the engine.
%%
start_link () ->
  start_link ([]).


%%====================================================================
%% Func: start_link/1
%%====================================================================
%% @spec start_link(Config) -> ok
%%       Config = [term()]
%%
%% @doc Starts the engine with the options specified.
%%
%% <p>Turns on full screen graphics. Example:</p>
%% <pre>
%%  rosen:start_link ([fullscreen]).
%% </pre>
%%
%% <p>Turns on collision, with the specified modules for the layers of
%% collision handling.
%% Example:</p>
%% <pre>
%%  LayerModules = [ object_not_in_a_compound_list,
%%                   to_object_dict,
%%                   add_mesh,
%%                   add_aabb,
%%                   to_object_couples,
%%                   broad_phase_aabbs_intersect,
%%                   narrow_phase_trimeshes_intersect,
%%                   from_collision_dict_to_object_list,
%%                   response_set_previous ]
%%  rosen:start_link ([{ collision, LayerModules }]).
%% </pre>
%%
%% <p>For details in writing a module for a layer of collision handling,
%% see the module <code>collision</code>.</p>
%%
start_link (Config) ->
  gen_fsm:start_link ({local, ?MODULE}, ?MODULE, Config, []).


%%====================================================================
%% Func: add_object/1
%%====================================================================
%% @spec add_object(ObjectPid) -> ok
%%
%% @doc Adds a new object (represented by its pid) to the engine.
%%
add_object (Object) ->
  gen_fsm:send_event (?MODULE, {add_object, Object}).


%%====================================================================
%% Func: zoom/1
%%====================================================================
%% @spec zoom(Zoom::number()) -> ok
%%
%% @doc Changes the zoom of the camera.
%%
zoom (Zoom) ->
  gen_fsm:send_event (?MODULE, {zoom, Zoom}).


%%====================================================================
%% Func: up/1
%%====================================================================
%% @spec up(PanUp::number()) -> ok
%%
%% @doc Changes the height of the camera.
%%
up (X) ->
  gen_fsm:send_event (?MODULE, {up_down, X}).


%%====================================================================
%% Func: left/1
%%====================================================================
%% @spec left(PanLeft::number()) -> ok
%%
%% @doc Changes the horizontal position of the camera.
%%
left (X) ->
  gen_fsm:send_event (?MODULE, {left_right, X}).



%%====================================================================
%% Func: color/1
%%====================================================================
color (X) when size (X) == 3 ->
  gl:color3fv (X);
color (X) when size (X) == 4 ->
  gl:color4fv (X).


%%====================================================================
%% Func: stop/0
%%====================================================================
%%
%% @spec stop() -> ok
%%
%% @doc Destroys the engine.
%%
stop () ->
  gen_fsm:send_all_state_event (?MODULE, stop).


%%====================================================================
%% Func: add_world/1
%%====================================================================
%% @spec add_world(WorldPid) -> ok
%%
%% @doc Adds a new world (represented by its pid) to the engine.
%%
add_world (WorldPid) ->
  gen_fsm:send_all_state_event (?MODULE, {add_world, WorldPid}).


%%====================================================================
%% Func: set_collision/1
%%====================================================================
%% @spec set_collision(Value) -> ok
%%
%% @doc Enable/Disable Collision Handler.
%%
set_collision (Value) ->
  gen_fsm:send_all_state_event (?MODULE, {set_collision, Value}).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: idle_draw/2
%%====================================================================
%% @private
%%
idle_draw ({zoom, ZoomValue}, StateData) ->
  NewStateData =
    StateData#engine_state {zoom = ZoomValue },
  {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
idle_draw ({up_down, NewX}, StateData) ->
  Y = (StateData#engine_state.rotation)#vector.y,
  Z = (StateData#engine_state.rotation)#vector.z,
  NewStateData =
    StateData#engine_state {rotation = ?VECTOR (NewX, Y, Z) },
  {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
idle_draw ({left_right, NewY}, StateData) ->
  X = (StateData#engine_state.rotation)#vector.x,
  Z = (StateData#engine_state.rotation)#vector.z,
  NewStateData =
    StateData#engine_state {rotation = ?VECTOR (X, NewY, Z) },
  {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
 idle_draw ({add_object, Pid}, StateData) ->
   {next_state, idle_draw,
    StateData#engine_state {
      object_list = [Pid | StateData#engine_state.object_list]
     },
    StateData#engine_state.period};

%%
%% 1. transla l'oggetto in modo da far passare l'asse di rotazione dall'origine
%% 2. ruota l'oggetto in modo che l'asse di rotazione coincida con uno degli assi coordinati
%% 3. ruotare a piacere
%% 4. applica la rotazioneinversa alla 2
%% 5. transla in modo inverso alla 1.
idle_draw (timeout, StateData) ->
  %%gl:rotatef (0.5, 0.0, 1.0, 0.0),
  gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
  gl:loadIdentity (),

  gl:translatef (0.0, 0.0, StateData#engine_state.zoom),

  R = StateData#engine_state.rotation,
  IncR = StateData#engine_state.rotation_increment,
  gl:rotatef (R#vector.x, 1.0, 0.0, 0.0),
  gl:rotatef (R#vector.y, 0.0, 1.0, 0.0),
  gl:rotatef (R#vector.z, 0.0, 0.0, 1.0),
  NewR = R#vector { x = R#vector.x + IncR#vector.x ,
                    y = R#vector.y + IncR#vector.y ,
                    z = R#vector.z + IncR#vector.z},

  NewStateData =
    StateData#engine_state
      { rotation = NewR,
        zoom = StateData#engine_state.zoom +
               StateData#engine_state.zoom_increment},

  %% if collision is enabled, handle collisions
  ok = if
%         NewStateData#engine_state.collision_options =/= undefined ->
         NewStateData#engine_state.collision_enable == true ->
           collision:handle (
             NewStateData#engine_state.collision_options,
             NewStateData#engine_state.object_list);
         true -> ok
      end,

%%   io:format ("--------\n"),

	
%% Update Time Parameters
NewStateTime = time_handler(NewStateData),
Time = NewStateTime#engine_state.time,
Delta =  NewStateTime#engine_state.delta,

	     
  lists:foreach (fun (Pid) ->
                     O = object3d:obj (Pid),
                     gl:pushMatrix (),
                     draw_object (Pid, O,O#object3d.visible),
                     gl:popMatrix (),

                     %% before executing activities, set previous
                     %% position, axis and up to current values
                     %% because doing so can be useful for collision
                     %% response
                     object3d:prev_position (Pid, O#object3d.position),
                     object3d:prev_axis (Pid, O#object3d.axis),
                     object3d:prev_up (Pid, O#object3d.up),

                     Acts = object3d:activities (Pid),
                     lists:foreach (fun (Act) ->
                                      %  gen_activity:step (Act, O)
				        gen_activity:step (Act, O,{Time,Delta})
                                    end,
                                    Acts)
                 end,
                 NewStateTime#engine_state.object_list),
  %%gl:flush(),
%%   io:format ("Mouse ~p~n", [sdl_keyboard:getKeyState()]),
  gl:swapBuffers (),
  case Evt = sdl_events:pollEvent() of
    #quit{} ->
      {stop, normal, NewStateTime};
    #resize{} ->
      %%io:format("Maximized: ~p,~p~n", [sdl_video:wm_isMaximized(), Evt]),
      W = Evt#resize.w,
      H = Evt#resize.h,
      set_viewport(W, H,
                   NewStateTime#engine_state.screen_w,
                   NewStateTime#engine_state.screen_h),
      gl:matrixMode(?GL_PROJECTION),
      gl:loadIdentity(),
      %%gl:ortho( -2.0, 2.0, -2.0, 2.0, -20.0, 20.0),
      glu:perspective( 50.0, (W * 1.0) / (H * 1.0), 1.0, 1000.0),
      gl:matrixMode(?GL_MODELVIEW),
      gl:loadIdentity(),
      {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period};
    no_event ->
      {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period};
    #keyboard{sym=$f} ->
      Surface = sdl_video:getVideoSurface(),
      %%io:format("~p\n", [sdl_video:wm_toggleFullScreen(Surface)]),
      {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period};
    #keyboard{sym=?SDLK_q} ->
      {stop, normal, NewStateTime};
    #keyboard{sym=?SDLK_ESCAPE} ->
      {stop, normal, NewStateTime};
    #keyboard{sym=?SDLK_l} ->
      gl:enable (?GL_LIGHTING),
      {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period};
    #keyboard{sym=?SDLK_x} ->
      gl:disable (?GL_LIGHTING),
      {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period};
    #keyboard{} = K ->
      processKey (K, NewStateTime);
    Event ->
      %%io:format("Got event ~p~n", [Event]),
      {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period}
  end.


%%
draw_object(Pid, Obj = #object3d {},false)->
    ok;
%% an object of a compound object
draw_object(Pid, Obj = #object3d {},_)
     when Obj#object3d.parent_object =/= noname ->
%%   io:format ("NoDraw object ~p, ~p, ~p~n",
%%              [Pid, Obj#object3d.type,
%%               Obj#object3d.name]),
  ok;
%%

%% not a compound object
draw_object(Pid, Obj = #object3d { objects = [] },_) ->
%%   io:format ("Drawing non-compound object ~p, ~p, ~p~n",
%%              [Pid, Obj#object3d.type,
%%               Obj#object3d.name]),
  Pos = Obj#object3d.position,
  Axis = Obj#object3d.axis,
  DefaultAxis = Obj#object3d.default_axis,
  Angle = geometry:angle (DefaultAxis, Axis),
  RotAxis = geometry:cross (DefaultAxis, Axis),
  gl:translatef (Pos#vector.x,
                 Pos#vector.y,
                 Pos#vector.z),
  gl:rotatef (Angle,
              RotAxis#vector.x,
              RotAxis#vector.y,
              RotAxis#vector.z),
  if
    Obj#object3d.up =/= undefined ->
      Up = Obj#object3d.up,
      DefaultUp = Obj#object3d.default_up,
      Angle2 = geometry:angle (DefaultUp, Up),
      RotAxis2 = geometry:cross (DefaultUp, Up),
      gl:rotatef (Angle2,
                  RotAxis2#vector.x,
                  RotAxis2#vector.y,
                  RotAxis2#vector.z);
    true ->
      ok
  end,
  object3d:draw (Pid),
  ok;
%%
draw_object(Pid, Obj = #object3d {},_ ) -> %% compound object
%%   io:format ("Drawing compound object ~p, ~p, ~p~n",
%%              [Pid, Obj#object3d.type,
%%               Obj#object3d.name]),
  Pos = Obj#object3d.position,
  Axis = Obj#object3d.axis,
  DefaultAxis = Obj#object3d.default_axis,
  Angle = geometry:angle (DefaultAxis, Axis),
  RotAxis = geometry:cross (DefaultAxis, Axis),
  if
    Obj#object3d.up =/= undefined ->
      Up = Obj#object3d.up,
      DefaultUp = Obj#object3d.default_up,
      Angle2 = geometry:angle (DefaultUp, Up),
      RotAxis2 = geometry:cross (DefaultUp, Up);
    true ->
      Angle2 = undefined,
      RotAxis2 = undefined
  end,
  lists:foreach (fun (P) ->
                     O = object3d:obj (P),
                     ChildDefaultAxis = O#object3d.default_axis,
                     ChildAxis = O#object3d.axis,
                     ChildAngle = geometry:angle (ChildDefaultAxis,
                                                  ChildAxis),
                     ChildRotAxis = geometry:cross (ChildDefaultAxis,
                                                    ChildAxis),

                     ChildPos = O#object3d.position,

                     if
                       O#object3d.type == compound ->

%%                          io:format ("[~p,~p] ~p~n", [Obj#object3d.name,
%%                                                      O#object3d.name,
%%                                                      Obj#object3d.position]),

                         gl:pushMatrix (),
                         gl:translatef (Pos#vector.x,
                                        Pos#vector.y,
                                        Pos#vector.z),
                         gl:rotatef (Angle,
                                     RotAxis#vector.x,
                                     RotAxis#vector.y,
                                     RotAxis#vector.z),

                         if
                           Angle2 =/= undefined ->
                             gl:rotatef (Angle2,
                                         RotAxis2#vector.x,
                                         RotAxis2#vector.y,
                                         RotAxis2#vector.z);
                           true ->
                             ok
                         end,

%%                          gl:translatef (ChildPos#vector.x,
%%                                         ChildPos#vector.y,
%%                                         ChildPos#vector.z),

%%                          gl:rotatef (ChildAngle,
%%                                      ChildRotAxis#vector.x,
%%                                      ChildRotAxis#vector.y,
%%                                      ChildRotAxis#vector.z),

%%                          if
%%                            O#object3d.up =/= undefined ->
%%                              Up3 = O#object3d.up,
%%                              DefaultUp3 = O#object3d.default_up,
%%                              Angle3 = geometry:angle (DefaultUp3, Up3),
%%                              RotAxis3 = geometry:cross (DefaultUp3, Up3),
%%                              gl:rotatef (Angle3,
%%                                          RotAxis3#vector.x,
%%                                          RotAxis3#vector.y,
%%                                          RotAxis3#vector.z);
%%                            true ->
%%                              ok
%%                          end,

                         draw_object (P,
                                      O#object3d { parent_object = noname },O#object3d.visible),
                         gl:popMatrix (),
                         ok;
                       true ->

                         gl:pushMatrix (),

%%                          io:format ("[~p, ~p] ~p, ~p~n",
%%                                     [Obj#object3d.name,
%%                                      O#object3d.name,
%%                                      Obj#object3d.position,
%%                                      O#object3d.position]),
                         gl:translatef (Pos#vector.x,
                                        Pos#vector.y,
                                        Pos#vector.z),

                         gl:rotatef (Angle,
                                     RotAxis#vector.x,
                                     RotAxis#vector.y,
                                     RotAxis#vector.z),

                         if
                           Angle2 =/= undefined ->
                             gl:rotatef (Angle2,
                                         RotAxis2#vector.x,
                                         RotAxis2#vector.y,
                                         RotAxis2#vector.z);
                           true ->
                             ok
                         end,

                         gl:translatef (ChildPos#vector.x,
                                        ChildPos#vector.y,
                                        ChildPos#vector.z),

                         gl:rotatef (ChildAngle,
                                     ChildRotAxis#vector.x,
                                     ChildRotAxis#vector.y,
                                     ChildRotAxis#vector.z),

                         if
                           O#object3d.up =/= undefined ->
                             Up3 = O#object3d.up,
                             DefaultUp3 = O#object3d.default_up,
                             Angle3 = geometry:angle (DefaultUp3, Up3),
                             RotAxis3 = geometry:cross (DefaultUp3, Up3),
                             gl:rotatef (Angle3,
                                         RotAxis3#vector.x,
                                         RotAxis3#vector.y,
                                         RotAxis3#vector.z);
                           true ->
                             ok
                         end,

                         object3d:draw (P),
                         gl:popMatrix ()
                     end
                 end,
                 Obj#object3d.pids),
  ok.
%%


gather_compound_objects (O) ->
  gather_compound_objects (O, []).
%%
gather_compound_objects ([], Acc) ->
  lists:flatten (Acc);
gather_compound_objects ([P | T], Acc) ->
  O = object3d:obj (P),
  if
    O#object3d.type == compound ->
      gather_compound_objects (T,
                               [gather_compound_objects (O#object3d.pids)
                                | Acc]);
    true ->
      gather_compound_objects (T,
                               [{P, O} | Acc])
  end.




%%
processKey (#keyboard{sym=?SDLK_q}, StateData) ->
      {stop, normal, StateData};
%%
processKey (#keyboard{sym=?SDLK_ESCAPE}, StateData) ->
      {stop, normal, StateData};
%%
processKey (#keyboard{sym=$f}, StateData) ->
  Surface = sdl_video:getVideoSurface(),
  %%io:format("~p\n", [sdl_video:wm_toggleFullScreen(Surface)]),
  {next_state, idle_draw, StateData, StateData#engine_state.period};
%%
processKey (#keyboard{sym=$c}, StateData) ->
  io:format ("Camera positon: X = ~p, Y = ~p, Zoom = ~p~n",
             [(StateData#engine_state.rotation)#vector.x,
              (StateData#engine_state.rotation)#vector.y,
              StateData#engine_state.zoom]),
  {next_state, idle_draw, StateData, StateData#engine_state.period};
%%
processKey (#keyboard{sym=?SDLK_LEFT}, StateData) ->
  IncR = StateData#engine_state.rotation_increment,
  NewIncR = IncR#vector { y = IncR#vector.y + ?DEFAULT_ROTATION_INCREMENT },
  NewStateData =
    StateData#engine_state {rotation_increment = NewIncR },
  {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
processKey (#keyboard{sym=?SDLK_RIGHT}, StateData) ->
  IncR = StateData#engine_state.rotation_increment,
  NewIncR = IncR#vector { y = IncR#vector.y - ?DEFAULT_ROTATION_INCREMENT },
  NewStateData =
    StateData#engine_state {rotation_increment = NewIncR },
  {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
processKey (#keyboard{sym=?SDLK_UP}, StateData) ->
  IncR = StateData#engine_state.rotation_increment,
  NewIncR = IncR#vector { x = IncR#vector.x + ?DEFAULT_ROTATION_INCREMENT },
  NewStateData =
    StateData#engine_state {rotation_increment = NewIncR },
  {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
processKey (#keyboard{sym=?SDLK_DOWN}, StateData) ->
  IncR = StateData#engine_state.rotation_increment,
  NewIncR = IncR#vector { x = IncR#vector.x - ?DEFAULT_ROTATION_INCREMENT },
  NewStateData =
    StateData#engine_state {rotation_increment = NewIncR },
  {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
processKey (#keyboard{sym=?SDLK_1}, StateData) ->
  ZI = StateData#engine_state.zoom_increment,
  NewStateData =
    StateData#engine_state {zoom_increment = ZI - ?DEFAULT_ZOOM_INCREMENT },
  {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
processKey (#keyboard{sym=?SDLK_2}, StateData) ->
  ZI = StateData#engine_state.zoom_increment,
  NewStateData =
    StateData#engine_state {zoom_increment = ZI + ?DEFAULT_ZOOM_INCREMENT },
  {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
processKey (#keyboard{sym=?SDLK_SPACE}, StateData) ->
  NewStateData =
    StateData#engine_state {zoom_increment = 0,
                            rotation_increment = ?VECTOR(0.0, 0.0, 0.0)},
  {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
processKey (K, StateData) ->
  %%io:format("Got event ~p~n", [K]),
  {next_state, idle_draw, StateData, StateData#engine_state.period}.


%%====================================================================
%% Func: terminate/3
%%====================================================================
%% @private
%%
terminate (_, _, StateData) ->
  lists:foreach (fun (Pid) ->
                     object3d:stop (Pid)
                 end,
                 StateData#engine_state.object_list),
  lists:foreach (fun (Pid) ->
                     rosen_world:stop (Pid)
                 end,
                 StateData#engine_state.world_list),
  ok.


%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
%%
init (Config) ->
  %% Init
  sdl:init(?SDL_INIT_VIDEO),
  sdl_util:debug(1),
  Flags =
    case lists:member(fullscreen, Config) of
      true ->
        ?SDL_OPENGL  bor ?SDL_FULLSCREEN;
      _ ->
        ?SDL_OPENGL  bor ?SDL_RESIZABLE
    end,
  sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),

  AvailableWindowedSzs = sdl_video:listModes(null, Flags bor ?SDL_FULLSCREEN),
  DriverName = sdl_video:videoDriverName(),

%%   io:format("Driver ~p ~n", [DriverName]),
%%   io:format("Available WindowSizes ~p ~n", [AvailableWindowedSzs]),

  case AvailableWindowedSzs of
    [{_, 0,0,W,H}|_] ->
      Res = [Test || Test <- [32,24,16,15],
                     true == sdl_video:videoModeOK(W,H,Test,Flags)];
%%       io:format("A guess at max video res is ~px~p:~p ~n", [W,H, hd(Res)]);
    _ ->
%%       io:format("Can't guess max resolution~n", []),
      W = 800, H = 600
  end,

  SR = sdl_video:setVideoMode(W, H, 16, Flags),
  Rs= sdl_video:gl_getAttribute(?SDL_GL_RED_SIZE),
  Gs= sdl_video:gl_getAttribute(?SDL_GL_GREEN_SIZE),
  Bs= sdl_video:gl_getAttribute(?SDL_GL_BLUE_SIZE),
  Ds= sdl_video:gl_getAttribute(?SDL_GL_DEPTH_SIZE),
  Db= (1 == sdl_video:gl_getAttribute(?SDL_GL_DOUBLEBUFFER)),
%%   io:format("OpenGL attributes ~n"),
%%   io:format("Sizes in bits Red ~p Green ~p Blue ~p Depth ~p Doublebuffered ~p~n",
%%             [Rs, Gs, Bs, Ds, Db]),
%%   io:format("Vendor:     ~s~n",  [gl:getString(?GL_VENDOR)]),
%%   io:format("Renderer:   ~s~n",  [gl:getString(?GL_RENDERER)]),
%%   io:format("Version:    ~s~n",  [gl:getString(?GL_VERSION)]),
%%   io:format("GL AUX BUFFERS ~p~n",  [gl:getIntegerv(?GL_AUX_BUFFERS)]),
%%   io:format("SDL Version ~p~n",  [sdl_video:wm_getInfo()]),

%%   io:format("Extensions: ~s~n",  [gl:getString(?GL_EXTENSIONS)]),
%%   io:format("Maximized: ~p~n",   [sdl_video:wm_isMaximized()]),

%%   io:format("~p", [catch gl:getConvolutionParameterfv(16#8011, 16#801A)]),

  sdl_events:eventState(?SDL_ALLEVENTS ,?SDL_IGNORE),
  sdl_events:eventState(?SDL_KEYDOWN ,?SDL_ENABLE),
  sdl_events:eventState(?SDL_QUIT ,?SDL_ENABLE),
  sdl_events:eventState(?SDL_VIDEORESIZE, ?SDL_ENABLE),
%%   ?printError(),

CollisionOptions = proplists:get_value (collision, Config),
TimeType = proplists:get_value (timeType, Config),
  EngineState = #engine_state { zoom = -10.0,
                                screen_h = H,
                                screen_w = W,
%				collision_options = proplists:get_value (collision, Config),
                                collision_options = CollisionOptions,
				collision_enable = valueColl(CollisionOptions),
				time_type = TimeType,
				time = 0.0,
				delta = ?SIMULATED_DELTATIME,
				last_time = setInitTime(TimeType)
			       },

  initWin(EngineState),
  sdl_util:debug(00),

  {ok, idle_draw, EngineState, ?DEFAULT_PERIOD}.


%
valueColl(undefined) -> false;
valueColl(Coll) -> true.
%



initWin (EngineState) ->
  W = EngineState#engine_state.screen_w,
  H = EngineState#engine_state.screen_h,
%%  set_viewport (W, H,
%%                EngineState#engine_state.screen_w,
%%                EngineState#engine_state.screen_h),
  gl:matrixMode(?GL_PROJECTION),
  gl:loadIdentity(),
  %%gl:ortho( -2.0, 2.0, -2.0, 2.0, -20.0, 20.0),
  glu:perspective( 50.0, (W * 1.0) / (H * 1.0), 1.0, 1000.0),
  gl:matrixMode(?GL_MODELVIEW),
  gl:loadIdentity(),

  gl:clearColor(0.0,0.0,0.0,1.0),
  gl:shadeModel (?GL_SMOOTH),
  gl:enable(?GL_DEPTH_TEST),
  gl:depthFunc(?GL_LEQUAL),
  gl:hint(?GL_PERSPECTIVE_CORRECTION_HINT, ?GL_NICEST),

  %// camera, look-at-point, up-vector
  %%glu:lookAt(1.0, 1.5, 15.0, 0.0, 0.0, -50.0, 0.0, 1.0, 0.0),
%%   glu:lookAt(0.0, 0.0, 10.0,
%%              0.0, 0.0, 0.0,
%%              0.0, 1.0, 0.0),

  gl:enable(?GL_DEPTH_TEST),
  gl:depthFunc(?GL_LESS),

  gl:lightfv(?GL_LIGHT0, ?GL_AMBIENT,
             { 0.01, 0.01, 0.01, 1.0}), %% add lighting. (ambient)
  gl:lightfv (?GL_LIGHT0, ?GL_DIFFUSE,
              { 0.7, 0.7, 0.7, 1.0 }), %% add lighting. (diffuse).
  gl:lightfv (?GL_LIGHT0, ?GL_POSITION,
              { -10.0, 10.0, 10.0, 0.0 }), %% set light position

  gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, { 0.3, 0.3, 0.3, 1.0 }),

  gl:lightModelfv(?GL_LIGHT_MODEL_LOCAL_VIEWER, { 0.0 }),

  gl:enable(?GL_LIGHT0),  %% turn light 0 on.
  gl:enable (?GL_LIGHTING),
  gl:enable(?GL_COLOR_MATERIAL),
  gl:enable(?GL_NORMALIZE),

  gl:enable (?GL_BLEND),
  gl:blendFunc (?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),

  %%gl:colorMaterial(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE),
  %%gl:materialfv(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE, {0.5, 0.5, 0.5, 1.0}),

%%   gl:enable(?GL_CULL_FACE),
%%   gl:cullFace(?GL_BACK),

  ok.



set_viewport (W, H, ScreenW, ScreenH) ->
  DW = (ScreenW - W) / 2,
  DH = (ScreenH - H) / 2,
  io:format ("set_viewport ~p~n", [{DW, DH, W, H}]),
  gl:viewport (trunc(DW), trunc(DH), W, H).



%%    glViewport(0, 0, w, h);
%%     glMatrixMode(GL_PROJECTION);
%%     glLoadIdentity();
%%     glFrustum(-1, 1, -1, 1, 1, 20); // left, right, bottom, top, near, far
%%     glMatrixMode(GL_MODELVIEW);
%%     glLoadIdentity();
%%     gluLookAt(cx, cy, cz, l1, l2, l3, u1, u2, u3); // camera, look-at-point, up-vector


%%====================================================================
%% Func: handle_event/3
%%====================================================================
%% @private
%%
handle_event ({add_world, WorldPid}, StateName, StateData) ->
  NewStateData =
    StateData#engine_state {world_list = [WorldPid | StateData#engine_state.world_list]},
  {next_state, StateName, NewStateData};

handle_event ({set_collision, Value}, StateName, StateData) ->
  NewStateData =
    StateData#engine_state {collision_enable = Value},
%io:format("Set delle Collisioni a ~p~n",[Value]),
  {next_state, StateName, NewStateData};
handle_event (stop, _StateName, StateData) ->
  {stop, normal, StateData}.


%%
%% time_handler 
%% To handle the real or simulated time 
     
time_handler(State = #engine_state {time_type = simulated}) ->   Now = State#engine_state.time,
								 Delta = ?SIMULATED_DELTATIME,
								 NewTime = State#engine_state.time + Delta,
%    io:format("Tempo Simulato Now  ~p Last Time ~p  Delta ~p NewTime ~p ~n",
						%[Now,State#activity_state.last_time, Delta, NewTime]),
								 NewState = State#engine_state{ last_time = Now,
												delta = Delta,
												time = NewTime
											      };
time_handler(State) ->  Now = now (),
			Delta = timer:now_diff (Now, State#engine_state.last_time) / 1000000.0,
			%% difference in seconds
			NewTime = State#engine_state.time + Delta,
%     io:format("Tempo Reale Now  ~p Last Time ~p  Delta ~p NewTime ~p Modulo ~p  ~n",
	%					[Now,State#activity_state.last_time, Delta, NewTime,State#activity_state.module ]),
			NewState = State#engine_state{time_type = real,
						      last_time = Now,
						      delta = Delta,
						      time = NewTime
						     }.

%
setInitTime(simulated)->    {0.0,0.0,0.0};
setInitTime(_)->    now().

%% setDeltaTime(simulated,undefined)->    Delta;
%% setDeltaTime(_,Delta)->    now().
