%
% collision.erl
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
% $Id: collision.erl,v 1.2 2008/07/16 20:24:10 slackydeb Exp $
%
%% @doc Main module for handling collisions and writing collision
%%      related stuff.
%%
%% <p>This module triggers collision layer modules specified by user
%% in <code>rosen:start_link/1</code>.</p>
%%
%% <p>This is also a <b>behaviour</b>, since it can be used as the
%% general part for implementing new collision layers.</p>

-module (collision).

-export([behaviour_info/1,
         handle/2]).


%%====================================================================
%% Function: behaviour_info/1
%% @private
%%====================================================================
behaviour_info (callbacks) ->
    [{layer, 2}].


%%====================================================================
%% Function: handle/2
%%====================================================================
%% @spec handle(CollisionLayerModuleList, CompleteObjectPidList) -> ok
%%       CollisionLayerModuleList = [atom()]
%%       CompleteObjectPidList = [pid()]
%%
%% @doc Triggers all layers, passing to every layer the output of the
%%      previous layer and the complete object list; the first layer
%%      receives two times the complete object list because there are
%%      no layers first.
%%
handle (CollisionLayerModuleList, CompleteObjectPidList) ->
    lists:foldl (fun (Module, AccIn) ->
                         Module:layer (AccIn, CompleteObjectPidList)
                 end,
                 CompleteObjectPidList,
                 CollisionLayerModuleList),
    ok.
