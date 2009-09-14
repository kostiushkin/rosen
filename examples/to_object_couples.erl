%
% to_object_couples.erl
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
% $Id: to_object_couples.erl,v 1.3 2008/08/04 21:49:03 slackydeb Exp $
%
%% @doc A collision layer creating object unordered couples.
%%
%% <p>This module expects a dictionary (use
%% <code>to_object_dict</code>) and creates object unordered
%% couples.</p>

-module (to_object_couples).
-behaviour (collision).

-export([layer/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Function: layer/2
%%====================================================================
%% @spec layer(ObjectDict,
%%             CompleteObjectPidList) -> {ObjectDict, ObjectPidCoupleList}
%%       ObjectDict = dictionary()
%%       CompleteObjectPidList = [pid()]
%%       ObjectPidCoupleList = {pid(), pid()}
%%
%% @doc Creates object unordered couples.
%%      The second parameter is not used.
%%
layer (ObjectDict, _CompleteObjectPidList) ->
    ObjectPidList = dict:fetch_keys (ObjectDict),
    AllUnorderedCoupleList =
        broad_phase_collision_detection:unordered_couples (ObjectPidList),
    %% N = length (ObjectPidList),
    %% NCouples = length (AllUnorderedCoupleList),
    %% next line is an assertion
    %% true = (NCouples == (N * (N - 1) / 2)),
    {ObjectDict, AllUnorderedCoupleList}.
