%
% broad_phase_collision_detection.erl
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
% $Id: broad_phase_collision_detection.erl,v 1.3 2008/08/06 10:55:26 slackydeb Exp $
%
%% @doc Module providing basic functions for the broad phase of
%%      collision detection.

-module (broad_phase_collision_detection).

-export([pairwise_prune/3,
         unordered_couples/1]).


%%====================================================================
%% Function: pairwise_prune/3
%%====================================================================
%% @spec pairwise_prune(Pred, Couples, Fun) -> PossiblyCollidingCouples
%%       Pred = fun(BoundingVolume1, BoundingVolume2) -> bool()
%%       Couples = PossiblyCollidingCouples = [{Elem1, Elem2}]
%%       Fun = fun(Elem) -> BoundingVolume
%%       BoundingVolume = BoundingVolume1 = BoundingVolume2 = any()
%%       Elem = Elem1 = Elem2 = any()
%%
%% @doc <code>PossiblyCollidingCouples</code> is a list of all couples
%%      in <code>Couples</code> for which
%%      <code>Pred(_BV1=Fun(Elem1),_BV2=Fun(Elem2))</code> returns
%%      <code>true</code>.
%%
pairwise_prune (Pred, Couples, Fun) ->
    lists:filter (fun ({Elem1, Elem2}) ->
                          Pred (_BV1 = Fun (Elem1),
                                _BV2 = Fun (Elem2))
                  end,
                  Couples).


%%====================================================================
%% Function: unordered_couples/1
%%====================================================================
%% @spec unordered_couples([Elem]) -> [{Elem1, Elem2}]
%%       Elem = Elem1 = Elem2 = any()
%%
%% @doc Returns all unordered couples of elements in the list
%%      specified. If the length of the list specified is <b>n</b>,
%%      then the length of the returned list is <b>(n *(n - 1)) /
%%      2</b>.
%%
unordered_couples (List) ->
    unordered_couples (List, []).

%% unordered_couples/2
%%
%% one or zero elements make no couples
unordered_couples (List, Acc) when length (List) =< 1 ->
    Acc;

%% main clause; all unordered couples between the first element and
%% all others
unordered_couples ([Fixed | OtherList], Acc) ->
    NewCouples = [{Fixed, X} || X <- OtherList],
    unordered_couples (OtherList,
                       lists:append (NewCouples, Acc)).
