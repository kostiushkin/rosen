%
% narrow_phase_trimeshes_intersect.erl
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
% $Id: narrow_phase_trimeshes_intersect.erl,v 1.2 2008/07/16 20:17:28 slackydeb Exp $
%
%% @doc A collision layer implementing a narrow phase collision
%%      detection between trimeshes (i.e. meshes of only triangles).
%%
%% <p>This module expects a list of couples (use
%% <code>to_object_couples</code>) and returns a dictionary containing
%% information about collisions (i.e. couples of intersecting
%% triangles).</p>

-module (narrow_phase_trimeshes_intersect).
-behaviour (collision).

-include ("triangle.hrl").

-export([layer/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Function: layer/2
%%====================================================================
%% @spec layer({ObjectDict, ObjectPidCoupleList},
%%             CompleteObjectPidList) -> CollisionDict
%%       ObjectDict = dictionary()
%%       ObjectPidCoupleList = {pid(), pid()}
%%       CompleteObjectPidList = [pid()]
%%       CollisionDict = dictionary()
%%
%% @doc For each object in the dictionary specified, considers the
%%      feature named <code>mesh</code> (that must be an absolute
%%      trimesh list), detects collisions and returns a dictionary,
%%      with
%%      <ul>
%%      <li>key: the object pid that collides</li>
%%      <li>value: another dictionary, with</li>
%%          <ul>
%%          <li>key: the face that collides (as a
%%              <code>#triangle{}</code> record with absolute
%%              vertices)</li>
%%          <li>value: another dictionary, with</li>
%%              <ul>
%%              <li>key: the pid of the other object that collides
%%                  that face</li>
%%              <li>value: the list of faces (as
%%                  <code>#triangle{}</code> records with absolute
%%                  vertices) of the other object that collides the
%%                  face of the first object.</li>
%%              </ul>
%%          </ul>
%%      </ul>
%%      The second parameter is not used.
%%
layer ({ObjectDict, ObjectPidCoupleList}, _CompleteObjectPidList) ->
    narrow_phase (ObjectDict, ObjectPidCoupleList).

%% narrow_phase/2
narrow_phase (ObjectDict, ObjectPidCoupleList) ->
    lists:foldl (
      fun ({ObjectPid1, ObjectPid2}, AccDict) ->
              %% assuming that the mesh fetched is already a trimesh
              AbsoluteTrimeshList1 =
                  dict:fetch (mesh, dict:fetch (ObjectPid1, ObjectDict)),
              AbsoluteTrimeshList2 =
                  dict:fetch (mesh, dict:fetch (ObjectPid2, ObjectDict)),
              NewCollisions =
                  trimesh:trimeshes_intersect (AbsoluteTrimeshList1,
                                               AbsoluteTrimeshList2),
              update_collisions_dict (
                NewCollisions, ObjectPid1, ObjectPid2, AccDict)
      end,
      dict:new (),
      ObjectPidCoupleList).



%%====================================================================
%% Internal functions
%%====================================================================
%%====================================================================
%% Function: update_collisions_dict/4
%% @private
%%====================================================================
%% @spec update_collisions_dict(TupleList,
%%                              Object1,
%%                              Object2,
%%                              CollisionDict) -> NewCollisionDict
%%       TupleList = [{triangle(), triangle()}]
%%       Object1 = Object2 = pid()
%%       CollisionDict = NewCollisionDict = dictionary()
%%
%% @doc Updates the collision dictionary inserting all collisions
%%      specified in the list of couples of triangles; every couple of
%%      triangles must mean that the first triangle, belonging to the
%%      first object, intersects with the second triangle, belonging
%%      to the second object.
%%

%% no collision (and no key) to add
update_collisions_dict (_TupleList = [], _Object1, _Object2, CollisionDict) ->
    CollisionDict;

%% main clause; add the first collision; each collision deals with two
%% objects
update_collisions_dict ([{T1 = #triangle{}, T2 = #triangle{}} | Tail],
                        Object1, Object2, CollisionDict) ->
    DictWithFirstPid =
        update_collisions_dict (add_only_the_first_key,
                                {T1, T2}, Object1, Object2, CollisionDict),
    DictWithBothPids =
        update_collisions_dict (add_only_the_first_key,
                                {T2, T1}, Object2, Object1, DictWithFirstPid),
    update_collisions_dict (Tail, Object1, Object2, DictWithBothPids).

%% update_collisions_dict/5
update_collisions_dict (add_only_the_first_key,
                        {T1 = #triangle{}, T2 = #triangle{}},
                        Object1, Object2, CollisionDict) ->
    dict:update (
      Object1,
      fun (Object1Dict) ->
              update_object_dict ({T1, T2}, Object2, Object1Dict)
      end,
      update_object_dict ({T1, T2}, Object2, dict:new ()),
      CollisionDict).


%%====================================================================
%% Function: update_object_dict/3
%% @private
%%====================================================================
%% @spec update_object_dict({MyTriangle, OtherTriangle},
%%                          OtherObject, 
%%                          ObjectDict) -> NewObjectDict
%%       MyTriangle = OtherTriangle = triangle()
%%       OtherObject = pid()
%%       ObjectDict = NewObjectDict = dictionary()
%%
%% @doc Updates the object dictionary inserting the collision
%%      specified.
%%
update_object_dict ({MyTriangle = #triangle{}, OtherTriangle = #triangle{}},
                    OtherObject, ObjectDict) ->
    dict:update (
      MyTriangle,
      fun (FaceDict) ->
              dict:update (
                OtherObject,
                fun (OtherObjectFacesList) ->
                        [OtherTriangle | OtherObjectFacesList]
                end,
                [OtherTriangle],
                FaceDict)
      end,
      dict:append (OtherObject, [OtherTriangle], dict:new ()),
      ObjectDict).
