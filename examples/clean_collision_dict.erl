%
% clean_collision_dict.erl
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
% $Id: clean_collision_dict.erl,v 1.4 2008/07/30 17:14:55 slackydeb Exp $
%
%% @doc A collision layer related to
%%      <code>narrow_phase_trimeshes_intersect</code>.
%%
%% <p>This module cleans the collision dictionary specified, as
%% returned by <code>narrow_phase_trimeshes_intersect</code>
%% layer.</p>
%%
%% <p>This module is useful to better understand
%% <code>narrow_phase_trimeshes_intersect</code> layer.</p>

-module (clean_collision_dict).
-behaviour (collision).

-export([layer/2]).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Function: layer/2
%%====================================================================
%% @spec layer(CollisionDict, CompleteObjectPidList) -> NewCollisionDict
%%       CollisionDict = NewCollisionDict = dictionary()
%%       CompleteObjectPidList = [pid()]
%%
%% @doc Cleans the collision dictionary specified, removing:
%%      <ul>
%%      <li>pids without faces colliding</li>
%%      <li>fixed a pid, faces without other pids colliding</li>
%%      <li>fixed a pid and a face, other pids without faces
%%          colliding</li>
%%      </ul>
%%      The second parameter is not used.
%%
layer (CollisionDict, _CompleteObjectPidList) ->
    clean_collision_dict (CollisionDict).

%% clean_collision_dict/1
clean_collision_dict (CollisionDict) ->
    %% remove objects with no colliding face
    dict:filter (fun (_ObjectPid, ObjectDict) ->
                         dict:size (ObjectDict) > 0
                 end,
                 clean_all_object_dicts (CollisionDict)).

%% clean_all_object_dicts/1
clean_all_object_dicts (CollisionDict) ->
    dict:map (fun (_ObjectPid, ObjectDict) ->
                      clean_object_dict (ObjectDict)
              end,
              CollisionDict).

%% clean_object_dict/1
clean_object_dict (ObjectDict) ->
    %% remove faces not colliding with al least one other object
    dict:filter (fun (_Face, FaceDict) ->
                         dict:size (FaceDict) > 0
                 end,
                 clean_all_face_dicts (ObjectDict)).

%% clean_all_face_dicts/1
clean_all_face_dicts (ObjectDict) ->
    dict:map (fun (_Face, FaceDict) ->
                      clean_face_dict (FaceDict)
              end,
              ObjectDict).

%% clean_face_dict/1
clean_face_dict (FaceDict) ->
    %% remove other objects with no face colliding
    dict:filter (fun (_OtherObjectPid, OtherObjectFacesList) ->
                         length (OtherObjectFacesList) > 0
                 end,
                 FaceDict).
