%
% triangle.erl
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
% $Id: triangle.erl,v 1.5 2008/08/06 09:23:52 slackydeb Exp $
%
%% @doc Module providing basic functions for triangles.
%%
%% @type triangle() = #triangle{}.
%% A record representing a triangle (or a plane) in 3D space; has the
%% fields <code>v0</code>, <code>v1</code> and <code>v2</code> (use
%% the <code>?VECTOR(X,Y,Z)</code> macro defined in
%% <code>geometry.hrl</code>).

-module (triangle).
-include ("triangle.hrl").
-include ("geometry.hrl").

%% tolerances
-define (TOL_LINE_PLANE_INTERSECT_PARALLEL, 1.0e-15).
-define (TOL_LINE_PLANE_INTERSECT_ADJACENT, 1.0e-15).
-define (TOL_IS_POINT_INSIDE_TRIANGLE, 1.0e-15).
%%
-define (TOL_TEST_NORMALIZE_VECTOR, 1.0e-15).
-define (TOL_TEST_DISTANCE_POINT_PLANE, 1.0e-15).

-export([triangles_intersect/2,
         centroid/1,
         aabb/1,
         test/0]).

                                                %TODO: understand and
                                                %comment
                                                %line_plane_intersect


%%====================================================================
%% Function: triangles_intersect/2
%%====================================================================
%% @spec triangles_intersect(T1, T2) -> bool()
%%       T1 = T2 = triangle()
%%
%% @doc Determines whether the two triangles intersect.
%%
triangles_intersect (T1 = #triangle{ v0 = T1V0, v1 = T1V1, v2 = T1V2 },
                     T2 = #triangle{ v0 = T2V0, v1 = T2V1, v2 = T2V2 }) ->
    %% determine if there is at least one segment of a triangle that
    %% intersects the other triangle
    lists:any(fun ({SegmentStart, SegmentEnd, Triangle}) ->
                      segment_triangle_intersect ({SegmentStart, SegmentEnd},
                                                  Triangle)
              end,
              [{T2V0, T2V1, T1},
               {T2V1, T2V2, T1},
               {T2V2, T2V0, T1},
               {T1V0, T1V1, T2},
               {T1V1, T1V2, T2},
               {T1V2, T1V0, T2}]).


%%====================================================================
%% Function: centroid/1
%%====================================================================
%% @spec centroid(triangle()) -> vector()
%%
%% @doc Computes the centroid (or barycenter) of the triangle
%%      specified.
%%
centroid (#triangle{ v0 = V0, v1 = V1, v2 = V2 }) ->
    geometry:centroid ([V0, V1, V2]).


%%====================================================================
%% Function: aabb/1
%%====================================================================
%% @spec aabb(triangle()) -> aabb()
%%
%% @doc Returns the AABB of the triangle specified.
%%
aabb (#triangle{ v0 = V0, v1 = V1, v2 = V2 }) ->
    aabb:from_vertex_list ([V0, V1, V2]).


test () ->
    %% geometry functions
    test_sub (),
    test_normalize_vector (),
    test_plane_normal_vector (),
    test_distance_point_plane (),
    test_cos (),

    %% internal functions
    test_is_point_inside_triangle (),
    test_segment_plane_intersect (),
    test_segment_triangle_intersect (),

    %% API
    test_triangles_intersect (),

    ok.



%%====================================================================
%% Geometry functions
%%====================================================================

%%====================================================================
%% Function: sub/2
%%====================================================================
%% @spec sub(V1::vector(), V2::vector()) -> vector()
%%
%% @doc Subs two vectors.
%% <br/>
%% <code>sub (V1, V2)</code> is V1-V2 (not V2-V1).
%%
sub (V1 = #vector{}, V2 = #vector{}) ->
    geometry:add (V1, geometry:dot_v (-1, V2)).


%%====================================================================
%% Function: normalize_vector/1
%%====================================================================
%% @spec normalize_vector(V::vector()) -> vector
%%
%% @doc Normalizes the non-null vector.
%%
normalize_vector (V = #vector{}) ->
    %% you cannot normalize a null vector; if V is a null vector, the
    %% next expression causes badarith (you want badarith in that
    %% case)
    geometry:dot_v (1 / geometry:norm (V), V).


%%====================================================================
%% Function: plane_normal_vector/1
%%====================================================================
%% @spec plane_normal_vector(triangle()) -> vector()
%%
%% @doc Plane normal vector.
%%
plane_normal_vector (#triangle{ v0 = V0, v1 = V1, v2 = V2 }) ->
    geometry:cross (sub (V1, V0), sub (V2, V0)).


%%====================================================================
%% Function: distance_point_plane/2
%%====================================================================
%% @spec distance_point_plane(Point::vector(), Plane::triangle()) -> number()
%%
%% @doc Computes the distance in 3D between the two arguments; it is
%%      always non-negative.
%%
distance_point_plane (Point = #vector{}, Plane = #triangle{}) ->
    abs (geometry:dot (normalize_vector (plane_normal_vector (Plane)),
                       sub (Point, Plane#triangle.v0))).



%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Function: segment_triangle_intersect/2
%%====================================================================
%% @spec segment_triangle_intersect({SegmentStart, SegmentEnd}, T) -> bool()
%%       SegmentStart = SegmentEnd = vector(),
%%       T = triangle()
%%
%% @doc Determines whether the segment intersects the triangle.
%%
segment_triangle_intersect ({SegmentStart = #vector{},
                             SegmentEnd = #vector{}},
                            T = #triangle{}) ->
    case segment_plane_intersect ({SegmentStart, SegmentEnd}, T) of
        {true, parallel_adjacent, _} ->
            %% the segment is adjacent the plane, but has it at least
            %% an endpoint in the triangle?
            is_point_inside_triangle (SegmentStart, T) orelse
                is_point_inside_triangle (SegmentEnd, T);
        {true, segment_intersecting, Point} ->
            %% the segment intersects the plane, but does it actually
            %% go through the triangle?
            is_point_inside_triangle (Point, T);
        {false, _Position, _} -> false
    end.


%%====================================================================
%% Function: segment_plane_intersect/2
%%====================================================================
%% @spec segment_plane_intersect({SegmentStart, SegmentEnd}, Plane) -> Result
%%       SegmentStart = SegmentEnd = vector(),
%%       Plane = triangle()
%%       Result = {false, parallel_disjoint, nil} |
%%                {true, parallel_adjacent, nil} |
%%                {false, line_intersecting, nil} |
%%                {true, segment_intersecting, IntersectionPoint}
%%       IntersectionPoint = vector()
%%
%% @doc Determines whether the segment intersects the plane.
%%
segment_plane_intersect ({SegmentStart = #vector{},
                          SegmentEnd = #vector{}},
                         Plane = #triangle{}) ->
    SegmentDirection = sub (SegmentEnd, SegmentStart),
    case line_plane_intersect ({SegmentStart, SegmentDirection}, Plane) of
        {false, parallel_disjoint, nil} = Result -> Result;
        {true, parallel_adjacent, nil} = Result -> Result;
        {true, intersecting, Percentage} ->
            intersection_point ({SegmentStart, SegmentDirection}, Percentage)
    end.

%% intersection_point/2
%%
%% line intersects plane, segment doesn't
intersection_point (_, Percentage)
  when (Percentage < 0.0) ; (Percentage > 1.0) ->
    {false, line_intersecting, nil};

%% segment intersects plane
intersection_point ({SegmentStart = #vector{}, SegmentDirection = #vector{}},
                    Percentage) ->
    IntersectionPoint =
        geometry:add (SegmentStart,
                      geometry:dot_v (Percentage, SegmentDirection)),
    {true, segment_intersecting, IntersectionPoint}.


%%====================================================================
%% Function: line_plane_intersect/2
%%====================================================================
%% @spec line_plane_intersect({LinePoint, LineDirection}, Plane) -> Result
%%       LinePoint = LineDirection = vector(),
%%       Plane = triangle()
%%       Result = {false, parallel_disjoint, nil} |
%%                {true, parallel_adjacent, nil} |
%%                {true, intersecting, Percentage}
%%       Percentage = number ()
%%
%% @doc Determines whether the line intersects the plane.
%%
line_plane_intersect ({LinePoint, LineDirection}, Plane) ->
    PlaneNormal = plane_normal_vector (Plane),
    Cos = geometry:cos (LineDirection, PlaneNormal),
    if
        abs (Cos) < ?TOL_LINE_PLANE_INTERSECT_PARALLEL -> %% parallel
            adjacent_or_disjoint (distance_point_plane (LinePoint, Plane));
        true -> %% not parallel
            PlanePoint = Plane#triangle.v0,
                                                %TODO: what are L1, N
                                                %and D?
            L1 = sub (PlanePoint, LinePoint),
            N = geometry:dot (L1, PlaneNormal),
            D = geometry:dot (LineDirection, PlaneNormal),

            Percentage = N / D,
            {true, intersecting, Percentage}
    end.

%% adjacent_or_disjoint/1
%%
%% parallel adjacent
adjacent_or_disjoint (Distance)
  when Distance < ?TOL_LINE_PLANE_INTERSECT_ADJACENT ->
    {true, parallel_adjacent, nil};

%% parallel disjoint
adjacent_or_disjoint (_Distance) -> {false, parallel_disjoint, nil}.


%%====================================================================
%% Function: is_point_inside_triangle/2
%%====================================================================
%% @spec is_point_inside_triangle(P::vector(), T::triangle()) -> bool()
%%
%% @doc Determines whether the point is inside the triangle in 3D.
%%
is_point_inside_triangle (P = #vector{}, T = #triangle{}) ->
    %% create three vectors that radiate out from the intersection
    %% point towards the triangle's three vertices
    V0 = sub (P, T#triangle.v0),
    V1 = sub (P, T#triangle.v1),
    V2 = sub (P, T#triangle.v2),

    %% if P is a vertex of the triangle, then consider it inside; also
    %% avoid to calculate the angle between a vector and a null vector
    case ?VECTOR(0.0,0.0,0.0) of
        V0 ->
            true;
        V1 ->
            true;
        V2 ->
            true;
        _ ->
            Angle1 = geometry:angle (V0, V1),
            Angle2 = geometry:angle (V1, V2),
            Angle3 = geometry:angle (V2, V0),

            TotalAngle = Angle1 + Angle2 + Angle3,

            abs (TotalAngle - 360.0) < ?TOL_IS_POINT_INSIDE_TRIANGLE
    end.



%%====================================================================
%% Test functions
%%====================================================================

test_sub () ->
    ?VECTOR(2,0,0) = sub (?VECTOR(1,1,0),?VECTOR(-1,1,0)),
    ok.


test_normalize_vector () ->
    ?VECTOR(1.0,0.0,0.0) = normalize_vector (?VECTOR(0.5,0,0)),

    abs (1 - geometry:norm (normalize_vector(
                              ?VECTOR(1,1,0)))) < ?TOL_TEST_NORMALIZE_VECTOR,

    abs (1 - geometry:norm (
               normalize_vector(
                 ?VECTOR(
                    ?TOL_TEST_NORMALIZE_VECTOR,
                    ?TOL_TEST_NORMALIZE_VECTOR,
                    ?TOL_TEST_NORMALIZE_VECTOR)))) < ?TOL_TEST_NORMALIZE_VECTOR,

    ok.


test_plane_normal_vector () ->    
    ?VECTOR(0,0,1) = plane_normal_vector (?TRIANGLE(?VECTOR(0,0,0),
                                                    ?VECTOR(1,0,0),
                                                    ?VECTOR(0,1,0))),
    ok.


test_distance_point_plane () ->
    0.0 = distance_point_plane (?VECTOR(0,0,0), ?TRIANGLE(
                                                   ?VECTOR(0,0,0),
                                                   ?VECTOR(1,0,0),
                                                   ?VECTOR(0,1,0))),

    1.0 = distance_point_plane (?VECTOR(0,0,1), ?TRIANGLE(
                                                   ?VECTOR(0,0,0),
                                                   ?VECTOR(1,0,0),
                                                   ?VECTOR(0,1,0))),

    %% the distance segment and the xy plane are parallel and adjacent
    abs (math:sqrt (0.5) - distance_point_plane (
                             ?VECTOR(0,0,0),
                             ?TRIANGLE(
                                ?VECTOR(1,0,1),
                                ?VECTOR(1,0,0),
                                ?VECTOR(0,1,0)))) <
        ?TOL_TEST_DISTANCE_POINT_PLANE,

    %% the distance segment isn't parallel to planes xy or yz or zx
    abs (math:sqrt (1/3) - distance_point_plane (
                             ?VECTOR(0,0,0),
                             ?TRIANGLE(
                                ?VECTOR(1,0,0),
                                ?VECTOR(0,1,0),
                                ?VECTOR(0,0,1)))) <
        ?TOL_TEST_DISTANCE_POINT_PLANE,

    ok.


test_cos () ->
    0.0 = geometry:cos (?VECTOR(1,0,0), ?VECTOR(0,1,0)),
    1.0 = geometry:cos (?VECTOR(1,0,0), ?VECTOR(1,0,0)),
    ok.


test_is_point_inside_triangle () ->
    %% different plane
    false = is_point_inside_triangle (?VECTOR(0,0,1),
                                      ?TRIANGLE(?VECTOR(0,0,0),
                                                ?VECTOR(0,1,0),
                                                ?VECTOR(1,0,0))),

    %% same plane but outside
    false = is_point_inside_triangle (?VECTOR(-1,-1,0),
                                      ?TRIANGLE(?VECTOR(0,0,0),
                                                ?VECTOR(0,1,0),
                                                ?VECTOR(1,0,0))),

    %% inside
    true = is_point_inside_triangle (?VECTOR(1,1,0),
                                     ?TRIANGLE(?VECTOR(0,0,0),
                                               ?VECTOR(0,10,0),
                                               ?VECTOR(10,0,0))),

    ok.


test_segment_plane_intersect () ->
    %% parallel adjacent
    {true, parallel_adjacent, _} =
        segment_plane_intersect ({?VECTOR(1,0,0),
                                  ?VECTOR(-1,0,0)},
                                 ?TRIANGLE(?VECTOR(0,0,0),
                                           ?VECTOR(1,0,0),
                                           ?VECTOR(0,1,0))),

    %% parallel disjoint
    {false, parallel_disjoint, _} =
        segment_plane_intersect ({?VECTOR(1,0,1),
                                  ?VECTOR(-1,0,1)},
                                 ?TRIANGLE(?VECTOR(0,0,0),
                                           ?VECTOR(1,0,0),
                                           ?VECTOR(0,1,0))),

    %% not parallel but too short segment
    {false, line_intersecting, _} =
        segment_plane_intersect ({?VECTOR(0,0,1),
                                  ?VECTOR(0,0,2)},
                                 ?TRIANGLE(?VECTOR(0,0,0),
                                           ?VECTOR(1,0,0),
                                           ?VECTOR(0,1,0))),

    %% intersecting
    {true, segment_intersecting, ?VECTOR(0.0,0.0,0.0)} =
        segment_plane_intersect ({?VECTOR(0,0,-1),
                                  ?VECTOR(0,0,1)},
                                 ?TRIANGLE(?VECTOR(0,0,0),
                                           ?VECTOR(1,0,0),
                                           ?VECTOR(0,1,0))),

    ok.


test_segment_triangle_intersect () ->
    %% parallel
    false = segment_triangle_intersect ({?VECTOR(0,0,1),
                                         ?VECTOR(1,0,1)},
                                        ?TRIANGLE(?VECTOR(0,0,0),
                                                  ?VECTOR(1,0,0),
                                                  ?VECTOR(0,1,0))),

    %% not parallel but too short segment
    false = segment_triangle_intersect ({?VECTOR(1,1,1),
                                         ?VECTOR(1,1,2)},
                                        ?TRIANGLE(?VECTOR(0,0,0),
                                                  ?VECTOR(10,0,0),
                                                  ?VECTOR(0,10,0))),

    %% not parallel, not too short segment but outside the triangle
    false = segment_triangle_intersect ({?VECTOR(1,1,-1),
                                         ?VECTOR(1,1,1)},
                                        ?TRIANGLE(?VECTOR(0,0,0),
                                                  ?VECTOR(1,0,0),
                                                  ?VECTOR(0,1,0))),

    %% intersecting
    true = segment_triangle_intersect ({?VECTOR(1,1,-1),
                                        ?VECTOR(1,1,1)},
                                       ?TRIANGLE(?VECTOR(0,0,0),
                                                 ?VECTOR(10,0,0),
                                                 ?VECTOR(0,10,0))),

    ok.


test_triangles_intersect () ->
    %% parallel
    false = triangles_intersect (?TRIANGLE(?VECTOR(0,0,0),
                                           ?VECTOR(1,0,0),
                                           ?VECTOR(0,1,0)),
                                 ?TRIANGLE(?VECTOR(0,0,1),
                                           ?VECTOR(1,0,1),
                                           ?VECTOR(0,1,1))),

    %% not parallel, a triangle not intersecting the other plane
    false = triangles_intersect (?TRIANGLE(?VECTOR(0,0,-1),
                                           ?VECTOR(1,0,-1),
                                           ?VECTOR(0,1,-1)),
                                 ?TRIANGLE(?VECTOR(0,0,0),
                                           ?VECTOR(0,0,1),
                                           ?VECTOR(0,1,0))),

    %% not parallel, a triangle intersecting the other plane but not
    %% the other triangle
    false = triangles_intersect (?TRIANGLE(?VECTOR(-1,-1,0.5),
                                           ?VECTOR(-1,-2,0.5),
                                           ?VECTOR(-2,-1,0.5)),
                                 ?TRIANGLE(?VECTOR(0,0,0),
                                           ?VECTOR(0,0,1),
                                           ?VECTOR(0,1,0))),

    %% intersecting
    true = triangles_intersect (?TRIANGLE(?VECTOR(0,0,0),
                                          ?VECTOR(1,0,0),
                                          ?VECTOR(0,1,0)),
                                ?TRIANGLE(?VECTOR(-1,-1,0),
                                          ?VECTOR(0.5,0.5,-1),
                                          ?VECTOR(0.5,0.5,1))),

    %% intersecting with one common vertex
    true = triangles_intersect (?TRIANGLE(?VECTOR(0,0,0),
                                          ?VECTOR(1,0,0),
                                          ?VECTOR(0,1,0)),
                                ?TRIANGLE(?VECTOR(0,0,0),
                                          ?VECTOR(0.5,0.5,-1),
                                          ?VECTOR(0.5,0.5,1))),

    ok.
