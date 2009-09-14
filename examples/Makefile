#
# Makefile
#

EBIN=../ebin/
#ERLC=erlc
BEAM=erlc -b beam -pa $(EBIN) -I ../include \
	-I /usr/local/lib/erlang/lib/esdl-0.96.0626/include/
YRL=erlc -W
FIND=find

.SUFFIXES: .erl .beam

OBJS=\
all_balls \
balls \
bounce \
eurobot \
obstacle_avoid \
mill \
test \
collision_test \
more_robots \
triangle \
trimesh \
object_not_in_a_compound_list \
to_object_dict \
add_mesh \
add_aabb \
to_object_couples \
broad_phase_aabbs_intersect \
narrow_phase_trimeshes_intersect \
clean_collision_dict \
set_objects_collision_points \
from_collision_dict_to_object_list \
response_set_previous \
simulWorld \
createWorld \
simpleWorld \
#world_discretization \
#path_handler \
#strategy \
#pheromoneCell\
#target_finder

ERL_OBJS=$(foreach file, $(OBJS), $(file).beam)

OTHER_OBJS=
FIND=find

all: $(ERL_OBJS) $(OTHER_OBJS)

%.beam: %.erl
	$(BEAM) $<

clean:
	@$(RM) -f *.beam

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

