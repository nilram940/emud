#Introduction

Mapping is a tough problem. There is very little to data to uniquely
identify rooms. The strategy I used relies on the short description
"Village church", "Village green", etc. I've attempted to use obvious
exits and long description in the past but those can change on rooms
(e.g. open a window and now there is a new obvious exit and the room
description now indicates the window is open)

#Rooms

The data structure itself is a graph each node contains information
about the particular room. The most relevant information is the short
description, the exit list and the sibling list.

## sibling list

The sibling list requires the most explanation. Two rooms are
considered siblings if they have the same short description. That is
they are indistinguishable using only the traits visible within the
room.  Two rooms are considered sibling adjacent if they are siblings
and an exit command from one leads to the other. The sibling contains
a flag indicating whether any two siblings are adjacent and list of
all siblings of the curent room.

The sibling list is shared among all siblings that is if it is updated
for any one sibling it is updated for all others as well.

## exit list

The exit list contains a list of exits from the current room each
element of the list is a `cons` with the `car` being the exit command and
the `cdr` being a room number for the adjacent room.

## short desription

The short description is pretty self explanatory. It is simply the
short description of the current room "Village church", "Village
green", etc.

#Map

The map itself is an array with rooms as elements along with
maintenance variables and a sibling hash. The sibling hash maps a
short description to a sibling list. This is used to find the correct
sibling hase for new rooms.

#Strategy

The mapping strategy is fairly conservative. The client "watches"
your moves around the mud and determines the commands that lead you to
move to a new room. When the client determines that you have indeed
moved into a new room, it checks the exit list of the prior room and
if there is an exit that cooresponds to the current exit it attempts
to follow the exit. Otherwise, it creates a new room. This means that
it is possible to create multiple copies of the same room.

For example if you go `s` from church to green and `n` from green to
church, your map will contain two copies of church -- one with a south
exit to green and one with no exits.

##merging

If you continue to move around your will set off merge events for the
example above if you go `s` from church again. The client will recognize
that you have two rooms labeled church with a `s` exit to green and
determine that the churchs must be the same and will merge
them. Moreover, it will check all exits from the merged rooms and
merge any exits that should be merged. In this case, the two green
rooms.

The merging is more aggressive with non-sibling adjacent rooms and
less aggressive if two rooms are sibling adjacent.


#Future

## Mazes
The mapping currenlty does not support mazes. The merging strategy is
too aggressive.

I've considered adding a `maze` flag for the sibling to indicate that
the current group of siblings is a maze and the merging strategy
should be less aggressive.

## Coordinates

General coordinates on the mud simply do not work. There are too many
rooms that are on top of each other.


However, there are areas where a coordinate system makes sense. For
example 20s of church around the knights guild. There are plains laid
out in a nice grid.

I've added some support for coordinates. The flag field of the sibling
list can contain `coord` indicating that the client should use the
coord field of the for mapping purposes and merge more aggressively
that it would for a normal sibling adjacent room.



