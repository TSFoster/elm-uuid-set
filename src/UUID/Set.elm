module UUID.Set exposing (Set, diff, empty, filter, foldl, foldr, fromList, insert, intersect, isEmpty, map, member, partition, remove, singleton, size, toList, union)

import Set
import UUID exposing (UUID, fromString, toString)


type Set
    = Set (Set.Set String)


empty : Set
empty =
    Set Set.empty


singleton : UUID -> Set
singleton =
    toString >> Set.singleton >> Set


insert : UUID -> Set -> Set
insert uuid (Set set) =
    Set (Set.insert (toString uuid) set)


remove : UUID -> Set -> Set
remove uuid (Set set) =
    Set (Set.remove (toString uuid) set)


isEmpty : Set -> Bool
isEmpty (Set set) =
    Set.isEmpty set


member : UUID -> Set -> Bool
member uuid (Set set) =
    Set.member (toString uuid) set


size : Set -> Int
size (Set set) =
    Set.size set


union : Set -> Set -> Set
union (Set set1) (Set set2) =
    Set (Set.union set1 set2)


intersect : Set -> Set -> Set
intersect (Set set1) (Set set2) =
    Set (Set.intersect set1 set2)


diff : Set -> Set -> Set
diff (Set set1) (Set set2) =
    Set (Set.diff set1 set2)


toList : Set -> List UUID
toList (Set set) =
    List.filterMap (fromString >> Result.toMaybe) (Set.toList set)


fromList : List UUID -> Set
fromList =
    List.map toString >> Set.fromList >> Set


map : (UUID -> UUID) -> Set -> Set
map fn =
    mapTo (fn >> toString) >> Set


mapTo : (UUID -> comparable) -> Set -> Set.Set comparable
mapTo fn (Set set) =
    Set.foldl (mapFold fn) Set.empty set


mapFold : (UUID -> comparable) -> String -> Set.Set comparable -> Set.Set comparable
mapFold fn canonical set =
    fromString canonical
        |> Result.map (\uuid -> Set.insert (fn uuid) set)
        |> Result.withDefault set


foldl : (UUID -> b -> b) -> b -> Set -> b
foldl fn initial (Set set) =
    Set.foldl (foldFold fn) initial set


foldr : (UUID -> b -> b) -> b -> Set -> b
foldr fn initial (Set set) =
    Set.foldr (foldFold fn) initial set


foldFold : (UUID -> b -> b) -> String -> b -> b
foldFold fn canonical accumulator =
    fromString canonical
        |> Result.map (\uuid -> fn uuid accumulator)
        |> Result.withDefault accumulator


filter : (UUID -> Bool) -> Set -> Set
filter fn (Set set) =
    Set (Set.filter (filterFn fn) set)


partition : (UUID -> Bool) -> Set -> ( Set, Set )
partition fn (Set set) =
    Set.partition (filterFn fn) set
        |> Tuple.mapBoth Set Set


filterFn : (UUID -> Bool) -> String -> Bool
filterFn fn =
    fromString >> Result.map fn >> Result.withDefault False
