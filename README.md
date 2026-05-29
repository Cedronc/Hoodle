# Timeslots and Attendees Chosen Datastructures.
## Timeslots: Sequence over Map
Why Sequence?
- Ordered collection with O(log n) indexed access (lookup, index)
- O(1) append and prepending (|>, <|) for adding slots
- O(log n) updates (deleteAt, update) for slot modifications
- Natural fit for position-based user interaction (e.g., "remove slot #2")

Map requires artificial keys, which for timeslots is unnecessary and annoying.
Though lookup is $O(log n)$ like Sequence, Sequence's positional indexing is more
natural for slot management.



## Participants/Attendees: Set over Map
Why Set?
- O(log n) membership (member) for registration checks
- Automatic uniqueness — no duplicate users
- O(log n) insert/delete for registration toggling

A Set has a simpler API with less fuss and does everything I need. Participants
cannot appear more than once so a Set is perfect. Map has the same time
complexion but since Set is closer to what I need, I used a Set.
