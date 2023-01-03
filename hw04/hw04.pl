:- dynamic room/6.
:- dynamic course/7.
:- dynamic student/3.
:- dynamic instructor/3.
% room(id, capacity, hours[], occupancy[], equipment, Handicapped access)
room(z23, 25, [8, 9, 10, 11, 12, 13, 14, 15, 16], [occupancy([10,11], cse341), occupancy([15,16], cse102)], smartboard, access).
room(z06, 130, [8, 9, 10, 11, 12, 13, 14, 15, 16], [occupancy([8,9], cse241), occupancy([12,13], cse332)],  projector, notAccess).
room(z10, 78, [8, 9, 10, 11, 12, 13, 14, 15, 16], [occupancy([12,13], cse231)],  projector, access).
room(z12, 54, [8, 9, 10, 11, 12, 13, 14, 15, 16], [occupancy([13,14], cse343)],  _, notAccess).
room(amfi, 300, [8, 9, 10, 11, 12, 13, 14, 15, 16], [],  projector, access).

% course(id, instructor, capacity, hours[], room, equipment[], Handicapped access)
course(cse241, ysa, 180, [8, 9], z06, projector, access).
course(cse341, genc, 120, [10, 11], z23, smartboard, access).
course(cse231, habil, 70, [12, 13], z10, projector, notAccess).
course(cse343, habil, 100, [13, 14], z12, projector, access).
course(cse332, ysa, 180, [12, 13], z06, projector, access).
course(cse102, genc, 100, [15, 16], z23 , smartboard, notAccess).

% instructor(id, courses[], preference)
instructor(ysa, [cse241, cse332], projector).
instructor(genc, [cse102, cse341], projector).
instructor(habil, [cse231, cse343], lab).

% student(id, courses[], access)
student(1, [cse341, cse231], notAccess).
student(2, [cse102, cse332], access).
student(3, [cse241, cse231], notAccess).
student(4, [cse332, cse341], notAccess).

% add new student
addStudent(ID, Courses, Handicapped) :-
    maplist(canEnroll(Handicapped), Courses), % check if the student can be enrolled in all of the specified courses
    assert(student(ID, Courses, Handicapped)), % add the student to the database
    write('Student added succesfully').

% add new course
addCourse(ID, Instructor, Capacity, Hours, Room, Equipment, Handicapped) :-
    assert(course(ID, Instructor, Capacity, Hours, Room, Equipment, Handicapped)),
    instructor(Instructor, Courses, _),
    \+ member(ID, Courses), % check if the course is already in the instructor's course list
    append(Courses, [ID], NewCourses), % add the course to the instructor's course list
    retract(instructor(Instructor, Courses, _)), % remove the old version of the instructor's course list
    assert(instructor(Instructor, NewCourses, _)), % add the updated version of the instructor's course list
    write('Course added succesfully').

% add new room
addRoom(ID, Capacity, HoursList, Equipment, HAccess) :-
    assert(room(ID, Capacity, HoursList, Equipment, HAccess)),
    write('Room added succesfully').

% Check if two lists have any elements in common
hasCommonElem([H1|T1], [H2|T2]) :-
    H1 = H2;
    hasCommonElem(T1, [H2|T2]);
    hasCommonElem([H1|T1], T2).

% Check if two course schedules overlap
overlaps(Course1, Course2) :-
    course(Course1, Inst1, _, Hours1, Room1, _, _),
    course(Course2, Inst2, _, Hours2, Room2, _, _),
    ((Inst1 = Inst2, Room1 \= Room2, hasCommonElem(Hours1, Hours2)); 
    (Course1 \= Course2, Room1 = Room2, hasCommonElem(Hours1, Hours2))).


% Check if a course schedule overlaps with a room occupancy
overlaps(Course, Room) :-
    course(Course, _, _, Hours, _, _, _),
    room(Room, _, _, Occupancy, _, _),
    member(occupancy(Hours, _), Occupancy).

% Check if a scheduling conflict exists
schedulingConflict(Course1, Course2) :-
    (overlaps(Course1, Course2)) ->
        (course(Course1, _, _, Hours1, _, _, _),
         course(Course2, _, _, Hours2, _, _, _),
         format('Scheduling conflict detected between courses ~w (~w) and ~w (~w)', [Course1, Hours1, Course2, Hours2]));
    true.

% Check if a scheduling conflict exists
schedulingConflict(Course, Room) :-
    (overlaps(Course, Room)) ->
        (course(Course, _, _, Hours, _, _, _),
         format('Scheduling conflict detected between course ~w (~w) and room ~w', [Course, Hours, Room]));
    true.

% Check which rooms can be assigned to a given class.
findRooms(Course, Rooms) :-
    course(Course, _, Capacity, Hours, _, Equipment, Handicapped),
    findall(Room, (room(Room, RoomCapacity, RoomHours, Occupancy, Equipment, Handicapped),
    RoomCapacity >= Capacity,
    not(member(Hours, Occupancy)),
    not(member(RoomHours, Hours))),
    Rooms).


% Check which classes can be assigned to a given room.
findClasses(Room, Classes) :-
    room(Room, RoomCapacity, _, Occupancy, RoomEquipment, RoomHandicapped),
    findall(Course, (course(Course, _, CourseCapacity, CourseHours, _, CourseEquipment, CourseHandicapped),
    CourseCapacity =< RoomCapacity,
    CourseEquipment = RoomEquipment,
    CourseHandicapped = RoomHandicapped,
    not(member((CourseHours, Course), Occupancy))),
    Classes).


% Check which classes a student can be assigned.
findAssignments(Student, Classes) :-
    student(Student, _, Handicapped),
    findall(Course, (course(Course, _, _, _, _, _, Handicapped)), Classes).


canEnroll(Handicapped, Course) :-
    course(Course, _, _, _, _, _, CourseHandicapped),
    Handicapped = CourseHandicapped.

% Check whether a student can be enrolled to a given class.
canEnroll(Student, Course) :-
    student(Student, _, Handicapped),
    course(Course, _, _, _, _, _, CourseHandicapped),
    Handicapped = CourseHandicapped.


% knowledge base
% schedule(from, to, cost).
schedule(istanbul, izmir, 2).
schedule(istanbul, ankara, 1).
schedule(istanbul, rize, 4).
schedule(rize, ankara, 5).
schedule(izmir, ankara, 6).
schedule(ankara, van, 4).
schedule(van, gaziantep, 3).
schedule(ankara, diyarbakir, 8).
schedule(antalya, izmir, 2).
schedule(diyarbakir, antalya, 4).
schedule(erzincan, antalya, 3).
schedule(canakkale, erzincan, 6).
schedule(canakkale, edirne, 3).
schedule(edirne, tekirdag, 1).

connection(X, Y, C) :- connected(X, Y, C, []).
connected(X, Y, C, Visited) :- schedule(X, Y, C), \+ member(Y, Visited).
connected(X, Y, C, Visited) :- schedule(X, Z, C1), \+ member(Z, Visited), connected(Z, Y, C2, [Z|Visited]), C is C1 + C2.
