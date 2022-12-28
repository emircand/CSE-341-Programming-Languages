:- dynamic room/4.
:- dynamic course/5.
:- dynamic student/3.
% room(id, capacity, hours[], occupancy[], equipment, handicapped access)
room(z23, 25, [8, 9, 10, 11, 12, 13, 14, 15, 16], [occupancy([8,9], cse241)], smartboard, access).
room(z06, 130, [8, 9, 10, 11, 12, 13, 14, 15, 16], [],  projector, notAccess).
room(z10, 78, [8, 9, 10, 11, 12, 13, 14, 15, 16], [],  projector, access).
room(z12, 54, [8, 9, 10, 11, 12, 13, 14, 15, 16], [],  _, notAccess).
room(amfi, 42, [8, 9, 10, 11, 12, 13, 14, 15, 16], [],  _, access).

% occupancy(hour, course)

% course(id, instructor, capacity, hours[], room, equipment, handicapped access)
course(cse241, ysa, 180, [8, 9], z06, projector, notHandicapped).
course(cse341, Genc, 120, [10, 11], z23, smartboard, handicapped).
course(cse231, Habil, 70, [12, 13], z10, lab, notHandicapped).
course(cse343, Habil, 100, [13, 14], z12, projector, handicapped).
course(cse332, ysa, 180, [12, 13], z06, projector, handicapped).
course(cse102, Genc, 100, [15, 16], z23 , smartboard, notHandicapped).

% instructor(id, courses[], preference)
instructor(ysa, [cse241, cse332], projector).
instructor(Genc, [cse102, cse341], projector).
instructor(Habil, [cse231, cse343], lab).

% student(id, courses[], handicapped)
student(1, [cse341, cse231], notHandicapped).
student(2, [cse102, cse332], handicapped).
student(3, [cse241, cse231], notHandicapped).
student(4, [cse332, cse341], notHandicapped).

% add new student
addStudent(ID, Courses, Handicapped) :-
    assert(student(ID, Courses, Handicapped)).
% add new course
addCourse(ID, Instructor, Capacity, Hours, Room, Handicapped) :-
    assert(course(ID, Instructor, Capacity, Hours, Room, Handicapped)).
% add new room
addRoom(ID, Capacity, HoursList, Equipment, HAccess) :-
    assert(room(ID, Capacity, HoursList, Equipment, HAccess)).

% Check if two lists have any elements in common
hasCommonElem([H1|T1], [H2|T2]) :-
    H1 = H2;
    hasCommonElem(T1, [H2|T2]);
    hasCommonElem([H1|T1], T2).

% Check if two course schedules overlap
overlaps(Course1, Course2) :-
    course(Course1, _, _, Hours1, _, _, _),
    course(Course2, _, _, Hours2, _, _, _),
    hasCommonElem(Hours1, Hours2).

% Check if a course schedule overlaps with a room occupancy
overlaps(Course, Room) :-
    course(Course, _, _, Hours, _, _, _),
    room(Room, _, _, Occupancy, _, _),
    member(occupancy(Hours, _), Occupancy).

% Check if a scheduling conflict exists
schedulingConflict(Course1, Course2) :-
    overlaps(Course1, Course2);
    overlaps(Course2, Course1).

schedulingConflict(Course, Room) :-
    overlaps(Course, Room);
    overlaps(Room, Course).

% Check which room can be assigned to a given class. 
% Find a suitable room for a given class
findRoom(Course, Room) :-
    course(Course, _, Capacity, _, _, Equipment, Handicapped),
    room(Room, Capacity, _, _, Equipment, Handicapped).

% Check which room can be assigned to which classes.
% Find the classes that can be assigned to a given room
findClasses(Room, Classes) :-
    findall(Course, (course(Course, _, _, _, Room, _, _)), Classes).


% Check whether a student can be enrolled to a given class.
canEnroll(Student, Course) :-
    student(Student, _, _),
    course(Course, _, Capacity, _, _, _, _),
    findall(S, student(S, Course, _), Students),
    length(Students, NumStudents),
    NumStudents < Capacity.

% Check which classes a student can be assigned.
% Find the classes that a student can be assigned to
findAssignments(Student, Classes) :-
    student(Student, _, Handicapped),
    findall(Course, (course(Course, _, _, _, _, _, Handicapped)), Classes).


