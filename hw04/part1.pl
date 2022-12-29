:- dynamic room/4.
:- dynamic course/5.
:- dynamic student/3.
% room(id, capacity, hours[], occupancy[], equipment, handicapped access)
room(z23, 25, [8, 9, 10, 11, 12, 13, 14, 15, 16], [occupancy([8,9], cse241)], smartboard, access).
room(z06, 130, [8, 9, 10, 11, 12, 13, 14, 15, 16], [],  projector, notAccess).
room(z10, 78, [8, 9, 10, 11, 12, 13, 14, 15, 16], [],  projector, access).
room(z12, 54, [8, 9, 10, 11, 12, 13, 14, 15, 16], [],  _, notAccess).
room(amfi, 42, [8, 9, 10, 11, 12, 13, 14, 15, 16], [],  _, access).


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
    maplist(canEnroll(ID), Courses), % check if the student can be enrolled in all of the specified courses
    assert(student(ID, Courses, Handicapped)), % add the student to the database
    write('Student added succesfully').

% add new course
addCourse(ID, Instructor, Capacity, Hours, Room, Handicapped) :-
    assert(course(ID, Instructor, Capacity, Hours, Room, Handicapped)),
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
    student(Student, _, Handicapped),
    course(Course, _, _, _, _, _, CourseHandicapped),
    Handicapped = CourseHandicapped.

% Check which classes a student can be assigned.
% Find the classes that a student can be assigned to
findAssignments(Student, Classes) :-
    student(Student, _, Handicapped),
    findall(Course, (course(Course, _, _, _, _, _, Handicapped)), Classes).


