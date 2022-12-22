% room(id, capacity, hours[], options)
room(z23, 25, [8, 9, 10, 11, 12, 13, 14, 15, 16], smartboard).
room(z06, 130, [8, 9, 10, 11, 12, 13, 14, 15, 16], projector).
room(z10, 78, [8, 9, 10, 11, 12, 13, 14, 15, 16], handicapped).
room(z12, 54, [8, 9, 10, 11, 12, 13, 14, 15, 16]).
room(amfi, 42, [8, 9, 10, 11, 12, 13, 14, 15, 16]).

% course(id, instructor, capacity, timeLength, options..)
%course(CSE241, YSA, 180, 2, projector).
course(CSE341, Genc, 120, 4, smartboard).
course(CSE231, Habil, 70, 3, lab).
course(CSE343, Habil, 100, 2, projector).
course(CSE332, YSA, 180, 6, projector).
course(CSE102, Genc, 100, 6, smartboard).

% instructor(id, courses[], preference)
instructor(YSA, [CSE241, CSE332], projector).
instructor(Genc, [CSE102, CSE341], projector).
%instructor(Habil, [CSE231, CSE343], lab).

% student(id, courses[], handicapped)
%student(1, [CSE341, CSE231], false).
student(2, [CSE102, CSE332], true).
student(3, [CSE241, CSE231], false).
student(4, [CSE332, CSE341], false).

% add new student
addStudent(ID, Courses, Handicapped) :-
    assert(student(ID, Courses, Handicapped)).
% add new course
addCourse(ID, Instructor, Capacity, TimeLength, Feature) :-
    assert(course(ID, Instructor, Capacity, TimeLength, Feature)).
% add new room
addRoom(ID, Capacity, HoursList, Feature) :-
    assert(room(ID, Capacity, HoursList, Feature)).
% Check whether there is any scheduling conflict. 
% Check which room can be assigned to a given class. 
% Check which room can be assigned to which classes. 
% Check whether a student can be enrolled to a given class.
% Check which classes a student can be assigned.


