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
schedule(canakkale, ankara, 2).
schedule(ankara, gaziantep, 6).

connection(X, Y, C) :-
    (   var(X) ->
        schedule(City, Y, C),
        writef('There is a route from %w to %w with cost %w', [City, Y, C]), nl,
        fail
    ;   var(Y) ->
        schedule(X, City, C),
        writef('There is a route from %w to %w with cost %w', [X, City, C]), nl,
        fail
    ;   schedule(X, Y, C) ->
        writef('There is a direct route between %w and %w with cost %w', [X, Y, C]), nl
    ;   writef('There is no direct route between %w and %w', [X, Y]), nl
    ).
