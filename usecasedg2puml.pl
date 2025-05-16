
% usecasedg2puml.pl
% ----------------------
% A Prolog program to manage UseCaseDG specs and emit PlantUML code.
% SWI-Prolog with DCG (library(dcg/basics)).

:- module(usecasedg2puml, [
    process_file/2,
    insert_package/1,
    insert_usecase/3,
    insert_actor/2,
    insert_actor/3,
    insert_relation/3,
    validate_relation/3,
    count_usecases/2,
    to_puml/1,
    spec_to_puml/1
]).

:- use_module(library(dcg/basics)).
:- dynamic
    uc_module/1,
    uc_case/3,          % uc_case(Package, Name, Alias)
    uc_actor/2,         % uc_actor(Name, Alias) - global actor
    uc_actor/3,         % uc_actor(Package, Name, Alias) - actor in package
    uc_rel/3.           % uc_rel(Type, From, To)

%% --------------------------------------------------------------------
%% 1. Insertion Predicates
%% --------------------------------------------------------------------

% Add a new package (module)
insert_package(Name) :-
    \+ uc_module(Name),
    assertz(uc_module(Name)).

% Add a use case under a package
insert_usecase(Package, Name, Alias) :-
    uc_module(Package),
    assertz(uc_case(Package, Name, Alias)).

% Add an actor (global)
insert_actor(Name, Alias) :-
    assertz(uc_actor(Name, Alias)).

% Add an actor to a specific package
insert_actor(Package, Name, Alias) :-
    uc_module(Package),
    assertz(uc_actor(Package, Name, Alias)).

% Add a relation with validation
insert_relation(Type, From, To) :-
    validate_relation(From, To, Type), % Validate before inserting
    !,
    assertz(uc_rel(Type, From, To)).
insert_relation(Type, From, To) :-
    format('ERROR: Invalid relation ~w between ~w and ~w~n', [Type, From, To]),
    fail.

%% --------------------------------------------------------------------
%% 2. Validation Predicate
%% --------------------------------------------------------------------

% Improved validation - checks that both entities exist
validate_relation(From, To, Type) :-
    atom(From), atom(To), Type \= '',
    % Check if From entity exists (either as actor or use case)
    (entity_exists(From) -> true ; 
        format('ERROR: Entity "~w" not declared~n', [From]), fail),
    % Check if To entity exists (either as actor or use case)
    (entity_exists(To) -> true ; 
        format('ERROR: Entity "~w" not declared~n', [To]), fail).

% Helper predicate to check if an entity exists
entity_exists(Entity) :-
    uc_actor(Entity, _);              % Global actor with name matching Entity
    uc_actor(_, Entity, _);           % Actor in package with name matching Entity
    uc_actor(_, _, Entity);           % Actor in package with alias matching Entity
    uc_case(_, Entity, _);            % Use case with name matching Entity
    uc_case(_, _, Entity).            % Use case with alias matching Entity

%% --------------------------------------------------------------------
%% 3. Counting Use Cases in a Package
%% --------------------------------------------------------------------

% count_usecases(+Package, -N)
count_usecases(Package, N) :-
    findall(Name, uc_case(Package, Name, _), L),
    length(L, N).

%% --------------------------------------------------------------------
%% 4. Parsing & Generation
%% --------------------------------------------------------------------

%% Entry point: process_file(+InPath, +OutPath)
process_file(In, Out) :-
    retractall(uc_module(_)),
    retractall(uc_case(_,_,_)),
    retractall(uc_actor(_,_)),
    retractall(uc_actor(_,_,_)),
    retractall(uc_rel(_,_,_)),
    read_file_to_string(In, Text, []),
    string_codes(Text, Codes),
    phrase(program(Spec), Codes),
    spec_to_facts(Spec),
    spec_to_puml(Out).

%% Convert parsed Spec term into dynamic facts (calling insert_*)
spec_to_facts(spec(module(Mod,Cases), Actors, Rels)) :-
    insert_package(Mod),
    forall(member(case(Name,Alias), Cases),
           insert_usecase(Mod, Name, Alias)),
    forall(member(actor(Name,Alias), Actors),
           insert_actor(Name, Alias)),
    forall(member(rel(Type, E1, E2), Rels),
           insert_relation(Type, E1, E2)).

%% Write PlantUML from current facts
to_puml(Out) :- 
    open(Out, write, S, [encoding(utf8)]),
    emit_header(S), 
    emit_actors(S), 
    emit_package(S),
    emit_relations(S), 
    emit_footer(S), 
    close(S).

%% Alias convenience
spec_to_puml(Out) :- to_puml(Out).

%% DCG for the DSL
program(spec(M, A, R)) --> 
    ws, module_decl(M), ws, 
    many(actor_decl, A), ws, 
    many(rel_decl, R), ws, 
    eos.

module_decl(module(Name,Cases)) --> 
    keyword("module"), ws, 
    "(", ws, quoted_string(Name), ws, ")", ws, 
    "{", ws, case_list(Cases), "}", ws.

case_list([C|Cs]) --> 
    case_decl(C), ws, 
    ("," , ws, case_list(Cs) ; { Cs = [] }).

case_decl(case(Name,Alias)) --> 
    keyword("case"), ws, 
    quoted_string(Name), ws, 
    ( keyword("as"), ws, id(Alias), ws ; { Alias = Name } ).

actor_decl(actor(Name,Alias)) --> 
    keyword("actor"), ws, 
    quoted_string(Name), ws, 
    ( keyword("as"), ws, id(Alias), ws ; { Alias = Name } ), 
    ";", ws.

rel_decl(rel(Type, E1, E2)) --> 
    entity(E1), ws, rel_op(Type), ws, entity(E2), ws, 
    optional(";"), ws.

entity(E) --> quoted_string(E).
entity(E) --> id(E).

rel_op(assoc)    --> "--".
rel_op(extend)   --> "-e>".
rel_op(include)  --> "-i>".
rel_op(gen)      --> ("->>" ; "-|>").
rel_op(dir)      --> "->".

quoted_string(S) --> "\"", string_without("\"", Chars), "\"", { atom_string(S, Chars) }.
quoted_string(S) --> "'", string_without("'", Chars), "'", { atom_string(S, Chars) }.

id(ID) --> ident_start(C), ident_rest(Cs), { atom_codes(ID, [C|Cs]) }.
ident_start(C) --> [C], { code_type(C, csymf) }.
ident_rest([C|Cs]) --> [C], { code_type(C, csym) }, !, ident_rest(Cs).
ident_rest([]) --> [].

keyword(K) --> { atom_codes(K, Codes) }, seq(Codes).
seq([]) --> [].
seq([H|T]) --> [H], seq(T).

ws --> [C], { code_type(C, space) }, !, ws.
ws --> comment, !, ws.
ws --> [].

comment --> "/*", comment_body, "*/".
comment --> "//", string_without("\n", _), "\n".

comment_body --> "*/", !, { fail }.
comment_body --> [_], comment_body.
comment_body --> [].

many(P, [X|Xs]) --> call(P, X), !, many(P, Xs).
many(_, []) --> [].
optional(X) --> X | [].

eos --> [], !.

%% Emission
emit_header(S) :- 
    format(S, "@startuml~nleft to right direction~n~n", []).

emit_actors(S) :- 
    forall(uc_actor(Name, Alias), 
        (Name = Alias ->
            format(S, 'actor "~w"~n', [Name])
        ;
            format(S, 'actor "~w" as ~w~n', [Name, Alias])
        )
    ),
    format(S, "~n", []).

emit_package(S) :- 
    uc_module(Mod), 
    format(S, 'package "~w" {~n', [Mod]), 
    forall(uc_case(Mod, Name, Alias), 
        (Name = Alias ->
            format(S, '    usecase "~w"~n', [Name])
        ;
            format(S, '    usecase "~w" as ~w~n', [Name, Alias])
        )
    ), 
    format(S, '}~n~n', []).

emit_relations(S) :- 
    forall(uc_rel(Type, From, To), 
        emit_one_rel(S, Type, From, To)).

emit_one_rel(S, assoc, From, To) :- 
    is_quoted(From, QuotedFrom),
    is_quoted(To, QuotedTo),
    format(S, '~w -- ~w~n', [QuotedFrom, QuotedTo]).

emit_one_rel(S, extend, From, To) :- 
    is_quoted(From, QuotedFrom),
    is_quoted(To, QuotedTo),
    format(S, '~w ..> ~w : <<extend>>~n', [QuotedFrom, QuotedTo]).

emit_one_rel(S, include, From, To) :- 
    is_quoted(From, QuotedFrom),
    is_quoted(To, QuotedTo),
    format(S, '~w ..> ~w : <<include>>~n', [QuotedFrom, QuotedTo]).

emit_one_rel(S, gen, From, To) :- 
    is_quoted(From, QuotedFrom),
    is_quoted(To, QuotedTo),
    format(S, '~w ..|> ~w~n', [QuotedFrom, QuotedTo]).

emit_one_rel(S, dir, From, To) :- 
    is_quoted(From, QuotedFrom),
    is_quoted(To, QuotedTo),
    format(S, '~w --> ~w~n', [QuotedFrom, QuotedTo]).

% Helper predicate to determine if an entity should be quoted
% If it's an alias and different from its name, don't quote
is_quoted(Entity, QuotedEntity) :-
    % Check if this is an alias defined with 'as' in the DSL
    (uc_case(_, Name, Entity), Name \= Entity ->
        % It's a defined alias for a use case - don't quote it
        QuotedEntity = Entity
    ;
        % Check if it's an actor with a defined alias
        (uc_actor(Name, Entity), Name \= Entity ->
            % It's a defined alias for an actor - don't quote it
            QuotedEntity = Entity
        ;
            % Not a defined alias - quote it
            format(atom(QuotedEntity), '"~w"', [Entity])
        )
    ).

emit_footer(S) :- 
    format(S, '~n@enduml~n', []).