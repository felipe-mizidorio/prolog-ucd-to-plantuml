:- use_module(usecasedg2puml).

% Test the entire process from file to PlantUML
test_hospital :-
    write('Testing hospital example...'), nl,
    process_file('hospital.txt', 'hospital.puml'),
    write('Conversion completed. Check hospital.puml'), nl.

% Test adding elements manually
test_manual :-
    write('Testing manual element addition...'), nl,
    retractall(usecasedg2puml:uc_module(_)),
    retractall(usecasedg2puml:uc_case(_,_,_)),
    retractall(usecasedg2puml:uc_actor(_,_)),
    retractall(usecasedg2puml:uc_rel(_,_,_)),
    insert_package('TestPackage'),
    insert_usecase('TestPackage', 'Login', none),
    insert_usecase('TestPackage', 'Register', none),
    insert_actor('User', none),
    insert_relation(assoc, 'User', 'Login'),
    insert_relation(assoc, 'User', 'Register'),
    count_usecases('TestPackage', Count),
    format('TestPackage has ~w use cases~n', [Count]),
    spec_to_puml('manual_test.puml'),
    write('Manual test completed. Check manual_test.puml'), nl.

% Run both tests
run_tests :-
    test_hospital,
    nl,
    test_manual.