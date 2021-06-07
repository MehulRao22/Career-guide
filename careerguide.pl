careerguide :-
	expert_system,
	reset_system,
	choose_career(Career),
	explain(Career), nl.
careerguide :-
	write('Can\'t find a career for you. Sorry! :('), nl.

expert_system :-
	write('I\'ll help you find the best career for you!'), nl,
	write('For each question, enter your choice followed by a dot'), nl, nl.

choose_career(Career) :-
	career(Career), !.

:- dynamic(answered/2).

reset_system :-
	retractall(answered(_,_)).
reset_system.

	
career(medicine) :-
	favourite_subject(biology),
	(
		satisfaction(safe_and_protect);
		satisfaction(strong_active); 
		best_part(help_people);
		(
			\+ presentation(sleeping),
			\+ schedule(freedom)
		);
		\+ desire(fame);
		incharge(no)
	).

career(writer) :-
	(
		work_environment(_),
		people_opinion(_),
		satisfaction(_),
		favourite_subject(literature),
		(
			\+ desire(job_security);
			jealous(be_creative);
			presentation(speech_writer);
			\+ schedule(full_week);
			people_opinion(creative)
		)
	);
	(
		(
			schedule(flexible);
			schedule(freedom)
		),
		presentation(speech_writer)
	);
	(
		(
			people_opinion(creative);
			jealous(be_creative)
		), 
		(
			favourite_subject(literature); 
			presentation(speech_writer)
		)
	);
	(
		presentation(speech_writer),
		(
			\+ desire(job_security);
			jealous(be_creative);
			favourite_subject(literature);
			\+ schedule(full_week);
			people_opinion(creative)
		)
	).

career(engineering) :-
	(
		(
			favourite_subject(computer);
			favourite_subject(math)
		),
		(
			(
				\+ people_opinion(creative),
				\+ desire(fame)
			);
			(
				\+ people_opinion(negotiator),
				\+ desire(fame)
			);
			(
				\+ jealous(be_in_charge),
				\+ work_environment(physically_active)
			)
		)
	);
	(
		desire(job_security),
		(
			jealous(difference);
			jealous(be_social)
		),
		\+ people_opinion(creative),
		\+ people_opinion(negotiator)
	).
	
explain(engineering) :-
	write('Engineering'), nl,
	write('Application of scientific knowledge in order to invent, innovate, design and improve things.').

explain(medicine) :-
	write('Medicine'), nl,
	write('It\'s the science and practice of diagnosis, treatment and prevention of disease.').

explain(writer) :-
	write('Writer'), nl,
	write('Become a writer!'), nl,
	write('You have a skill for language, your imagination is vast, your brain is overflowing with ideas.').

question(incharge) :-
	write('Do you want to be incharge of others?'), nl.

question(satisfaction) :-
	write('Which of these would give you the greatest satisfaction?'), nl.

question(people_opinion) :-
	write('What do most people say about you?'), nl.

question(work_environment) :-
	write('Which of these work environments sounds most appealing to you?'), nl.

question(favourite_subject) :-
	write('What was your favourite subject in school?'), nl.

question(desire) :-
	write('Which of these do you want out of a career?'), nl.

question(jealous) :-
	write('You are jealous of your friend\'s career because he gets to:'), nl.

question(schedule) :-
	write('What kind of schedule do you want in your career?'), nl.

question(presentation) :-
	write('Suppose it\'s presentation time. You\'re the one who:'), nl.

question(best_part) :-
	write('Best part of your very first job?'), nl.
	
	option(yes) :-
	write('Yes').
option(no) :-
	write('No').


option(safe_and_protect) :-
	write('Knowing others were safe and protected because of my work').
option(impact) :-
	write('Creating something that had an impact on someone').
option(negotiation) :-
	write('Successfully closing a difficult negotiation').
	
	
option(detail_oriented) :-
	write('Detail oriented').
option(professional) :-
	write('Professional').
option(outgoing) :-
	write('Outgoing').
	
	
option(outside) :-
	write('I\'d like to be outside, interacting with different people').
option(home) :-
	write('I\'d like to work from home').
	
	
option(history) :-
	write('History/Social Studies').
option(literature) :-
	write('English literature').
option(economics) :-
	write('Economics').
option(art) :-
	write('Art').
	
	
option(paycheck) :-
	write('A big paycheck').
option(balance) :-
	write('Good work-life balance').
	
	
option(difference) :-
	write('Make a difference in people\'s everyday lives').
option(be_creative) :-
	write('Be creative, everyday').
	
	
option(full_week) :-
	write('Put in the full week to work and have the weekend to myself').
option(freedom) :-
	write('Complete freedom to be flexible depending on my job\'s schedule').
	

option(podium) :-
	write('is at the podium').
option(speech_writer) :-
	write('wrote the speech').
	
	
option(new_friends) :-
	write('Made new friends').
option(learn_something) :-
	write('Learning something new').
	
	
incharge(Answer) :-
	answered(incharge, Answer), !.
incharge(Answer) :-
	\+ answered(incharge, _),
	ask(incharge, Answer, [yes, no]).

satisfaction(Answer) :-
	answered(satisfaction, Answer), !.
satisfaction(Answer) :-
	\+ answered(satisfaction, _),
	ask(satisfaction, Answer, [safe_and_protect, impact, negotiation]).

people_opinion(Answer) :-
	answered(people_opinion, Answer), !.
people_opinion(Answer) :-
	\+ answered(people_opinion, _),
	ask(people_opinion, Answer, [detail_oriented, professional, outgoing]).

work_environment(Answer) :-
	answered(work_environment, Answer), !.
work_environment(Answer) :-
	\+ answered(work_environment, _),
	ask(work_environment, Answer, [outside, home]).

favourite_subject(Answer) :-
	answered(favourite_subject, Answer), !.
favourite_subject(Answer) :-
	\+ answered(favourite_subject, _),
	ask(favourite_subject, Answer, [history, literature, economics, art]).

desire(Answer) :-
	answered(desire, Answer), !.
desire(Answer) :-
	\+ answered(desire, _),
	ask(desire, Answer, [paycheck, balance]).

jealous(Answer) :-
	answered(jealous, Answer), !.
jealous(Answer) :-
	\+ answered(jealous, _),
	ask(jealous, Answer, [difference, be_creative]).

schedule(Answer) :-
	answered(schedule, Answer), !.
schedule(Answer) :-
	\+ answered(schedule, _),
	ask(schedule, Answer, [full_week, freedom]).

presentation(Answer) :-
	answered(presentation, Answer), !.
presentation(Answer) :-
	\+ answered(presentation, _),
	ask(presentation, Answer, [podium, speech_writer]).

best_part(Answer) :-
	answered(best_part, Answer), !.
best_part(Answer) :-
	\+ answered(best_part, _),
	ask(best_part, Answer, [new_friends, learn_something]).
	
	
ask(Question, Answer, Options) :-
	question(Question),
	generate_options(Options, 1),
	read(Index),
	find_option(Index, Options, Selection),
	asserta(answered(Question, Selection)),
	Selection = Answer.


find_option(1, [Head|_] , Head).
find_option(Index, [_|Tail], Result) :-
	Nextindex is Index -1,
	find_option(Nextindex, Tail, Result).


generate_options([],_).
generate_options([Head|Tail], Index) :-
	write(Index), write(' '),
	option(Head), nl,
	Nextindex is Index +1,
	generate_options(Tail, Nextindex).

	
