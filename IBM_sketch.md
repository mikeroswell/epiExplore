Notes 2024 Sep 09 (Mon) (suggestions from JD mostly)

I like the general approach to the contact matrix, we should end by unpacking into a data frame (rateFrame) with columns index, person1, person2, rate

I also like the approach of contactOrder and I'm going to suggest another data frame (contactEvents) with columns of time, person1, person1. We will fill in the times of contacts by incrementing an exponential (this can be done with a while loop and a final time, or we could just pick a fixed number that we expect to outlive the epidemic).

To simulate, we can save some time by handling only the events in contactOrder as events. We will have an individual data frame that will record (as now) how many people the individual infected, but will also record infection time and recovery time -- we can pick recovery time when the individual is infected (in theory we might save some time with ticky code and not even bother to pick a recovery time for individuals who don't infect).

Simple way: 
* Populate our people frame with recovery delays before we start
* When we reach an event,
	* first check if either person is susceptible
		* if not, we're done with that event
	* then check if the other person is currently infectious
		* first look for infected time, then look at time elapsed

This makes it a tiny bit ugly to know current prevalence; we can track pseudo-prevalence (increase when you're infected and decrease when we _notice_ you're recovered) -- this is a valid but inefficient stopping criterion.

