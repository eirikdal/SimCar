#include "aa.h"

#define NUM_THREADS 3;

pthread_barrier_t barrier_vote_reset;
pthread_barrier_t barrier_vote;
pthread_barrier_t barrier;

pthread_barrierattr_t attr;

int* votes;
int vote = 0;

void aa_init() {
	pthread_barrier_init(&barrier_vote_reset, NULL, 3);
	pthread_barrier_init(&barrier_vote, NULL, 3);
	pthread_barrier_init(&barrier, NULL, 3);

	votes = malloc(sizeof(int)*3);

	for (int i=0;i<3;i++)
		votes[i] = -1;
}

// initiate atomic action before starting the other threads

void aa_enter() {
	pthread_barrier_wait(&barrier);
}

void aa_vote_reset(int v) {
	for (int i=0;i<3;i++)
		votes[i] = -1;

	vote = -1;
}

void aa_submit(int* thread, int* v) {
	int idx = (int)*(&thread);

	votes[idx] = *v;
}

void aa_wait(pthread_barrier_t *barrier) {
	pthread_barrier_wait(barrier);
}

int aa_result() {
	return vote;
}

boolean aa_vote(int* thread, int* v) {
	aa_vote_reset(*v);
	aa_wait(&barrier_vote_reset);
	aa_submit(thread, v);
	aa_wait(&barrier_vote);

	boolean b_vote = true;
	int temp = votes[0];

	for (int i=1;i<3;i++) {
		if (votes[i] != temp) {
			b_vote = false;
		}
	}

	if (b_vote)
		vote = votes[0];

	return b_vote;
}
