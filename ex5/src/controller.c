
#include "controller.h"

#define NUM_THREADS 3;
#define sleep(seconds) Sleep((seconds)*1000)

int g_var = 0;
int g_var_last = 0;

void* controller(void* threadId) {
	int* id = (int*)threadId;

	for(int i=0;i<10;i++) {
		sleep(1);
		aa_enter();
		int temp = g_var+1;

		if(rand()%10 == 0) // fails every ten times
			temp = rand_r(*(&id));

		boolean result = aa_vote(id, &temp);

		printf("thread %d: g_var: %d\n", *(&id), temp);

		if(result) {
			g_var = aa_result();
		} else {
			// backtrack
		}
	}
}

int main(int argc, char** args) {
	pthread_t threads[3];

	aa_init();

	for (int i=0;i<3;i++) {
		pthread_create(&threads[i], NULL, controller, (void *)i);
	}

	for (int i=0;i<3;i++)
		pthread_join(threads[i], NULL);

	return 0;
}
