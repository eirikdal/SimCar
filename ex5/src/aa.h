/*
 * aa.h
 *
 *  Created on: 26. feb. 2012
 *      Author: hauk184
 */

#ifndef AA_H_
#define AA_H_

typedef unsigned int boolean;

#define false 0
#define true (!false)

void aa_init();
void aa_enter();
boolean aa_vote(int* thread, int* v);
void aa_leave();
int aa_result();

#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

#endif /* AA_H_ */
