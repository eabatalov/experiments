#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include "CodePatcher.h"

void* CodePathSwitcherWork(void* arg)
{
	if (arg == 0) 
	{
		pthread_t codePathSwitcherThread;
		memset(&codePathSwitcherThread, 0, sizeof(codePathSwitcherThread));
		pthread_create(&codePathSwitcherThread, NULL, CodePathSwitcherWork, (void*)1);
		return NULL;
	} else
	{
		static unsigned int on = 0;
		for(;;) 
		{
			usleep(1 * 1000 * 1000);
			printf("Switching code paths\n");
			on = !on;
			if (on)
				PathsOn();
			else 
				PathsOff();
		}
		return NULL;
	}
}

void Foo(const char *msg, int val) 
{
	printf("%s: ", __FUNCTION__);
	printf("%s %d \n", msg, val);
}

void Bar(const char *msg, int val)
{
	printf("%s: ", __FUNCTION__);
	printf("%s %d \n", msg, val);
}

int main()
{
	CodePatcherInit();
	//Registering can be avoided adding first hit register trap to CP_*_CALL macro
	CP_CALL_BYPASS_REG(Foo);
	CP_CALL_BYPASS_REG(Bar);
	
	CodePathSwitcherWork((void*)0);
	for(;;) 
	{
		usleep(0.5 * 1000 * 1000);
		printf("New Loop\n");
		CP_BYPASSABLE_CALL2(Foo, "ho-ho-ho!", 1);
		CP_BYPASSABLE_CALL2(Bar, "One-two-three!", 2);
	}
	return 0;
}
