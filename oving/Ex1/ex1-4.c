// gcc 4.7.2 +
// gcc -std=gnu99 -Wall -g -o helloworld_c helloworld_c.c -lpthread

#include <pthread.h>
#include <stdio.h>

int i;

void* foo(){
	for (int j = 0; j < 10000; ++j)
	{
		i++;
	}
	return NULL;
}

void* bar(){
	for (int j = 0; j < 10000; ++j)
	{
		i--;
	}
	return NULL;
}

int main(){
	i = 0;
    pthread_t first;
    pthread_create(&first, NULL, foo, NULL);

    pthread_t second;
    pthread_create(&second, NULL, bar, NULL);

    
    pthread_join(first, NULL);
    pthread_join(second, NULL);

    printf("%i \n", i);

    return 0;
    
}