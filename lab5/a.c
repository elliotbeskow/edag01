#include <stdio.h>

#define 	N 100

float		a[N];
float		b[N];
float		c[N];

int main()
{
	int	i;

	for (i = 0; i < N; i += 1)
		a[i] = b[i] + c[i];
}
