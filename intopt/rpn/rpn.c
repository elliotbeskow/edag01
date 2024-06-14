#include <stdio.h>
#include <ctype.h>
#define N		(10)
int stack[N];
int top = -1;
int line = 1;

int skipline(int c) {
	while ((c != '\n') && (c != EOF)) {
		c = getchar();
	}
	top = -1;
	line++;
	return c;
}

int main(void)
{
	int c;
	do {
		c = getchar();
		if (isdigit(c)) {
			int num = 0;
			int ch = c;
			while (isdigit(ch)) {
				c = ch;
				num *= 10;
				num += c - '0';
				ch = getchar();
			}
			if (top == N-1) {
				printf("line %d: error at %c\n", line, c);
				c = skipline(ch);
				continue;
			}
			ungetc(ch, stdin);
			top++;
			stack[top] = num;
		//	printf("Added number to stack: %d at position %d\n", num, top);
		} else if (c == ' ') {
		} else if (c == '\n') {
			if (top == 0) {
				printf("line %d: %d\n", line, stack[0]);
			} else {
				printf("line %d: error at \\n\n", line);
			}
			line++;
			top = -1;
		} else if (c == '+' || c == '-' || c == '*' || c == '/') {
			if (top < 1) {
				printf("line %d: error at %c\n", line, c);
				c = skipline(c);
			} else {
				if (c == '+')
					stack[top-1] = stack[top-1] + stack[top];
				else if (c == '-')
					stack[top-1] = stack[top-1] - stack[top];
				else if (c == '*')
					stack[top-1] = stack[top-1] * stack[top];
				else if (c == '/') {
					if (stack[top] == 0) {
						printf("line %d: error at /\n", line);
						c = skipline(c);
						continue;
					}
					stack[top-1] = stack[top-1] / stack[top];
				}
				top--;
				//printf("top=%d, c=%c, stack[top]=%d\n", top, c, stack[top]);
			}
		} else if (c == EOF) {} else {
			printf("line %d: error at %c\n", line, c);
			c = skipline(c);
			top = -1;
		}
	} while (c != EOF);
}
