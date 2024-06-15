#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

#include "error.h"
#include "poly.h"

struct poly_t {
	int* coefficients;
	int* exponents;
	int size;
};

poly_t* new_poly_from_string(const char* s) {
	int c, j;
	int i = 0;
	int size = 1;
	do {
		if (s[i] == '+' || s[i] == '-')
			size++;
		i++;
	} while (s[i] != '\0');
	i = 0;
	j = 0;
	int* coefficients = calloc(size, sizeof(int));
	int* exponents = calloc(size, sizeof(int));
	int* coefficient = NULL;
	int* exponent = NULL;
	int sign = 1;
	do {
		c = s[i];
		if (isdigit(c)) {
			int num = 0;
			while (isdigit(c)) {
				num *= 10;
				num += c - '0';
				i++;
				c = s[i];
			}
			if (coefficient == NULL) {
				coefficient = calloc(1, sizeof(int));
				*coefficient = num;
			} else {
				exponent = calloc(1, sizeof(int));
				*exponent = num;
			}
			continue;
		} else if (c == '^') {
			if (coefficient == NULL) {
				coefficient = calloc(1, sizeof(int));
				*coefficient = 1;
			}
		} else if (c == '+' || c == '-') {
			coefficients[j] = sign*(*coefficient);
			if (exponent == NULL)
				exponents[j] = 1;
			else
				exponents[j] = *exponent;
			free(coefficient);
			free(exponent);
			coefficient = NULL;
			exponent = NULL;
			if (c == '+')
				sign = 1;
			else
				sign = -1;
			j++;
		}
		i++;
	} while (c != '\0');
	coefficients[j] = sign*(*coefficient);
	free(coefficient);
	if (exponent == NULL)
		exponents[j] = 0;
	else {
		exponents[j] = *exponent;
		free(exponent);
	}
	poly_t* p = calloc(1, sizeof(poly_t));
	p->exponents = exponents;
	p->coefficients = coefficients;
	p->size = size;
	return p;
}

void free_poly(poly_t* p) {
	free(p->coefficients);
	free(p->exponents);
	free(p);	
}

// poly_t* mul(poly_t* a, poly_t* b) {
// 	int i, j, k, l;
// 	int size = a->size * b->size; // max size
// 	int* coefficients = calloc(size, sizeof(int));
// 	int* exponents = calloc(size, sizeof(int));
// 	for (i=0; i<a->size; i++)
// 		for (j=0; j<b->size; j++) {
// 			k = i*a->size + j;
// 			int exponent = a->exponents[k] + b->exponents[k];
// 			int coefficient = a->coefficients[k] * b->coefficients[k];
// 			for(l=0; l<k; l++) {
// 				if (exponents[l] == exponent) {
// 					k = l;
// 					break;
// 				}
// 			}
// 			exponents[k] = exponent;
// 			coefficients[k] += coefficient;
// 		}
// }

int compare_ints(const void* a, const void* b) {
    return (*(int*)b - *(int*)a);
}

poly_t* mul(poly_t* p, poly_t* q) {
	int i = 0, j = 0, k = 0, l;
 	int max_size = p->size * q->size; // max size
 	int* exponents = calloc(max_size, sizeof(int));
	int exponent;
	for (i=0; i<p->size; i++) {
		for (j=0;j<q->size; j++) {
			exponent = p->exponents[i] + q->exponents[j];
			int seen = 0;
			for (l=0; l<k; l++) {
				if (exponents[l] == exponent) {
					seen = 1;
					break;
				}
			}
			if (!seen) {
				exponents[k] = exponent;
				k++;
			}
		}
	}
	int size = k;
	i = 0, j = 0, k = 0;
	exponents = realloc(exponents, size * sizeof(int));
	qsort(exponents, size, sizeof(int), compare_ints);
 	int* coefficients = calloc(size, sizeof(int));
	int coefficient;
	for (i=0; i<p->size; i++) {
		for (j=0; j<q->size; j++) {
			exponent = p->exponents[i] + q->exponents[j];
			coefficient = p->coefficients[i] * q->coefficients[j];
			for (l=0; l<size; l++) {
				if (exponents[l] == exponent) {
					k = l;
					break;
				}
			}
			coefficients[k] += coefficient;
		}
	}
 	poly_t* r = malloc(sizeof(poly_t));
 	r->coefficients = coefficients;
 	r->exponents = exponents;
 	r->size = size;
 	return r;
}

void print_poly(poly_t* p) {
	int i;
	for (i=0; i<p->size; i++) {
		int coeff = p->coefficients[i];
		int exp = p->exponents[i];
		if (coeff!=0) {
			if (i>0) {
				if (coeff > 0) {
					printf(" + ");
				} else {
					printf(" - ");
				}
			}
			if (coeff != 1 || exp == 0)
				printf("%d", abs(coeff));
			if (exp==1) {
				printf("x");
			} else if (exp > 1) {
				printf("x^%d", exp);
			}
		}
	}	
	printf("\n");
}
