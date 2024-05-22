#include <bits/types/sigset_t.h>
#include <endian.h>
# include <stdio.h>
# include <stdlib.h>
# include <math.h>

#define eps 1e-6


struct simplex_t
{
	int     m;
	int     n;
	int     *var;
	double  **a;
	double  *b;
	double  *x;
	double  *c;
	double  y;
};

struct node_t
{
	int m;
	int n;
	int k;
	int h;
	double xh;
	double ak;
	double bk;
	double *min;
	double *max;
	double **a;
	double *b;
	double *x;
	double *c;
	double z;
};

struct set_t
{
	int size;
	struct node_t **nodes;
};

int initial(struct simplex_t *s, int m, int n, double **a, double *b, double *c, double *x, double y, int *var);
void pop(struct set_t *h);
int init(struct simplex_t *s, int m, int n, double **a, double *b, double *c, double *x, double y, int* var)
{
	int i, k;
	s->m = m;
	s->n = n;
	s->a = a;
	s->b = b;
	s->c = c;
	s->x = x;
	s->y = y;
	s->var = var; 
	if (s->var == NULL)
	{
		s->var = malloc((m+n+1)*sizeof(int));
		for (i = 0; i<m+n; i++)
			s->var[i] = i;
	}
	for (k = 0, i = 1; i < m; i++)
		if (b[i] < b[k])
			k = i;
	return k;
}

int select_nonbasic(struct simplex_t *s)
{
	int i;
	for (i = 0; i<s->n; i = i + 1)
		if (s->c[i] > eps)
			return i;
	return -1;
}

void pivot(struct simplex_t *s, int row, int col)
{
	double **a = s->a;
	double *b = s->b;
	double *c = s->c;
	int m = s->m;
	int n = s->n;
	int i, j, t;
	t = s->var[col];
	s->var[col] = s->var[n + row];
	s->var[n + row] = t;
	s->y = s->y + c[col] * b[row] / a[row][col];
	for (i = 0; i < n; i++)
		if (i != col)
			c[i] = c[i] - c[col] * a[row][i] / a[row][col];
	c[col] = -c[col]/a[row][col];
	for (i=0; i < m; i = i + 1)
		if (i != row)
			b[i] = b[i] - a[i][col] * b[row] / a[row][col];
	for (i = 0; i < m; i++)
		if (i != row)
			for (j = 0; j < n; j++)
				if (j != col)
					a[i][j] = a[i][j] - a[i][col] * a[row][j] / a[row][col];
	for (i=0; i<m; i++)
		if (i != row)
			a[i][col] = -a[i][col]/a[row][col];
	for (i = 0; i < n; i++)
		if (i !=col)
			a[row][i] = a[row][i] / a[row][col];
	b[row] = b[row] / a[row][col];
	a[row][col] = 1 / a[row][col];
}

void prepare(struct simplex_t *s, int k)
{
	int m = s->m;
	int n = s->n;
	int i;
	// make room for x_(m+n) at s.var[n] by moving s.var[n, ..., n+m-1] one step to the right.
	for (i = m + n; i > n; i = i - 1)
		s->var[i] = s->var[i-1];
	s->var[n] = m + n;
	// add x_(m+n) to each constraint
	n += 1;
	for (i = 0; i < m; i = i + 1)
		s->a[i][n-1] = -1;
	s->x = calloc(m + n, sizeof(double));
	s->c = calloc(n, sizeof(double));
	s->c[n-1] = -1;
	s->n = n;
	pivot(s, k, n-1);
}


double xsimplex(int m, int n, double **a, double *b, double *c, double *x, double y, int *var, int h)
{
	struct simplex_t s;
	int i, row, col;
	if (!initial(&s, m, n, a, b, c, x, y, var)) {
		free(s.var);
		s.var = NULL;
		return NAN;
	}
	while ((col = select_nonbasic(&s)) >= 0) {
		row = -1;
		for (i = 0; i < m; i = i + 1)
			if ((a[i][col] > eps) && ((row < 0) || (b[i]/a[i][col]<b[row]/a[row][col])))
				row = i;
		if (row < 0)
		{
			free(s.var);
			s.var = NULL;
			return INFINITY;
		}
		pivot(&s, row, col);
	}
	if (h == 0)
	{
		for (i = 0; i < n; i++)
			if (s.var[i] < n)
				x[s.var[i]] = 0;
		for (i = 0; i < m; i++)
			if (s.var[i+n] < n)
				x[s.var[n+i]] = s.b[i];
		free(s.var);
		s.var = NULL;
	}
	else {
		for (i = 0; i < n; i = i + 1)
			x[i] = 0;
		for (i = n; i< n + m; i = i + 1)
			x[i] = s.b[i-n];
	}
	//printf("%lf %lf", x[0], x[1]);
	//printf("%lf %lf", s.x[0], s.x[1]);
	return s.y;
}

int initial(struct simplex_t *s, int m, int n, double **a, double *b, double *c, double *x, double y, int *var)
{
	int i, j, k;
	double w;
	k = init(s, m, n, a, b, c, x, y, var);
	if (b[k] >= 0)
		return 1;
	prepare(s, k);
	n = s->n;
	s->y = xsimplex(m, n, s->a, s->b, s->c, s->x, 0, s->var, 1);
	for (i=0; i < m + n; i++)
		if (s->var[i] == m + n - 1){
			if (fabs(s->x[i])>eps) {
				free(s->x);
				s->x = NULL;
				free(s->c);
				s->c = NULL;
				return 0;
			} else {
				break;
			}
		}
	if (i >= n) {
		// x[n+m] is basic. find good nonbasic.
		for (j = k = 0; k < n; k++)
			if (fabs(s->a[i-n][k]) > fabs(s->a[i-n][j])) {
				j = k;
			}
		pivot(s, i-n, j);
		i = j;
	}
	if (i<n-1) {
		// x[n+m] is nonbasic and not last. swap columns i and n-1
		k = s->var[i]; s->var[i] = s->var[n-1]; s->var[n-1] = k;
		for (k = 0; k < m; k = k + 1) {
			w = s->a[k][n-1]; s->a[k][n-1] = s->a[k][i]; s->a[k][i] = w;
		}
	}
	free(s->c);
	s->c = c;
	s->y = y;
	for (k = n-1; k < n + m - 1; k++)
		s->var[k] = s->var[k+1];
	n = s->n = s->n - 1;
	double *t = calloc(n, sizeof(double));
	for (k = 0; k < n; k++) {
		for (j = 0; j < n; j++) {
			if (k ==s->var[j]) {
				// x[k] is nonbasic, add c[k]
				t[j] = t[j] + s->c[k];
				goto next_k;
			}
		}
		for (j = 0; j < m; j++) {
			if (s->var[n+j] == k) break;

		}
		s->y = s->y + s->c[k] * s->b[j];
		for (i = 0; i < n; i++)
			t[i] = t[i] - s->c[k] * s->a[j][i];
next_k:;
	}
	for (i=0; i < n; i++)
		s->c[i] = t[i];
	free(t);
	free(s->x);
	t = NULL;
	s->x = NULL;
	return 1;
}

double simplex(int m, int n, double **a, double *b, double *c, double *x, double y)
{
	return xsimplex(m, n, a, b, c, x, y, NULL, 0);
}

struct node_t *initial_node(int m, int n, double **a, double *b, double *c)
{
	int i, j;
	struct node_t *p = calloc(1, sizeof(struct node_t));
	p->a = calloc(m + 1, sizeof(double*));
	for (i=0; i<m+1; i++) {
		p->a[i] = calloc(n+1, sizeof(double));
	} 
	p->b = calloc(m + 1, sizeof(double));
	p->c = calloc(n + 1, sizeof(double));
	p->x = calloc(m + n + 1, sizeof(double));
	p->min = calloc(n, sizeof(double));
	p->max = calloc(n, sizeof(double));
	p->m = m;
	p->n = n;
	// it might be that the following loops should go to i<n, i<m.
	for (i = 0; i<n; i++) {
		for (j = 0; j<m; j++)
			p->a[j][i] = a[j][i];
		p->c[i] = c[i];
	}
	for (i = 0; i<m; i++)
		p->b[i] = b[i];
	for (i=0; i < n; i++)
	{
		p->min[i] = -INFINITY; 
		p->max[i] = INFINITY;
	}
	return p;
}

struct node_t *extend(struct node_t *p, int m, int n, double **a, double *b, double *c, int k, double ak, double bk)
{
	struct node_t *q = calloc(1, sizeof(struct node_t));
	int i, j;
	q->k = k;
	q->ak = ak;
	q->bk = bk;
	if (ak > 0 && p->max[k] < INFINITY)
		q->m = p->m;
	else if (ak < 0 && p->min[k] > 0)
		q->m = p->m;
	else
		q->m = p->m + 1;
	q->n = p->n;
	q->h = -1;
	q->a = calloc(q->m + 1, sizeof(double*));
	for (i=0; i < q->m + 1; i++) {
		q->a[i] = calloc(q->n + 1, sizeof(double));
	}
	q->b = calloc(q->m + 1, sizeof(double));
	q->c = calloc(q->n + 1, sizeof(double));
	q->x = calloc(q->n + 1, sizeof(double));
	q->min = calloc(n, sizeof(double));
	q->max = calloc(n, sizeof(double));
	for (i = 0; i<n; i++) {
		q->min[i] = p->min[i];
		q->max[i] = p->max[i];
		q->c[i] = c[i];
	}
	for (i=0; i<m; i++) {
		for (j=0; j<n; j++)
			q->a[i][j] = a[i][j];
		q->b[i] = b[i];
	}
	if (ak > 0) {
		if ((q->max[k] == INFINITY) || (bk < q->max[k]))
			q->max[k] = bk;
	} else if ((q->min[k] == -INFINITY) || (-bk > q->min[k])) {
		q->min[k] = -bk;
	}
	for (i = m, j = 0; j < n; j++) {
		if (q->min[j] > -INFINITY) {
			q->a[i][j] = -1;
			q->b[i] = -q->min[j];
			i++;
		}
		if (q->max[j] < INFINITY) {
			q->a[i][j] = 1;
			q->b[i] = q->max[j];
			i++;
		}
	}
	return q;
}

int is_integer(double *xp)
{
	double x = *xp;
	double r = lround(x);
	if (fabs(r-x) < eps) {
		*xp = r;
		return 1;
	}
	return 0;
}

int integer(struct node_t *p)
{
	for (int i = 0; i<p->n; i++)
		if (!is_integer(&(p->x[i])))
			return 0;
	return 1;
}

void free_node(struct node_t *p)
{
	if (p->a!=NULL){
		for (int i = 0; i < p->m+1; i++) {
			free(p->a[i]);
		}
		free(p->a);
	}
	if (p->b!=NULL)
		free(p->b);
	if (p->c!=NULL)
		free(p->c);
	if (p->x!=NULL)
		free(p->x);
	free(p->min);
	free(p->max);
	free(p);
}

void bound(struct node_t *p, struct set_t *h, double *zp, double *x, int n)
{
	int i;
	if (p->z > *zp) {
		*zp = p->z;
		for (i = 0; i < n; i++)
			x[i] = p->x[i];
		for (i = 0; i < h->size; i++) {
			if (h->nodes[i]->z < p->z) {
				free_node(h->nodes[i]);
				for (int j = i; j<h->size-1; j++)
					h->nodes[j] = h->nodes[j+1];
        h->size--;
        h->nodes = realloc(h->nodes, h-> size * sizeof(struct node_t *));
				//free(h->nodes[h->size]);
			} 
		}
	}
}

int branch(struct node_t *q, double z) {
	double min, max;
	if (q->z < z)
		return 0;
	for (int h=0; h<q->n; h = h + 1) {
		if (!is_integer(&(q->x[h]))) {
			if (q->min[h] == -INFINITY)
				min = 0;
			else
				min = q->min[h];
			max = q->max[h];
			if (floor(q->x[h]) < min || ceil(q->x[h]) > max)
				continue;
			q->h = h;
			q->xh = q->x[h];
			for (int i=0; i<q->n+1; i++) {
				free(q->a[i]);
			}
			free(q->a);
			free(q->b);
			free(q->c);
			free(q->x);
			q->a = NULL;
			q->b = NULL;
			q->c = NULL;
			q->x = NULL;
			return 1;
		}
	}
	return 0;
}

void succ(struct node_t *p, struct set_t *h, int m, int n, double **a, double *b, double *c, int k, double ak, double bk, double *zp, double *x) {
	struct node_t *q = extend(p, m, n, a, b, c, k, ak, bk);
	if (q==NULL)
		return;
	q->z = simplex(q->m, q->n, q->a, q->b, q->c, q->x, 0);
	if (isfinite(q->z)) {
		if(integer(q)){
			bound(q, h, zp, x, n);
		} else if (branch(q, *zp)) {
			h->size++;
			struct node_t **new_array = calloc(h->size, sizeof(struct node_t*));
			for (int i = 1; i<h->size; i++) {
				new_array[i] = h->nodes[i-1];
			}
			new_array[0] = q;
			//free(h->nodes);
			h->nodes = new_array;
			//free(new_array);
			return;
		}
	}
	free_node(q);
	q = NULL;
}

double intopt(int m, int n, double **a, double *b, double *c, double *x) {
	struct node_t *p = initial_node(m, n, a, b, c);
	struct set_t h = {1,&p};
	double z = -INFINITY;
	p->z = simplex(p->m, p->n, p->a, p->b, p->c, p->x, 0);
	if (integer(p) || !isfinite(p->z)) {
		z = p->z;
		if (integer(p)) {
			for (int i=0; i<n; i++)
				x[i] = p->x[i];
		}
		free_node(p);
		return z;
	}
	branch(p, z);
	while (h.size) {
		//printf("z = %lf \n", z);
		struct node_t *q = h.nodes[h.size-1];
    h.size--;
    if (h.size > 0) {
      h.nodes = realloc(h.nodes, h. size * sizeof(struct node_t *));
    }
    
		succ(q, &h, m, n, a, b, c, q->h, 1, floor(q->xh), &z, x);
		succ(q, &h, m, n, a, b, c, q->h, -1, -ceil(q->xh), &z, x);
    free_node(q);
	}
	if (z == -INFINITY)
		return NAN;
	else
		return z;
}

int main(void)
{
	int m;
	int n;
	double** a;
	double* b;
	double* c;
	scanf("%d %d", &m, &n);
	a = calloc(m, sizeof(double*));
	for(int i=0; i<m; i+=1)
		a[i] = calloc(n, sizeof(double));
	b = calloc(m, sizeof(double));
	c = calloc(n, sizeof(double));
	for(int i=0; i<n; i+=1)
		scanf("%lf", &c[i]);
	for(int i=0; i<m; i+=1)
	{
		for(int j=0; j<n; j+=1)
			scanf("%lf", &a[i][j]);
	}
	for(int i=0; i<m; i+=1)
		scanf("%lf", &b[i]);
	printf("LINEAR PROGRAM (m=%d n=%d)\n", m, n); 
	printf("--------------------------\n\n");
	printf("max z = %lfx_0", c[0]);
	for(int i=1; i<n; i+=1)
		printf(" + %lfx_%d", c[i], i);
	printf("\n");
	for(int i=0; i<m; i+=1)
	{
		printf("%lfx_0", a[i][0]);
		for (int j=1; j<n; j+=1)
			printf(" + %lfx_%d", a[i][j], j);
		printf("<= %lf\n", b[i]);            
	}
	printf("x_0, ..., x_%d >= 0\n", n-1);
	double *x = calloc(n, sizeof(double));
	for (int i=0; i<n; i++)
		x[i] = 0;
	double y = 0;       
	printf("Solution y=%lf\n", intopt(m, n, a, b, c, x));
	for (int i=0; i<n; i++)
		printf("%lf, ", x[i]);
	for(int i=0; i<m; i+=1)
		free(a[i]);
	free(a); free(b); free(c); free(x);
	return 0;
}

