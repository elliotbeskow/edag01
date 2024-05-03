# include <stdio.h>
# include <stdlib.h>
# include <math.h>

const double eps = 1e-6;
int glob = 0;

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

int init(struct simplex_t *s, int m, int n, double **a, double *b, double *c, double *x, double y, int* var)
{
        glob += 1;
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
                s->var = malloc((m+n)*sizeof(int));
                for (i = 0; i<m+n; i++)
                        s->var[i] = i;
        }
        for (k = 0, i = 1; i < m; i++)
                if (b[i] < b[k])
                        k = i;
        return 0;
}

int select_nonbasic(struct simplex_t *s)
{
        int i;
        for (i = 0; i<s->n; i = i + 1)
                if (s->c[i] > eps)
                        return i;
        return -1;
}

int initial(struct simplex_t *s, int m, int n, double **a, double *b, double *c, double *x, double y, int *var)
{
        init(s, m, n, a, b, c, x, y, var);
        return 1;
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
        for (i=0; i < n; i = i + 1)
                if (i != row)
                        b[i] = b[i] - a[i][col] * b[row] / a[row][col];
        for (i = 0; i < m; i = i + 1)
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
        return s.y;
}

double simplex(int m, int n, double **a, double *b, double *c, double *x, double y)
{
        return xsimplex(m, n, a, b, c, x, y, NULL, 0);
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
        printf("Solution y=%lf\n", simplex(m, n, a, b, c, x, y));
        return 0;
}

