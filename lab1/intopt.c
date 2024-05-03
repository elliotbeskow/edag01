# include <stdio.h>
# include <stdlib.h>

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
        c = calloc(n-1, sizeof(double));
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
        printf("--------------------------\n");
        printf("max z = %5.2lfx_0", c[0]);
        for(int i=1; i<n; i+=1)
                printf(" + %5.2lfx_%d", c[i], i);
        printf("\ns.t.    ");
        for(int i=0; i<m; i+=1)
        {
                if (i>0)
                        printf("        ");
                printf("%5.2lfx_0", a[i][0]);
                for (int j=1; j<n; j+=1)
                        printf(" + %5.2lfx_%d", a[i][j], j);
                printf(" <= %5.2lf\n", b[i]);                
        }
        printf("x_0, ..., x_%d >= 0\n", n-1);
        return 0;
}

