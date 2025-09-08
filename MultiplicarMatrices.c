int main() {
    int A[2][2], B[2][2], C[2][2];
    int i,j,k;

    printf("Ingrese los elementos de la matriz A:\n");
    for(i=0;i<2;i++)
        for(j=0;j<2;j++)
            scanf("%d",&A[i][j]);

    printf("Ingrese los elementos de la matriz B:\n");
    for(i=0;i<2;i++)
        for(j=0;j<2;j++)
            scanf("%d",&B[i][j]);

    // multiplicación
    for(i=0;i<2;i++){
        for(j=0;j<2;j++){
            C[i][j] = 0;
            for(k=0;k<2;k++)
                C[i][j] += A[i][k]*B[k][j];
        }
    }

    printf("Matriz resultante:\n");
    for(i=0;i<2;i++){
        for(j=0;j<2;j++)
            printf("%d ", C[i][j]);
        printf("\n");
    }
    return 0;
}

