int main() {
    char minusculas[] = "abcdefghijklmnopqrstuvwxyz";
    char mayusculas[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    char numeros[] = "0123456789";
    char simbolos[] = "!@#$%^&*()-_=+[]{};:,.<>?";
    char caracteres[200] = "";
    char password[50];
    int longitud, i, index;
    char resp;

    srand(time(NULL));

    printf("Ingrese la longitud de la contraseña (8-16): ");
    scanf("%d", &longitud);

    strcat(caracteres, minusculas);

    printf("¿Incluir mayusculas? (s/n): ");
    scanf(" %c", &resp);
    if (resp == 's' || resp == 'S') strcat(caracteres, mayusculas);

    printf("¿Incluir numeros? (s/n): ");
    scanf(" %c", &resp);
    if (resp == 's' || resp == 'S') strcat(caracteres, numeros);

    printf("¿Incluir simbolos? (s/n): ");
    scanf(" %c", &resp);
    if (resp == 's' || resp == 'S') strcat(caracteres, simbolos);

    for (i = 0; i < longitud; i++) {
        index = rand() % strlen(caracteres);
        password[i] = caracteres[index];
    }
    password[longitud] = '\0';

    printf("Contraseña generada: %s\n", password);

    return 0;
}

