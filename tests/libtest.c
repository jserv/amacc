
int main()
{
    char *PATH = getenv("PATH");
    char *addPath = getenv("HOME");
    if (!strstr(PATH, addPath)) {
        int bytes = strlen(PATH) + strlen(addPath) + 2 ;
        char *newPATH = (char *) malloc(bytes);
        strcpy(newPATH, PATH);
        strcat(newPATH, ":");
        strcat(newPATH, addPath);
        printf("%s\n", newPATH);
    }

    return 0;
}
