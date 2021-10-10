int main()
{
   int fp;
   char *value = "2021";
   char *scratch = "this is a string!" ;
   char *fname = "/this/long/directory/name.c";
   char *PATH = getenv("PATH") ;

   printf("%d\n", strlen(scratch));
   printf("%d\n", abs(-3));
   printf("%d\n", atoi(value));
   printf(basename(fname));
   printf("\n");
   printf(PATH);
   printf("\n");

   return 0;
}
