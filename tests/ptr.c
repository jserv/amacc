int main()
{
  int *s, *e, v;

  s = (int*)0xbebebeb0;
  e = (int*)0xbebebeb4;
  v = e - s;
  if (v == 1)
    printf("passed\n");
  else
    printf("failed, e - s = %x\n", v);
  v = e - 1;
  if ((int)v == s)
    printf("passed\n");
  else
    printf("failed, e - s = %x\n", v);
}
