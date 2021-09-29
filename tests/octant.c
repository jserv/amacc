/* generate points for octant of a circle */
void octant(int radius)
{
    int x = radius;
    int y = 0;
    int d = 1 - radius;
    while (x>=y) {
        printf("(%d, %d)\n", x, y);
        ++y;
        if (d<0) d += y*2+1;
        else { --x; d += (y-x)*2+1; }
    }
}

int main()
{
   octant(100);
   return 0;
}
