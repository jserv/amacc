struct s2 {
    int x, y;
};

struct s3 {
    int x, y, z;
};

union pt {
    struct s2 plane;
    struct s3 space;
};

int main()
{
    union pt *p;
    int i;

    p = malloc(4 * sizeof(union pt));

    for (i = 0; i < 4; ++i) {
        p[i].space.x = i;
        p[i].space.y = i + 1;
        p[i].space.z = 4 - i;
        printf("(%d, %d, %d)\n", p[i].space.x, p[i].space.y, p[i].space.z);
    }

    for (i = 0; i < 4; ++i) {
        printf("(%d, %d)\n", p[i].plane.x, p[i].plane.y);
    }

    return 0;
}
