/* generate points for quadrant of a circle */
/* fixed point arithmetic with three bit fraction */

int *pool;
int top;
char **frac;

// stack for local array vars
int *adj(int n)
{
    int *retVal = &pool[top];
    top += n;
    return retVal;
}

int hull_area(int *x, int *y)
{
    int area = ((x[0] * y[1] + x[1] * y[2] + x[2] * y[3] + x[3] * y[0]) -
                (x[1] * y[0] + x[2] * y[1] + x[3] * y[2] + x[0] * y[3]));
    return ((area < 0) ? -area : area) / 2;
}

void cubic_bezier(int *px, int *py, int threshold)
{
    if (hull_area(px, py) < threshold) {
        int i, lx, ly;
        for (i = 0; i < 4; ++i) {
            if (lx != px[i] || ly != py[i]) {
                lx = px[i];
                ly = py[i];
                printf("(%d.%s, %d.%s)\n", lx / 8, frac[lx % 8], ly / 8,
                       frac[ly % 8]);
            }
        }
        return;
    }

    int tx, ty;
    int *x, *y;

    x = adj(7);
    y = adj(7);

    tx = (px[1] + px[2]) / 2;
    ty = (py[1] + py[2]) / 2;

    x[0] = px[0];
    y[0] = py[0];
    x[6] = px[3];
    y[6] = py[3];

    x[1] = (px[0] + px[1]) / 2;
    y[1] = (py[0] + py[1]) / 2;
    x[5] = (px[2] + px[3]) / 2;
    y[5] = (py[2] + py[3]) / 2;

    x[2] = (x[1] + tx) / 2;
    y[2] = (y[1] + ty) / 2;
    x[4] = (tx + x[5]) / 2;
    y[4] = (ty + y[5]) / 2;

    x[3] = (x[2] + x[4]) / 2;
    y[3] = (y[2] + y[4]) / 2;

    cubic_bezier(x, y, threshold);
    cubic_bezier(&x[3], &y[3], threshold);

    adj(-14);
}

int main()
{
    int *px, *py;

    frac = malloc(8 * sizeof(char *));
    frac[0] = "000";
    frac[1] = "125";
    frac[2] = "250";
    frac[3] = "375";
    frac[4] = "500";
    frac[5] = "625";
    frac[6] = "750";
    frac[7] = "875";

    pool = malloc(10000 * sizeof(int));
    top = 0;

    px = adj(4);
    py = adj(4);

    px[0] = 0 * 8;
    py[0] = 100 * 8;
    px[1] = 55 * 8;
    py[1] = 100 * 8;
    px[2] = 100 * 8;
    py[2] = 55 * 8;
    px[3] = 100 * 8;
    py[3] = 0 * 8;

    cubic_bezier(px, py, 1 * 128);

    adj(-8);

    free(pool);
    free(frac);

    return 0;
}
