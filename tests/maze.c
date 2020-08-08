/* Maze generator in C.
 * Written by Joe Wingbermuehle
 * 1999-08-05
 * Sourced from https://raw.githubusercontent.com/joewing/maze/master/maze.c
 * Tweaked for AMaCC.
 */

#include <stdio.h>
#include <stdlib.h>

int maze_rand_v;
int maze_rand()
{
    return ((maze_rand_v = maze_rand_v * 214013 + 2531011) >> 16) & 0x7fff;
}

int maze_atoi(char *str, int radix)
{
    int v, sign;

    v = 0;
    sign = 1;
    if (*str == '-') {
        sign = -1;
        ++str;
    }
    while ((*str >= 'A' && *str <= 'Z') || (*str >= 'a' && *str <= 'z') ||
           (*str >= '0' && *str <= '9')) {
        v = v * radix +
            ((*str > '9') ? (*str & ~0x20) - 'A' + 10 : (*str - '0'));
        ++str;
    }
    return v * sign;
}

/* Display the maze. */
void show_maze(char *maze, int width, int height)
{
    int x, y;
    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {
            switch (maze[y * width + x]) {
            case 1:
                printf("[]");
                break;
            case 2:
                printf("<>");
                break;
            default:
                printf("  ");
                break;
            }
        }
        printf("\n");
    }
}

/*  Carve the maze starting at x, y. */
void carve_maze(char *maze, int width, int height, int x, int y)
{
    int x1, y1;
    int x2, y2;
    int dx, dy;
    int dir, count;

    dir = maze_rand() % 4;
    count = 0;
    while (count < 4) {
        dx = 0;
        dy = 0;
        switch (dir) {
        case 0:
            dx = 1;
            break;
        case 1:
            dy = 1;
            break;
        case 2:
            dx = -1;
            break;
        default:
            dy = -1;
            break;
        }
        x1 = x + dx;
        y1 = y + dy;
        x2 = x1 + dx;
        y2 = y1 + dy;
        if (x2 > 0 && x2 < width && y2 > 0 && y2 < height &&
            maze[y1 * width + x1] == 1 && maze[y2 * width + x2] == 1) {
            maze[y1 * width + x1] = 0;
            maze[y2 * width + x2] = 0;
            x = x2;
            y = y2;
            dir = maze_rand() % 4;
            count = 0;
        } else {
            dir = (dir + 1) % 4;
            count++;
        }
    }
}

/* Generate maze in matrix maze with size width, height. */
void generate_maze(char *maze, int width, int height)
{
    int x, y;

    /* Initialize the maze. */
    for (x = 0; x < width * height; x++)
        maze[x] = 1;
    maze[1 * width + 1] = 0;

    /* Carve the maze. */
    for (y = 1; y < height; y += 2)
        for (x = 1; x < width; x += 2)
            carve_maze(maze, width, height, x, y);

    /* Set up the entry and exit. */
    maze[0 * width + 1] = 0;
    maze[(height - 1) * width + (width - 2)] = 0;
}

/* Solve the maze. */
void solve_maze(char *maze, int width, int height)
{
    int dir, count;
    int x, y;
    int dx, dy;
    int forward;

    /* Remove the entry and exit. */
    maze[0 * width + 1] = 1;
    maze[(height - 1) * width + (width - 2)] = 1;

    forward = 1;
    dir = 0;
    count = 0;
    x = 1;
    y = 1;
    while (x != width - 2 || y != height - 2) {
        dx = 0;
        dy = 0;
        switch (dir) {
        case 0:
            dx = 1;
            break;
        case 1:
            dy = 1;
            break;
        case 2:
            dx = -1;
            break;
        default:
            dy = -1;
            break;
        }
        if ((forward && maze[(y + dy) * width + (x + dx)] == 0) ||
            (!forward && maze[(y + dy) * width + (x + dx)] == 2)) {
            maze[y * width + x] = forward ? 2 : 3;
            x = x + dx;
            y = y + dy;
            forward = 1;
            count = 0;
            dir = 0;
        } else {
            dir = (dir + 1) % 4;
            count = count + 1;
            if (count > 3) {
                forward = 0;
                count = 0;
            }
        }
    }

    /* Replace the entry and exit. */
    maze[(height - 2) * width + (width - 2)] = 2;
    maze[(height - 1) * width + (width - 2)] = 2;
}

enum { A_RANDV, A_WIDTH, A_HEIGHT, A_SOLVE };

int main(int argc, char **argv)
{
    int width, height, solve, mode, v;
    char *maze, *invocation;

    maze_rand_v = 6;  // chosen by fair dice roll, guaranteed to be random
    width = 10 * 2 + 3;
    height = 10 * 2 + 3;
    solve = 0;

    invocation = *argv;
    --argc;
    ++argv;
    mode = A_RANDV;
    while (argc > 0) {
        if (**argv == '-' && *(*argv + 1) == 'h') {
            printf("Usage: %s [seed] [width] [height] [s]\n", invocation);
            return 0;
        }
        v = maze_atoi(*argv, 10);
        if (**argv == 's' || **argv == '2')
            solve = 1;
        else if (mode == A_RANDV)
            maze_rand_v = v * 0xfffa;
        else if (mode == A_WIDTH)
            width = v * 2 + 3;
        else if (mode == A_HEIGHT)
            height = v * 2 + 3;
        else {
            printf("Unknown argument: '%s'\n", *argv);
            return 1;
        }
        ++mode;
        --argc;
        ++argv;
    }
    /* Get and validate the size. */
    if (width < 7)
        width = 7;
    if (height < 7)
        height = 7;

    /* Allocate the maze array. */
    maze = (char *) malloc(width * height * sizeof(char));
    if (maze == 0) {
        printf("error: not enough memory\n");
        exit(1);
    }

    /* Generate and display the maze. */
    generate_maze(maze, width, height);
    show_maze(maze, width, height);

    /* Solve the maze if requested. */
    if (solve) {
        solve_maze(maze, width, height);
        show_maze(maze, width, height);
    }

    /* Clean up. */
    free(maze);
    return 0;
}
