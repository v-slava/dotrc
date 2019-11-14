#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "core.h"
#include "graphics.h"

enum Line horizontalGrid[Y_CELLS_NUMBER - 1][X_CELLS_NUMBER];
enum Line verticalGrid[X_CELLS_NUMBER - 1][Y_CELLS_NUMBER];

enum CellColor gridCells[X_CELLS_NUMBER][Y_CELLS_NUMBER];


void drawStep()
{
    const int ms = 5;
    updateScreen();
    usleep(1000 * ms);
}

void setGridLines(enum Line value)
{
    int i, j;
    for (i = 0; i < Y_CELLS_NUMBER - 1; ++i)
        for (j = 0; j < X_CELLS_NUMBER; ++j)
            horizontalGrid[i][j] = value;
    for (i = 0; i < X_CELLS_NUMBER - 1; ++i)
        for (j = 0; j < Y_CELLS_NUMBER; ++j)
            verticalGrid[i][j] = value;
}

void setGridCells(enum CellColor value)
{
    int x, y;
    for (x = 0; x < X_CELLS_NUMBER; ++x)
        for (y = 0; y < Y_CELLS_NUMBER; ++y)
            gridCells[x][y] = value;
}

void generateLabyrinth()
{
    srand(time(NULL)); // initialize pseudo-random numbers generator with current time
    recursiveDivisionAlgorythm();
    /* myAlgorythm(); */
}
