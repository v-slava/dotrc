// include guards:
#ifndef CORE_H
#define CORE_H

#define X_CELLS_NUMBER 30
#define Y_CELLS_NUMBER 30

enum Line
{
    NOT_EXISTS,
    EXISTS
};

extern enum Line horizontalGrid[Y_CELLS_NUMBER - 1][X_CELLS_NUMBER];
extern enum Line verticalGrid[X_CELLS_NUMBER - 1][Y_CELLS_NUMBER];

enum CellColor
{
    GRAY,
    RED,
    GREEN,
    BLUE
};
extern enum CellColor gridCells[X_CELLS_NUMBER][Y_CELLS_NUMBER];

void generateLabyrinth();
void drawStep();
void setGridLines(enum Line value);
void setGridCells(enum CellColor value);

#endif
