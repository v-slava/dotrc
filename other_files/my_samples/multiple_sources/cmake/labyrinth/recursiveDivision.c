#include "core.h"

static void iteration(int Xleft, int Ytop, int Xright, int Ybottom);

static void splitByVerticalLine(int Xleft, int Ytop, int Xright, int Ybottom)
{
    int i, randomNumberX = rand() % (Xright - Xleft - 1) + Xleft;
    for (i = Ytop; i < Ybottom; ++i)
        verticalGrid[randomNumberX][i] = EXISTS;
    drawStep();
    verticalGrid[randomNumberX][rand() % (Ybottom - Ytop) + Ytop] = NOT_EXISTS;
    drawStep();
    iteration(Xleft, Ytop, randomNumberX + 1, Ybottom); // recursive call for left-side subarea
    iteration(randomNumberX + 1, Ytop, Xright, Ybottom); // recursive call for right-side subarea
}

static void splitByHorizontalLine(int Xleft, int Ytop, int Xright, int Ybottom)
{
    int i, randomNumberY = rand() % (Ybottom - Ytop - 1) + Ytop;
    for (i = Xleft; i < Xright; ++i)
        horizontalGrid[randomNumberY][i] = EXISTS;
    drawStep();
    horizontalGrid[randomNumberY][rand() % (Xright - Xleft) + Xleft] = NOT_EXISTS;
    drawStep();
    iteration(Xleft, Ytop, Xright, randomNumberY + 1); // recursive call for up-side subarea
    iteration(Xleft, randomNumberY + 1, Xright, Ybottom); // recursive call for down-side subarea
}

static void iteration(int Xleft, int Ytop, int Xright, int Ybottom)
{
    if ((Xright - Xleft == 1) || (Ybottom - Ytop == 1))
        return;
    if (Xright - Xleft > Ybottom - Ytop)
        splitByVerticalLine(Xleft, Ytop, Xright, Ybottom);
    else
    {
        if (Xright - Xleft < Ybottom - Ytop)
            splitByHorizontalLine(Xleft, Ytop, Xright, Ybottom);
        else // if (Xright - Xleft == Ybottom - Ytop)
        {
            if (rand() % 2 == 0)
                splitByHorizontalLine(Xleft, Ytop, Xright, Ybottom);
            else
                splitByVerticalLine(Xleft, Ytop, Xright, Ybottom);
        }
    }
}

void recursiveDivisionAlgorythm()
{
    setGridLines(NOT_EXISTS);
    setGridCells(GRAY);
    iteration(0, 0, X_CELLS_NUMBER, Y_CELLS_NUMBER);
}
