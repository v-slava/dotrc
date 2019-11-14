#include <gtk/gtk.h>
#include <cairo.h>
#include <stdlib.h>

#include "graphics.h"
#include "core.h"

static double cellWidth = CELL_WIDTH;
static double cellHeight = CELL_HEIGHT;

static int windowWidth = X_CELLS_NUMBER * CELL_WIDTH;
static int windowHeight = Y_CELLS_NUMBER * CELL_HEIGHT;

static GtkWidget *windowWidget = 0;

static void doDrawing(cairo_t *cr);

static gboolean onResizeEvent(GtkWidget *widget, GdkEvent *event, GtkWidget *window)
{
    gtk_window_get_size(GTK_WINDOW(windowWidget), &windowWidth, &windowHeight);
    cellWidth = (double)windowWidth / X_CELLS_NUMBER;
    cellHeight = (double)windowHeight / Y_CELLS_NUMBER;
    return TRUE;
}

static gboolean onDrawEvent(GtkWidget *widget, cairo_t *cr, gpointer userData)
{
    doDrawing(cr);
    return TRUE;
}

void initGraphics()
{
    gtk_init(NULL, NULL);
    windowWidget = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    GtkWidget *drawingAreaWidget = gtk_drawing_area_new();
    gtk_container_add(GTK_CONTAINER(windowWidget), drawingAreaWidget);
    g_signal_connect(G_OBJECT(drawingAreaWidget), "draw", G_CALLBACK(onDrawEvent), NULL);
    g_signal_connect(G_OBJECT(drawingAreaWidget), "configure_event", G_CALLBACK(onResizeEvent), NULL);
    g_signal_connect(windowWidget, "destroy", G_CALLBACK(gtk_main_quit), NULL);
    gtk_window_set_position(GTK_WINDOW(windowWidget), GTK_WIN_POS_CENTER);
    gtk_window_set_default_size(GTK_WINDOW(windowWidget), windowWidth, windowHeight);
    gtk_window_set_title(GTK_WINDOW(windowWidget), "Egor Volkov: labyrinth generation");
    gtk_widget_show_all(windowWidget);
    gtk_main();
}

void updateScreen()
{
    if (windowWidget)
        gtk_widget_queue_draw(windowWidget);
}

static void drawGridLines(cairo_t *cr)
{
    static const double gridIntensity = 0.7;
    cairo_set_source_rgb(cr, gridIntensity, gridIntensity, gridIntensity);
    int i;
    for (i = 1; i < Y_CELLS_NUMBER; ++i)
    {
        double y = i * cellHeight;
        cairo_move_to(cr, 0, y);
        cairo_line_to(cr, (double)windowWidth, y);
    }
    for(i = 1; i < X_CELLS_NUMBER; ++i)
    {
        double x = i * cellWidth;
        cairo_move_to(cr, x, 0);
        cairo_line_to(cr, x, (double)windowHeight);
    }
    cairo_stroke(cr);
}

static void drawLabyrinth(cairo_t *cr)
{
    static const double intensity = 0;
    cairo_set_source_rgb(cr, intensity, intensity, intensity);
    int sublineNumber, lineNumber;
    for(lineNumber = 1; lineNumber < Y_CELLS_NUMBER; ++lineNumber)
    {
        for (sublineNumber = 0; sublineNumber < X_CELLS_NUMBER; ++sublineNumber)
        {
            if (horizontalGrid[lineNumber - 1][sublineNumber] == EXISTS)
            {
                cairo_move_to(cr, sublineNumber * cellWidth, lineNumber * cellHeight);
                cairo_line_to(cr, (sublineNumber + 1) * cellWidth, lineNumber * cellHeight);
            }
        }
    }
    for (lineNumber = 1; lineNumber < X_CELLS_NUMBER; ++ lineNumber)
    {
        for (sublineNumber = 0; sublineNumber < Y_CELLS_NUMBER; ++ sublineNumber)
        {
            if (verticalGrid[lineNumber - 1][sublineNumber] == EXISTS)
            {
                cairo_move_to(cr, lineNumber * cellWidth, sublineNumber * cellHeight);
                cairo_line_to(cr, lineNumber * cellWidth, (sublineNumber + 1) * cellHeight);
            }
        }
    }
    cairo_stroke(cr);
}

static void drawCells(cairo_t *cr)
{
    int i, j;
    for (i = 0; i < X_CELLS_NUMBER; ++i)
        for (j = 0; j < Y_CELLS_NUMBER; ++j)
        {
            switch (gridCells[i][j])
            {
            case GRAY:
                cairo_set_source_rgb(cr, 0.9, 0.9, 0.9);
                break;
            case RED:
                cairo_set_source_rgb(cr, 1.0, 0.0, 0.0);
                break;
            case GREEN:
                cairo_set_source_rgb(cr, 0.0, 1.0, 0.0);
                break;
            case BLUE:
                cairo_set_source_rgb(cr, 0.0, 0.0, 1.0);
                break;
            default:
                fprintf(stderr, "Error: unexpected cell color (file: %s, line: %d)\n", __FILE__, __LINE__);
                exit(1);
            }
            cairo_rectangle(cr, i * cellWidth, j * cellHeight, cellWidth, cellHeight);
            cairo_fill(cr);
        }
}

static void doDrawing(cairo_t *cr)
{
    cairo_set_line_width(cr, 1);
    drawCells(cr);
    drawGridLines(cr);
    cairo_set_line_width(cr, 2);
    drawLabyrinth(cr);
}
