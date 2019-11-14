#include "core.h"
#include "graphics.h"

#include <stdlib.h>
#include <glib.h>

gpointer threadEntry()
{
    generateLabyrinth();
    updateScreen();
}

int main(void)
{
    g_thread_new(0, threadEntry, NULL);
    initGraphics();
    return EXIT_SUCCESS;
}
