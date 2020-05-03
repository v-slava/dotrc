// gcc -lEGL version.c

#include <stdio.h>
#include <EGL/egl.h>

int main(void) {
	EGLDisplay eglDisplay = eglGetDisplay(EGL_DEFAULT_DISPLAY);
	EGLint major, minor;
	EGLBoolean eglInitialized = eglInitialize(eglDisplay, &major, &minor);
	printf("%d %d %d\n", major, minor, eglInitialized);
	return 0;
}

