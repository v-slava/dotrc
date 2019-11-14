#include "led.h"

/*
 * Change led state.
 * @param led_number IN - led number to turn on.
 *     Can be [12; 15] (12 for D12, ...).
 * @param action IN - action to execute: {TOGGLE, ON, OFF}.
 * @returns 0 on success, 1 on error.
 */
int led(int led_number, LedAction action)
{
    if ((led_number < 12) || (led_number > 15))
        return 1;

    volatile unsigned char* gpio_ba = (unsigned char*)0x11400000;
    volatile unsigned int* GPY6CON = (unsigned int*)(gpio_ba + 0x0260);
    volatile unsigned char* GPY6DAT = gpio_ba + 0x0264;
    volatile unsigned short* GPY6PUD = (unsigned short*)(gpio_ba + 0x0268);

    // GPY6CON[7:4] = output:
    *GPY6CON = 0x11110000;

    // Disable Pull-up/down:
    *GPY6PUD = 0x0055;

    // Change LED state:
    unsigned char constant;

    if (led_number == 15)
        constant = 0x10;
    else
        constant = 1 << (led_number - 7);

    switch (action)
    {
        case TOGGLE:
            *GPY6DAT ^= constant;
            break;
        case ON:
            *GPY6DAT |= constant;
            break;
        case OFF:
            *GPY6DAT &= ~constant;
    }
    return 0;
}
