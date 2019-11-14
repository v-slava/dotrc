#ifndef LED_H
#define LED_H

typedef enum LedAction
{
    TOGGLE,
    ON,
    OFF
} LedAction;

int led(int led_number, LedAction action);

#endif
