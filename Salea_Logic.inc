
;       *** 

    macro make_pulse 

    xor     A
    set     \1,A    
    out (gpio_out),A
    out (gpio_out),A
    out (gpio_out),A
    out (gpio_out),A
    xor     A
    out (gpio_out),A
    

    endm